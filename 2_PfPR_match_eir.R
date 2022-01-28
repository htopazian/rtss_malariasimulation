library(didehpc)
setwd('Q:/rtss_malariasimulation/')

options(didehpc.cluster = "fi--didemrchnb",
        didehpc.username = "htopazia")

source('./1_functions.R')

# remotes::install_github('mrc-ide/malariasimulation@test/severe_demography', force=T)
src <- conan::conan_sources("github::mrc-ide/malariasimulation@dev")

ctx <- context::context_save(path = "Q:/contexts",
                             sources = c('./1_functions.R'),
                             packages = c("tidyverse", "malariasimulation"),
                             package_sources = src)

share <- didehpc::path_mapping('Home drive', "Q:", '//fi--san03.dide.ic.ac.uk/homes/htopazia', "M:")
config <- didehpc::didehpc_config(shares = share,
                                  use_rrq = FALSE,
                                  cores = 1,
                                  cluster = "fi--didemrchnb",
                                  parallel = FALSE)

obj <- didehpc::queue_didehpc(ctx, config = config)


# Now set up your job ---------------------------------------------------------
library(tidyverse)
year <- 365

# FIRST
seas_name <- 'highly seasonal'
seasonality <- list(c(0.284596,-0.317878,-0.0017527,0.116455,-0.331361,0.293128,-0.0617547))
s1 <- crossing(seasonality, seas_name)

# SECOND
seas_name <- 'seasonal'
seasonality <- list(c(0.285505,-0.325352,-0.0109352,0.0779865,-0.132815,0.104675,-0.013919))
s2 <- crossing(seasonality, seas_name)

stable <- rbind(s1, s2)

# loop over malariasimulation runs
init_EIR <- c(seq(1,9,1), seq(10, 80, by=2)) # set EIR values
combo <- crossing(stable, init_EIR) %>% mutate(name = paste0('EIR', "_", row_number()))


# Run tasks -------------------------------------------------------------------
combo <- combo %>% mutate(f = paste0("./rds/HPC/PR/",combo$name,".rds")) %>%
  mutate(exist=case_when(file.exists(f) ~ 1, !file.exists(f) ~ 0)) %>%
  filter(exist==0) %>%
  select(-f, -exist)

t <- obj$enqueue_bulk(combo, PRmatch)
t$status()


# RESULTS ----------------------------------------------------------------------
library(data.table)
library(fuzzyjoin)
files <- list.files(path = "./rds/HPC/PR", pattern = "*.rds", full.names = TRUE)
dat_list <- lapply(files, function (x) readRDS(x))
EIR_prev <-  do.call("rbind", dat_list) %>% as_tibble() %>%
  mutate(init_EIR = as.numeric(init_EIR),
         prev = as.numeric(prev))

p <- ggplot(data=EIR_prev) +
  geom_point(aes(x=init_EIR, y=prev), color='black') +
  stat_smooth(aes(x=init_EIR, y=prev), color='red', method = 'gam', n=1000) +
  labs(x='initial EIR', y=expression(paste(italic(Pf),"PR"[2-10]))) +
  facet_wrap('seas_name', nrow=6, ncol=4) +
  theme_classic()

p


# MATCH EIR AND PfPR -----------------------------------------------------------
# extract points from stat_smooth
p2 <- ggplot_build(p)
p2 <- p2$data[[2]]

pnames <- EIR_prev %>% select(seas_name) %>% distinct() %>% arrange(seas_name) %>% mutate(PANEL=row_number())

p2 <- p2 %>% as_tibble() %>% select(x,y,PANEL) %>% rename(init_EIR=x, pred=y) %>%
  mutate(PANEL = as.numeric(PANEL)) %>%
  left_join(pnames, by='PANEL')

# Pre-intervention baseline PfPR2-10
PfPR <- as_tibble_col(c(.10, .25, .35, .55), column_name="pfpr")


# match via stat_smooth predictions
match <- PfPR %>%
  fuzzyjoin::difference_left_join(p2, by=c("pfpr"="pred"),
                                  max_dist=1, distance_col="dist") %>%
  group_by(pfpr, seas_name) %>% slice_min(dist)

match <- match %>% rename(starting_EIR = init_EIR) %>%
  select(pfpr, starting_EIR, seas_name)

saveRDS(match, "./rds/PR_EIRmatch.rds")


# test w/ malariasimulation run ------------------------------------------------
seas_name <- 'highly seasonal'
seasonality <- list(c(0.284596,-0.317878,-0.0017527,0.116455,-0.331361,0.293128,-0.0617547))
init_EIR <- 20

  year <- 365
  month <- year/12
  human_population <- 10000
  
  params <- get_parameters(list(
    human_population = human_population,
    model_seasonality = TRUE,   # assign seasonality
    g0 = unlist(seasonality)[1],
    g = unlist(seasonality)[2:4],
    h = unlist(seasonality)[5:7],
    prevalence_rendering_min_ages = 2 * year,
    prevalence_rendering_max_ages = 10 * year,
    individual_mosquitoes = FALSE))
  
  flat_demog <- read.table('./Flat_demog.txt') # from mlgts
  ages <- round(flat_demog$V3 * year) # top of age bracket
  deathrates <- flat_demog$V5 / 365 # age-specific death rates
  params <- set_demography(
    params,
    agegroups = ages,
    timesteps = 1,
    deathrates = matrix(deathrates, nrow = 1),
    birthrates = find_birthrates(human_population, ages, deathrates)
  )
  
  params <- set_species(params, species = list(arab_params, fun_params, gamb_params),
                        proportions = c(0.25, 0.25, 0.5))
  
  params <- set_drugs(params, list(AL_params, SP_AQ_params))
  params <- set_clinical_treatment(params, 1, c(1), c(0.45))
  
  params <- set_equilibrium(params, as.numeric(init_EIR))
  
  output <- run_simulation(
    timesteps = 10 * year,
    parameters = params,
    correlations = NULL)
  
  prev <-
    mean(
      output[
        output$timestep %in% seq(9 * 365, 10 * 365),
        'n_detect_730_3650'
      ] / output[
        output$timestep %in% seq(9 * 365, 10 * 365),
        'n_730_3650'
      ]
    )
  

output %>% filter(timestep %in% seq(9*year, 10*year)) %>% summarize(PfPR = mean(n_detect_730_3650/n_730_3650))

