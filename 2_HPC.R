# Set-up ----------------------------------------------------------------------
library(didehpc)
setwd('M:/Hillary/rtss_malariasimulation')
# remotes::install_github('mrc-ide/malariasimulation@bug/severe', force=T)
source('./1_functions.R')

options(didehpc.cluster = "fi--didemrchnb",
        didehpc.username = "htopazia")

didehpc::didehpc_config() 
didehpc::web_login()

root <- "context" 

sources <- c('./1_functions.R')

# src <- conan::conan_sources("github::mrc-ide/malariasimulation@dev")
src <- conan::conan_sources("github::mrc-ide/malariasimulation@bug/severe")

ctx <- context::context_save(root,
                             sources = sources,
                             packages = c("tidyverse", "malariasimulation"),
                             package_sources = src)

share <- didehpc::path_mapping("malaria", "M:", "//fi--didef3.dide.ic.ac.uk/malaria", "M:")
config <- didehpc::didehpc_config(shares = share,
                                  use_rrq = FALSE,
                                  cores = 1, 
                                  cluster = "fi--didemrchnb",
                                  parallel = FALSE)

obj <- didehpc::queue_didehpc(ctx, config = config)


# Now set up your job ---------------------------------------------------------
year <- 365
month <- year/12
warmup <- 20 * year
sim_length <- 15 * year
human_population <- 100000

# highly seasonal parameters
high_seas <- get_parameters(list(
  human_population = human_population,
  model_seasonality = TRUE, 
  g0 = 0.284596, 
  g = c(-0.317878, -0.0017527, 0.116455),
  h = c(-0.331361, 0.293128, -0.0617547),
  incidence_rendering_min_ages = 0,
  incidence_rendering_max_ages = 100 * year,
  clinical_incidence_rendering_min_ages = 0,
  clinical_incidence_rendering_max_ages = 100 * year,
  severe_incidence_rendering_min_ages = 0,
  severe_incidence_rendering_max_ages = 100 * year,
  fvt = 0,
  v = 0,
  severe_enabled = T))

# seasonal parameters
low_seas <- get_parameters(list(
  human_population = human_population,
  model_seasonality = TRUE, 
  g0 = 0.285505, 
  g = c(-0.325352, -0.0109352, 0.0779865),
  h = c(-0.132815, 0.104675, -0.013919),
  incidence_rendering_min_ages = 0,
  incidence_rendering_max_ages = 100 * year,
  clinical_incidence_rendering_min_ages = 0,
  clinical_incidence_rendering_max_ages = 100 * year,
  severe_incidence_rendering_min_ages = 0,
  severe_incidence_rendering_max_ages = 100 * year,
  fvt = 0,
  v = 0,
  severe_enabled = T))

params <- tibble(params=list(high_seas,low_seas))
season <- c(rep('high_seas',4), rep('low_seas',4))
starting_EIR <- c(0.9, 3.6, 6.8, 21.9)

combo <- crossing(params, starting_EIR, warmup, sim_length) %>% cbind(season) %>%
  as_tibble()

# Run tasks -------------------------------------------------------------------
# NO INTERVENTION
t <- obj$enqueue_bulk(combo, runsim_none)
t$status()

# EPI alone
t <- obj$enqueue_bulk(combo, runsim_epi)
t$status()

# SV4
boosters <- round(12*month+2*month)
booster_coverage <- rep(.80, 1)
rtss_cs_boost <- 5.56277
name <- 'SV4_'

combo2 <- combo %>% cbind(boosters, booster_coverage, rtss_cs_boost, name)

t <- obj$enqueue_bulk(combo2, runsim_SV)
t$status()

# SV4 updated
boosters <- round(12*month+2*month)
booster_coverage <- rep(.80, 1)
rtss_cs_boost <- 6.37008 
name <- 'SV4updated_'

combo2 <- combo %>% cbind(boosters, booster_coverage, rtss_cs_boost, name)

t <- obj$enqueue_bulk(combo2, runsim_SV)
t$status()

# SV5
boosters <- tibble(boosters = list(round(c(12*month+2*month, 24*month+2*month))))
booster_coverage <- tibble(booster_coverage = list(rep(.80, 2)))
rtss_cs_boost <- 5.56277 
name <- 'SV5_'

combo2 <- combo %>% cbind(boosters, booster_coverage, rtss_cs_boost, name)

t <- obj$enqueue_bulk(combo2, runsim_SV)
t$status()

# SV5 updated
boosters <- tibble(boosters = list(round(c(12*month+2*month, 24*month+2*month))))
booster_coverage <- tibble(booster_coverage = list(rep(.80, 2)))
rtss_cs_boost <- 6.37008
name <- 'SV5updated_'

combo2 <- combo %>% cbind(boosters, booster_coverage, rtss_cs_boost, name)

t <- obj$enqueue_bulk(combo2, runsim_SV)
t$status()

# SMC alone
shape <- 3.67
scale <- 41.48
name <- 'SMCalone_'

combo2 <- combo %>% cbind(shape, scale, name)

t <- obj$enqueue_bulk(combo2, runsim_SMC)
t$status()

# EPI + SMC
shape <- 3.67
scale <- 41.48
name <- 'SMCEPI_'

combo2 <- combo %>% cbind(shape, scale, name)

t <- obj$enqueue_bulk(combo2, runsim_SMCEPI)
t$status()

# SV4 + SMC
shape <- 3.67
scale <- 41.48
boosters <- round(12*month+2)
booster_coverage <- rep(.80, 1)
rtss_cs_boost <- 5.56277
name <- 'SV4SMC_'

combo2 <- combo %>% cbind(shape, scale, boosters, booster_coverage, rtss_cs_boost, name)

t <- obj$enqueue_bulk(combo2, runsim_SMCSV)
t$status()
  
# SV5 + SMC
shape <- 3.67
scale <- 41.48
boosters <- tibble(boosters = list(round(c(12*month+2*month, 24*month+2*month))))
booster_coverage <- tibble(booster_coverage = list(rep(.80, 2)))
rtss_cs_boost <- 5.56277 
name <- 'SV5SMC_'

combo2 <- combo %>% cbind(shape, scale, boosters, booster_coverage, rtss_cs_boost, name)

t <- obj$enqueue_bulk(combo2, runsim_SMCSV)
t$status()

# SV4 updated + SMC
shape <- 3.67
scale <- 41.48
boosters <- round(12*month+2*month)
booster_coverage <- rep(.80, 1)
rtss_cs_boost <- 6.37008
name <- 'SV4updatedSMC_'

combo2 <- combo %>% cbind(shape, scale, boosters, booster_coverage, rtss_cs_boost, name)

t <- obj$enqueue_bulk(combo2, runsim_SMCSV)
t$status()

# SV5 updated + SMC
shape <- 3.67
scale <- 41.48
boosters <- tibble(boosters = list(round(c(12*month+2*month, 24*month+2*month))))
booster_coverage <- tibble(booster_coverage = list(rep(.80, 2)))
rtss_cs_boost <- 6.37008 
name <- 'SV5updatedSMC_'

combo2 <- combo %>% cbind(shape, scale, boosters, booster_coverage, rtss_cs_boost, name)

t <- obj$enqueue_bulk(combo2, runsim_SMCSV)
t$status()

# SV4 synergy + SMC
shape <- 2.955348
scale <- 58.99686
boosters <- round(12*month+2*month)
booster_coverage <- rep(.80, 1)
rtss_cs_boost <- 6.37008
name <- 'SV4SMCsynergy_'

combo2 <- combo %>% cbind(shape, scale, boosters, booster_coverage, rtss_cs_boost, name)

t <- obj$enqueue_bulk(combo2, runsim_SMCSV)
t$status()

# SV5 synergy + SMC
shape <- 2.955348
scale <- 58.99686
boosters <- tibble(boosters = list(round(c(12*month+2*month, 24*month+2*month))))
booster_coverage <- tibble(booster_coverage = list(rep(.80, 2)))
rtss_cs_boost <- 6.37008
name <- 'SV5SMCsynergy_'

combo2 <- combo %>% cbind(shape, scale, boosters, booster_coverage, rtss_cs_boost, name)

t <- obj$enqueue_bulk(combo2, runsim_SMCSV)
t$status()


# Process data ----------------------------------------------------------------
# read in files
require(data.table)

files <- list.files(path = "M:/Hillary/rtss_malariasimulation/rds/HPC", pattern = "*.rds", full.names = TRUE)
dat_list <- lapply(files, function (x) data.table(readRDS(x)))

dat <- rbindlist(dat_list, fill = TRUE, idcol="file") %>% 
  mutate(file = files[file],
         file = gsub("M:/Hillary/rtss_malariasimulation/rds/HPC/","",file),
         intervention = case_when(grepl('EPIalone',file)~"EPI",
                                  grepl('SV4_',file)~"SV 4-dose",
                                  grepl('SV5_',file)~"SV 5-dose",
                                  grepl('SV4updated_',file)~"SV 4-dose - updated booster",
                                  grepl('SV5updated_',file)~"SV 5-dose - updated booster",
                                  grepl('SMCEPI_',file)~"EPI + SMC",
                                  grepl('SV4SMC_',file)~"SV 4-dose + SMC",
                                  grepl('SV5SMC_',file)~"SV 5-dose + SMC",
                                  grepl('SV4updatedSMC',file)~"SV 4-dose - updated booster + SMC",
                                  grepl('SV5updatedSMC',file)~"SV 5-dose - updated booster + SMC",
                                  grepl('SV4SMCsynergy',file)~"SV 4-dose synergy + SMC",
                                  grepl('SV5SMCsynergy',file)~"SV 5-dose synergy + SMC",
                                  grepl('SMCalone',file)~"SMC alone",
                                  grepl('none',file)~"none"),
         season = case_when(model=='high_seas'~"highly_seasonal",
                            model=='low_seas'~"seasonal"))

saveRDS(dat,"C:/Users/htopazia/OneDrive - Imperial College London/Github/rtss_malariasimulation/rds/rtss_smc_raw.rds")

summary(dat$n_0_36500)

dat2 <- dat %>% 
  mutate(cases = (n_inc_clinical_0_36500/n_0_36500) * human_population,
         n = n_0_36500,
         severe = (n_inc_severe_0_36500/n_0_36500) * human_population,
         deaths = 0.215 * (n_inc_severe_0_36500/n_0_36500) * human_population) %>% 
  rowwise() %>%
  mutate(dose1 = sum(n_rtss_epi_dose_1,n_rtss_mass_dose_1,na.rm=T),
         dose2 = sum(n_rtss_epi_dose_2,n_rtss_mass_dose_2,na.rm=T),
         dose3 = sum(n_rtss_epi_dose_3,n_rtss_mass_dose_3,na.rm=T),
         dose4 = sum(n_rtss_epi_booster_1,n_rtss_mass_booster_1,na.rm=T),
         dose5 = sum(n_rtss_mass_booster_2,na.rm=T),
         dosecomplete = dose3) %>% ungroup()

saveRDS(dat2,"C:/Users/htopazia/OneDrive - Imperial College London/Github/rtss_malariasimulation/rds/rtss_smc_all.rds")
         
none <- dat2 %>% filter(intervention == 'none') %>%
  rename(base_case = cases, 
         base_n = n_0_36500,
         base_death = deaths) %>%
  select(timestep, base_case, base_n, base_death, eir, season)

dat3 <- dat2 %>% filter(intervention != 'none') %>% left_join(none) %>%
  group_by(eir, season, intervention) %>%
  summarize(base_case = sum(base_case, na.rm=T),
            base_death = sum(base_death, na.rm=T),
            dosecomplete = sum(dosecomplete, na.rm=T),
            cases = sum(cases, na.rm=T),
            pop = mean(n_0_36500, na.rm=T),
            severe = sum(severe, na.rm=T),
            deaths = sum(deaths, na.rm=T),
            cases_averted = base_case - cases,
            cases_averted_per_100000_fvp = (base_case - cases)/dosecomplete * human_population,
            deaths_averted = base_death - deaths,
            deaths_averted_per_100000_fvp = (base_death - deaths)/dosecomplete * human_population)

saveRDS(dat3,"C:/Users/htopazia/OneDrive - Imperial College London/Github/rtss_malariasimulation/rds/rtss_smc_averted.rds")
