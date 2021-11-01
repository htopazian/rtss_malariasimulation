# Set-up ----------------------------------------------------------------------
library(didehpc)
setwd('M:/Hillary/rtss_malariasimulation')
# remotes::install_github('mrc-ide/malariasimulation@feat/simple_severe', force=T)
source('./1_functions.R')

options(didehpc.cluster = "fi--didemrchnb",
        didehpc.username = "htopazia")

didehpc::didehpc_config() 
didehpc::web_login()

root <- "context" 

sources <- c('./1_functions.R')

# src <- conan::conan_sources("github::mrc-ide/malariasimulation@dfeat/simple_severe)
src <- conan::conan_sources("github::mrc-ide/malariasimulationfeat/simple_severe")

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
  average_age = 8453.323, # to match flat_demog
  model_seasonality = TRUE, 
  g0 = 0.284596, 
  g = c(-0.317878, -0.0017527, 0.116455),
  h = c(-0.331361, 0.293128, -0.0617547),
  incidence_rendering_min_ages = c(0, 0),
  incidence_rendering_max_ages = c(5*year, 100 * year),
  clinical_incidence_rendering_min_ages = c(0, 0),
  clinical_incidence_rendering_max_ages = c(5*year, 100 * year),
  severe_incidence_rendering_min_ages = c(0, 0),
  severe_incidence_rendering_max_ages = c(5*year, 100 * year),
  fvt = 0,
  v = 0,
  individual_mosquitoes = FALSE,
  # individual_mosquitoes = TRUE,
  severe_enabled = T))

# seasonal parameters
low_seas <- get_parameters(list(
  human_population = human_population,
  average_age = 8453.323, # to match flat_demog
  model_seasonality = TRUE, 
  g0 = 0.285505, 
  g = c(-0.325352, -0.0109352, 0.0779865),
  h = c(-0.132815, 0.104675, -0.013919),
  incidence_rendering_min_ages = c(0, 0),
  incidence_rendering_max_ages = c(5*year, 100 * year),
  clinical_incidence_rendering_min_ages = c(0, 0),
  clinical_incidence_rendering_max_ages = c(5*year, 100 * year),
  severe_incidence_rendering_min_ages = c(0, 0),
  severe_incidence_rendering_max_ages = c(5*year, 100 * year),
  fvt = 0,
  v = 0,
  individual_mosquitoes = FALSE,
  # individual_mosquitoes = TRUE,
  severe_enabled = T))

params <- tibble(params=rep(list(high_seas,high_seas,high_seas,high_seas,
                                 low_seas,low_seas,low_seas,low_seas)))
season <- c(rep('high_seas',4), rep('low_seas',4))

# EIR_high <- c(1.1, 4.7, 10.1, 38.2) # malariasimulation, individual_mosquitoes=T
# EIR_low <- c(1.2, 4.0, 8.7, 32.6) # malariasimulation, individual_mosquitoes=T
EIR_high <- c(1.61, 6.92, 13.7, 52.3) # malariasimulation, individual_mosquitoes=F
EIR_low <- c(1.51, 6.12, 12.1, 44.1) # malariasimulation, individual_mosquitoes=F

starting_EIR <- c(EIR_high, EIR_low)

combo <- cbind(params, warmup, sim_length, starting_EIR, season)

combo

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

# SV5
boosters <- tibble(boosters = list(round(c(12*month+2*month, 24*month+2*month))))
booster_coverage <- tibble(booster_coverage = list(rep(.80, 2)))
rtss_cs_boost <- 5.56277 
name <- 'SV5_'

combo2 <- combo %>% cbind(boosters, booster_coverage, rtss_cs_boost, name)

t <- obj$enqueue_bulk(combo2, runsim_SV)
t$status()

# Hybrid
minwait <- round(seq(0,11,1)*month)
fifth <- c(0,1)

combo2 <- crossing(minwait, combo, fifth) %>% select(params, minwait, starting_EIR, warmup, sim_length, season, fifth) %>% as_tibble()

t <- obj$enqueue_bulk(combo2, runsim_hybrid)
t$status()


# Process data ----------------------------------------------------------------
# read in files
require(data.table)
library(tidyverse)

# rename HPC folder to 'HPC_hybrid'

files <- list.files(path = "M:/Hillary/rtss_malariasimulation/rds/HPC", pattern = "*.rds", full.names = TRUE)
dat_list <- lapply(files, function (x) data.table(readRDS(x)))

dat <- rbindlist(dat_list, fill = TRUE, idcol="file", use.names = T) %>% 
  mutate(file = files[file],
         file = gsub("M:/Hillary/rtss_malariasimulation/rds/HPC_hybrid/","",file),
         intervention = case_when(grepl('EPIalone',file)~"EPI",
                                  grepl('SV4_',file)~"SV 4-dose",
                                  grepl('SV5_',file)~"SV 5-dose",
                                  grepl('hybrid_',file)~"hybrid",
                                  grepl('none',file)~"none"))

saveRDS(dat,"C:/Users/htopazia/OneDrive - Imperial College London/Github/rtss_malariasimulation/rds/rtss_hybrid_raw.rds")

summary(dat$n_0_36500)

dat2 <- dat %>% 
  mutate(cases = (n_inc_clinical_0_36500/n_0_36500) * human_population,
         n = n_0_36500,
         cases_p = (p_inc_clinical_0_36500/n_0_36500) * human_population,
         severe = (n_inc_severe_0_36500/n_0_36500) * human_population,
         severe_p = (p_inc_severe_0_36500/n_0_36500) * human_population,
         deaths = 0.215 * (n_inc_severe_0_36500/n_0_36500) * human_population,
         deaths_p = 0.215 * (p_inc_severe_0_36500/n_0_36500) * human_population,
         
         cases05 = (n_inc_clinical_0_1825/n_0_1825) * human_population,
         n05 = n_0_1825,
         cases05_p = (p_inc_clinical_0_1825/n_0_1825) * human_population,
         severe05 = (n_inc_severe_0_1825/n_0_1825) * human_population,
         severe05_p = (p_inc_severe_0_1825/n_0_1825) * human_population,
         deaths05 = 0.215 * (n_inc_severe_0_1825/n_0_1825) * human_population,
         deaths05_p = 0.215 * (p_inc_severe_0_1825/n_0_1825) * human_population) %>%
  
  rowwise() %>%
  mutate(dose1 = sum(n_rtss_epi_dose_1,n_rtss_mass_dose_1,na.rm=T),
         dose2 = sum(n_rtss_epi_dose_2,n_rtss_mass_dose_2,na.rm=T),
         dose3 = sum(n_rtss_epi_dose_3,n_rtss_mass_dose_3,na.rm=T),
         dose4 = sum(n_rtss_epi_booster_1,n_rtss_mass_booster_1,na.rm=T),
         dose5 = sum(n_rtss_epi_booster_2, n_rtss_mass_booster_2,na.rm=T), 
         dosecomplete = dose3) %>%
  ungroup()

saveRDS(dat2,"C:/Users/htopazia/OneDrive - Imperial College London/Github/rtss_malariasimulation/rds/rtss_hybrid_all.rds")
         
none <- dat2 %>% filter(intervention == 'none') %>%
  rename(base_case = cases, 
         base_n = n_0_36500,
         base_death = deaths) %>%
  select(timestep, base_case, base_n, base_death, eir, model)

dat3 <- dat2 %>% filter(intervention != 'none') %>% left_join(none) %>%
  group_by(eir, model, intervention) %>%
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

saveRDS(dat3,"C:/Users/htopazia/OneDrive - Imperial College London/Github/rtss_malariasimulation/rds/rtss_hybrid_averted.rds")
