# ------------------------------------------------------------------------------
# READING PARAMETER SETS FROM MALARIALAUNCHR INTO MALARIASIMULATION
# ------------------------------------------------------------------------------

library(malariasimulation)
library(tidyverse)
library(readxl)

# clone the repository for MalariaLaunchR into git desktop
# https://github.com/mrc-ide/MalariaLaunchR

datadir <- 'C:/Users/htopazia/OneDrive - Imperial College London/Github/MalariaLaunchR/inst/model_files/parameters' # please set to your data directory

# pulling in parameter lists and combining
params <- list.files(path = datadir, pattern = "*.txt", full.names = TRUE) %>% 
  lapply(read.table) %>% 
  do.call(cbind, .)

params <- params[, !duplicated(as.list(params))] # de-duplicate columns

colnames(params) <- c('old',seq(0,50,1)) # rename columns

# match to malariasimulation names
print(params[,1:2]) 
matchnames <- readxl::read_excel('C:/Users/htopazia/OneDrive - Imperial College London/Github/rtss_malariasimulation/parameters_malarialaunchR_malariasim.xlsx')[,1:2] # matching guide

test <- merge(params, matchnames) %>% filter(!is.na(new)) %>% select(-old)
test <- pivot_longer(data = test, cols = `0`:`50`, names_to = 'draw', values_to = 'value')
test <- pivot_wider(data = test, names_from = new, values_from = value)

draw <- vector("list", length = nrow(test))

for (row in 1:nrow(test)) {
  draw[row] <- list(test[row,])
}


# RUN MODEL --------------------------------------------------------------------
year <- 365
human_population <- 1000

# function to get malariasim parameters and modify them to represent the 51 draws
setparams <- function(draw){
  
  params <- get_parameters(get_parameters(list(
    human_population = human_population,
    clinical_incidence_rendering_min_ages = 0,
    clinical_incidence_rendering_max_ages = 100 * year
  )))
  
  draw <- as.data.frame(draw)
  
  params$theta0 <- draw$theta0
  params$theta1 <- draw$theta1
  
  return(params)
  
}

# loop over parameter draws
drawset <- lapply(draw, setparams)


# function to run the model over each draw's parameter set
runsim <- function(params){

  params <- set_equilibrium(params, init_EIR = 10)    # set equilibirum
  
  output <- run_simulation(timesteps = 365, params) # run model
  
  return(output)
  
}

# loop over parameter draws
output <- map_dfr(.x=drawset, .f=runsim, .id='draw')

 
