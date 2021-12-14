setwd('M:/Hillary/rtss_malariasimulation')
# remotes::install_github('mrc-ide/umbrella@gee', force=T)

library(umbrella)
library(rgee)
library(sf)
library(dplyr)
library(ggplot2)
library(reticulate)

py_discover_config()
import('sys')$executable

# rgee::ee_install(py_env = "C:/Users/htopazia/AppData/Local/r-miniconda/envs/rgee")
rgee::ee_clean_pyenv()
rgee::ee_install_set_pyenv(py_path = "C:/Users/htopazia/Anaconda3/envs/rgee/python.exe", py_env = 'rgee')
rgee::ee_install()

reticulate::use_python('C:/Users/htopazia/AppData/Local/R-MINI~1/envs/rgee', required=T)


rgee::ee_install_upgrade()

reticulate::use_python('C:/Users/htopazia/Anaconda3/r-reticulate/python.exe')
Sys.setenv(RETICULATE_PYTHON="C:/Users/htopazia/r-miniconda/envs/rgee/python.exe")
options(reticulate.conda_binary = "C:/Users/htopazia/r-miniconda/envs/rgee")
usethis::edit_r_environ() 
ee_check()
ee_Initialize()
ee_user_info()
ee_discover_pyenvs()

reticulate::py_config()
reticulate::conda_version()



reticulate::py_config()


# Spatial data
admin0 <- readRDS("M:/Eradication/rds/admin0.RDS") %>% filter(Country %in% c('Mali', 'Burkina Faso', 'Senegal', 'Guinea', 'Mauritania', 'Algeria', 'Niger', "CÃ´te d'Ivoire", 'Ghana', 'Togo', 'Benin', 'Nigeria', 'Sierra Leone', 'Morocco'))
countries <- filter(admin0, Country %in% c('Mali', 'Burkina Faso')) 
sites <- filter(admin1, admin1 %in% c('Hauts-Bassins', 'Sikasso'))

ggplot() + 
  geom_sf(data = admin0, fill="cornsilk2",color="cornsilk3") +
  geom_sf(data = countries, fill="cornsilk", color="tan4", size=0.5) +
  geom_sf(data = sites, aes(fill = Country)) + 
  labs(fill = 'country sites', x = "", y = "") + 
  theme_bw() + 
  scale_x_continuous(limits=c(-12, 4)) + 
  scale_y_continuous(limits=c(9.5, 25)) + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

# Specify time-range
start_date <- "2014-09-01"
end_date <- "2016-12-31"

# Extract
daily_rain <- pull_daily_rainfall(sf = sites, start_date = start_date, end_date = end_date)

# Process
seasonality <- process(gee_output = daily_rain, ISO, Country, admin1)

# Inspect
seasonality %>%
  select(admin1, coefficients) %>%
  tidyr::unnest(cols = c(coefficients))
pd <- seasonality %>%
  select(admin1, profile) %>%
  tidyr::unnest(cols = c(profile)) %>%
  mutate(day = t * 365)
ggplot(pd, aes(x = day, y = y, col = admin1)) +
  geom_line() +
  xlab("Day") +
  ylab("Precipitation") +
  theme_bw()
  