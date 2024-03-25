
library(tigris)
library(lehdr) # remotes::install_github('jamgreen/lehdr')
library(tidyverse)
library(sf)
library(tictoc) # install.packages("tictoc")
library(mapboxapi) # remotes::install_github('walkerke/mapboxapi')
options(tigris_use_cache = TRUE)

state_codes <- c(state.abb, "DC")
names(state_codes) <- state_codes
age_groups <- read_rds("C:/Users/jacks/Documents/R/ages.rds")

# Cache the blocks to prevent download errors
# purrr::walk(state_codes, ~blocks(state = .x, year = 2016))

