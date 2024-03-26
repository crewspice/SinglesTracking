
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

state_dots <- purrr::map(state_codes, ~{
  print(sprintf("Processing %s...", .x))
  state_blocks <- blocks(.x, year = 2016)
  state_lodes <- grab_lodes(state = .x, year = 2016, lodes_type = "rac",
                         segment = "S000")

   tic()
  state_with_rac <- state_blocks %>%
    filter(GEOID10 %in% state_lodes$h_geocode) %>%
    transmute(tract_id = str_sub(GEOID10, end = -5)) %>%
    group_by(tract_id) %>%
    summarize()
  toc()

 state_groups <- age_groups %>%
    filter(str_sub(GEOID, end = 2) == tigris:::validate_state(.x))

    group_names <- names(immigrant_groups)[3:11]

  
