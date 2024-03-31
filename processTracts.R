
library(tigris)
library(lehdr) # remotes::install_github('jamgreen/lehdr')
library(tidyverse)
library(sf)
library(tictoc) # install.packages("tictoc")
library(mapboxapi) # remotes::install_github('walkerke/mapboxapi')
options(tigris_use_cache = TRUE)

state_codes <- c(state.abb, "DC")
names(state_codes) <- state_codes
age_groups <- read_rds("C:/Users/jacks/OneDrive/Documents/R/ages.rds")

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

group_names <- names(age_groups)[15:20]

 joined_tracts <- state_with_rac %>%
    left_join(age_groups, by = c("tract_id" = "GEOID")) %>%
    mutate_at(vars(surplus_females_early20s:surplus_males_all20s), ~if_else(is.na(.x), 0L, .x))
  
  all_dots <- map(group_names, function(name) {
    print(glue::glue("Processing {name}..."))
    suppressMessages(st_sample(joined_tracts, size = joined_tracts[[name]], 
                               exact = TRUE)) %>%
      st_sf() %>%
      mutate(group = name) %>%
      st_transform(4326) 
  }) %>%
    reduce(rbind) %>%
    slice_sample(prop = 1)
  
  # Write state dataset to temp file to guard against unforeseen errors
  write_rds(all_dots, glue::glue("data/temp/{.x}_dots.rds"))
  
  return(all_dots)
})
  
