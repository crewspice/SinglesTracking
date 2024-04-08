
library(tigris)
library(lehdr) # remotes::install_github('jamgreen/lehdr')
library(tidyverse)
library(sf)
library(tictoc) # install.packages("tictoc")
library(mapboxapi) # remotes::install_github('walkerke/mapboxapi')
options(tigris_use_cache = TRUE)

state_codes <- c(state.abb, "DC")
names(state_codes) <- state_codes

# Get index of CT and DE in state_codes
#index_CT <- which(state_codes == "CT")
index_DE <- which(state_codes == "DE")

# Subset state_codes array from CT to DE
subset_states <- state_codes[index_DE:index_DE]

age_groups <- read_rds("C:/Users/jacks/OneDrive/Documents/R/ages.rds")

# Cache the blocks to prevent download errors
# purrr::walk(state_codes, ~blocks(state = .x, year = 2016))

#state_dots <- purrr::map(state_codes, ~{
state_dots <- purrr::map(subset_states, ~{
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
	file_path <- file.path("C:/Users/jacks/OneDrive/Documents/R/temp/", paste0(.x, "_dots.rds"))
	saveRDS(all_dots, file = file_path, compress = FALSE)
	# write_rds(all_dots, glue::glue("data/temp/{.x}_dots.rds"))
  
  return(all_dots)
}) 
  #

# Write full dataset to guard against unforeseen errors
state_dots <- list.files("C:/Users/jacks/OneDrive/Documents/R/temp", 
                         pattern = "_dots.rds$", 
                         full.names = TRUE) %>%
  map(~read_rds(.x))

write_rds(state_dots, "C:/Users/jacks/OneDrive/Documents/R/temp/state_dots_list.rds")

# Then, combine
# state_dots_combined <- read_rds("data/temp/state_dots_list.rds")
#state_dots_combined <- state_dots %>%
#  data.table::rbindlist() %>%
#  st_as_sf(crs = 4326)

# the following chunk from chatGPT about how to resolve class mismatch between the columns of the two items being combined
# Convert all geometries to points
state_dots_combined <- lapply(state_dots, function(x) {
  st_geometry(x) <- st_point_on_surface(st_geometry(x))
  x
})

# Now, bind the items
state_dots_combined <- data.table::rbindlist(state_dots_combined)

# Convert to sf object
state_dots_combined <- st_as_sf(state_dots_combined, crs = 4326)

st_write(state_dots_combined, "C:/Users/jacks/OneDrive/Documents/R/state_dots_combined.geojson")

# stop here and run tippiecanoe on ubuntu
# code for state_dots_combined:
# ./tippecanoe -o /mnt/c/Users/jacks/OneDrive/Documents/R/state_dots_combined.mbtiles --layer=state_dots3 -zg --minimum-zoom=3 /mnt/c/Users/jacks/OneDrive/Documents/R/state_dots_combined.geojson

# i couldn't get this to work so far. i think the next troubleshooting step would be to 
# reach out to mapbox support with the error that i'm getting.
upload_tiles("C:/Users/jacks/OneDrive/Documents/R/state_dots_combined.mbtiles", 
             username = "crewspice",
	#     access_token = "sk.eyJ1IjoiY3Jld3NwaWNlIiwiYSI6ImNsdXE5cDRwcTJnd24yaWxob2owZGd1YWYifQ.g8x_NZVNC_gn3l20yBQ38Q",
             tileset_name = "state_dots3")
