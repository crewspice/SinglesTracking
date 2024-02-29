library(tidycensus)
library(tidyverse)

vars <- load_variables(2022, "acs5", cache = TRUE) %>%
 dplyr::filter(str_detect(name, "B01001"))

# Grab data without geometry as we'll want to wrangle the geometry separately
 state_codes <- c(state.abb, "DC")

 ages_data <- map_dfr(state_codes, ~{
   get_acs(geography = "tract",
           state = .x,
           variables = c(
             females_20yrs = "B01001_032",
             males_20yrs = "B01001_008",
             females_21yrs = "B01001_033",
             males_21yrs = "B01001_009",
             females_mid20s = "B01001_034",
             males_mid20s = "B01001_010",
             females_late20s = "B01001_035",
             males_late20s = "B01001_011"
           ),
           output = "wide")
   })

ages_grouped <- ages_data %>%
   transmute(GEOID = GEOID,
             NAME = NAME,
             females_early20s = females_20yrsE + females_21yrsE + females_mid20sE,
             males_early20s = males_20yrsE + males_21yrsE + males_mid20sE,
             all_early20s = females_early20s + males_early20s,
             females_late20s = females_late20sE,
             males_late20s = males_late20sE,
             all_late20s = females_late20s + males_late20s,
             females20s = females_early20s + females_late20s,
             males20s = males_early20s + males_late20s,
             all20s = all_early20s + all_late20s,
             females_pct_20s = females20s / all20s * 100,
             females_pct_early20s = females_early20s / all_early20s * 100,
             females_pct_late20s = females_late20s / all_late20s * 100)

write_rds(ages_grouped, "C:/Users/jacks/Documents/R/ages.rds")
