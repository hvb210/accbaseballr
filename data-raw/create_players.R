library(tidyverse)

players <- tibble(
  player_id = c("smith_jake_unc_2025",
                "brown_eli_dsu_2025"),
  first_name = c("Jake", "Eli"),
  last_name = c("Smith", "Brown"),
  bats = c("R", "L"),
  throws = c("R", "R")
)

usethis::use_data(players, overwrite = TRUE)
