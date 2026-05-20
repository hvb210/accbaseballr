library(tidyverse)

pitching <- tibble(
  player_id = c("smith_jake_unc_2025",
                "brown_eli_dsu_2025"),
  year = c(2025, 2025),
  team = c("UNC", "DSU"),
  IP = c(62.1, 55.0),
  K_pct = c(27.4, 22.1),
  BB_pct = c(7.8, 10.5),
  K_BB_pct = c(19.6, 11.6),
  FIP = c(3.42, 4.11),
  xFIP = c(3.60, 4.25)
)

usethis::use_data(pitching, overwrite = TRUE)
