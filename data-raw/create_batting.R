library(tidyverse)

batting <- tibble(
  player_id = c("smith_jake_unc_2025",
                "jones_mike_dsu_2025"),
  year = c(2025, 2025),
  team = c("UNC", "DSU"),
  PA = c(210, 195),
  ISO = c(.245, .188),
  BABIP = c(.351, .322),
  BB_pct = c(12.4, 9.1),
  K_pct = c(18.2, 21.5),
  wOBA = c(.421, .377),
  wRC_plus = c(152, 128)
)

usethis::use_data(batting, overwrite = TRUE)
