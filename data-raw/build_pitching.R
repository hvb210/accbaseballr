library(tidyverse)

pitching_raw <- read.csv("data-raw/pitching.csv")

pitching <- pitching_raw |>
  mutate(
    throws = case_when(
      str_detect(Name, "\\*") ~ "Left",
      str_detect(Name, "#") ~ "Both",
      str_detect(Name, "\\?") ~ "Unknown",
      TRUE ~ "Right"
    ),
    Name = str_remove_all(Name, "\\*|#|\\?"),
    player_id = str_extract(Name.additional, "(?<=id=).*")
  )

pitching <- pitching |>
  mutate(
    K_pct = SO / BF,
    BB_pct = BB / BF,
    K_BB_pct = K_pct - BB_pct,
    FIP = (13 * HR + 3 * (BB + HBP) - 2 * SO) / IP + 3.10
  )

# drop columns not used in calculations
pitching <- pitching |>
  select(-c(W, L, W.L., G, GF, CG, SHO, SV, BK, WP, H9, HR9, BB9, SO9, SO.W, Notes, Name.additional))

# reorder columns
pitching <- pitching |>
  select(player_id, everything())

usethis::use_data(pitching, overwrite = TRUE)
