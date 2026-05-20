library(tidyverse)

batting_raw <- read.csv("data-raw/batting.csv")

batting <- batting_raw |>
  mutate(
    bats = case_when(
      str_detect(Name, "\\*") ~ "Left",
      str_detect(Name, "#") ~ "Both",
      str_detect(Name, "\\?") ~ "Unknown",
      TRUE ~ "Right"
    ),
    Name = str_remove_all(Name, "\\*|#|\\?"),
    player_id = str_extract(Name.additional, "(?<=id=).*")
  )

batting <- batting |>
  mutate(
    ISO = SLG - BA,
    K_pct = SO / PA,
    BABIP = (H - HR) / (AB - SO - HR + SF),
    BB_pct = BB / PA
  )

batting <- batting |>
  mutate(
    X1B = H - X2B - X3B - HR
  )

batting <- batting |>
  mutate(
    wOBA =
      (
        0.69 * BB +
          0.72 * HBP +
          0.89 * X1B +
          1.27 * X2B +
          1.62 * X3B +
          2.10 * HR
      ) /
      (AB + BB + HBP + SF)
  )

# join the league_wOBA with player level batting data
batting <- batting |>
  left_join(
    league_context |> select(Season, league_wOBA),
    by = "Season"
  )

# create a scale of 1.25 for normalizing wRC+
batting <- batting |>
  group_by(Season) |>
  mutate(
    scale = 1.25
  ) |>
  ungroup()

# calculate wRC+
batting <- batting |>
  mutate(
    wRC_plus = 100 * ((wOBA - league_wOBA) / scale + 1)
  )

# drop columns not used in calculations
batting <- batting |>
  select(-c(CS, RBI, SB, OBP, OPS, TB, GDP, SH, Notes, Name.additional))

# reorder columns
batting <- batting |>
  select(player_id, everything())

usethis::use_data(batting, overwrite = TRUE)

