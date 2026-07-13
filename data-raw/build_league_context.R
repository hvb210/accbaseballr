library(tidyverse)

batting_raw <- read.csv("data-raw/batting.csv")
pitching_raw <- read.csv("data-raw/pitching.csv")

# league batting stats by season
league_batting <- batting_raw |>
  group_by(Season) |>
  summarise(
    league_PA = sum(PA, na.rm = TRUE),
    league_H = sum(H, na.rm = TRUE),
    league_2B = sum(X2B, na.rm = TRUE),
    league_3B = sum(X3B, na.rm = TRUE),
    league_HR = sum(HR, na.rm = TRUE),
    league_BB = sum(BB, na.rm = TRUE),
    league_HBP = sum(HBP, na.rm = TRUE),
    league_SF = sum(SF, na.rm = TRUE),
    league_AB = sum(AB, na.rm = TRUE),
    league_BA = sum(BA, na.rm = TRUE),
    .groups = "drop"
  )

# league 1B and wOBA calculation
league_context <- league_batting |>
  mutate(
    league_1B = league_H - league_2B - league_3B - league_HR,
    league_wOBA =
      (
        0.69 * league_BB +
          0.72 * league_HBP +
          0.89 * league_1B +
          1.27 * league_2B +
          1.62 * league_3B +
          2.10 * league_HR
      ) /
      (league_AB + league_BB + league_HBP + league_SF)
  )

# adding in league averages for bases on balls
league_context <- league_context |>
  mutate(
    league_BB_rate = league_BB / league_PA
  )

# league pitching stats by season
league_pitching <- pitching_raw |>
  group_by(Season) |>
  summarize(
    league_SO = sum(SO, na.rm = TRUE),
    p_league_BB = sum(BB, na.rm = TRUE),
    p_league_HR = sum(HR, na.rm = TRUE),
    league_IP = sum(IP, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    league_K_per_9 = (league_SO / league_IP) * 9,
    league_BB_per_9 = (p_league_BB / league_IP) * 9,
    league_HR_per_9 = (p_league_HR / league_IP) * 9
  )

# combine league averages for batting and pitching
league_context <- left_join(
  league_context,
  league_pitching,
  by = "Season"
)

usethis::use_data(league_context, overwrite = TRUE)
