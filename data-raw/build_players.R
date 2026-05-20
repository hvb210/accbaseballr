library(tidyverse)

batting_players <- batting |>
  select(player_id, Name, bats) |>
  group_by(player_id) |>
  summarise(
    Name = first(Name),
    bats = first(na.omit(bats)), #omits missing values
    .groups = "drop"
  )

pitching_players <- pitching |>
  select(player_id, Name, throws) |>
  group_by(player_id) |>
  summarise(
    Name = first(Name),
    throws = first(na.omit(throws)),
    .groups = "drop"
  )

players <- full_join(
  batting_players,
  pitching_players,
  by = c("player_id", "Name")
)

usethis::use_data(players, overwrite = TRUE)
