library(dplyr)
library(readr)

batting_files <- list.files(
  "data",
  pattern = "_batting\\.csv$",
  full.names = TRUE
)

batting <- batting_files |>
  lapply(read_csv) |>
  bind_rows()

pitching_files <- list.files(
  "data",
  pattern = "_pitching\\.csv$",
  full.names = TRUE
)

pitching <- pitching_files |>
  lapply(read_csv) |>
  bind_rows()

saveRDS(
  batting,
  "data/batting.rds"
)

saveRDS(
  pitching,
  "data/pitching.rds"
)

