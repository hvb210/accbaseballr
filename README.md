
<!-- README.md is generated from README.Rmd. Please edit that file -->

# accbaseballr

<!-- badges: start -->

<!-- badges: end -->

`accbaseballr` provides curated ACC baseball datasets with advanced
sabermetric metrics at the player-season level.

The package includes: - Batting statistics - Pitching statistics -
Player information - League context statistics - Advanced metrics such
as wOBA, wRC+, and FIP

## Interactive Shiny App

Explore ACC baseball advanced metrics through the Shiny dashboard:

[Launch the ACC Baseball
Explorer](https://hanabaskin.shinyapps.io/accbaseballr-shiny/)

## Installation

You can install the development version of accbaseballr like so:

``` r
# install.packages("remotes")
remotes::install_github("hvb210/accbaseballr")
```

## Available datasets

| Dataset | Description |
|----|----|
| `batting` | ACC batting statistics with advanced offensive metrics |
| `pitching` | ACC pitching statistics with advanced pitching metrics |
| `players` | Player metadata including handedness |
| `league_context` | League-average statistics used for normalization and metric calculations |

## Example

``` r
library(accbaseballr)
library(dplyr)

batting |>
  filter(Season == 2025) |>
  arrange(desc(wRC_plus)) |>
  select(Name, Team, wRC_plus, wOBA) |>
  head(10)
```

## Data source

Statistics are derived from publicly available ACC baseball data from
Sports-Reference and processed into analysis-ready datasets.

## License

MIT License
