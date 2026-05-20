
<!-- README.md is generated from README.Rmd. Please edit that file -->

# accbaseballr

<!-- badges: start -->

<!-- badges: end -->

`accbaseballr` provides curated ACC baseball datasets with advanced
sabermetric metrics at the player-season level.

The package includes: - Batting statistics - Pitching statistics -
Player information - League context statistics - Advanced metrics such
as wOBA, wRC+, and FIP

## Installation

You can install the development version of accbaseballr like so:

``` r
# install.packages("remotes")
remotes::install_github("hvb210/accbaseballr")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo hvb210/accbaseballr@HEAD
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/7j/c0vsmflj6dx0lrgkzvykz15w0000gp/T/RtmpoYbqTf/remotes5656089037d/hvb210-accbaseballr-1ff21d6/DESCRIPTION’ ... OK
#> * preparing ‘accbaseballr’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘accbaseballr_0.1.0.tar.gz’
#> Installing package into '/private/var/folders/7j/c0vsmflj6dx0lrgkzvykz15w0000gp/T/Rtmp4yergN/temp_libpath1656453d99bbe'
#> (as 'lib' is unspecified)
suppressPackageStartupMessages(library(dplyr))
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

invisible(
  batting |>
  filter(Season == 2025) |>
  arrange(desc(wRC_plus)) |>
  select(Name, Team, wRC_plus, wOBA) |>
  head(10)
)
```

## Data source

Statistics are derived from publicly available ACC baseball data from
Sports-Reference and processed into analysis-ready datasets.

## License

MIT License
