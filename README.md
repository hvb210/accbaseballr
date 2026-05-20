
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
#> * checking for file ‘/private/var/folders/7j/c0vsmflj6dx0lrgkzvykz15w0000gp/T/RtmpkIVXOP/remotes3dd44ed1843/hvb210-accbaseballr-44a841b/DESCRIPTION’ ... OK
#> * preparing ‘accbaseballr’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘accbaseballr_0.1.0.tar.gz’
#> Installing package into '/private/var/folders/7j/c0vsmflj6dx0lrgkzvykz15w0000gp/T/Rtmp4yergN/temp_libpath1656416725949'
#> (as 'lib' is unspecified)
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
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

batting |>
  filter(Season == 2025) |>
  arrange(desc(wRC_plus)) |>
  select(Name, Team, wRC_plus, wOBA) |>
  head(10)
#> # A tibble: 10 × 4
#>    Name            Team           wRC_plus  wOBA
#>    <chr>           <chr>             <dbl> <dbl>
#>  1 Adam McKelvey   Georgia Tech       171. 1.27 
#>  2 Hideki Prather  Clemson            130. 0.762
#>  3 Troy Reader     Notre Dame         127. 0.72 
#>  4 Luca Perriello  Virginia Tech      126. 0.708
#>  5 Will Broderick  Virginia           125. 0.69 
#>  6 Jack Brown      Louisville         120. 0.635
#>  7 Jay Slater      Duke               120. 0.628
#>  8 Zach Jackson    Duke               118. 0.604
#>  9 Austin Hartsell Boston College     112. 0.537
#> 10 Ryder Kirtley   Virginia Tech      111. 0.527
```

## Data source

Statistics are derived from publicly available ACC baseball data from
Sports-Reference and processed into analysis-ready datasets.

## License

MIT License
