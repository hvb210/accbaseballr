#' ACC League Context Data
#'
#' Season-level offensive environment metrics used for normalization of player statistics.
#'
#' @format A data frame with one row per season:
#' \describe{
#'   \item{Season}{Season year}
#'   \item{league_PA}{League-wide plate appearances}
#'   \item{league_H}{League-wide hits}
#'   \item{league_2B}{League-wide doubles hit}
#'   \item{league_3B}{League-wide triples hit}
#'   \item{league_HR}{League_wide home runs hit}
#'   \item{league_BB}{League-wide bases on balls}
#'   \item{league_HBP}{League-wide times hit by pitch}
#'   \item{league_SF}{League-wide sacrifice flies}
#'   \item{league_AB}{League-wide at bats}
#'   \item{league_BA}{League-wide hits/at bats}
#'   \item{league_1B}{League-wide singles hit}
#'   \item{league_wOBA}{League-wide weighted on-base average}
#'   \item{league_BB_rate}{walk rate}
#'   \item{league_SO}{League-wide strikeouts}
#'   \item{p_league_BB}{League-wide bases on balls (pitching)}
#'   \item{p_league_HR}{League-wide home runs allowed}
#'   \item{league_IP}{League-wide innings pitched}
#'   \item{league_K_per_9}{League-wide strikeouts per nine innings}
#'   \item{league_BB_per_9}{League-wide bases on balls per nine innings}
#'   \item{league_HR_per_9}{League-wide home runs allowed per nine innings}
#' }
#'
#' @source Publicly available ACC baseball statistics transformed into sabermetric metrics for analysis.
"league_context"
