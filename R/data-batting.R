#' ACC Batting Data
#'
#' Player-season batting metrics for ACC baseball.
#'
#' @description
#' This dataset contains advanced batting metrics for ACC baseball players.
#' It is designed for sabermetric analysis rather than raw box-score reporting.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{player_id}{Unique player-season identifier}
#'   \item{Name}{Player first and last name}
#'   \item{Age}{Player age during season}
#'   \item{Season}{Year of season}
#'   \item{Team}{ACC team}
#'   \item{G}{Games played}
#'   \item{PA}{Plate appearances}
#'   \item{AB}{At bats}
#'   \item{R}{Runs scored}
#'   \item{H}{Hits}
#'   \item{X1B}{Singles hit}
#'   \item{X2B}{Doubles hit}
#'   \item{X3B}{Triples hit}
#'   \item{HR}{Home runs hit}
#'   \item{BB}{Bases on balls}
#'   \item{SO}{Strikeouts}
#'   \item{BA}{Hits/At bats}
#'   \item{SLG}{Total bases/At bats}
#'   \item{HBP}{Times hit by a pitch}
#'   \item{SF}{Sacrifice flies}
#'   \item{IBB}{Intentional bases on balls}
#'   \item{bats}{Batting handedness}
#'   \item{ISO}{Isolated power}
#'   \item{K_pct}{Strikeout percentage}
#'   \item{BABIP}{Batting average on balls in play}
#'   \item{BB_pct}{Walk percentage}
#'   \item{wOBA}{Weighted on-base average}
#'   \item{league_wOBA}{Average wOBA for league for season}
#'   \item{scale}{wOBA scale}
#'   \item{wRC_plus}{Weighted runs created plus}
#' }
#'
#' @source Data compiled from publicly available college baseball statistics from Sports-Reference and transformed into sabermetric metrics.
"batting"
