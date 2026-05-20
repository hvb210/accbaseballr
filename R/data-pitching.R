#' ACC Pitching Data
#'
#' Player-season pitching metrics for ACC baseball.
#'
#' @description
#' Advanced pitching metrics for ACC pitchers including fielding-independent measures.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{player_id}{Unique player-season identifier}
#'   \item{Name}{Player first and last name}
#'   \item{Age}{Player age during season}
#'   \item{Season}{Year of season}
#'   \item{Team}{ACC team}
#'   \item{ERA}{Earned run average}
#'   \item{GS}{Games started}
#'   \item{IP}{Innings pitched}
#'   \item{H}{Hits allowed}
#'   \item{R}{Runs allowed}
#'   \item{ER}{Earned runs allowed}
#'   \item{HR}{Home runs allowed}
#'   \item{BB}{Bases on balls}
#'   \item{IBB}{Intentional bases on balls}
#'   \item{SO}{Strikeouts}
#'   \item{HBP}{Times hit by a pitch}
#'   \item{BF}{Batters faced}
#'   \item{WHIP}{Walks plus hits per inning pitched}
#'   \item{throws}{Throwing handedness}
#'   \item{K_pct}{Strikeout percentage}
#'   \item{BB_pct}{Walk percentage}
#'   \item{K_BB_pct}{Strikeout minus walk percentage}
#'   \item{FIP}{Fielding independent pitching}
#' }
#'
#' @source Data compiled from publicly available college baseball statistics from Sports-Reference and transformed into sabermetric metrics.
"pitching"
