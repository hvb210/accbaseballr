#' ACC batting statistics with advanced offensive metrics
#'
#' A dataset containing ACC baseball batting statistics at the player-season
#' level, including traditional counting stats and advanced sabermetric
#' offensive metrics.
#'
#' @format A data frame with one row per player-season and the following
#'   columns:
#'   \describe{
#'     \item{player_id}{Character. Unique player identifier.}
#'     \item{Season}{Integer. The season (calendar year).}
#'     \item{Name}{Character. Player full name.}
#'     \item{Age}{Character. Age of player in season.}
#'     \item{Team}{Character. ACC team name.}
#'     \item{G}{Integer. Games played.}
#'     \item{PA}{Integer. Plate appearances.}
#'     \item{AB}{Integer. At-bats.}
#'     \item{H}{Integer. Hits.}
#'     \item{X1B}{Integer. Singles.}
#'     \item{X2B}{Integer. Doubles.}
#'     \item{X3B}{Integer. Triples.}
#'     \item{HR}{Integer. Home runs.}
#'     \item{R}{Integer. Runs scored.}
#'     \item{BB}{Integer. Walks.}
#'     \item{SO}{Integer. Strikeouts.}
#'     \item{SLG}{Numeric. Slugging percentage.}
#'     \item{HBP}{Integer. Hit by pitch.}
#'     \item{SF}{Integer. Sacrifice fly.}
#'     \item{IBB}{Integer. Intentional walk.}
#'     \item{bats}{Character. Batting handedness: \code{"Right"}, \code{"Left"},
#'       \code{"Both"} or \code{"Unkown"}.}
#'     \item{ISO}{Numeric. Isolated power.}
#'     \item{K_pct}{Numeric. Strikeout percentage.}
#'     \item{BABIP}{Numeric. Batting average on balls in play.}
#'     \item{wOBA}{Numeric. Weighted on-base average.}
#'     \item{league_wOBA}{Numeric. League-wide weighted on-base average.}
#'     \item{scale}{Numeric. wOBA scale}
#'     \item{wRC_plus}{Numeric. Weighted runs created plus (100 = league average).}
#'   }
#' @source Derived from publicly available ACC baseball data from
#'   Sports-Reference (<https://www.sports-reference.com/>).
#' @examples
#' data(batting)
#' head(batting)
"batting"


#' ACC pitching statistics with advanced pitching metrics
#'
#' A dataset containing ACC baseball pitching statistics at the player-season
#' level, including traditional counting stats and advanced sabermetric
#' pitching metrics.
#'
#' @format A data frame with one row per player-season and the following
#'   columns:
#'   \describe{
#'     \item{Season}{Integer. The season (calendar year).}
#'     \item{player_id}{Character. Unique player identifier.}
#'     \item{Name}{Character. Player full name.}
#'     \item{Age}{Character. Age of player in season.}
#'     \item{Team}{Character. ACC team abbreviation or name.}
#'     \item{ERA}{Numeric. Earned run average.}
#'     \item{GS}{Integer. Games started.}
#'     \item{IP}{Numeric. Innings pitched.}
#'     \item{H}{Integer. Hits allowed.}
#'     \item{R}{Integer. Runs allowed.}
#'     \item{ER}{Integer. Earned runs allowed.}
#'     \item{BB}{Integer. Walks allowed.}
#'     \item{IBB}{Integer. Intentional walks.}
#'     \item{SO}{Integer. Strikeouts.}
#'     \item{HBP}{Integer. Hit by pitch.}
#'     \item{HR}{Integer. Home runs allowed.}
#'     \item{BF}{Integer. Batters faced.}
#'     \item{WHIP}{Numeric. Walks plus hits per inning pitched.}
#'     \item{throws}{Character. Throwing handedness: \code{"Right"}, \code{"Left"},
#'       \code{"Both"} or \code{"Unkown"}.}
#'     \item{K_pct}{Numeric. Strikeout percentage.}
#'     \item{BB_pct}{Numeric. Walk percentage.}
#'     \item{K_BB_pct}{Numeric. Strikeout minus walk percentage.}
#'     \item{FIP}{Numeric. Fielding-independent pitching.}
#'   }
#' @source Derived from publicly available ACC baseball data from
#'   Sports-Reference (<https://www.sports-reference.com/>).
#' @examples
#' data(pitching)
#' head(pitching)
"pitching"


#' ACC baseball player metadata
#'
#' A dataset containing player-level metadata for ACC baseball players,
#' including handedness information.
#'
#' @format A data frame with one row per player and the following columns:
#'   \describe{
#'     \item{Name}{Character. Player full name.}
#'     \item{player_id}{Character. Unique player identifier.}
#'     \item{bats}{Character. Batting handedness: \code{"Right"}, \code{"Left"},
#'       \code{"Both"} or \code{"Unkown"}.}
#'     \item{throws}{Character. Throwing handedness: \code{"Right"}, \code{"Left"},
#'        \code{"Both"} or \code{"Unkown"}.}
#'   }
#' @source Derived from publicly available ACC baseball data from
#'   Sports-Reference (<https://www.sports-reference.com/>).
#' @examples
#' data(players)
#' head(players)
"players"


#' ACC baseball league context statistics
#'
#' A dataset containing league-average statistics used for normalization
#' and calculation of advanced metrics such as wOBA and wRC+.
#'
#' @format A data frame with one row per season and the following columns:
#'   \describe{
#'     \item{Season}{Integer. The season (calendar year).}
#'     \item{league_PA}{Integer. League-wide plate appearances.}
#'     \item{league_H}{Integer. League-wide hits.}
#'     \item{league_2B}{Integer. League-wide doubles hit.}
#'     \item{league_3B}{Integer. League-wide triples hit.}
#'    \item{league_HR}{Integer. League_wide home runs hit.}
#'    \item{league_BB}{Integer. League-wide bases on balls.}
#'    \item{league_HBP}{Integer. League-wide times hit by pitch.}
#'    \item{league_SF}{Integer. League-wide sacrifice flies.}
#'    \item{league_AB}{Integer. League-wide at bats.}
#'    \item{league_BA}{Numeric. League-wide hits/at bats.}
#'    \item{league_1B}{Integer. League-wide singles hit.}
#'    \item{league_wOBA}{Numeric. League-wide weighted on-base average.}
#'    \item{league_BB_rate}{Numeric. League-wide walk rate.}
#'    \item{league_SO}{Integer. League-wide strikeouts.}
#'    \item{p_league_BB}{Integer. League-wide bases on balls (pitching).}
#'    \item{p_league_HR}{Integer. League-wide home runs allowed.}
#'    \item{league_IP}{Numeric. League-wide innings pitched.}
#'    \item{league_K_per_9}{Numeric. League-wide strikeouts per nine innings.}
#'    \item{league_BB_per_9}{Numeric. League-wide bases on balls per nine innings.}
#'    \item{league_HR_per_9}{Numeric. League-wide home runs allowed per nine innings.}
#'   }
#' @source Derived from publicly available ACC baseball data from
#'   Sports-Reference (<https://www.sports-reference.com/>).
#' @examples
#' data(league_context)
#' head(league_context)
"league_context"
