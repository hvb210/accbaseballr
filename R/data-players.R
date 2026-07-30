#' ACC Player Data
#'
#' Player metadata for ACC baseball players.
#'
#' @description
#' Basic player identification information used to join batting and pitching datasets.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{player_id}{Unique player identifier}
#'   \item{Name}{Player first and last name}
#'   \item{bats}{Batting handedness}
#'   \item{throws}{Throwing handedness}
#' }
#'
#' @source Compiled from publicly available roster information from Sports-Reference.
"players"
