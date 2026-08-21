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

### BATTED BALLS INFO AND BATTER MATCHING ######################################

# pulls scoreboard for one day and returns a table of games
get_scoreboard_games <- function(date) {

  url <- glue(
    "https://site.api.espn.com/apis/site/v2/sports/",
    "baseball/college-baseball/scoreboard",
    "?dates={date}&limit=200"
  )

  resp <- request(url) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)

  # if a day has zero games, return an empty table
  if (is.null(resp$events) || length(resp$events) == 0) {
    return(tibble())
  }

  # for each game that day, pull out these fields into one row
  map_dfr(resp$events, \(event) {

    comp <- event$competitions[[1]]
    teams <- comp$competitors

    tibble(
      game_id = event$id,
      date = date,
      name = event$name %||% NA_character_,
      venue = comp$venue$fullName %||% NA_character_,
      completed = comp$status$type$completed %||% NA,
      pbp_available = comp$playByPlayAvailable %||% FALSE,

      team1_id = teams[[1]]$team$id %||% NA_character_,
      team1 = teams[[1]]$team$displayName %||% NA_character_,

      team2_id = teams[[2]]$team$id %||% NA_character_,
      team2 = teams[[2]]$team$displayName %||% NA_character_
    )
  })
}

# build a list of every date in the 2026 season
dates <- seq.Date(
  as.Date("2026-02-13"),
  as.Date("2026-06-24"),
  by = "day"
) |>
  format("%Y%m%d")

# check to see if the date is already saved so it doesn't repull every time the script is run
scoreboard_cache_path <- "data/all_games_2026.rds"

if (file.exists(scoreboard_cache_path)) {
  message("Loading cached scoreboard pull")
  all_games <- readRDS(scoreboard_cache_path)
} else {
  all_games <- map_dfr(dates, \(d) {
    message("Pulling scoreboard: ", d) # prints progress
    Sys.sleep(0.25) # pause between requests to avoid getting blocked
    tryCatch( # try to get the games but if there's an error, move on with an empty result for that day
      get_scoreboard_games(d),
      error = function(e) {
        warning("Failed date ", d, ": ", conditionMessage(e))
        tibble()
      }
    )
  })
  # save the results
  saveRDS(all_games, scoreboard_cache_path)
}

# gets the games where at least one team is in a power four conference
usable_p4_games <- all_games |>
  filter(
    completed == TRUE,
    pbp_available == TRUE,
    team1_id %in% p4_ids | team2_id %in% p4_ids
  ) |>
  distinct(game_id, .keep_all = TRUE)

message("Usable P4 games: ", nrow(usable_p4_games))

# download full box score and play by play JSON
get_summary <- function(game_id, sport = "baseball", league = "college-baseball") {
  url <- glue(
    "https://site.api.espn.com/apis/site/v2/sports/{sport}/{league}/summary?event={game_id}"
  )
  request(url) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
}

#Given one game's data, find every play that represents a ball put into play, and extract its data.
get_batted_balls <- function(game_id, game_date = NA_character_) {

  pbp <- get_summary(game_id)

  # if a game doesn't have any plays, skip it with an empty table
  if (is.null(pbp$plays)) {
    return(tibble())
  }

  plays <- pbp$plays

  # Find the final Play Result for each plate appearance
  result_plays <- plays |>
    purrr::keep(\(x) {
      identical(x$type$id, "57") &&
        !is.null(x$atBatId)
    })

  # keep plays that are in play or foul (21)
  coordinate_plays <- plays |>
    purrr::keep(\(x) {

      if (is.null(x$hitCoordinate)) {
        return(FALSE)
      }

      text <- x$text %||% ""

      is_foul <- identical(x$type$id, "21") ||
        stringr::str_detect(
          text,
          regex("foul", ignore_case = TRUE)
        )

      is_in_play <- stringr::str_detect(
        text,
        regex("Ball In Play", ignore_case = TRUE)
      )

      is_foul || is_in_play
    })

  # for every play kept, get the batter id
  purrr::map_dfr(coordinate_plays, \(x) {

    at_bat_id <- x$atBatId %||% NA_character_

    # Find the final result for this plate appearance
    result_play <- result_plays |>
      purrr::keep(\(r) identical(r$atBatId, at_bat_id)) |>
      purrr::pluck(1, .default = NULL)

    # Batter ID
    batter_id <- x$participants |>
      purrr::keep(\(p) identical(p$type, "batter")) |>
      purrr::pluck(
        1,
        "athlete",
        "id",
        .default = NA_character_
      )

    # Identify whether this coordinate event is a foul
    is_foul <- identical(x$type$id, "21") ||
      stringr::str_detect(
        x$text %||% "",
        regex("foul", ignore_case = TRUE)
      )

    # Identify whether this is an actual ball in play
    is_in_play <- stringr::str_detect(
      x$text %||% "",
      regex("Ball In Play", ignore_case = TRUE)
    )

    tibble::tibble(
      game_id   = game_id,
      date      = game_date,
      year      = as.integer(substr(game_date, 1, 4)),
      at_bat_id = x$atBatId %||% NA_character_,
      inning    = x$period$number %||% NA_integer_,
      half      = x$period$type %||% NA_character_,
      batter_id = batter_id,

      # Final result of the plate appearance
      result = if (!is.null(result_play)) {
        result_play$text %||% NA_character_
      } else {
        x$type$text %||% NA_character_
      },

      # Information about this specific coordinate event
      event_type = dplyr::case_when(
        is_foul ~ "foul",
        is_in_play ~ "in_play",
        TRUE ~ "other"
      ),

      play_type = x$type$text %||% NA_character_,
      play_text = x$text %||% NA_character_,

      hit_x = x$hitCoordinate$x %||% NA_real_,
      hit_y = x$hitCoordinate$y %||% NA_real_
    )
  })
}


# Actual coordinate events for every game
batted_balls_path <- "data/batted_balls_multi_2026.rds"
# Every game already checked including games with no coordinate data
checked_games_path <- "data/batted_balls_checked_2026.rds"


# Load existing batted-ball data
if (file.exists(batted_balls_path)) {

  message("Loading cached batted-ball data...")

  batted_balls_multi <- readRDS(batted_balls_path)

} else {

  batted_balls_multi <- tibble()

}

message(
  "Cached coordinate rows: ",
  nrow(batted_balls_multi),
  " / ",
  n_distinct(batted_balls_multi$game_id),
  " games"
)


# Load games that have already been checked
if (file.exists(checked_games_path)) {

  message("Loading checked-game cache...")

  checked_games <- readRDS(checked_games_path)

} else {

  checked_games <- tibble(
    game_id = character(),
    date = character(),
    coordinate_rows = integer()
  )

}

message(
  "Games already checked: ",
  nrow(checked_games)
)


# Make sure the games with coordinate data are also marked as checked
if (nrow(batted_balls_multi) > 0) {

  coordinate_games <- batted_balls_multi |>
    distinct(game_id) |>
    left_join(
      usable_p4_games |>
        select(game_id, date),
      by = "game_id"
    ) |>
    mutate(
      coordinate_rows = 1L
    )

  checked_games <- bind_rows(
    checked_games,
    coordinate_games
  ) |>
    distinct(game_id, .keep_all = TRUE)

}

# Determine which games still need to be queried
games_to_pull <- usable_p4_games |>
  filter(
    !game_id %in% checked_games$game_id
  )

message(
  "Games remaining to check: ",
  nrow(games_to_pull)
)


# Pull games that have not yet been checked
if (nrow(games_to_pull) > 0) {

  for (i in seq_len(nrow(games_to_pull))) {

    game_id <- games_to_pull$game_id[i]
    game_date <- games_to_pull$date[i]

    message(
      "[",
      i,
      "/",
      nrow(games_to_pull),
      "] ",
      game_id
    )

    result <- tryCatch(

      get_batted_balls(
        game_id = game_id,
        game_date = game_date
      ),

      error = function(e) {

        warning(
          "Failed game ",
          game_id,
          ": ",
          conditionMessage(e)
        )

        NULL

      }

    )


    # If coordinates were found, append them to the batted-ball data
    if (!is.null(result) && nrow(result) > 0) {

      batted_balls_multi <- bind_rows(
        batted_balls_multi,
        result
      )

      coordinate_rows <- nrow(result)

    } else {

      coordinate_rows <- 0L

    }


    # Regardless of whether coordinates were found, mark the game as checked.
    checked_games <- bind_rows(
      checked_games,
      tibble(
        game_id = game_id,
        date = game_date,
        coordinate_rows = coordinate_rows
      )
    ) |>
      distinct(game_id, .keep_all = TRUE)


    # Save every 10 games
    if (i %% 10 == 0 || i == nrow(games_to_pull)) {

      saveRDS(
        batted_balls_multi,
        batted_balls_path
      )

      saveRDS(
        checked_games,
        checked_games_path
      )

      message(
        "  -> checkpoint:",
        "\n     coordinate rows: ",
        nrow(batted_balls_multi),
        "\n     coordinate games: ",
        n_distinct(batted_balls_multi$game_id),
        "\n     games checked: ",
        nrow(checked_games)
      )

    }

    Sys.sleep(0.5)
  }

} else {

  message("No new games need to be checked.")

}


# Final save
saveRDS(
  batted_balls_multi,
  batted_balls_path
)

saveRDS(
  checked_games,
  checked_games_path
)

message(
  "Done.",
  "\nCoordinate rows: ",
  nrow(batted_balls_multi),
  "\nCoordinate games: ",
  n_distinct(batted_balls_multi$game_id),
  "\nGames checked: ",
  nrow(checked_games)
)

# match ESPN batter id with player name
get_batter_lookup <- function(game_id) {

  pbp <- get_summary(game_id)

  if (is.null(pbp$rosters)) {
    return(tibble())
  }

  purrr::map_dfr(pbp$rosters, \(team) {

    purrr::map_dfr(team$roster, \(player) {

      athlete <- player$athlete

      tibble(
        batter_id = athlete$id %||% NA_character_,
        batter_name = athlete$displayName %||% NA_character_
      )
    })
  })
}

spray_game_ids <- batted_balls_multi |>
  distinct(game_id) |>
  pull(game_id)

# cache the batter lookup to match ESPN batter id with player name
batter_lookup_path <- "data/batter_lookup_2026.rds"

if (file.exists(batter_lookup_path)) {

  message("Loading cached batter lookup")
  batter_lookup <- readRDS(batter_lookup_path)

} else {

  batter_lookup <- purrr::map_dfr(
    spray_game_ids,
    \(game_id) {

      message("Getting roster: ", game_id)

      result <- tryCatch(
        get_batter_lookup(game_id),
        error = function(e) {
          warning(
            "Failed game ", game_id, ": ",
            conditionMessage(e)
          )
          tibble()
        }
      )

      Sys.sleep(0.25)

      result
    }
  ) |>
    filter(!is.na(batter_id)) |>
    distinct(batter_id, .keep_all = TRUE)

  saveRDS(batter_lookup, batter_lookup_path)
}

batted_balls_multi <- batted_balls_multi |>
  left_join(
    batter_lookup,
    by = "batter_id"
  )

