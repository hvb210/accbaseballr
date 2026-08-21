library(httr2) # web requests
library(purrr) # tools for looping/mapping over lists (map_dfr, keep, pluck)
library(tibble)
library(dplyr)
library(lubridate) # dates
library(glue)
library(stringr)
library(ggplot2)

# if a is null, use be instead
`%||%` <- function(a, b) if (is.null(a)) b else a

# gets list of every college baseball team from ESPN
get_espn_teams <- function() { 
  
  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/",
    "baseball/college-baseball/teams?limit=1000"
  )
  
  request(url) |>
    req_perform() |> # sends web request
    resp_body_json(simplifyVector = FALSE) # turns JSON to R list
}

# stores the result of the function
espn_teams_raw <- get_espn_teams() 

#for each item in this list, run this function, and stick all the results together into one table
espn_teams <- purrr::map_dfr(
  espn_teams_raw$sports[[1]]$leagues[[1]]$teams,
  \(x) {
    
    # for each team: creates a table of team_id, team display name, and abbreviation
    team <- x$team
    
    tibble( 
      team_id = team$id %||% NA_character_,
      team_name = team$displayName %||% NA_character_,
      abbreviation = team$abbreviation %||% NA_character_
    )
  }
)

p4_names <- tribble(
  ~team_name, ~conference,
  
  # ACC
  "Boston College Eagles", "ACC",
  "California Golden Bears", "ACC",
  "Clemson Tigers", "ACC",
  "Duke Blue Devils", "ACC",
  "Florida State Seminoles", "ACC",
  "Georgia Tech Yellow Jackets", "ACC",
  "Louisville Cardinals", "ACC",
  "Miami Hurricanes", "ACC",
  "NC State Wolfpack", "ACC",
  "North Carolina Tar Heels", "ACC",
  "Notre Dame Fighting Irish", "ACC",
  "Pittsburgh Panthers", "ACC",
  "Stanford Cardinal", "ACC",
  "Syracuse Orange", "ACC",
  "Virginia Cavaliers", "ACC",
  "Virginia Tech Hokies", "ACC",
  "Wake Forest Demon Deacons", "ACC",
  
  # Big Ten
  "Illinois Fighting Illini", "Big Ten",
  "Indiana Hoosiers", "Big Ten",
  "Iowa Hawkeyes", "Big Ten",
  "Maryland Terrapins", "Big Ten",
  "Michigan Wolverines", "Big Ten",
  "Michigan State Spartans", "Big Ten",
  "Minnesota Golden Gophers", "Big Ten",
  "Nebraska Cornhuskers", "Big Ten",
  "Northwestern Wildcats", "Big Ten",
  "Ohio State Buckeyes", "Big Ten",
  "Oregon Ducks", "Big Ten",
  "Penn State Nittany Lions", "Big Ten",
  "Purdue Boilermakers", "Big Ten",
  "Rutgers Scarlet Knights", "Big Ten",
  "UCLA Bruins", "Big Ten",
  "USC Trojans", "Big Ten",
  "Washington Huskies", "Big Ten",
  
  # Big 12
  "Arizona Wildcats", "Big 12",
  "Arizona State Sun Devils", "Big 12",
  "Baylor Bears", "Big 12",
  "BYU Cougars", "Big 12",
  "Cincinnati Bearcats", "Big 12",
  "Houston Cougars", "Big 12",
  "Kansas Jayhawks", "Big 12",
  "Kansas State Wildcats", "Big 12",
  "Oklahoma State Cowboys", "Big 12",
  "TCU Horned Frogs", "Big 12",
  "Texas Tech Red Raiders", "Big 12",
  "UCF Knights", "Big 12",
  "Utah Utes", "Big 12",
  "West Virginia Mountaineers", "Big 12",
  
  # SEC
  "Alabama Crimson Tide", "SEC",
  "Arkansas Razorbacks", "SEC",
  "Auburn Tigers", "SEC",
  "Florida Gators", "SEC",
  "Georgia Bulldogs", "SEC",
  "Kentucky Wildcats", "SEC",
  "LSU Tigers", "SEC",
  "Mississippi State Bulldogs", "SEC",
  "Missouri Tigers", "SEC",
  "Ole Miss Rebels", "SEC",
  "Oklahoma Sooners", "SEC",
  "South Carolina Gamecocks", "SEC",
  "Tennessee Volunteers", "SEC",
  "Texas Longhorns", "SEC",
  "Texas A&M Aggies", "SEC",
  "Vanderbilt Commodores", "SEC"
)

# joins team_id, team display name, and abbreviation with conference for only power four teams
p4_teams <- espn_teams |>
  inner_join(
    p4_names,
    by = "team_name"
  )

# stores the id numbers
p4_ids <- p4_teams$team_id

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

get_game_pitcher_lookup <- function(pbp) {
  
  if (is.null(pbp$boxscore$players)) {
    return(tibble(
      pitcher_id = character(),
      pitcher_espn_name = character(),
      pitcher_team = character()
    ))
  }
  
  pbp$boxscore$players |>
    purrr::map_dfr(\(team) {
      
      team_abbreviation <- team$team$abbreviation %||% NA_character_
      
      pitching_stat <- team$statistics |>
        purrr::keep(\(stat) identical(stat$type, "pitching")) |>
        purrr::pluck(1, .default = list())
      
      pitcher_rows <- pitching_stat$athletes %||% list()
      
      purrr::map_dfr(pitcher_rows, \(row) {
        tibble(
          pitcher_id = row$athlete$id %||% NA_character_,
          
          pitcher_espn_name =
            (row$athlete$displayName %||% NA_character_) |>
            str_replace("\\s+-\\s+P\\s+", " ") |>
            str_squish(),
          
          pitcher_team = team_abbreviation
        )
      })
    }) |>
    filter(!is.na(pitcher_id)) |>
    distinct()
}

#Given one game's data, find every play that represents a ball put into play, and extract its data.
get_batted_balls <- function(game_id, game_date = NA_character_) {
  
  pbp <- get_summary(game_id)
  
  # if a game doesn't have any plays, skip it with an empty table
  if (is.null(pbp$plays)) {
    return(tibble())
  }
  
  pitcher_lookup <- get_game_pitcher_lookup(pbp)
  
  # filter to only "plate appearance result" (57) with hit coordinates
  batted_balls <- pbp$plays |>
    purrr::keep(\(x) {
      !is.null(x$hitCoordinate) &&
        x$type$id %in% c("57", "2")
    }) |>
    
    # for each remaining play, get the batter's id (athlete id)
    purrr::map_dfr(\(x) {
      
      batter <- x$participants |>
        purrr::keep(\(p) identical(p$type, "batter"))
      
      pitcher <- x$participants |>
        purrr::keep(\(p) identical(p$type, "pitcher"))
      
      batter_id <- if (length(batter) > 0) {
        batter[[1]]$athlete$id %||% NA_character_
      } else {
        NA_character_
      }
      
      pitcher_id <- if (length(pitcher) > 0) {
        pitcher[[1]]$athlete$id %||% NA_character_
      } else {
        NA_character_
      }
      
      
      tibble::tibble(
        game_id   = game_id,
        date      = game_date,
        year      = as.integer(substr(game_date, 1, 4)),
        at_bat_id = x$atBatId %||% NA_character_,
        inning    = x$period$number %||% NA_integer_,
        half      = x$period$type %||% NA_character_,
        batter_id    = batter_id,
        pitcher_id   = pitcher_id,
        result    = x$text %||% NA_character_,
        event_type = case_when(
          identical(x$type$id, "57") ~ "in_play",
          identical(x$type$id, "21") ~ "foul",
          TRUE ~ "other"
        ),
        play_type = x$alternativeType$text %||% NA_character_,
        play_text = x$shortText %||% x$text %||% NA_character_,
        hit_x     = x$hitCoordinate$x %||% NA_real_,
        hit_y     = x$hitCoordinate$y %||% NA_real_
      )
    })
  
  batted_balls |>
    left_join(
      pitcher_lookup,
      by = "pitcher_id"
    )
}

# Because this step makes one web request PER GAME and there
# could be hundreds of games, it might take a long time and
# could fail partway through (network issue, rate limiting,
# you closing your laptop, etc). To avoid losing all that work,
# we save progress periodically, and check at the start whether
# there's already a partial run saved that we can pick back up.
checkpoint_path <- "data/batted_balls_multi_2026_partial.rds"
final_path      <- "data/batted_balls_multi_2026.rds"


already_pulled <- character(0)
if (file.exists(checkpoint_path)) {
  message("Found existing checkpoint — resuming")
  prior_results <- readRDS(checkpoint_path)
  already_pulled <- unique(prior_results$game_id)
} else {
  prior_results <- tibble()
}

games_to_pull <- usable_p4_games |>
  filter(!(game_id %in% already_pulled))

message(
  "Games already done: ", length(already_pulled),
  " | Games remaining: ", nrow(games_to_pull)
)

results_list <- vector("list", nrow(games_to_pull))

for (i in seq_len(nrow(games_to_pull))) {
  game_id   <- games_to_pull$game_id[i]
  game_date <- games_to_pull$date[i]
  
  message("[", i, "/", nrow(games_to_pull), "] ", game_id)
  
  results_list[[i]] <- tryCatch(
    get_batted_balls(game_id, game_date),
    error = function(e) {
      warning("Failed game ", game_id, ": ", conditionMessage(e))
      tibble()
    }
  )
  
  Sys.sleep(0.5)
  
  # checkpoint every 50 games so a crash/rate-limit doesn't cost the whole run
  if (i %% 50 == 0 || i == nrow(games_to_pull)) {
    combined_so_far <- bind_rows(prior_results, bind_rows(results_list))
    saveRDS(combined_so_far, checkpoint_path)
    message("  -> checkpoint saved (", nrow(combined_so_far), " rows)")
  }
}

batted_balls_multi <- bind_rows(prior_results, bind_rows(results_list))

saveRDS(batted_balls_multi, final_path)
message("Done. Final dataset: ", nrow(batted_balls_multi), " batted balls saved to ", final_path)

