# =========================
# ACC Baseball Package Build
# =========================

library(tidyverse)
library(devtools)

stopifnot(file.exists("data-raw/build_batting.R"))
stopifnot(file.exists("data-raw/build_pitching.R"))
stopifnot(file.exists("data-raw/build_league_context.R"))

# -------------------------
# 1. League context
# -------------------------
source("data-raw/build_league_context.R")

# -------------------------
# 2. Batting (depends on league_context)
# -------------------------
source("data-raw/build_batting.R")

# -------------------------
# 3. Pitching
# -------------------------
source("data-raw/build_pitching.R")

# -------------------------
# 4. Players (depends on batting + pitching)
# -------------------------
source("data-raw/build_players.R")

# -------------------------
# 5. Package rebuild steps
# -------------------------
devtools::document()
devtools::install()

message("ACC Baseball package successfully rebuilt.")
