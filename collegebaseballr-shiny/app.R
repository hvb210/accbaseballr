library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(bslib)
library(reactable)
library(ggplot2)
library(ggimage)


college_batting_raw <- readRDS("data/batting.rds")
college_pitching_raw <- readRDS("data/pitching.rds")

### BUILD LEAGUE CONTEXT #######################################################

# league batting stats by season
league_batting <- college_batting_raw |>
  group_by(Season) |>
  summarise(
    league_PA = sum(PA, na.rm = TRUE),
    league_H = sum(H, na.rm = TRUE),
    league_2B = sum(`2B`, na.rm = TRUE),
    league_3B = sum(`3B`, na.rm = TRUE),
    league_HR = sum(HR, na.rm = TRUE),
    league_BB = sum(BB, na.rm = TRUE),
    league_HBP = sum(HBP, na.rm = TRUE),
    league_SF = sum(SF, na.rm = TRUE),
    league_AB = sum(AB, na.rm = TRUE),
    league_BA = sum(BA, na.rm = TRUE),
    .groups = "drop"
  )

# league 1B and wOBA calculation
league_context <- league_batting |>
  mutate(
    league_1B = league_H - league_2B - league_3B - league_HR,
    league_wOBA =
      (
        0.69 * league_BB +
          0.72 * league_HBP +
          0.89 * league_1B +
          1.27 * league_2B +
          1.62 * league_3B +
          2.10 * league_HR
      ) /
      (league_AB + league_BB + league_HBP + league_SF)
  )

# adding in league averages for bases on balls
league_context <- league_context |>
  mutate(
    league_BB_rate = league_BB / league_PA
  )

# league pitching stats by season
league_pitching <- college_pitching_raw |>
  group_by(Season) |>
  summarize(
    league_SO = sum(SO, na.rm = TRUE),
    p_league_BB = sum(BB, na.rm = TRUE),
    p_league_HR = sum(HR, na.rm = TRUE),
    league_IP = sum(IP, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    league_K_per_9 = (league_SO / league_IP) * 9,
    league_BB_per_9 = (p_league_BB / league_IP) * 9,
    league_HR_per_9 = (p_league_HR / league_IP) * 9
  )

# combine league averages for batting and pitching
league_context <- left_join(
  league_context,
  league_pitching,
  by = "Season"
)

### BATTING SABERMETRICS #######################################################

college_batting <- college_batting_raw |>
  mutate(
    bats = case_when(
      str_detect(Name, "\\*") ~ "Left",
      str_detect(Name, "#") ~ "Both",
      str_detect(Name, "\\?") ~ "Unknown",
      TRUE ~ "Right"
    ),
    Name = str_remove_all(Name, "\\*|#|\\?"),
    player_id = str_extract(`Name-additional`, "(?<=id=).*")
  )

college_batting <- college_batting |>
  mutate(
    ISO = SLG - BA,
    K_pct = SO / PA,
    BABIP = (H - HR) / (AB - SO - HR + SF),
    BB_pct = BB / PA,
    SecA = (TB - H + BB + SB - CS) / AB
  )

college_batting <- college_batting |>
  mutate(
    `1B` = H - `2B` - `3B` - HR
  )

college_batting <- college_batting |>
  mutate(
    wOBA =
      (
        0.69 * BB +
          0.72 * HBP +
          0.89 * `1B` +
          1.27 * `2B` +
          1.62 * `3B` +
          2.10 * HR
      ) /
      (AB + BB + HBP + SF)
  )

# join the league_wOBA with player level batting data
college_batting <- college_batting |>
  left_join(
    league_context |> select(Season, league_wOBA),
    by = "Season"
  )

# create a scale of 1.25 for normalizing wRC+
college_batting <- college_batting |>
  group_by(Season) |>
  mutate(
    scale = 1.25
  ) |>
  ungroup()

# calculate wRC+
college_batting <- college_batting |>
  mutate(
    wRC_plus = 100 * ((wOBA - league_wOBA) / scale + 1)
  )

# drop columns not used in calculations
college_batting <- college_batting |>
  select(-c(CS, RBI, SB, OBP, OPS, TB, GDP, SH, Notes, `Name-additional`, `...31`, `...32`, `...33`))

### PITCHING SABERMETRICS ######################################################

college_pitching <- college_pitching_raw |>
  mutate(
    throws = case_when(
      str_detect(Name, "\\*") ~ "Left",
      str_detect(Name, "#") ~ "Both",
      str_detect(Name, "\\?") ~ "Unknown",
      TRUE ~ "Right"
    ),
    Name = str_remove_all(Name, "\\*|#|\\?"),
    player_id = str_extract(`Name-additional`, "(?<=id=).*")
  )

college_pitching <- college_pitching |>
  mutate(
    K_pct = SO / BF,
    BB_pct = BB / BF,
    K_BB_pct = K_pct - BB_pct,
    FIP = (13 * HR + 3 * (BB + HBP) - 2 * SO) / IP + 3.10
  )

# drop columns not used in calculations
college_pitching <- college_pitching |>
  select(-c(W, L, `W-L%`, G, GF, CG, SHO, SV, BK, WP, H9, HR9, BB9, SO9, `SO/W`, Notes, `Name-additional`))

# creating this variable for checkbox selection in the UI
all_conferences <- sort(unique(college_batting$Conference))

### UI BUILD ###################################################################

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#FFFFFF",
    fg = "#013ca6"
  ),

  titlePanel("College Baseball Explorer"),

  #### SIDEBAR DROPDOWNS AND SLIDERS #############################################

  layout_sidebar(

    sidebar = sidebar(

      # creating checkbox options

      pickerInput(
        inputId = "Conference", # dataset variable name
        label = "Conference", # display name in app
        choices = all_conferences, # list of all conferences
        selected = all_conferences, # default selection of All
        multiple = TRUE, #may select multiple conferences
        options = list(
          `actions-box` = TRUE, # adds 'select all' and 'deselect all' buttons
          `selected-text-format` = "count > 3" # displays number selected for 4+
        )
      ),

      # creating sidebar drop down options

      selectInput(
        "Season",
        "Season",
        choices = sort(unique(college_batting$Season),
                       decreasing = TRUE)
      ),

      selectInput(
        "Team",
        "Team",
        choices = "All"
      ),


      # create conditional sliders for numeric filters

      conditionalPanel(
        condition = "input.tabs == 'Batting'|| input.tabs == 'Player Profiles'",

        sliderInput(
          "min_PA",
          "Minimum PA",
          min = 0,
          max = max(college_batting$PA),
          value = 50,  # setting default value to 50 PA
          step = 5
        ),
      ),

      conditionalPanel(
        condition = "input.tabs == 'Pitching'|| input.tabs == 'Player Profiles'",

        sliderInput(
          "min_IP",
          "Minimum IP",
          min = 0,
          max = max(college_pitching$IP),
          value = 50,  # setting default value to 50 IP
          step = 5
        )
      )

    ),


    #### SITE NAVIGATION BUTTONS ###################################################

    card(

      # creating navigation tabs for each section of the app

      navset_tab(

        id = "tabs",  # tracks which tab user is viewing for conditional sliders

        nav_panel(
          "Batting",
          h3(textOutput("batting_title")),
          downloadButton("download_batting", "Download CSV"),
          withSpinner(
            reactableOutput("batting_table"),
            type = 1
          )
        ),

        nav_panel(
          "Pitching",
          h3(textOutput("pitching_title")),
          downloadButton("download_pitching", "Download CSV"),
          withSpinner(
            reactableOutput("pitching_table"),
            type = 1
          )
        ),

        nav_panel(
          "Player Profiles",

          # creates subtabs within the Player Profile panel
          navset_tab(

            nav_panel(
              "Batters",

              selectInput(
                "batter_select",
                "Select Batter",
                choices = NULL # this gets populated with updateSelectInput()
              ),

              withSpinner(
                reactableOutput("batter_profile"),
                type = 1
                ),

              p("Percentiles are calculated within each season among selected conference players
                who meet the minimum PA threshold set by the user.")
            ),

            nav_panel(
              "Pitchers",

              selectInput(
                "pitcher_select",
                "Select Pitcher",
                choices = NULL # this gets populated with updateSelectInput()
              ),

              withSpinner(
                reactableOutput("pitcher_profile"),
                type = 1
                ),

              p("Percentiles are calculated within each season among selected conference players
                who meet the minimum IP threshold set by the user.")
            ),

          )
        ),

        nav_panel(
          "Conference Leaderboard",
          h3(textOutput("team_leader_title")),
          downloadButton("download_leader", "Download CSV"),
          withSpinner(
            reactableOutput("team_leader_table"),
            type = 1
          )
        ),

        nav_panel(
          "Conference Visualizations",
          h3(textOutput("visualization_title")),
          card(
            style = "min-height: 700px;",  # set so the panel is long enough to see the drop down options
            p("Select a metric to display a team comparison chart."),
            selectInput(
              "team_metric",
              "Metric",
              choices = c(
                "wRC+" = "wRC_plus",
                "wOBA vs ISO" = "offensive_profile",
                "FIP vs K-BB%" = "pitching_profile",
                "BB% vs ISO %iles" = "plate_discipline_vs_power")
            ),
            withSpinner(
              plotOutput("team_plot",
                       height = "500px"),
              type = 1
              ), # keeps text from overlapping plot
            uiOutput("plot_description") # allows for multiple paragraph description under plot
          )
        ),


        nav_panel(
          "About",
          h3("About the College Baseball Explorer"),
          p("This app explores historical Power Four baseball performance using data curated from Sports Reference
            and enhanced with advanced sabermetric calculations."),
          p("The dataset includes player-level batting and pitching statistics from ACC, SEC, and Big 12 baseball seasons from 2011-present. (Big Ten coming soon)"),
          h4("Batting Metrics"),
          p("wOBA (weighted On-Base Average) measures a hitter’s total offensive value per plate appearance using the following weights
          for different offensive outcomes: 0.69 (BB), 0.72 (HBP), 0.89 (1B), 1.27 (2B), 1.62 (3B), and 2.10 (HR)."),
          p("wRC+ (weighted Runs Created Plus) measures a hitter's total offensive value and run-creation per plate appearance,
            scaled so that 100 is league average."),
          p("ISO (Isolate Power) measures a batter's raw power by tracking extra-base hits. Calculated by subtracting batting average
            from slugging percentage."),
          p("BABIP (Batting Average from Balls in Play) measures how often a ball put into play by a hitter turns into a base hit,
            leaving out home runs, strikeouts, and walks."),
          p("SecA (Secondary Average) measures the sum of extra bases gained on hits, walks, and stolen bases (less times caught stealing) per at bat
            to evaluate the number of bases a player gained independent of batting average."),
          h4("Pitching Metrics"),
          p("WHIP (Walk plus Hits per Innings Pitched) measures the average number of base runners a pitcher allows per inning."),
          p("K-BB% measures a pitchers control by subtracting the number of walks from strikeouts."),
          p("FIP (Fielding Independent Pitching) measures a pitcher's true performance using only strikeouts, walks, hit-by-pitches, and home runs.
            It removes balls hit into play to eliminate luck and the quality of team defense."),
          h4("Data Source"),
          p("The underlying player statistics were collected from Sports Reference and processed using R
            to calculate and exploring metrics such as wOBA, wRC+, and FIP
            that are not typically available through standard college baseball statistics sources."),
          p("Created by Hana Baskin as a project exploring advanced analytics in college baseball.")
        )
      )

    )
  )
)

logos <- tibble(
  Team = c(
    # ACC
    "Duke",
    "North Carolina",
    "Virginia",
    "Notre Dame",
    "Virginia Tech",
    "Stanford",
    "Boston College",
    "Clemson",
    "Pittsburgh",
    "Miami",
    "Georgia Tech",
    "NC State",
    "Florida State",
    "Louisville",
    "Wake Forest",
    "California",
    "Maryland", # also in Big Ten
    # SEC
    "Georgia",
    "Alabama",
    "Texas", # also in Big 12
    "LSU",
    "Arkansas",
    "Texas A&M", # also in Big 12
    "Auburn",
    "Mississippi State",
    "Florida",
    "Oklahoma", # also Big 12
    "Tennessee",
    "Kentucky",
    "Ole Miss",
    "South Carolina",
    "Missouri", # also Big 12
    "Vanderbilt",
    # Big 12
    "West Virginia",
    "Central Florida",
    "Kansas",
    "Baylor",
    "Arizona State",
    "Oklahoma State",
    "Kansas State",
    "Cincinnati",
    "TCU",
    "Texas Tech",
    "Utah",
    "BYU",
    "Houston",
    "Arizona",
    # Big Ten
    "Illinois",
    "Indiana",
    "Iowa",
    "Michigan State",
    "Michigan",
    "Minnesota",
    "Nebraska",
    "Northwestern",
    "Ohio State",
    "Penn State",
    "Purdue",
    "Rutgers",
    "Oregon",
    "UCLA",
    "USC",
    "Washington"
  ),
  logo = c(
    # ACC
    "www/duke_logo.png",
    "www/unc_logo.png",
    "www/uva_logo.png",
    "www/nd_logo.png",
    "www/vt_logo.png",
    "www/stanford_logo.png",
    "www/bc_logo.png",
    "www/clemson_logo.png",
    "www/pitt_logo.png",
    "www/miami_logo.png",
    "www/gt_logo.png",
    "www/ncstate_logo.png",
    "www/fsu_logo.png",
    "www/louisville_logo.png",
    "www/wake_logo.png",
    "www/cal_logo.png",
    "www/maryland_logo.png",
    # SEC
    "www/georgia_logo.png",
    "www/alabama_logo.png",
    "www/texas_logo.png",
    "www/lsu_logo.png",
    "www/arkansas_logo.png",
    "www/texasam_logo.png",
    "www/auburn_logo.png",
    "www/mississippi_state_logo.png",
    "www/florida_logo.png",
    "www/oklahoma_logo.png",
    "www/tennessee_logo.png",
    "www/kentucky_logo.png",
    "www/ole_miss_logo.png",
    "www/south_carolina_logo.png",
    "www/missouri_logo.png",
    "www/vanderbilt_logo.png",
    # Big 12
    "www/west_virginia_logo.png",
    "www/ucf_logo.png",
    "www/kansas_logo.png",
    "www/baylor_logo.png",
    "www/arizona_state_logo.png",
    "www/oklahoma_state_logo.png",
    "www/kansas_state_logo.png",
    "www/cincinnati_logo.png",
    "www/tcu_logo.png",
    "www/texas_tech_logo.png",
    "www/utah_logo.png",
    "www/byu_logo.png",
    "www/houston_logo.png",
    "www/arizona_logo.png",
    # Big Ten
    "www/illinois_logo.png",
    "www/indiana_logo.png",
    "www/iowa_logo.png",
    "www/michigan_state_logo.png",
    "www/michigan_logo.png",
    "www/minnesota_logo.png",
    "www/nebraska_logo.png",
    "www/northwestern_logo.png",
    "www/ohio_state_logo.png",
    "www/penn_state_logo.png",
    "www/purdue_logo.png",
    "www/rutgers_logo.png",
    "www/oregon_logo.png",
    "www/ucla_logo.png",
    "www/usc_logo.png",
    "www/washington_logo.png"
  )

)

server <- function(input, output, session) {

  selected_conferences <- reactive({

    input$Conference

  })
  observe({

    req(input$Conference, input$Season)

    team_choices <- college_batting |>
     filter(Conference %in% input$Conference, # only teams in that conference and season will display
            Season %in% input$Season) |>
     pull(Team) |>
     unique() |>
     sort()

   updateSelectInput(
      session,
      "Team",
      choices = c("All", team_choices),
      selected = "All"
    )

  })


  #### BATTING STATISTICS TABLE ##################################################

  batting_data <- reactive({

    data <- college_batting |>
      filter(Season == input$Season,
             PA >= input$min_PA,
             Conference %in% selected_conferences()
             )


    if(input$Team !="All") {
      data <- data |>
        filter(Team == input$Team)
    }

    if (input$Team != "All") {
      data |>
        select(Name, bats, G, PA, AB, R, H, HR, BB, SO, BA, SecA, ISO, K_pct, BABIP, BB_pct, wOBA, wRC_plus)
    }

    else {

      data |>
        select(Name, Team, Conference, bats, G, PA, AB, R, H, HR, BB, SO, BA, SecA, ISO, K_pct, BABIP, BB_pct, wOBA, wRC_plus)
    }

  })

  output$batting_table <- renderReactable({

    reactable(batting_data(),
              columns = list(
                Name = colDef(
                  sticky = "left",
                  width = 160
                ),
                Team = colDef(
                  sticky = "left",
                  width = 140,
                  style = list( # keep this column from overlapping the first
                    left = "160px"
                  )
                ),
                bats = colDef(
                  name = "Bats"
                ),
                SecA = colDef(
                  format = colFormat(digits = 3)
                ),
                BA = colDef(
                  format = colFormat(digits = 3)
                ),
                ISO = colDef(
                  format = colFormat(digits = 3)
                ),
                K_pct = colDef(
                  name = "K%",
                  format = colFormat(digits = 3)
                ),
                BABIP = colDef(
                  format = colFormat(digits = 3)
                ),
                BB_pct = colDef(
                  name = "BB%",
                  format = colFormat(digits = 3)
                ),
                wOBA = colDef(
                  format = colFormat(digits = 3)
                ),
                wRC_plus = colDef(
                  name = "wRC+",
                  format = colFormat(digits = 3)
                )
              ),
              defaultColDef = colDef(
                style = list(whiteSpace = "nowrap"),
                headerStyle = list(whiteSpace = "nowrap")
              ),
              filterable = TRUE,
              striped = TRUE,
              highlight = TRUE,
              bordered = TRUE,
              defaultPageSize = 20)

  })

  #### PITCHING STATISTICS TABLE #################################################

  pitching_data <- reactive({

    data <- college_pitching |>
      filter(Season == input$Season,
             IP >= input$min_IP,
             Conference %in% selected_conferences()
             )


    if (input$Team != "All") {
      data <- data |>
        filter(Team == input$Team)
    }

    if (input$Team != "All") {
      data |>
        select(Name, throws, GS, IP, ERA, H, R, HR, BB, IBB, SO, HBP, WHIP, K_pct, BB_pct, K_BB_pct, FIP)
    }

    else {

      data |>
        select(Name, Team, Conference, throws, GS, IP, ERA, H, R, HR, BB, IBB, SO, HBP, WHIP, K_pct, BB_pct, K_BB_pct, FIP)
    }

  })

  output$pitching_table <- renderReactable({

    reactable(pitching_data(),
              columns = list(
                Name = colDef(
                  sticky = "left",
                  width = 160
                ),
                Team = colDef(
                  sticky = "left",
                  width = 140,
                  style = list( # keep this column from overlapping the first
                    left = "160px"
                  )
                ),
                throws = colDef(
                  name = "Throws"
                ),
                K_pct = colDef(
                  name = "K%",
                  format = colFormat(digits = 3)
                ),
                BB_pct = colDef(
                  name = "BB%",
                  format = colFormat(digits = 3)
                ),
                K_BB_pct = colDef(
                  name = "K-BB%",
                  format = colFormat(digits = 3)
                ),
                FIP = colDef(
                  format = colFormat(digits = 3)
                )
              ),
              defaultColDef = colDef(
                style = list(whiteSpace = "nowrap"),
                headerStyle = list(whiteSpace = "nowrap")
              ),
              filterable = TRUE,
              striped = TRUE,
              highlight = TRUE,
              bordered = TRUE,
              defaultPageSize = 20)
  })

  #### BATTER/PITCHER PLAYER PROFILE ##########################

  # update choices for batter/pitcher based on Season and Team selections
  observe({

    batter_choices <- college_batting |>
      filter(
        Season == input$Season,
        Conference %in% selected_conferences()
      )


    if (input$Team != "All") {
      batter_choices <- batter_choices |>
        filter(Team == input$Team)
    }


    updateSelectInput(
      session,
      "batter_select",
      choices = sort(unique(batter_choices$Name))
    )

  })

  observe({

    pitcher_choices <- college_pitching |>
      filter(
        Season == input$Season,
        Conference %in% selected_conferences()
      )


    if (input$Team != "All") {
      pitcher_choices <- pitcher_choices |>
        filter(Team == input$Team)
    }

    updateSelectInput(
      session,
      "pitcher_select",
      choices = sort(unique(pitcher_choices$Name))
    )

  })

  batter_profile_data <- reactive({

    batter_percentile <- college_batting |>
      filter(PA >= input$min_PA, # keeps percentile rankings more accurate
             Conference %in% selected_conferences() # percentile calculated based on conferences selected
             ) |>
      group_by(Season) |>
      mutate(
        wRC_plus_percentile = percent_rank(wRC_plus) * 100,
        BB_percentile = percent_rank(BB_pct) * 100,
        K_percentile = (1 - percent_rank(K_pct)) * 100,
        BABIP_percentile = percent_rank(BABIP) * 100,
        ISO_percentile = percent_rank(ISO) * 100,
        SecA_percentile = percent_rank(SecA) * 100
      ) |>
      ungroup() |>
      select(
        Season,
        Name,
        wRC_plus_percentile,
        BB_percentile,
        K_percentile,
        BABIP_percentile,
        ISO_percentile,
        SecA_percentile
      )

    college_batting |>
      left_join(
        batter_percentile,
        by = c("Season", "Name"),
        relationship = "many-to-many"
      ) |>
      filter(Name == input$batter_select) |>
      arrange(desc(Season))

  })

  output$batter_profile <- renderReactable({

    batter_profile_data() |>
      select(
        Season,
        Team,
        Conference,
        PA,
        wRC_plus,
        wRC_plus_percentile,
        BB_pct,
        BB_percentile,
        K_pct,
        K_percentile,
        BABIP,
        BABIP_percentile,
        ISO,
        ISO_percentile,
        SecA,
        SecA_percentile
      ) |>
      reactable(
        columns = list(
          Season = colDef( # freeze column
            sticky = "left",
            width = 160
          ),
          Team = colDef(
            sticky = "left",
            width = 140,
            style = list( # keep this column from overlapping the first
              left = "160px"
            )
          ),
          wRC_plus = colDef(
            name = "wRC+",
            format = colFormat(digits = 3)
          ),
          ISO = colDef(
            name = "ISO",
            format = colFormat(digits = 3)
          ),
          wRC_plus_percentile = colDef(
            name = "wRC+ %ile",
            format = colFormat(digits = 1)
          ),
          ISO_percentile = colDef(
            name = "ISO %ile",
            format = colFormat(digits = 1)
          ),
          BB_pct = colDef(
            name = "BB%",
            format = colFormat(digits = 3)
          ),
          BB_percentile = colDef(
            name = "BB% %ile",
            format = colFormat(digits = 1)
          ),
          K_pct = colDef(
            name = "K%",
            format = colFormat(digits = 1)
          ),
          K_percentile = colDef(
            name = "K% %ile",
            format = colFormat(digits = 1)
          ),
          BABIP = colDef(
            name = "BABIP",
            format = colFormat(digits = 3)
          ),
          BABIP_percentile = colDef(
            name = "BABIP %ile",
            format = colFormat(digits = 1)
          ),
          SecA = colDef(
            format = colFormat(digits = 3)
          ),
          SecA_percentile = colDef(
            name = "SecA %ile",
            format = colFormat(digits = 1)
          )
        ),
        defaultColDef = colDef(
          style = list(whiteSpace = "nowrap"),
          headerStyle = list(whiteSpace = "nowrap")
        ),
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        defaultPageSize = 20
      )

  })

  pitcher_profile_data <- reactive({

    pitcher_percentile <- college_pitching |>
      filter(
        IP >= input$min_IP, # keeps percentile rankings more accurate
        Conference %in% selected_conferences() # percentile calculated based on conferences selected
        ) |>
      group_by(Season) |>
      mutate(
        K_BB_percentile = percent_rank(K_BB_pct) * 100,
        FIP_percentile = (1 - percent_rank(FIP)) * 100,
        WHIP_percentile = (1 - percent_rank(WHIP)) * 100
      ) |>
      ungroup() |>
      select(
        Season,
        Name,
        K_BB_percentile,
        FIP_percentile,
        WHIP_percentile
      )

    college_pitching |>
      left_join(
        pitcher_percentile,
        by = c("Season", "Name")
      ) |>
      filter(Name == input$pitcher_select) |>
      arrange(desc(Season))

  })

  output$pitcher_profile <- renderReactable({

    pitcher_profile_data() |>
      select(
        Season,
        Team,
        IP,
        ERA,
        WHIP,
        WHIP_percentile,
        K_BB_pct,
        K_BB_percentile,
        FIP,
        FIP_percentile
      ) |>
      reactable(
        columns = list(
          Season = colDef(
            sticky = "left",
            width = 160
          ),
          Team = colDef(
            sticky = "left",
            width = 140,
            style = list( # keep this column from overlapping the first
              left = "160px"
            )
          ),
          K_BB_pct = colDef(
            name = "K-BB%",
            format = colFormat(digits = 3)
          ),
          K_BB_percentile = colDef(
            name = "K-BB% %ile",
            format = colFormat(digits = 1)
          ),
          FIP = colDef(
            name = "FIP",
            format = colFormat(digits = 3)
          ),
          FIP_percentile = colDef(
            name = "FIP %ile",
            format = colFormat(digits = 1)
          ),
          WHIP = colDef(
            name = "WHIP",
            format = colFormat(digits = 3)
          ),
          WHIP_percentile = colDef(
            name = "WHIP %ile",
            format = colFormat(digits = 1)
          )
        ),
        defaultColDef = colDef(
          style = list(whiteSpace = "nowrap"),
          headerStyle = list(whiteSpace = "nowrap")
        ),
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        defaultPageSize = 20
      )

  })

  #### TEAM LEADERBOARD TABLE ####################################################

  team_leader_data <- reactive({

    #batting team averages
    batting_team <- college_batting |>
      filter(Season == input$Season,
             PA > 0,
             AB > 0,
             Conference %in% selected_conferences()
             )


    batting_team <- batting_team |>
      group_by(Conference, Team) |>
      summarise(
        wRC_plus = mean(wRC_plus, na.rm = TRUE),
        wOBA = mean(wOBA, na.rm = TRUE),
        ISO = mean(ISO, na.rm = TRUE),
        total_HR = sum(HR, na.rm = TRUE),
        K_pct = mean(K_pct, na.rm = TRUE),
        SO_bat = mean(SO, na.rm = TRUE),
        SecA = mean(SecA, na.rm = TRUE)
      )

    # pitching team averages
    pitching_team <- college_pitching |>
      filter(Season == input$Season,
             IP > 0,
             Conference %in% selected_conferences()
             )


    pitching_team <- pitching_team |>
      group_by(Conference, Team) |>
      summarise(
        FIP = mean(FIP, na.rm = TRUE),
        K_BB_pct = mean(K_BB_pct, na.rm = TRUE),
        ERA = mean(ERA, na.rm = TRUE),
        WHIP = mean(WHIP, na.rm = TRUE),
        SO_pitch = mean(SO, na.rm = TRUE),
        total_HBP = sum(HBP, na.rm = TRUE),
        .groups = "drop"
      ) |>
      select(-Conference) # removes column so the join doesn't duplicate

    # join batting and pitching stats together by team name
    leaderboard_data <- batting_team |>
      left_join(
        pitching_team,
        by = "Team"
      ) |>
      select(
        Team, # puts teams first so this column can be frozen
        Conference,
        everything()
      ) |>
      arrange(desc(wRC_plus))
  })

  output$team_leader_table <- renderReactable({

    reactable(team_leader_data(),
              defaultSorted = "wRC_plus",
              defaultSortOrder = "desc",
              columns = list(
                Team = colDef(
                  sticky = "left",
                  width = 160
                ),
                wRC_plus = colDef(
                  name = "Avg wRC+",
                  format = colFormat(digits = 3)
                ),
                wOBA = colDef(
                  name = "Avg wOBA",
                  format = colFormat(digits = 3)
                ),
                ISO = colDef(
                  name = "Avg ISO",
                  format = colFormat(digits = 3)
                ),
                total_HR = colDef(
                  name = "Total HR"
                ),
                K_pct = colDef(
                  name = "Avg K%",
                  format = colFormat(digits = 3)
                ),
                SO_bat = colDef(
                  name = "Avg Batting SO",
                  format = colFormat(digits = 3),
                  width = 160
                ),
                total_HBP = colDef(
                  name = "Total HBP"
                ),
                FIP = colDef(
                  name = "Avg FIP",
                  format = colFormat(digits = 3)
                ),
                K_BB_pct = colDef(
                  name = "Avg K-BB%",
                  format = colFormat(digits = 3)
                ),
                ERA = colDef(
                  name = "Avg ERA",
                  format = colFormat(digits = 3)
                ),
                WHIP = colDef(
                  name = "Avg WHIP",
                  format = colFormat(digits = 3)
                ),
                SO_pitch = colDef(
                  name = "Avg Pitching SO",
                  format = colFormat(digits = 3),
                  width = 160
                ),
                SecA = colDef(
                  name = "Avg SecA",
                  format = colFormat(digits = 3)
                )
              ),

              defaultColDef = colDef(
                style = list(whiteSpace = "nowrap"),
                headerStyle = list(whiteSpace = "nowrap")
              ),
              filterable = TRUE,
              striped = TRUE,
              highlight = TRUE,
              bordered = TRUE,
              defaultPageSize = 20)
  })

  #### DOWNLOAD TABLES TO CSV ####################################################

  output$download_batting <- downloadHandler(

    filename = function() {
      paste0(
        "batting_",
        input$Season,
        "_",
        input$Team,
        ".csv"
      )
    },

    content = function(file) {
      write.csv(
        batting_data(),
        file,
        row.names = FALSE
      )
    }

  )

  output$download_pitching <- downloadHandler(

    filename = function() {
      paste0(
        "pitching_",
        input$Season,
        "_",
        input$Team,
        ".csv"
      )
    },

    content = function(file) {
      write.csv(
        pitching_data(),
        file,
        row.names = FALSE
      )
    }

  )

  output$download_leader <- downloadHandler(

    filename = function() {
      paste0(
        paste(input$Conference, collapse = "_"),
        "_leader_",
        input$Season,
        ".csv"
      )
    },

    content = function(file) {
      write.csv(
        team_leader_data(),
        file,
        row.names = FALSE
      )
    }

  )
  #### VISUALIZATIONS ############################################################

  filtered_batting <- reactive({

    data <- college_batting |>
      filter(Season == input$Season,
             Conference %in% selected_conferences())

    data

  })

  filtered_pitching <- reactive({

    data <- college_pitching |>
      filter(Season == input$Season,
             Conference %in% selected_conferences()
             )

    data

  })

  team_percentile_data <- reactive({

    college_batting |>
      filter(
        Season == input$Season,
        Conference %in% selected_conferences(),
        PA > 0
      ) |>
      group_by(Team) |>
      summarise(
        team_PA = sum(PA, na.rm = TRUE),
        team_BB = sum(BB, na.rm = TRUE),
        team_BA = sum(BA, na.rm = TRUE),
        team_SLG = sum(SLG, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        team_BB_pct = team_BB / team_PA * 100,
        team_ISO = team_SLG - team_BA
      ) |>
      mutate(
        BB_percentile = percent_rank(team_BB_pct) * 100,
        ISO_percentile = percent_rank(team_ISO) * 100
      ) |>
      left_join(logos, by = "Team")

  })

  output$team_plot <- renderPlot({

    metric <- input$team_metric

    if(metric == "wRC_plus") {

      filtered_batting() |>
        filter(PA > 0) |>
        group_by(Team) |>
        summarise(
          wRC_plus = mean(wRC_plus, na.rm = TRUE)
        ) |>
        ggplot(aes(x = reorder(Team, wRC_plus),
                   y = wRC_plus)) +
        geom_col(fill = "#013ca6") +
        coord_flip() +
        labs(
          x = NULL,
          y = "Team Avg wRC+",
          title = paste(input$Season, "Team Offensive Production")
        ) +
        theme_minimal()
    }

    else if(metric == "offensive_profile") {


      filtered_batting() |>
        filter(PA > 0) |>
        group_by(Team) |>
        summarise(
          avg_ISO = mean(ISO, na.rm = TRUE),
          avg_wOBA = mean(wOBA, na.rm = TRUE)
        ) |>
        left_join(logos, by = "Team") |>
        ggplot(aes(x = avg_ISO, y = avg_wOBA)) +
        geom_image(aes(image = logo), size = 0.06) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(
          x = "ISO",
          y = "wOBA",
          title = paste(input$Season, "Team Average wOBA vs ISO")
        ) +
        theme_minimal()

    }

    else if(metric == "pitching_profile") {


      filtered_pitching() |>
        filter(IP > 0) |>
        group_by(Team) |>
        summarise(
          avg_K_BB_pct = mean(K_BB_pct, na.rm = TRUE),
          avg_FIP = mean(FIP, na.rm = TRUE)
        ) |>
        left_join(logos, by = "Team") |>
        ggplot(aes(x = avg_K_BB_pct, y = avg_FIP)) +
        geom_image(aes(image = logo), size = 0.06) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(
          x = "K-BB%",
          y = "FIP",
          title = paste(input$Season, "Team Average FIP vs K-BB%")
        ) +
        theme_minimal()

    }

    else if(metric == "plate_discipline_vs_power") {

      team_percentile_data() |>
        ggplot(aes(x = ISO_percentile, y = BB_percentile)) +
        geom_vline(
          xintercept = 50,
          linetype = "dashed",
          color = "gray"
        ) +
        geom_hline(
          yintercept = 50,
          linetype = "dashed",
          color = "gray"
        ) +
        geom_image(
          aes(image = logo),
          size = 0.07
        ) +
        labs(
          x = "ISO Percentile",
          y = "BB% Percentile",
          title = paste(
            input$Season,
            conference_label(),
            "Team Plate Discipline vs Power"
          )
        ) +
        theme_minimal()
    }
  })

  #### DYNAMIC TABLE TITLES AND DESCRIPTIONS #####################################

  conference_label <- reactive({

    if ("All" %in% input$Conference) {
      "All Conferences"
    } else{
      paste(input$Conference, collapse =", ")
    }
  })

  output$batting_title <- renderText({

    if (input$Team != "All") {

      paste(input$Season,
            conference_label(),
            input$Team,
            "Batting Statistics (",
            input$min_PA,
            "+ PA)"
      )

    } else {

      paste(input$Season,
            conference_label(),
            "Batting Statistics (",
            input$min_PA,
            "+ PA)"
      )

    }

  })

  output$pitching_title <- renderText({

    if (input$Team != "All") {

      paste(input$Season,
            conference_label(),
            input$Team,
            "Pitching Statistics (",
            input$min_IP,
            "+ IP)"
      )

    }

    else {

      paste(input$Season,
            conference_label(),
            "Pitching Statistics (",
            input$min_IP,
            "+ IP)"
      )

    }
  })

  output$team_leader_title <- renderText({

    paste(input$Season,
          conference_label(),
          "Team Statistics Leaderboard"
    )

  })

  output$visualization_title <- renderText({

    paste(input$Season,
          conference_label(),
          "Team Visualizations"
    )
  })

  output$plot_description <- renderUI({

    metric <- input$team_metric

    if(metric == "offensive_profile") {

      "Teams above the line are getting on base more through walks, hit by pitch, and singles
        relative to what their ISO would suggest, as doubles, triples, and home runs
        are weighted more heavily in ISO."

    }

    else if(metric == "pitching_profile") {

      "The negative trend line shows that better strikeout-to-walk dominance (K-BB%)
        leads to fewer runs than expected scored against the team (FIP).
        Teams below the trend line have better-than-expected run prevention given their strikeout and walk profile."

    }

    else if(metric == "plate_discipline_vs_power") {

      tagList(
        p("Teams in the upper-right quadrant rank above the median in both walk rate and isolated power. They combine a disciplined approach at the plate with strong extra-base power."),

        p("Teams in the upper-left quadrant rank above the median in walk rate but below the median in isolated power. They tend to work counts and draw walks but generate less extra-base power."),

        p("Teams in the lower-right quadrant rank below the median in walk rate but above the median in isolated power. They walk less often but compensate with greater extra-base power."),

        p("Teams in the lower-left quadrant rank below the median in both walk rate and isolated power. They draw fewer walks and generate less extra-base power, reflecting a more aggressive, lower-power offensive approach.")
      )

    }

  })
}

shinyApp(ui, server)

