library(shiny)
library(accbaseballr)
library(dplyr)
library(bslib)
library(reactable)
library(ggplot2)
library(ggimage)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#FFFFFF",
    fg = "#013ca6"
  ),

  titlePanel("ACC Baseball Explorer"),

#### SIDEBAR DROPDOWNS AND SLIDERS #############################################

  layout_sidebar(

    sidebar = sidebar(

      # creating static sidebar drop down options

      selectInput(
        "Season",  # variable name in dataset
        "Season",  # label shown in app
        choices = sort(unique(batting$Season),
                       decreasing = TRUE)
      ),

      selectInput(
        "Team",
        "Team",
        choices = c("All", sort(unique(batting$Team),
                       decreasing = FALSE))
      ),


      # create conditional sliders for numeric filters

      conditionalPanel(
        condition = "input.tabs == 'Batters'",

        sliderInput(
          "min_PA",
          "Minimum PA",
          min = 0,
          max = max(batting$PA),
          value = 50,  # setting default value to 50 PA
          step = 5
          ),
      ),

      conditionalPanel(
        condition = "input.tabs == 'Pitchers'",

        sliderInput(
          "min_IP",
          "Minimum IP",
          min = 0,
          max = max(pitching$IP),
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
          "Batters",
          h3(textOutput("batting_title")),
          downloadButton("download_batting", "Download CSV"),
          reactableOutput("batting_table")
        ),

        nav_panel(
          "Pitchers",
          h3(textOutput("pitching_title")),
          downloadButton("download_pitching", "Download CSV"),
          reactableOutput("pitching_table")
        ),

        nav_panel(
          "Conference Leaderboard",
          h3(textOutput("team_leader_title")),
          downloadButton("download_leader", "Download CSV"),
          reactableOutput("team_leader_table")
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
                "FIP vs K-BB%" = "pitching_profile")
              ),
            plotOutput("team_plot"),
            textOutput("plot_description")
          )
        ),


        nav_panel(
          "About",
          h3("About the ACC Baseball Explorer"),
          p("This app explores historical ACC baseball performance using data from the accbaseballr R package."),
          p("The dataset includes player-level batting and pitching statistics from ACC baseball seasons."),
          h4("Batting Metrics"),
          p("wOBA (weighted On-Base Average) measures a hitter’s total offensive value per plate appearance using the following weights
          for different offensive outcomes: 0.69 (BB), 0.72 (HBP), 0.89 (1B), 1.27 (2B), 1.62 (3B), and 2.10 (HR)."),
          p("wRC+ (weighted Runs Created Plus) measures a hitter's total offensive value and run-creation per plate appearance,
            scaled so that 100 is league average."),
          p("ISO (Isolate Power) measures a batter's raw power by tracking extra-base hits. Calculated by subtracting batting average
            from slugging percentage."),
          p("BABIP (Batting Average from Balls in Play) measures how often a ball put into play by a hitter turns into a base hit,
            leaving out home runs, strikeouts, and walks."),
          h4("Pitching Metrics"),
          p("WHIP (Walk plus Hits per Innings Pitched) measures the average number of base runners a pitcher allows per inning"),
          p("K-BB% measures a pitchers control by subtracting the number of walks from strikeouts"),
          p("FIP (Fielding Independent Pitching) measures a pitcher's true performance using only strikeouts, walks, hit-by-pitches, and home runs.
            It removes balls hit into play to eliminate luck and the quality of team defense."),
          h4("Data Source"),
          p("The underlying player statistics were collected from Sports Reference and processed through accbaseballr,
          an R package created to provide access to advanced ACC baseball statistics.
          The package includes tools for calculating and exploring metrics such as wOBA, wRC+, and FIP
            that are not typically available through standard college baseball statistics sources."),
          p("accbaseballr is publicly available through CRAN, making these data and analytical tools accessible to the broader R community."),
          p("Created by Hana Baskin as a project exploring advanced analytics in college baseball.")
        )
      )

    )
  )
)

logos <- tibble(
  Team = c(
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
    "California"
  ),
  logo = c(
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
    "www/cal_logo.png"
  )

)

server <- function(input, output) {

#### BATTING STATISTICS TABLE ##################################################

  batting_data <- reactive({

    data <- batting |>
      filter(Season == input$Season,
             PA >= input$min_PA)


    if (input$Team != "All") {
      data |>
        filter(Team == input$Team) |>
        select(Name, bats, G, PA, AB, R, H, HR, BB, SO, BA, ISO, K_pct, BABIP, BB_pct, wOBA, wRC_plus)
    }

    else {

      data |>
        select(Name, Team, bats, G, PA, AB, R, H, HR, BB, SO, BA, ISO, K_pct, BABIP, BB_pct, wOBA, wRC_plus)
    }

  })

  output$batting_table <- renderReactable({

    reactable(batting_data(),
              columns = list(
                bats = colDef(
                  name = "Bats"
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

              filterable = TRUE,
              striped = TRUE,
              highlight = TRUE,
              bordered = TRUE,
              defaultPageSize = 20)

  })

#### PITCHING STATISTICS TABLE #################################################

  pitching_data <- reactive({

    data <- pitching |>
      filter(Season == input$Season,
             IP >= input$min_IP)


    if (input$Team != "All") {
      data |>
        filter(Team == input$Team) |>
        select(Name, throws, GS, IP, ERA, H, R, HR, BB, IBB, SO, HBP, WHIP, K_pct, BB_pct, K_BB_pct, FIP)
    }

    else {

      data |>
        select(Name, Team, throws, GS, IP, ERA, H, R, HR, BB, IBB, SO, HBP, WHIP, K_pct, BB_pct, K_BB_pct, FIP)
    }

  })

  output$pitching_table <- renderReactable({

    reactable(pitching_data(),
              columns = list(
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

              filterable = TRUE,
              striped = TRUE,
              highlight = TRUE,
              bordered = TRUE,
              defaultPageSize = 20)
   })


#### TEAM LEADERBOARD TABLE ####################################################

  team_leader_data <- reactive({

    #batting team averages
    batting_team <- batting |>
      filter(Season == input$Season,
             PA > 0) |>
      group_by(Team) |>
      summarise(
        wRC_plus = mean(wRC_plus, na.rm = TRUE),
        wOBA = mean(wOBA, na.rm = TRUE),
        ISO = mean(ISO, na.rm = TRUE),
        total_HR = sum(HR, na.rm = TRUE),
        K_pct = mean(K_pct, na.rm = TRUE),
        SO_bat = mean(SO, na.rm = TRUE)
      )

    # pitching team averages
    pitching_team <- pitching |>
      filter(Season == input$Season,
             IP > 0) |>
      group_by(Team) |>
      summarise(
        FIP = mean(FIP, na.rm = TRUE),
        K_BB_pct = mean(K_BB_pct, na.rm = TRUE),
        ERA = mean(ERA, na.rm = TRUE),
        WHIP = mean(WHIP, na.rm = TRUE),
        SO_pitch = mean(SO, na.rm = TRUE),
        total_HBP = sum(HBP, na.rm = TRUE)
      )

    # join batting and pitching stats together by team name
    leaderboard_data <- batting_team |>
      left_join(
        pitching_team,
        by = "Team"
      ) |>
      arrange(desc(wRC_plus))
  })

  output$team_leader_table <- renderReactable({

    reactable(team_leader_data(),
              defaultSorted = "wRC_plus",
              defaultSortOrder = "desc",
              columns = list(
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
                  format = colFormat(digits = 3)
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
                  format = colFormat(digits = 3)
                )
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
        "acc_batting_",
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
        "acc_pitching_",
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
        "acc_leader_",
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

  output$team_plot <- renderPlot({

    metric <- input$team_metric

    if(metric == "wRC_plus") {

    batting |>
      filter(
        Season == input$Season,
        PA > 0) |>
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


      batting |>
        filter(
          Season == input$Season,
          PA > 0) |>
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


      pitching |>
        filter(
          Season == input$Season,
          IP > 0) |>
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
  })

#### DYNAMIC TABLE TITLES AND DESCRIPTIONS #####################################

  output$batting_title <- renderText({

    if (input$Team != "All") {

      paste(input$Season,
            input$Team,
            "Batting Statistics (",
            input$min_PA,
            "+ PA)"
            )

    }

    else {

      paste(input$Season,
            "ACC Batting Statistics (",
            input$min_PA,
            "+ PA)"
      )

    }

  }
)

  output$pitching_title <- renderText({

    if (input$Team != "All") {

      paste(input$Season,
            input$Team,
            "Pitching Statistics (",
            input$min_IP,
            "+ IP)"
      )

    }

    else {

      paste(input$Season,
            "ACC Pitching Statistics (",
            input$min_IP,
            "+ IP)"
      )

    }
  }
  )

    output$team_leader_title <- renderText({

      paste(input$Season,
            "ACC Team Statistics Leaderboard"
            )

  }
  )

    output$visualization_title <- renderText({

      paste(input$Season,
            "ACC Team Visualizations"
            )
    })

    output$plot_description <- renderText({

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

    })
}

shinyApp(ui, server)
