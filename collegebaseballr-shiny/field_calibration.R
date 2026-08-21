library(tidyverse)

batted_balls_multi <- readRDS("data/batted_balls_multi_2026.rds")

home_x <- 125.2
home_y <- 203.2

third_x <- 100.7
third_y <- 176.6

second_x <- 125.2
second_y <- 150.1

first_x <- 149.6
first_y <- 176.7

left_foul_x <- 24.5
left_foul_y <- 102.6

right_foul_x <- 221.0
right_foul_y <- 106.6

foul_lines <- tibble(
  x = c(home_x, left_foul_x, NA, home_x, right_foul_x),
  y = c(home_y, left_foul_y, NA, home_y, right_foul_y)
)

diamond <- tibble(
  x = c(home_x, third_x, second_x, first_x, home_x),
  y = c(home_y, third_y, second_y, first_y, home_y)
)

width_lookup <- tibble(
  depth = c(0, 15, 30, 45, 65, 85),
  width = c(32.5, 54.8, 111, 131, 170, 185)
)

transform_espn_coordinates <- function(data) {
  data |>
    mutate(
      dx = hit_x - home_x,
      depth = home_y - hit_y,
      
      depth_for_lookup = pmin(
        pmax(depth, 0),
        85
      ),
      
      espn_width = approx(
        width_lookup$depth,
        width_lookup$width,
        xout = depth_for_lookup,
        rule = 2
      )$y,
      
      y_svg = hit_y,
      
      left_boundary =
        home_x +
        (left_foul_x - home_x) *
        (y_svg - home_y) /
        (left_foul_y - home_y),
      
      right_boundary =
        home_x +
        (right_foul_x - home_x) *
        (y_svg - home_y) /
        (right_foul_y - home_y),
      
      svg_width = right_boundary - left_boundary,
      
      relative_x = dx / (espn_width / 2),
      
      x_svg = home_x + relative_x * svg_width / 2
    )
}

verification_points <- tibble(
  label = c(
    "Home plate",
    "Center: shallow",
    "Center: medium",
    "Center: deep",
    "Left foul pole",
    "Right foul pole"
  ),
  
  hit_x = c(125, 125, 125, 125, 25, 221),
  hit_y = c(203, 176, 125, 103, 103, 107)
)

verification_transformed <- verification_points |>
  transform_espn_coordinates()

print(
  verification_transformed |>
    select(
      label,
      hit_x,
      hit_y,
      depth,
      x_svg,
      y_svg,
      left_boundary,
      right_boundary
    )
)

ggplot() +
  geom_path(
    data = foul_lines,
    aes(x = x, y = y),
    linewidth = 1
  ) +
  geom_path(
    data = diamond,
    aes(x = x, y = y),
    linewidth = 1
  ) +
  geom_point(
    data = verification_transformed,
    aes(x = x_svg, y = y_svg, color = label),
    size = 4
  ) +
  geom_text(
    data = verification_transformed,
    aes(x = x_svg, y = y_svg, label = label),
    nudge_y = 7,
    size = 3,
    show.legend = FALSE
  ) +
  coord_fixed(
    xlim = c(0, 250),
    ylim = c(0, 225)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom"
  )