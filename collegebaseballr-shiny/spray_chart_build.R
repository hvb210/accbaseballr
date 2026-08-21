library(tidyverse)
library(ggplot2)

make_spray_chart <- function(selected_batter) {
  
  
  spray_batter <- batted_balls_multi |>
    filter(
      batter_id == selected_batter
    ) |>
    mutate(
      dx = hit_x - 120,
      depth = 185 - hit_y
    )
  
# field coordinates
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

# foul lines
foul_lines <- tibble(
  x = c(
    home_x, left_foul_x, NA,
    home_x, right_foul_x
  ),
  y = c(
    home_y, left_foul_y, NA,
    home_y, right_foul_y
  )
)

# infield diamond
diamond <- tibble(
  x = c(
    home_x,
    third_x,
    second_x,
    first_x,
    home_x
  ),
  y = c(
    home_y,
    third_y,
    second_y,
    first_y,
    home_y
  )
)

width_lookup <- tibble(
  depth = c(0, 15, 30, 45, 65, 85),
  width = c(32.5, 54.8, 111, 131, 170, 185)
)

# convert ESPN coordinates to SVG coordinates (Baseball Savant MLB)
spray_batter <- spray_batter |>
  mutate(
    depth_for_lookup = pmin(
      pmax(depth, 0),
      85
    ),
    
    espn_width = approx(
      width_lookup$depth,
      width_lookup$width,
      xout = depth_for_lookup,
      rule = 2
    )$y
  )

spray_batter <- spray_batter |>
  mutate(
    y_svg = home_y - depth
  )

spray_batter <- spray_batter |>
  mutate(
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
    
    svg_width =
      right_boundary - left_boundary
  )

spray_batter <- spray_batter |>
  mutate(
    relative_x =
      dx / (espn_width / 2),
    
    x_svg =
      home_x +
      relative_x * svg_width / 2
  )

ggplot() +
  
  # Foul lines
  geom_path(
    data = foul_lines,
    aes(x, y),
    linewidth = 1
  ) +
  
  # Infield diamond
  geom_path(
    data = diamond,
    aes(x, y),
    linewidth = 1
  ) +
  
  # Batted balls
  geom_point(
    data = spray_batter,
    aes(
      x = x_svg,
      y = y_svg,
      color = fielder
    ),
    size = 2,
    alpha = 0.65
  ) +
  
  coord_fixed() +
  
  scale_y_reverse() +
  
  facet_wrap(~year) +
  
  theme_void() +
  
  labs(
    title = "Batted Ball Spray Chart",
    color = "Fielder"
    )
}


