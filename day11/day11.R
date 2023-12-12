##################################################
# Day 11
##################################################
library(tidyverse)

void_count <- function(void_locations, start, end)
  # given a vector, count number of elements between start and end
{
  if (start < end) {
    length(void_locations[void_locations > start & void_locations < end])
  } else {
    length(void_locations[void_locations > end & void_locations < start])
  }
}

d_input <- readLines("day11.txt")

space <- matrix(str_split(d_input, "") %>% unlist(), ncol = length(str_split(d_input[1], "")[[1]]), byrow = TRUE)

# Get locations of all void rows and columns
void_columns <- NULL
void_rows <- NULL
# Columns
for (i in 1:ncol(space)) {
  if (length(unique(space[,i])) == 1 && unique(space[,i]) == "." ) {
    void_columns <- c(void_columns, i)
  }
}
# Rows
for (i in 1:nrow(space)) {
  if (length(unique(space[i,])) == 1 && unique(space[i,]) == "." ) {
    void_rows <- c(void_rows, i)
  }
}

# get galaxy locations
galaxies <- NULL
for (i in 1:nrow(space)) {
  for (j in 1:ncol(space)) {
    if (space[i,j] == "#") {
      galaxies <- galaxies %>%
        bind_rows(data.frame(x=j, y=i))
    }
  }
}
galaxies <- galaxies %>% mutate(galaxy = row_number())

# generate pairs of galaxies and count steps between them
galaxy_pairs <- galaxies %>%
  cross_join(galaxies) %>%
  filter(galaxy.x != galaxy.y) %>%
  mutate(galaxy_pair = if_else(galaxy.x > galaxy.y, paste0(galaxy.x, ",", galaxy.y), paste0(galaxy.y, ",", galaxy.x))) %>%
  distinct(galaxy_pair, .keep_all = TRUE) %>%
  mutate(x_steps = abs(x.x - x.y),
         y_steps = abs(y.x - y.y)) %>%
  rowwise() %>%
  mutate(void_column_count = void_count(void_columns, x.x, x.y),
         void_row_count = void_count(void_rows, y.x, y.y)) %>%
  mutate(steps = x_steps + y_steps)

# sum all steps and void row/column correction
print(paste0(
  "Part 1: ",
  sum(galaxy_pairs$steps + (galaxy_pairs$void_column_count + galaxy_pairs$void_row_count) * 1)
))
print(paste0(
  "Part 2: ",
  sum(galaxy_pairs$steps + (galaxy_pairs$void_column_count + galaxy_pairs$void_row_count) * 999999)
))