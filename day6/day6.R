##################################################
# Day 6
##################################################
library(tidyverse)

d_input <- readLines("day6.txt")

# Part 1
times <- str_split(d_input[1], " ")[[1]] %>% str_replace_all("[^0-9]*","")
times <- times[times != ""] %>% as.integer()

distances <- str_split(d_input[2], " ")[[1]] %>% str_replace_all("[^0-9]*","")
distances <- distances[distances != ""] %>% as.integer()

data <- matrix(c(times, distances, rep(0, length(times))), ncol = 3, byrow = FALSE)

for (i in 1:nrow(data)) {
  time <- data[i,1]
  distance <- data[i,2]
  
  for (h in 1:time) {
    if (h * (time - h) > distance)
      data[i,3] <- data[i,3] + 1
  }
}
print(prod(data[,3]))

# Part 2
times <- str_split(d_input[1], " ")[[1]] %>% str_replace_all("[^0-9]*","")
times <- times[times != ""] %>% paste0(collapse = "") %>% as.integer()

distances <- str_split(d_input[2], " ")[[1]] %>% str_replace_all("[^0-9]*","")
distances <- distances[distances != ""] %>% paste0(collapse = "") %>% as.double()

data <- matrix(c(times, distances, rep(0, length(times))), ncol = 3, byrow = FALSE)

for (i in 1:nrow(data)) {
  time <- data[i,1]
  distance <- data[i,2]
  
  start <- ceiling(( (-1 * time) + sqrt(time^2 - (4 * distance)) ) / (-2))
  end   <- floor(( (-1 * time) - sqrt(time^2 - (4 * distance)) ) / (-2))
  
  data[i,3] <- end - start + 1
}
print(prod(data[,3]))
