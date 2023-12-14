##################################################
# Day 12 - Part 1
##################################################
library(tidyverse)
library(r2r)
options(scipen=999)

d_input <- readLines("day12.txt") %>% str_split(" ")

data <- lapply(d_input, \(x) {
  list(
    springs = str_split(x[1], "", simplify = FALSE)[[1]],
    nums = as.integer(str_split(x[2], ",", simplify = TRUE))
  )
})


cc <- 0
count_possibilities <- function(d) {
  cc <<- cc + 1
  
  current_springs <- length(d$springs[d$springs == "#"])
  if (current_springs == sum(d$nums)) {
    # exact number of springs, see if it fits the pattern
    t <- d$springs
    t[t == "?"] <- "."
    spring_counts <- rle(t)
    if ( length(spring_counts$lengths[spring_counts$values == "#"]) == length(d$nums) &&
         all(spring_counts$lengths[spring_counts$values == "#"] == d$nums) ) {
      return (1)
    }
    else {
      return (0)
    }
  }
  else if (current_springs < sum(d$nums)) {
    # cannot satisfy conditions
    
    # see if there are enough openings for springs
    current_openings <- length(d$springs[d$springs == "?"])
    if (sum(d$nums) - current_openings - current_springs > 0) {
      return (0)
    }
    spring_counts <- rle(d$springs)
    # see if we have too many springs in a run
    if (max(spring_counts$lengths[spring_counts$values == "#"]) > max(d$nums)) {
      return(0)
    }
  }
  
  
  total_possibilities <- 0
  if ("?" %in% d$springs) {
    index <- which(d$springs == "?")[1]
    for (fill in c("#",".")) {
      d$springs[index] <- fill
      total_possibilities <- total_possibilities + count_possibilities(d)
    }
  }
  return(total_possibilities)
}

sapply(data, \(x) {count_possibilities(x)}) %>%
  sum() %>%
  paste0("Part 1: ", .)
