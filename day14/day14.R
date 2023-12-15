##################################################
# Day 14
##################################################
library(tidyverse)
library(r2r)
options(scipen=999)

d_input <- readLines("day14.txt") %>%
  str_split("", simplify = TRUE)

calc_weight <- function(x) {
  compare <- rep("O", length(x[1,]))
  
  weight <- 0
  for (i in 1:nrow(x)) {
    weight <- weight + (sum(x[i,] == compare) * (nrow(x)+1-i))
  }
  return(weight)
}

tilt_platform <- function(x, dir = "up") {
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      if (x[i,j] == "O") {
        i2 <- i
        while(i2 > 1) {
          i2 <- i2 - 1
          if (x[i2,j] != ".") {
            i2 <- i2 + 1
            break
          }
        }
        x[i,j] <- "."
        x[i2,j] <- "O"
      }
    }
  }
  return(x)
}

##
# Part 1
tilt_platform(d_input) %>%
  calc_weight()

##
# Part 2
rotate <- function(mat) t(mat[nrow(mat):1,,drop=FALSE])

# Run this and wait until you see the output of SECOND REPEAT, then wait a bit longer
m <- hashmap()
m_repeats <- hashmap()
qq <- d_input
last_repeat <- i
repeats <- c()
first_repeat <- TRUE
second_repeat <- FALSE
for (i in 1:(4*100000)) {
  qq <- tilt_platform(qq)
  
  if (i %% 4 == 0) {
    #print(i)
    mat <- paste0(qq, collapse = "")
    test <- m[[mat]]
    if (!is.null(test)) {
      if (second_repeat) {
        print(paste0("SECOND REPEAT: ", i, " :: ", test))
        second_repeat <- FALSE
      }
      if (first_repeat) {
        print(paste0("FIRST REPEAT: ", i, " :: ", test))
        first_repeat <- FALSE
        second_repeat <- TRUE
      }
      #print("found repeat")
      repeats <- c(repeats, test)
      m_repeats[[paste0(test)]] <- paste0(qq, collapse = "")
      if (i - test == 4)
        break
    } else {
      m[[paste0(qq, collapse = "")]] <- i
    }
  }
  
  qq <- rotate(qq)
}

# Check the index of the FIRST LOOP output
# subtract 4 from it and divide by 4, this is how many iterations
# it takes to reach the main loop
pre_loop_iters <- 173

# Analyze the 'repeats' vector and look for the loop
# this is the number of elements in the loop
loop_size <- 78

loc <- ((1000000000 - pre_loop_iters) %% loop_size)
if (loc == 0) {
  loc <- loop_size
}

final_state <- m_repeats[[ as.character(repeats[loc]) ]]
final_state <- final_state %>%
  str_split("", simplify = FALSE) %>%
  unlist() %>%
  matrix(ncol = ncol(d_input), byrow = TRUE) %>%
  calc_weight()
