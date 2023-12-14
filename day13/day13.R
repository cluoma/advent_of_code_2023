##################################################
# Day 13
##################################################
library(tidyverse)

##
# Parse input into list of matrices
d_input <- readLines("day13.txt") %>% str_replace_all("#","1") %>% str_replace_all("\\.","0")

matrices <- list()
m_tmp <- NULL
for (i in 1:length(d_input)) {
  if (d_input[i] == "") {
    matrices <- append(matrices, list(unname(m_tmp)))
    m_tmp <- NULL
  }
  else if (is.null(m_tmp)) {
    line <- str_split(d_input[i], "")[[1]] %>% as.integer()
    m_tmp <- matrix(line, nrow = 1, byrow = TRUE)
  }
  else {
    line <- str_split(d_input[i], "")[[1]] %>% as.integer()
    m_tmp <- m_tmp %>% rbind(line)
  }
}
matrices <- append(matrices, list(m_tmp))


##
# returns number of columns to the left of the mirror
# given a smudge limit
get_left_values <- function(x, smudge_limit = 0) {
  for (i in 1:(ncol(x)-1)) {
    smudges <- 0
    j <- 0
    while (i-j > 0 && i+1+j <= ncol(x)) {
      col_diff <- sum(!(x[,i-j] == x[,i+1+j]))
      smudges <- smudges + col_diff
      if (smudges > smudge_limit) break
      j <- j+1
    }
    if (smudges == smudge_limit) {
      return(i)
    }
  }
  return(0)
}

##
# Part 1
left_values <- c()
for (area in matrices) {
  lv <- get_left_values(area)
  if (lv == 0) {
    lv <- get_left_values(t(area)) * 100
  }
  left_values <- c(left_values, lv)
}
print(paste0("Part 1: ", sum(left_values)))


##
# Part 2
left_values <- c()
for (area in matrices) {
  lv <- get_left_values(area, 1)
  if (lv == 0) {
    lv <- get_left_values(t(area), 1) * 100
  }
  left_values <- c(left_values, lv)
}
print(paste0("Part 2: ", sum(left_values)))