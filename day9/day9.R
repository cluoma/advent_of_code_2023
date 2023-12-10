##################################################
# Day 6
##################################################
library(tidyverse)

d_input <- readLines("day9.txt")

seq_sub <- function(x) {
  x <- rev(x)
  cur <- x[1]
  for (i in 2:length(x)) {
    cur <- x[i] - cur
  }
  return(cur)
}

final_line_numbers <- c()
left_line_numbers <- c()

for (line in d_input) {
  numbers <- str_split(line, " ")[[1]] %>% as.double()
  print(numbers)
  
  cur_final_numbers <- numbers[length(numbers)]
  cur_left_numbers <- numbers[1]
  
  while ( sum(numbers == 0) != length(numbers) ) {
  # while ( sum(numbers) == 0 ) {
    numbers <- diff(numbers)
    print(numbers)
    cur_final_numbers <- c(cur_final_numbers, numbers[length(numbers)])
    cur_left_numbers <- c(cur_left_numbers, numbers[1])
  }
  
  final_line_numbers <- c(final_line_numbers, sum(cur_final_numbers)[1] )
  left_line_numbers <- c(left_line_numbers, seq_sub(cur_left_numbers)[1] )
  
  print(paste0("Line finals: ", cur_final_numbers))
  print(paste0("Line lefts: ", cur_left_numbers))
}

print(paste0(final_line_numbers))
print(paste0(left_line_numbers))

print(paste0("Part 1: ", sum(final_line_numbers)))
print(paste0("Part 2: ", sum(left_line_numbers)))

