##################################################
# Day 4
##################################################

##
# I did this in R because I was sick and thought it would be easier than c++
# part 2 took some time, in my opinion, because of Rs weirdness

library(tidyverse)

parseSeeds <- function(x)
{
  seeds <- gsub("seeds\\:", "", x) %>%
    str_trim() %>%
    str_split(" ", simplify = TRUE) %>%
    as.double()
  seeds[as.double(seeds) & !is.na(seeds)]
}

input <- readLines("day5.txt")

seeds <- parseSeeds(input[1])

# split transitions into chunks
chunks <- list()
is_in_chunk <- FALSE
cur_name <- ""
for (line in input) {
  if (line == "")
    is_in_chunk <- FALSE
  
  if (is_in_chunk) {
    chunks[[cur_name]] <- c(chunks[[cur_name]], line)
  }
  
  if (str_ends(line, "map\\:")) {
    cur_name <- str_extract(line, "[^ ]*")
    is_in_chunk <- TRUE
  }
}


## Part 1
locs <- NULL
for (seed in seeds) {
  
  orig_seed <- seed
  for (chunk in chunks)
  {
    for (mapping in chunk)
    {
      nums <- as.double(str_split(mapping, " ")[[1]])
      #print(paste0("1: ", nums[1], " 2: ", nums[2], " 3: ", nums[3]))
      if (seed >= nums[2] && seed <= nums[2] + nums[3] - 1)
      {
        seed <- nums[1] + (seed - nums[2])
        break;
      }
    }
  }
  locs <- c(locs, seed)
  print(paste0("Seed ", orig_seed, " at loc ", seed))
}
min(locs)


## Part 2

# Testing code
# rr <- list()
# rr <- append(rr, list(c(7, 10)))
# rr <- append(rr, list(c(3, 5)))
# rr <- append(rr, list(c(11, 15)))
# 
# unlist(rr)
# sort(unlist(rr))
# 
# qq <- c(1, 20)
# 
# unmapped_ranges(qq, rr)


unmapped_ranges <- function(range, used_ranges)
{
  unused_ranges <- list()
  
  if (length(used_ranges) == 0)
    return(append(unused_ranges, list(c(range[1], range[2]))))
   
  tmp_nums <- c()
  nums <- sort(unlist(used_ranges))
  
  # check start
  if (range[1] < nums[1]) {
    tmp_nums <- c(range[1], nums[1]-1)
  }
  
  i <- 2
  while (i < length(nums)) {
    #print(paste0(nums[i], " - ", nums[i+1]))
    if (nums[i+1] - nums[i] > 1) {
      tmp_nums <- c(tmp_nums, c(nums[i]+1, nums[i+1]-1))
    }
    i <- i+2
  }
  
  # check end
  if (range[2] > nums[length(nums)]) {
    tmp_nums <- c(tmp_nums, c(nums[length(nums)]+1, range[2]))
  }
  
  i <- 1
  while (i < length(tmp_nums)) {
    unused_ranges <- append(unused_ranges, list(c(tmp_nums[i], tmp_nums[i+1])))
    i <- i+2
  }
  
  return(unused_ranges)
}

min_loc <- function(range, transformations)
{
  used_ranges <- list()
  ranges <- list()
  if (length(transformations) > 0)
  {
    chunk <- transformations[[1]]
    for (mapping in chunk)
    {
      # get range for the mapping
      nums <- as.double(str_split(mapping, " ")[[1]])
      range_map <- c(nums[2], nums[2] + nums[3] - 1)
      # calculate the overlapping range and transform
      overlapping_range <- c(max(range[1], range_map[1]), min(range[2], range_map[2]))
      overlapping_range_trans <- nums[1] + (overlapping_range - nums[2])
      
      # check if there actually is overlap and add to ranges
      if (overlapping_range_trans[1] <= overlapping_range_trans[2]) {
        used_ranges <- append(used_ranges, list(overlapping_range))
        ranges <- append(ranges, list(overlapping_range_trans))
      }
    }
  }
  # add unmapped ranges
  ranges <- append(ranges, unmapped_ranges(range, used_ranges))
  
  # do it again for the next set of transformations
  if (length(transformations) > 1) {
    for (r in ranges) {
      min_loc(r, transformations[2:length(transformations)])
    }
  }
  else {
    # this was the last transformation
    min_ranges <<- c(min_ranges, min(unlist(ranges)))
    return(min(unlist(ranges)))
  }
}

min_ranges <- NULL
i <- 1
while (i < length(seeds)) {
  print(paste0("seed range: ", seeds[i], seeds[i]+seeds[i+1]-1))
  min_loc(c(seeds[i], seeds[i]+seeds[i+1]-1), chunks)
  print(min(min_ranges))
  min_ranges <- NULL
  i <- i + 2
}

# find the minimum number from the output
# i didnt feel like making a proper output



