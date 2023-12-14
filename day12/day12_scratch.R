##################################################
# Day 12
##################################################
library(tidyverse)

d_input <- readLines("day12.txt") %>% str_split(" ")

data <- lapply(d_input, \(x) {
  list(
    springs = str_split(x[1], "", simplify = FALSE)[[1]],
    nums = as.integer(str_split(x[2], ",", simplify = TRUE))
  )
})

test <- data[[1]]

count_possibilities <- function(d) {
  # too many springs, this is not possible
  if (length(d$springs[d$springs == "#"]) > sum(d$nums))
    return (0)
  
  # exact number of springs, see if it fits the pattern
  if (length(d$springs[d$springs == "#"]) == sum(d$nums)) {
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
  
  # cannot satisfy condition
  
  
  total_possibilities <- 0
  if ("?" %in% d$springs) {
    index <- which(d$springs == "?")[1]
    for (fill in c("#",".")) {
      #print(paste0("o: ", paste0(d$springs, collapse= " ")))
      d$springs[index] <- fill
      print(paste0("r: ", paste0(d$springs, collapse= " ")))
      total_possibilities <- total_possibilities + count_possibilities(d)
    }
  }
  return(total_possibilities)
}

count_possibilities(test)

sapply(data, \(x) {count_possibilities(x)}) %>% sum()


###
# Part 2
options(scipen=999)
d_input <- readLines("day12.txt") %>% str_split(" ")

data <- lapply(d_input, \(x) {
  list(
    springs = rep(c(str_split(x[1], "", simplify = FALSE)[[1]], "?"), 5),
    nums = rep(as.integer(str_split(x[2], ",", simplify = TRUE)), 5)
  )
})
for (i in 1:length(data)) {
  data[[i]]$springs <- data[[i]]$springs[-length(data[[i]]$springs)]
}
d_input <- readLines("day12.txt") %>% str_split(" ")

data <- lapply(d_input, \(x) {
  list(
    springs = str_split(x[1], "", simplify = FALSE)[[1]],
    nums = as.integer(str_split(x[2], ",", simplify = TRUE))
  )
})

test <- data[[1]]

m <- hashmap()
cc <- 0
count_possibilities <- function(d) {
  cc <<- cc + 1
  
  # if (!is.null(m[[paste0(d$springs, collapse = "")]])) {
  #   print("found hash")
  #   return(m[[paste0(d$springs, collapse = "")]])
  # }
  
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
    # see if we have enough openings to split into required groups
    # t <- d$springs
    # t[t == "?"] <- "#"
    # group_counts <- rle(t)
    # group_counts <- length(group_counts$values[group_counts$values == "#"])
    # if (group_counts < length(d$nums) &&
    #     max(spring_counts$lengths[spring_counts$values == "?"]) < 2 ) {
    #   return (0)
    # }
    # see if we have too many springs in a run
    if (max(spring_counts$lengths[spring_counts$values == "#"]) > max(d$nums)) {
      return(0)
    }
  }
  
  
  total_possibilities <- 0
  if ("?" %in% d$springs) {
    index <- which(d$springs == "?")[1]
    for (fill in c("#",".")) {
      #print(paste0("o: ", paste0(d$springs, collapse= " ")))
      d$springs[index] <- fill
      #print(paste0("r: ", paste0(d$springs, collapse= " ")))
      pos <- count_possibilities(d)
      #m[[paste0(d$springs, collapse = "")]] <- pos
      total_possibilities <- total_possibilities + pos
    }
    #print(index)
  }
  return(total_possibilities)
}

sapply(data, \(x) {print(x); count_possibilities(x)}) %>% sum()

count_possibilities(test)
cc

###
# Part 2 - part 2
library(tidyverse)
options(scipen=999)
d_input <- readLines("day12.txt") %>% str_split(" ")

data <- lapply(d_input, \(x) {
  list(
    springs = rep(c(str_split(x[1], "", simplify = FALSE)[[1]], "?"), 5),
    nums = rep(as.integer(str_split(x[2], ",", simplify = TRUE)), 5)
  )
})
for (i in 1:length(data)) {
  data[[i]]$springs <- data[[i]]$springs[-length(data[[i]]$springs)]
}
d_input <- readLines("day12.txt") %>% str_split(" ")

data <- lapply(d_input, \(x) {
  list(
    springs = str_split(x[1], "", simplify = FALSE)[[1]],
    nums = as.integer(str_split(x[2], ",", simplify = TRUE))
  )
})

test <- data[[3]]

split_indices <- function(x) {
  split_ind <- as.integer()
  for (i in 2:(length(x)-1)) {
    if (x[i] == "?" &&
        x[i-1] %in% c("?","#") &&
        x[i+1] %in% c("?","#")) {
      split_ind <- c(split_ind, i)
    }
  }
  return(split_ind)
}

cc <- 0
count_possibilities <- function(d) {
  cc <<- cc + 1
  
  if (length(d$nums) > 1)
  {
    t <- d$springs
    t[t == "?"] <- "#"
    spring_groups <- rle(t)
    spring_groups <- spring_groups$lengths[spring_groups$values == "#"]
    
    if (length(spring_groups) == length(d$nums))
    {
      split_pos <- 1
      for (g in split_groups(d)) {
        split_pos <- split_pos * count_possibilities(g)
      }
      return(split_pos)
    }
  }
  
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
      for (fill in c(".","#")) {
        d$springs[index] <- fill
        total_possibilities <- total_possibilities + count_possibilities(d)
      }
    }
  return(total_possibilities)
}

count_possibilities(test)
cc


test2 <- test
t <- test2$springs
t[t == "?"] <- "#"
spring_groups <- rle(t)
spring_groups <- spring_groups$lengths[spring_groups$values == "#"]
needed_splits <- length(test2$nums) - length(spring_groups)

split_candidates <- split_indices(test2$springs)
split_combos <- combn(split_candidates, needed_splits, simplify = FALSE)

for (i in 1:ncol(split_combos)) {
  spl <- split_combos[,i]
  
  if (abs(min(spl) - max(spl)) == length(spl)-1)
    next
  
  test2$springs[spl] <- "."
  print(test2$springs)
  # print(split_groups(test2)[[1]])
  # print(count_possibilities(test2))
  
  split_pos <- 1
  for (g in split_groups(test2)) {
    split_pos <- split_pos * count_possibilities(g)
  }
  print(split_pos)
  
  test2$springs[spl] <- "?"
}
split_groups(test)

split_groups <- function(x)
{
  ret <- NULL
  tmp <- c(x$springs[1])
  group <- 1
  for (i in 2:length(x$springs))
  {
    if (x$springs[i] != ".")
      tmp <- c(tmp, x$springs[i])
    if ( (x$springs[i] == "." && x$springs[i-1] != ".") ||
             i == length(x$springs)) {
      
      ret[[group]] <- list(springs = tmp, nums = x$nums[group])
      tmp <- c()
      group <- group + 1
    }
  }
  return(ret)
}



test

t <- test$springs
t[t == "?"] <- "#"
t
spring_groups <- rle(t)
spring_groups <- length(spring_groups$values[spring_groups$values == "#"])
spring_groups





####################################
###########

d_input <- readLines("day12.txt") %>% str_split(" ")

data <- lapply(d_input, \(x) {
  list(
    springs = str_split(x[1], "", simplify = FALSE)[[1]],
    nums = as.integer(str_split(x[2], ",", simplify = TRUE))
  )
})

test <- data[[6]]

split_indices <- function(x) {
  split_ind <- as.integer()
  
  if (x[1] == "?")
    split_ind <- c(split_ind, 1)
  if (x[length(x)] == "?")
    split_ind <- c(split_ind, length(x))
  
  for (i in 2:(length(x)-1)) {
    if (x[i] == "?" &&
        x[i-1] %in% c("?","#") &&
        x[i+1] %in% c("?","#")) {
      split_ind <- c(split_ind, i)
    }
  }
  return(split_ind)
}

cc <- 0
count_possibilities <- function(d) {
  cc <<- cc + 1

  print(d$springs)

  t <- d$springs
  t[t == "?"] <- "#"
  spring_groups <- rle(t)
  spring_groups <- spring_groups$lengths[spring_groups$values == "#"]

  if (length(spring_groups) > length(d$nums)) {  # too many groups
    return(0)
  }
  else if (length(spring_groups) == length(d$nums)) {  # exactly enough groups
    if (all(spring_groups == d$nums)) {  # all groups match the pattern
      return(1)
    }
  } else {  # not enough groups
    current_openings <- length(d$springs[d$springs == "?"])
    current_springs <- length(d$springs[d$springs == "#"])
    if (sum(d$nums) - current_openings - current_springs > 0) {
      return (0)
    }
  }

  total_possibilities <- 0

  split_ind <- split_indices(d$springs)
  #print(split_ind)
  if (length(split_ind) > 0) {  # split until we can't
    for (split_ind in split_indices(d$springs))
    {
      d$springs[split_ind] <- "."
      total_possibilities <- total_possibilities + count_possibilities(d)
      
      # for (index in which(d$springs == "?"))
      # {
      #   d$springs[index] <- "."
      #   total_possibilities <- total_possibilities + count_possibilities(d)
      #   d$springs[index] <- "?"
      # }
      
      d$springs[split_ind] <- "?"
    }
  } else {  # done splitting, now test each group individually
    
    # t <- d$springs
    # t[t == "?"] <- "#"
    # spring_groups <- rle(t)
    # spring_groups <- c(1, cumsum(spring_groups$lengths)[spring_groups$values == "#"])
    # 
    # for (group in cumsum(spring_groups)) {
    #   d_sub$springs <- d$springs[]
    # }
    
    
    for (index in which(d$springs == "?"))
    {
      d$springs[index] <- "."
      total_possibilities <- total_possibilities + count_possibilities(d)
      d$springs[index] <- "?"
    }
  }

  #print(index)
  return(total_possibilities)
}

count_possibilities(test)
cc

##############################
#############################