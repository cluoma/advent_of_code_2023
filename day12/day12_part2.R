##################################################
# Day 12 - Part 2
##################################################
library(tidyverse)
library(r2r)
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


#cc <- 0
count_possibilities <- function(d) {
  #cc <<- cc + 1
  
  # print(paste0("o: ", paste0(d$springs, collapse= " ")))
  # print(paste0("n: ", paste0(d$nums, collapse= " ")))
  
  hash_val <- m[[paste0(paste0(d$springs, collapse = ""), "::", paste0(d$nums, collapse = ","))]]
  if (!is.null(hash_val)) {
    #print("found hash")
    return(hash_val)
  }
  
  if (length(d$nums) == 0) {
    if ( !("#" %in% d$springs) ) {
      return(1)
    } else {
      return(0)
    }
  }
  
  if (sum(d$nums) + length(d$nums) - 1 > length(d$springs)) {
    return(0)
  }
  
  if (d$springs[1] == ".") {
    tmp <- d
    tmp$springs <- tmp$springs[-1]
    
    val <- count_possibilities(tmp)
    m[[paste0(paste0(tmp$springs, collapse = ""), "::", paste0(tmp$nums, collapse = ","))]] <- val
    return(val)
  }
  
  total_possibilities <- 0
  if (d$springs[1] == "?") {
    tmp <- d
    tmp$springs <- tmp$springs[-1]
    
    val <- count_possibilities(tmp)
    m[[paste0(paste0(tmp$springs, collapse = ""), "::", paste0(tmp$nums, collapse = ","))]] <- val
    total_possibilities <- total_possibilities + val
  }
  
  if (
    !("." %in% d$springs[1:d$nums[1]]) &&
    (length(d$springs) <= d$nums[1] || (length(d$springs) > d$nums[1] && d$springs[(d$nums[1]+1)] != "#") )
  ) {
    tmp <- d
    if (d$nums[1]+2 > length(tmp$springs)) {
      tmp$springs <- as.character()
    } else {
      tmp$springs <- tmp$springs[(d$nums[1]+2):length(tmp$springs)]
    }
    tmp$nums <- tmp$nums[-1]
    
    val <- count_possibilities(tmp)
    m[[paste0(paste0(tmp$springs, collapse = ""), "::", paste0(tmp$nums, collapse = ","))]] <- val
    total_possibilities <- total_possibilities + val
  }

  return(total_possibilities)
}

m <- hashmap()
vals <- c()
for (map in data) {
  vals <- c(vals, count_possibilities(map))
}
print("Part 2: ", sum(vals))
