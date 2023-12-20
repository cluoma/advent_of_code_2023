##################################################
# Day 19
##################################################
rm(list = ls()); gc()
library(tidyverse)
options(scipen=999)

d_data <- readLines("day19.txt")

d_rules <- d_data[1:(which(d_data == "", arr.ind = TRUE)-1)]
d_items <- d_data[(which(d_data == "", arr.ind = TRUE)+1):length(d_data)]


# parse rules
l_rules <- list()
for (r in d_rules)
{
  name <- str_extract(r, "([a-z]+)\\{(.*)\\}", group = 1)
  
  rules <- str_extract(r, "([a-z]+)\\{(.*)\\}", group = 2) %>%
    str_extract_all("([a-z<>0-9:]*[ARa-z])")
  rules <- rules[[1]]
  
  rl <- str_split(rules, ":")
  rl <- lapply(rl, \(x) {if(length(x) == 1) c("TRUE", x) else x})
  l_rules[[name]] <- rl
}

# parse items
m_items <- str_extract(d_items, "\\{x=([0-9]+),m=([0-9]+),a=([0-9]+),s=([0-9]+)\\}", group = c(1,2,3,4))


##################################################
# Part 1
##################################################
eval_item <- function(rules, x, m, a, s)
{
  r <- rules[["in"]]
  while(TRUE)
  {
    i <- 1
    #print(paste0("Checking ", r[[i]][1], " target ", r[[i]][2]))
    while (eval(parse(text = r[[i]][1])) != TRUE)
    {
      i <- i + 1
      #print(paste0("Checking ", r[[i]][1], " target ", r[[i]][2]))
    }
    if (r[[i]][2] == "A")
    {
      #print("accepted")
      return(TRUE)
    }
    else if (r[[i]][2] == "R")
    {
      #print("rejected")
      return(FALSE)
    }
    else
      r <- rules[[r[[i]][2]]]
  }
}

total_sum <- 0
for (i in 1:nrow(m_items))
{
  #print(m_items[i,])
  tmp_m <- m_items[i,] %>% as.numeric()
  if (eval_item(l_rules, tmp_m[1], tmp_m[2], tmp_m[3], tmp_m[4]))
    total_sum <- total_sum + tmp_m[1] + tmp_m[2] + tmp_m[3] + tmp_m[4]
}

print(paste0("Part 1 ", total_sum))


##################################################
# Part 2
##################################################

rule_applies <- function(ranges, rule)
{
  if (rule == "TRUE")
    return(TRUE)
  
  parts <- str_extract(rule, "([xmas]{1})([><]{1})([0-9]+)", group = c(1,2,3))
  if (parts[2] == "<" && ranges[[parts[1]]][1] < as.numeric(parts[3]))
  {
      return(TRUE)
  }
  else if (parts[2] == ">" && ranges[[parts[1]]][2] > as.numeric(parts[3]))
  {
    return(TRUE)
  }
  
  return(FALSE)
}

ranges_equal <- function(ranges1, ranges2)
{
  equal_count <- 0
  for (label in c("x","m","a","s"))
  {
    equal_count <- equal_count +
      as.integer(ranges1[[label]][1] == ranges2[[label]][1]) +
      as.integer(ranges1[[label]][2] == ranges2[[label]][2])
  }
  if (equal_count == 8)
    return(TRUE)
  else
    return(FALSE)
}

reduce_range_from_rule <- function(ranges, rule)
{
  if (rule == "TRUE")
    return(range)
  parts <- str_extract(rule, "([xmas]{1})([><]{1})([0-9]+)", group = c(1,2,3))
  
  if (parts[2] == "<")
    ranges[[parts[1]]][2] <- min(ranges[[parts[1]]][2], as.numeric(parts[3])-1)
  else
    ranges[[parts[1]]][1] <- max(ranges[[parts[1]]][1], as.numeric(parts[3])+1)
  
  return(ranges)
}

invert_range_from_rule <- function(ranges, rule)
{
  if (rule == "TRUE")
    return(range)
  parts <- str_extract(rule, "([xmas]{1})([><]{1})([0-9]+)", group = c(1,2,3))
  
  if (parts[2] == "<")
    ranges[[parts[1]]][1] <- max(ranges[[parts[1]]][1], as.numeric(parts[3]))
  else
    ranges[[parts[1]]][2] <- min(ranges[[parts[1]]][2], as.numeric(parts[3]))
  
  return(ranges)
}

is_valid_range <- function(ranges)
{
  for (l in c("x","m","a","s"))
  {
    if (ranges[[l]][1] > ranges[[l]][2])
      return(FALSE)
  }
  return(TRUE)
}

all_ranges <- list()
eval_range <- function(rules, rule = "in", ranges = list(x=c(1,4000), m=c(1,4000), a=c(1,4000), s=c(1,4000)))
{
  r <- rules[[rule]]
  #print(paste0("-----  ", rule))
  #print(ranges)
  continue_range <- ranges
  for (step in r)
  {
    #print(step)
    #print(paste0("Rule applies: ", rule_applies(continue_range, step[1])))
    if (step[1] != "TRUE" && rule_applies(continue_range, step[1]))
      # keep evaluation rules
    {
      # follow the target of the rule if it leads to a nother rule
      # otherwise add it to the list of acceptable ranges
      follow_range <- reduce_range_from_rule(continue_range, step[1])
      if (step[2] == "A")
      {
        #print("Accepted")
        #print(follow_range)
        all_ranges <<- append(all_ranges, list(follow_range))
      }
      else if (step[2] == "R")
      {
      }
      else
      {
        eval_range(rules, step[2], follow_range)
      }
      # update non-branching range
      continue_range <- invert_range_from_rule(continue_range, step[1])
      
      if (ranges_equal(follow_range, continue_range))
      {
        #print("cutting short")
        break
      }
    }
    else if (step[1] == "TRUE")
      # this is the final step in the rule list
    {
      #print("Final Step")
      if (step[2] == "A")
      {
        #print("Accepted")
        #print(continue_range)
        all_ranges <<- append(all_ranges, list(continue_range))
      }
      else if (step[2] == "R")
      {
        return(0)
      }
      else
      {
        eval_range(rules, step[2], continue_range)
      }
    }
  }
}

full_range <- list(x=c(1,4000), m=c(1,4000), a=c(1,4000), s=c(1,4000))
eval_range(
  l_rules,
  "in",
  full_range
)

possibilities <- 0
for (l in all_ranges)
{
  possibilities <- possibilities +
    (l[["x"]][2] - l[["x"]][1] + 1) *
    (l[["m"]][2] - l[["m"]][1] + 1) *
    (l[["a"]][2] - l[["a"]][1] + 1) *
    (l[["s"]][2] - l[["s"]][1] + 1)
}
print(paste0("Part 2 ", possibilities))

