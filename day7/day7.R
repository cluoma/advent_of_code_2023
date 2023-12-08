##################################################
# Day 7
##################################################
library(tidyverse)

d_input <- readLines("day7.txt")

##
# Part 1
label_hand <- function(x)
{
  ret <- c()
  for (hand in x)
  {
    repeats <- rle(sort(str_split(hand, "")[[1]]))
    if (max(repeats$length) == 5)
      ret <- c(ret, 1)
    else if (max(repeats$length) == 4)
      ret <- c(ret, 2)
    else if (max(repeats$length) == 3 & 2 %in% repeats$length)
      ret <- c(ret, 3)
    else if (max(repeats$length) == 3)
      ret <- c(ret, 4)
    else if (max(repeats$length) == 2 & length(repeats$length[repeats$length == 2]) > 1)
      ret <- c(ret, 5)
    else if (max(repeats$length) == 2)
      ret <- c(ret, 6)
    else
      ret <- c(ret, 7)
  }
  return(ret)
}

data <- data.frame(input = d_input) %>%
  # Get hand and bet
  separate(input, into = c("hand","bet"), " ", convert = TRUE) %>%
  # label hand type
  mutate(hand_type = label_hand(hand)) %>%
  # replace card names with sortable letters
  mutate(
    hand = str_replace_all(hand, "A", "E"),
    hand = str_replace_all(hand, "T", "A"),
    hand = str_replace_all(hand, "J", "B"),
    hand = str_replace_all(hand, "Q", "C"),
    hand = str_replace_all(hand, "K", "D")
  ) %>%
  ungroup() %>%
  arrange(desc(hand_type), hand) %>%
  mutate(winnings = bet * row_number())

print(paste0("Part 1: ", sum(as.double(data$winnings))))


##
# Part 2
label_hand <- function(x)
{
  ret <- c()
  for (hand in x)
  {
    # get number of jokers and add to highest non-joker count
    # this always produces the best hand
    repeats <- rle(sort(str_split(hand, "")[[1]]))
    jokers <- repeats$lengths[repeats$values == "J"]
    jokers <- ifelse(length(jokers) >= 1, jokers, 0)
    lengths <- rev(sort(repeats$lengths[repeats$values != "J"]))
    if (length(lengths) == 0) {
      lengths <- jokers
    } else {
      lengths[1] <- lengths[1] + jokers
    }
    
    if (max(lengths) == 5)
      ret <- c(ret, 1)
    else if (max(lengths) == 4)
      ret <- c(ret, 2)
    else if (max(lengths) == 3 & 2 %in% lengths)
      ret <- c(ret, 3)
    else if (max(lengths) == 3)
      ret <- c(ret, 4)
    else if (max(lengths) == 2 & length(lengths[lengths == 2]) > 1)
      ret <- c(ret, 5)
    else if (max(lengths) == 2)
      ret <- c(ret, 6)
    else
      ret <- c(ret, 7)
  }
  return(ret)
}

data <- data.frame(input = d_input) %>%
  # Get hand and bet
  separate(input, into = c("hand","bet"), " ", convert = TRUE) %>%
  # label hand type
  mutate(hand_type = label_hand(hand)) %>%
  # replace card names with sortable letters
  mutate(
    hand = str_replace_all(hand, "A", "E"),
    hand = str_replace_all(hand, "T", "A"),
    hand = str_replace_all(hand, "J", "1"),
    hand = str_replace_all(hand, "Q", "C"),
    hand = str_replace_all(hand, "K", "D")
  ) %>%
  ungroup() %>%
  arrange(desc(hand_type), hand) %>%
  mutate(winnings = bet * row_number())

print(paste0("Part 2: ", sum(as.double(data$winnings))))