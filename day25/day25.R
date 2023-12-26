##################################################
# Day 25
##################################################
rm(list = ls()); gc()
library(tidyverse)
library(igraph)
options(scipen=999)

d_data <- readLines("day25.txt")

graph_df <- NULL
for (line in d_data)
{
  tmp <- str_split(line, ":")[[1]] %>% str_trim()
  tmp <- data.frame(
    from = tmp[1],
    to = str_split(tmp[2], " ")[[1]]
  )
  graph_df <- graph_df %>% bind_rows(tmp)
}

g <- graph_from_data_frame(graph_df, directed = FALSE)

cut_g <- min_cut(g, value.only = FALSE)

v_1 <- cut_g$partition1 %>% length()
v_2 <- cut_g$partition2 %>% length()

print(paste0("Part 1: ", v_1 * v_2))


