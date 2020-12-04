# Part 1
# Problem statement: 
# Starting at the top-left corner of your map and following a slope of right 3
# and down 1, how many trees would you encounter?

library(tidyverse)
map_df <- read_delim("puzzle_input.csv", col_names = FALSE, delim = " ") %>% 
  separate(col = X1, sep = "", into = paste0("C", 0:31)) %>% 
  select(-C0)

example_df <- read_delim("example_data.csv", col_names = FALSE, delim = " ") %>% 
  separate(col = X1, sep = "", into = paste0("C", 0:66)) %>% 
  select(-C0)


# We need to make the df bigger or we need to wrap around. 
big_map <- bind_cols(rep(map_df, 100)) 
example_map <- bind_cols(rep(example_df, 10))
num_row <- nrow(big_map)
# Make all of the indices to check
col_nums <- c(1, 1:num_row * 3 + 1)


trees_hit <- 0 
for (i in 1:num_row) {
  # (i, col_nums[i]) gives the coordinates in the dataframe to check
  if (big_map[i, col_nums[i]] == "#") {
    trees_hit <- trees_hit + 1 
  }
}
trees_hit # number of trees hit

# Part 2
# Problem statement
# Determine the number of trees you would encounter if, for each of the
# following slopes, you start at the top-left corner and traverse the map all
# the way to the bottom:
#
# Right 1, down 1. 
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1. 
# Right 7, down 1. 
# Right 1, down 2. 
# In the above example, these
# slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; multiplied together,
# these produce the answer 336.
#
# What do you get if you multiply together the number of trees encountered on
# each of the listed slopes?

# Make a function of the script we already ran: 

count_trees_hit <- function(x_change, y_change, map) {
  num_row <- nrow(map)
  row_indices <- c(1, 1 + y_change * 1:floor(nrow(map) / y_change))
  col_indices <- c(1, 1:num_row * x_change + 1)
  trees_hit <- 0 

  for (i in 1:length(row_indices)) {
    if (row_indices[i] <= num_row) {
      # (row_indices[i] , col_indices[i]) gives the coordinates
      if (map[row_indices[i], col_indices[i]] == "#") {
        trees_hit <- trees_hit + 1 
      }
    }
  }
  return(trees_hit) # number of trees hit
}

# Check that the code works as expected on the example map provided
count_trees_hit(1, 1, example_map) == 2
count_trees_hit(3, 1, example_map) == 7
count_trees_hit(5, 1, example_map) == 3
count_trees_hit(7, 1, example_map) == 4
count_trees_hit(1, 2, example_map) == 2

# Run on the puzzle map
ntree1 <- count_trees_hit(1, 1, big_map)
ntree2 <- count_trees_hit(3, 1, big_map)
ntree3 <- count_trees_hit(5, 1, big_map)
ntree4 <- count_trees_hit(7, 1, big_map)
ntree5 <- count_trees_hit(1, 2, big_map)

prod(ntree1, ntree2, ntree3, ntree4, ntree5)
