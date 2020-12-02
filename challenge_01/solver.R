# Part 1: 
# Goal: Find the two entries that sum to 2020 and then multiply those two 
# numbers together. Return the answer. 

library(tidyverse)

# First, let's read in the puzzle input and then check some assumptions. 

num_df <- read_csv("puzzle_input.csv", col_names = FALSE)
colnames(num_df) <- "value"
# Are they all positive numbers less than 2020? 

bad_values <- num_df %>% 
  select(value) %>% 
  filter(value > 2020,
         value < 1, 
         !is.double(value)) %>% 
  pull
is_empty(bad_values) # TRUE therefore Empty

# Easy solution is to run two for loops.

num_value <- nrow(num_df)

for (i in 1:num_value) {
  for (j in 1:num_value) {
    num1 <- num_df$value[i] 
    num2 <- num_df$value[j]
    if (num1 + num2 == 2020) {
      print(paste(num1, num2))
      print(num1 * num2)
    }
  }
}

# What's a more elegant way? 

# Part 2:
# Goal: Find three numbers in your expense report that meet the same criteria.
# Same input file, so still working with num_df. 

for (i in 1:num_value) {
  for (j in 1:num_value) {
    for (k in 1:num_value) {
      num1 <- num_df$value[i] 
      num2 <- num_df$value[j]
      num3 <- num_df$value[k]
      if (num1 + num2 + num3 == 2020) {
        print(paste(num1, num2, num3))
        print(num1 * num2 * num3)
      }
    }
  }
}

# This loop within loop within loop is noticeably slow. 

