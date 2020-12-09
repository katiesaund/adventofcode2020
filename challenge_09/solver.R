# Part 1
# Problem statement
# The first step of attacking the weakness in the XMAS data is to find the first 
# number in the list (after the preamble) which is not the sum of two of the 25
# numbers before it. What is the first number that does not have this property?

# For the example the preamble and window size is 5
# For the real puzzle the preamble and window size is 25

library(tidyverse)

df <- read_csv("puzzle_input.txt", col_names = FALSE)
preamble_size <- 25

get_possible_2_num_sums <- function(df, window_size, current_row){
  possible_sums_vec <- NULL
  if (current_row <= window_size) {
    nums <- df$X1[1:window_size]
    for (i in 1:length(nums)) {
      for (j in 1:length(nums)) {
        if (nums[i] != nums[j]) {
          possible_sums_vec <- c(possible_sums_vec, nums[i] + nums[j])
        }
      }
    }
  } else {
    nums <- df$X1[(current_row - window_size):(current_row - 1)]
    for (i in 1:length(nums)) {
      for (j in 1:length(nums)) {
        if (nums[i] != nums[j]) {
          possible_sums_vec <- c(possible_sums_vec, nums[i] + nums[j])
        }
      }
    }
  }
  return(unique(possible_sums_vec))
}

for (i in (preamble_size + 1):nrow(df)) {
  current_possible_sums_vec <- get_possible_2_num_sums(df, preamble_size, i)
  if (!df$X1[i] %in% current_possible_sums_vec) {
    stop(print(paste(df$X1[i], " does not have this property")))
  }
}
