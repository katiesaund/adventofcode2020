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

# Part 2
# Problem statement
# find a contiguous set of at least two numbers in your list which sum to the 
# invalid number from step 1. To find the encryption weakness, add together the 
# smallest and largest number in this contiguous range

part1_num <- df$X1[i]

# Subset dataframe to only potential possible answers
df <- df[1:(which(df$X1 == part1_num) - 1), ]

find_contiguous_sums <- function(df){
  lens <- 2:nrow(df)
  sums <- as.data.frame(matrix(NA, nrow = nrow(df), ncol = nrow(df) - 1))
  for (j in 1:nrow(df)) {
    for (k in 1:length(lens)) {
      sum <- sum(df$X1[j:(j + lens[k] - 1)])
      sums[j, k] <- sum
    }
  }
  colnames(sums) <- c(1:ncol(sums)) + 1
  return(sums)
}

sum_df <- find_contiguous_sums(df)
# Column names in sum_df are band sizes
# Rows names are the starting row number
index <- which(sum_df == part1_num, arr.ind = TRUE) 

continguous_set <- df$X1[index[1]:(index[1] + index[2])]
encryption_weakness <- sum(min(continguous_set), max(continguous_set))
encryption_weakness