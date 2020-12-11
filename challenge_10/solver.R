# Part 1
# Problem statement

# Any given adapter can take an input 1, 2, or 3 jolts lower than its rating and 
# still produce its rated output joltage.

# In addition, your device has a built-in joltage adapter rated for 3 jolts
# higher than the highest-rated adapter in your bag.
 
# Find a chain that uses all of your adapters to connect the charging outlet to
# your device's built-in adapter and count the joltage differences between the
# charging outlet, the adapters, and your device. What is the number of 1-jolt
# differences multiplied by the number of 3-jolt differences?

# Answer for example_input.txt is 22 (differences of 1 jolt) * 10 (differences 
# of 3 jolts) which is = 220.

library(tidyverse)
vec <- sort(read_csv("small_example_input.txt", col_names = FALSE) %>% pull(X1))
vec <- c(0, vec, max(vec) + 3) # Account for your device
diff <- vec[1:length(vec) - 1] - vec[2:length(vec)]
sum(diff == -1) * sum(diff == -3) # Answer



# Part 2 
# Problem Statement

# What is the total number of distinct ways you can arrange the adapters to 
# connect the charging outlet to your device?

# Answer for small_example_input.txt is 8
# Answer for example_input.txt is 19208

vec <- sort(read_csv("puzzle_input.txt", col_names = FALSE) %>% pull(X1))
vec <- c(0, vec, max(vec) + 3) # Account for your device
diff <- abs(vec[1:length(vec) - 1] - vec[2:length(vec)])

potentially_deletable_nums <- vec[c(FALSE, diff < 3)]
len <- length(potentially_deletable_nums)

empty_df <- matrix(NA, nrow = 10000000, ncol = len)
to_be_deleted <- matrix(NA, nrow = 0, ncol = len)
for (i in 1:len) {
  combo_temp <- t(combn(potentially_deletable_nums, i))
  current_df <- empty_df
  current_df[1:nrow(combo_temp), 1:ncol(combo_temp)] <- combo_temp
  current_df <- current_df[rowSums(current_df, na.rm = TRUE) > 0, , drop = FALSE]
  to_be_deleted <- rbind(to_be_deleted, current_df)
}

success_count <- 1 # We know the original arrangement works
for (i in 1:nrow(to_be_deleted)) {
  new_vec <- vec[!vec %in% to_be_deleted[i, , drop = TRUE]]
  new_diff <- abs(new_vec[1:length(new_vec) - 1] - new_vec[2:length(new_vec)])
  if (max(new_diff) <= 3) {
    success_count <- success_count + 1
  } 
}
success_count


