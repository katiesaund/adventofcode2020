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
vec <- sort(read_csv("puzzle_input.txt", col_names = FALSE) %>% pull(X1))
vec <- c(0, vec, max(vec) + 3) # Account for your device
diff <- vec[1:length(vec) - 1] - vec[2:length(vec)]
sum(diff == -1) * sum(diff == -3)
