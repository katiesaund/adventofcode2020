# Part 1
# Problem statement
# How many bag colors can eventually contain at least one shiny gold bag?

# Make data tidy
library(tidyverse)

df <- read.csv("puzzle_input.txt", header = FALSE, col.names = paste0("X", 1:4), stringsAsFactors = FALSE) 
df <- as_tibble(df) %>% 
  separate(X1, into = c("bag_type", "contains1"), sep = " contain ", remove = TRUE)

df[] <- lapply(df, function(x) gsub("bags[.]", "bag", x))
df[] <- lapply(df, function(x) gsub("bags", "bag", x))
df[] <- lapply(df, function(x) gsub("bag[.]", "bag", x))
df[] <- lapply(df, function(x) gsub("[0-9]+ ", "", x))
df[] <- lapply(df, function(x) gsub("^ ", "", x))


find_valid_holders <- function(bag_name){
  bag_holders <- NULL
  for (i in 1:nrow(df)) {
    for (j in 2:ncol(df)) {
      current_bag <- df[i, j]
      if (!is.na(current_bag)) {
        if (current_bag == bag_name) {
          bag_holders <- c(bag_holders, unname(unlist(df[i, 1])))
        }
      }
    }
  }
  return(bag_holders)
}

# When using example_input.txt
# shiny_gold_1 <- find_valid_holders("shiny gold bag")
# shiny_gold_2 <- unique(c(find_valid_holders(shiny_gold_1[1]), find_valid_holders(shiny_gold_1[2])))
# shiny_gold_3 <- unique(c(find_valid_holders(shiny_gold_2[1]), find_valid_holders(shiny_gold_2[2])))
# length(unique(c(shiny_gold_1, shiny_gold_2)))


find_all_holders <- function(bag_vec) {
  len <- length(unique(bag_vec))
  unique_bags <- NULL
  for (i in 1:len) {
    temp <- find_valid_holders(bag_vec[i])
    unique_bags <- c(unique_bags, temp)
    unique_bags <- unique(unique_bags)
  }
  return(unique_bags)
}

return_only_new_bags <- function(old, new){
  new <- setdiff(new, old)
  return(new)
}

recursive_function <- function(bag_name){
  output <- find_valid_holders("shiny gold bag")
  new_output <- find_all_holders(output)
  not_tested_output <- setdiff(new_output, output)
  already_tested_output <- c(output, new_output[!new_output %in% not_tested_output])

  results <- NULL
  while (!is.null(new_output)) {
    results <- c(results, not_tested_output, already_tested_output)
    new_output <- find_all_holders(not_tested_output)
    not_tested_output <- setdiff(new_output, not_tested_output)
    already_tested_output <- c(not_tested_output, new_output[!new_output %in% not_tested_output])
  }
  results <- c(results, not_tested_output, already_tested_output)
  results <- unique(results)
  
  return(results)
}

out <- recursive_function("shiny gold bag")
length(out)

             