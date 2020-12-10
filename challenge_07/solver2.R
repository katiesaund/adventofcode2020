# Part 2
# Problem statement
# How many individual bags are required inside your single shiny gold bag?
library(tidyverse)
df <- read.csv("example_input.txt", header = FALSE, col.names = paste0("X", 1:4), stringsAsFactors = FALSE) 
df <- as_tibble(df) %>% 
  separate(X1, into = c("bag_type", "contains1"), sep = " contain ", remove = TRUE)

df[] <- lapply(df, function(x) gsub("bags[.]", "bag", x))
df[] <- lapply(df, function(x) gsub("bags", "bag", x))
df[] <- lapply(df, function(x) gsub("bag[.]", "bag", x))
df[] <- lapply(df, function(x) gsub("^ ", "", x))
df[] <- lapply(df, function(x) gsub("no other bag", "0", x))
df[] <- lapply(df, function(x) gsub("^ ", "0", x))
df <- df %>% 
  pivot_longer(cols = colnames(df)[2:5], names_to = "contents") %>% 
  filter(!is.na(value), 
         value != "", 
         value != "0") %>% 
  select(bag_type, value) %>% 
  mutate("num" = as.numeric(gsub(" .*", "", value)), 
         "contents" = gsub("[0-9] ", "", value)) %>% 
  select(-value)




# I got stuck 
get_contained_bags <- function(bag_name){
  remaining_df <- df %>% 
    filter(bag_type != bag_name)
  temp_df <- df %>% 
    filter(bag_type == bag_name)

  if (nrow(remaining_df) == 0))) {
    return(sum(temp_df$num * num_vec))
  } else {
    
    return( + get_contained_bags())
  }
}

sum_of_squares <- function(vec) {
  if (length(vec) <= 1) { # terminating condition: the length of the vector
    return(vec ^ 2)
  } else {
    return(sum(vec[1]^2 + sum_of_squares(vec[-1])))
  }
}  


get_contained_bags("shiny gold bag")  