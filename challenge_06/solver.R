# Part 1
# Problem Statement

# For each group, count the number of questions to which anyone answered "yes".
# What is the sum of those counts?

library(tidyverse)

make_data_tidy <- function(file_name) {
  df <- read.delim(file = file_name, header = FALSE, sep = "\n") %>% 
    mutate(indiv_id = "", group_id = "")
  
  # Assign individual ids to each person
  counter <- group_counter <- 1
  for (i in 1:nrow(df)) {
    counter <- counter + 1
    if (grepl("^[0-9]+$", df$V1[i])) {
      group_counter <- group_counter + 1
    }
    df$indiv_id[i] <- counter
    df$group_id[i] <- group_counter
  }
  
  # Remove number char I used to assign individual ids and remove those formerly empty lines
  df <- df %>% 
    mutate(V1 = gsub(pattern = "^[0-9]+", replacement = "", x = V1)) %>% 
   filter(nchar(V1) > 0)
  return(df)
}

df <- make_data_tidy("puzzle_input2.txt")

df_wide <- df %>% 
  pivot_wider(names_from = indiv_id, values_from = V1)

df_wide <- unite(df_wide, New, -group_id, sep = "", na.rm = TRUE)

count <- 0 
for (i in 1:nrow(df_wide)) {
  count <- count + length(unique(str_split(string = df_wide$New[i], "")[[1]]))
}
count

# Part 2
# Problem statement
# For each group, count the number of questions to which everyone answered
# "yes". What is the sum of those counts?

df_wide2 <- df %>% 
  group_by(group_id) %>% 
  mutate(num_indiv = n()) %>% 
  pivot_wider(names_from = indiv_id, values_from = V1) 

df_wide2 <- unite(df_wide2, New, -c(group_id, num_indiv), sep = "", na.rm = TRUE)

count <- 0 
for (i in 1:nrow(df_wide)) {
  num_indiv <- df_wide2$num_indiv[i]
  tbl_out <- table(str_split(string = df_wide2$New[i], "")[[1]]) 
  count <- count + length(names(tbl_out)[tbl_out == num_indiv])
}
count
