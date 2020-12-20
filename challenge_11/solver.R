# Part 1
# Problem Statement
# The following rules are applied to every seat simultaneously:

# If a seat is empty (L) and there are no occupied seats adjacent to it, the
#    seat becomes occupied.
# If a seat is occupied (#) and four or more seats adjacent to it are also 
#   occupied, the seat becomes empty. Otherwise, the seat's state does not 
#   change.
# Floor (.) never changes; seats don't move, and nobody sits on the floor.
# -- I'm chaing floor to "F"

# Simulate your seating area by applying the seating rules repeatedly until no
# seats change state. How many seats end up occupied?

# The example answer is 37.

library(tidyverse)
# df <- read_csv("puzzle_input.txt", col_names = FALSE) %>% 
#   mutate(X1 = gsub("[.]", "F", X1)) %>% 
#   separate(col = X1, into = paste0("C", 1:99), sep = 1:99, remove = TRUE)


df <- read_csv("example_input.txt", col_names = FALSE) %>% 
  mutate(X1 = gsub("[.]", "F", X1)) %>% 
  separate(col = X1, into = paste0("C", 1:10), sep = 1:10, remove = TRUE)


count_occupied_adjacent_seats <- function(df, i, j) {
  if (i > 1 & i < nrow(df) & j > 1 & j < ncol(df)) {
    surrounding_seats <- df[(i - 1):(i + 1), (j - 1):(j + 1), drop = FALSE]
  } else if (i == 1 & j > 1 & j < ncol(df)) {
    surrounding_seats <- df[(i):(i + 1), (j - 1):(j + 1), drop = FALSE]
  } else if (i == nrow(df) & j > 1 & j < ncol(df)) {
    surrounding_seats <- df[(i - 1):i, (j - 1):(j + 1), drop = FALSE]
  } else if (i > 1 & i < nrow(df) & j == 1) {
    surrounding_seats <- df[(i - 1):(i + 1), j:(j + 1), drop = FALSE]
  } else if (i > 1 & i < nrow(df) & j == ncol(df)) {
    surrounding_seats <- df[(i - 1):(i + 1), (j - 1):j, drop = FALSE]
  } else if (i == 1 & j == 1) {
    surrounding_seats <- df[i:(i + 1), j:(j + 1), drop = FALSE]
  } else if (i == 1 & j == ncol(df)) {
    surrounding_seats <- df[i:(i + 1), (j - 1):j, drop = FALSE]
  } else if (i == nrow(df) & j == 1) {
    surrounding_seats <- df[(i - 1):i, j:(j + 1), drop = FALSE]
  } else if (i == nrow(df) & j == ncol(df)) {
    surrounding_seats <- df[(i - 1):i, (j - 1):j, drop = FALSE]
  } else {
    stop("bad index")
  }
  num_occupied_seats <- sum(surrounding_seats == "#")
  return(num_occupied_seats)
}


update_seats <- function(df) {
  updated_df <- df
  for (i in 1:nrow(df)) {
    for (j in 1:ncol(df)) {
      if (df[i, j] == "F") {
        current_spot <- "floor"
        # Floor (.) never changes;
        # Therefore don't need to count number of occupied adjacent seat
      } else {
          if (df[i, j] == "L") { 
            current_spot <- "empty"
            # If a seat is empty (L) and there are no occupied seats adjacent to it, 
            # the seat becomes occupied.
            } else {
              current_spot <- "occupied"
            }
        dat <- df
        dat[i, j] <- 0
        
        num_occupied_adjacent_seats <- -9999
        if (current_spot != "floor") {
          num_occupied_adjacent_seats <- count_occupied_adjacent_seats(dat, i, j)
        }
        
        if (num_occupied_adjacent_seats == 0 & current_spot == "empty") {
          updated_df[i, j] <- "#"
        } else if (num_occupied_adjacent_seats > 3 & current_spot == "occupied") {
          updated_df[i, j] <- "L"
        }
      }
    }
  }
  return(updated_df)
}

update_until_no_changes <- function(df){
  num_empty_seats1 <- sum(df == "#")
  num_empty_seats2 <- sum(update_seats(df) == "#")
  if (num_empty_seats2 == num_empty_seats1) {
    return(num_empty_seats1)
  } else {
    return(update_until_no_changes(update_seats(df)))
  }
}
# # 
# sum(df == "#")
# df2 <- update_seats(df)
# df3 <- update_seats(df2)
# df4 <- update_seats(df3)
# sum(df4 == "#")
# df5 <- update_seats(df4)
# df6 <- update_seats(df5)
# df7 <- update_seats(df6)
# sum(df7 == "#")

update_until_no_changes(df)
