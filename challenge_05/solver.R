# Part 1 
# Problem statement

# Examples in example_input.csv
# FBFBBFFRLR, 357
# BFFFBBFRRR, 567.
# FFFBBBFRRR, 119.
# BBFFBBFRLL, 820
#
#
# The first 7 characters will either be F or B; these specify exactly one of the
# 128 rows on the plane (numbered 0 through 127). Each letter tells you which
# half of a region the given seat is in. Start with the whole list of rows; the
# first letter indicates whether the seat is in the front (0 through 63) or the
# back (64 through 127). The next letter indicates which half of that region the
# seat is in, and so on until you're left with exactly one row.
#
# The last three characters will be either L or R; these specify exactly one of
# the 8 columns of seats on the plane (numbered 0 through 7). The same process
# as above proceeds again, this time with only three steps. L means to keep the
# lower half, while R means to keep the upper half.
#
# Every seat also has a unique seat ID: multiply the row by 8, then add the
# column. In this example, the seat has ID 44 * 8 + 5 = 357.
#
# What is the highest seat ID on a boarding pass?

library(tidyverse)

puz_df <- read_csv("puzzle_input.txt", col_names = FALSE) %>% 
  separate(col = X1, sep = "", into = paste0("C", 0:10), remove = TRUE) %>% 
  select(-C0)


# Example: (0, 127) --> 0, 63
#          (64, 127) --> 64, 95
front_half <- function(min, max){
  new_max <- floor((max - min) / 2) + min
  return(c(min, new_max))
}

# Example: (0, 127) --> 64, 127
#          (64, 127) --> 96, 127
#          (64, 95) --> 80, 85
back_half <- function(min, max){
  new_min <- ceiling((max - min) / 2) + min
  return(c(new_min, max))
}

get_seat_num <- function(df){
  seat_min <- 0
  seat_max <- 7
  for (i in 1:ncol(df)) {
    if (df[i] == "L") {
      seat_min <- front_half(seat_min, seat_max)[1]
      seat_max <- front_half(seat_min, seat_max)[2]
    } 
    
    if (df[i] == "R") {
      seat_min <- back_half(seat_min, seat_max)[1]
      seat_max <- back_half(seat_min, seat_max)[2]
    }
    
    if (seat_min == seat_max) {
      seat <- seat_min
    }
  }
  return(seat)
}

get_row_num <- function(df){
  row_min <- 0
  row_max <- 127
  for (i in 1:ncol(df)) {
    if (df[i] == "F") {
      row_min <- front_half(row_min, row_max)[1]
      row_max <- front_half(row_min, row_max)[2]
    } 
    
    if (df[i] == "B") {
      row_min <- back_half(row_min, row_max)[1]
      row_max <- back_half(row_min, row_max)[2]
    }
    
    if (row_min == row_max) {
      row <- row_min
    }
  }
  return(row)
}

get_unique_index <- function(seat_num, row_num){
 index <- 8 * row_num +  seat_num
 return(index)
}


filled_seat_indices <- rep(0, nrow(puz_df))
for (i in 1:nrow(puz_df)) {
  temp_df <- puz_df[i, ]
  seat <- get_seat_num(temp_df)
  row <- get_row_num(temp_df)
  index <- get_unique_index(seat, row)
  filled_seat_indices[i] <- index
}

max(filled_seat_indices)



# Part 2
# Problem statement: 
# Your seat wasn't at the very front or back, though; the seats with IDs +1 and 
# -1 from yours will be in your list.
# What is the ID of your seat?

# Make a df of every index
indices_df <- as.data.frame(matrix(NA, nrow = 128, ncol = 8))
for (i in 1:nrow(indices_df)) {
  for (j in 1:ncol(indices_df)) {
    indices_df[i, j] <- i * 8 + j
  }
}

# Make a vec of possible seats excluding first and last row
possible_seat_indices_vec <- unname(unlist(indices_df[2:127, ]))

# Make a vec of unfilled seats 
my_possible_seat_indices <- 
  possible_seat_indices_vec[possible_seat_indices_vec %in% setdiff(possible_seat_indices_vec, filled_seat_indices)]

# Check to make sure the seats with indices +/-1 of the unfilled seats are filled
for (i in 1:length(my_possible_seat_indices)){
  above <- my_possible_seat_indices[i] + 1
  below <- my_possible_seat_indices[i] - 1
  
  if (above %in% filled_seat_indices & below %in% filled_seat_indices) {
    print(my_possible_seat_indices[i])
  }
}
