# Part 1

# Action N means to move north by the given value.
# Action S means to move south by the given value.
# Action E means to move east by the given value.
# Action W means to move west by the given value.
# Action L means to turn left the given number of degrees.
# Action R means to turn right the given number of degrees.
# Action F means to move forward by the given value in the direction the ship is 
#          currently facing.

# The ship starts by facing east. Only the L and R actions change the direction 
# the ship is facing.

#  Example: 
# F10 would move the ship 10 units east (because the ship starts by facing east) 
#     to east 10, north 0.
# N3 would move the ship 3 units north to east 10, north 3.
# F7 would move the ship another 7 units east (because the ship is still facing 
#    east) to east 17, north 3.
# R90 would cause the ship to turn right by 90 degrees and face south; it 
#    remains at east 17, north 3.
# F11 would move the ship 11 units south to east 17, south 8.
# At the end of these instructions, the ship's Manhattan distance (sum of the 
#   absolute values of its east/west position and its north/south position) from 
#   its starting position is 17 + 8 = 25.

# Figure out where the navigation instructions lead. What is the Manhattan 
#  distance between that location and the ship's starting position?

library(tidyverse)
df <- read_csv("example_input.txt", col_names = FALSE)
df <- as.data.frame(cbind(gsub(pattern = "[0-9]+", replacement = "", x = df$X1), 
            as.numeric(str_split(df$X1, "[A-Z]", simplify = TRUE)[, 2])))
colnames(df) <- c("direction", "value")
class(df$value) <- "numeric"
# Note values appear to be only 90, 180, or 270- makes things a lot easier

# R90 E -> S, S -> W, W -> N, N -> E
# R 180 E -> W, W -> E, N -> S, S -> N
# R270 E -> N, N -> W, W -> S, S -> E 
change_direction_df <- as.data.frame(matrix(NA, nrow = 24, ncol = 4))
colnames(change_direction_df) <- c("rot_dir","rot_num", "start_dir", "end_dir")
change_direction_df$rot_dir <- c(rep("R", 12), rep("L", 12))
change_direction_df$rot_num <- rep(c(rep(90, 4), rep(180, 4), rep(270, 4)), 2)
change_direction_df$start_dir <- rep(c("E", "S", "W", "N"), 6)
change_direction_df$end_dir <- c("S", "W", "N", "E", # R 90 
                                 "W", "N", "E", "S", # R 180
                                 "N", "E", "S", "W",  # R 270
                                 "N", "E", "S", "W", # L 90
                                 "W", "N", "E", "S", # L 180
                                  "S", "W", "N", "E") # L 270

convert_to_cardinal <- function(dat){
  dat <- dat %>% 
    mutate("turn" = as.logical(direction %in% c("L", "R")))
  north_south_num <- east_west_num <- 0
  # convert all F directions into N/S/E/W
  temp_direction <- "E"
  for (i in 1:nrow(dat)) {
    if (dat$direction[i] %in% c("R", "L", "F")) {
      if (dat$direction[i] == "F") {
        dat$direction[i] <- temp_direction
      } else if (dat$direction[i] == "R") {
        temp_direction <- change_direction_df %>% 
          filter(rot_dir == "R", rot_num == dat$value[i], start_dir == temp_direction) %>% 
          pull(end_dir)
        dat$direction[i] <- temp_direction
      } else if (dat$direction[i] == "L") {
        temp_direction <- change_direction_df %>% 
          filter(rot_dir == "L", rot_num == dat$value[i], start_dir == temp_direction) %>% 
          pull(end_dir)
        dat$direction[i] <- temp_direction
        
      }
    }
  }
  dat <- dat %>% 
    filter(!turn)
  return(dat)
}


calculate_manhattan_distance <- function(dat) {
  # Sum all of the N/S movement separately
  north_num <- dat %>% 
    filter(direction == "N") %>% 
    pull(value) %>% 
    sum(.)
  south_num <- dat %>% 
    filter(direction == "S") %>% 
    pull(value) %>% 
    sum(.)
  if (is_empty(south_num)) {south_num <- 0} 
  if (is_empty(north_num)) {south_num <- 0} 
  
  ns_num <- north_num - south_num
  
  # Sum all of the E/W movement separately
  east_num <- dat %>% 
    filter(direction == "E") %>% 
    pull(value) %>% 
    sum(.)
  west_num <- dat %>% 
    filter(direction == "W") %>% 
    pull(value) %>% 
    sum(.)
  if (is_empty(east_num)) {east_num <- 0} 
  if (is_empty(west_num)) {west_num <- 0} 
  ew_num <- west_num - east_num
  
  # Calculate manhattan distance
  man_dist <- sum(abs(ns_num) + abs(ew_num))
  return(man_dist)
}

cardinal_df <- convert_to_cardinal(df)
man_dist <- calculate_manhattan_distance(cardinal_df)
  

