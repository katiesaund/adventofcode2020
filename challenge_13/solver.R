# Part 1
# Each bus has an ID number that also indicates how often the bus leaves for the airport.

# The first line is your estimate of the earliest timestamp you could depart on a bus.
# The second line lists the bus IDs that are in service according to the shuttle company; entries that show x must be out of service, so you decide to ignore them.

# To save time once you arrive, your goal is to figure out the earliest bus you can take to the airport. (There will be exactly one such bus.)

# What is the ID of the earliest bus you can take to the airport multiplied by the number of minutes you'll need to wait for that bus?

# Example solution == 295

# Read in data and format
library(tidyverse)
timestamp <- as.numeric(readLines("puzzle_input.txt")[1])
bus_names <- read_csv("puzzle_input.txt", skip = 1, col_names = FALSE) %>% unlist() %>% unname() 
bus_route_vec <- bus_names[!grepl("x", bus_names)] %>% as.numeric()

# Get the bus that will depart next for each line after the timestampe
next_bus_vec <- ceiling(timestamp / bus_route_vec) * bus_route_vec
names(next_bus_vec) <- bus_route_vec
next_bus_num <- as.numeric(names(next_bus_vec)[next_bus_vec == min(next_bus_vec)])

solution <- (min(next_bus_vec) - timestamp) * next_bus_num
solution

# Part 2 --------
# The first line in your input is no longer relevant

# find the earliest timestamp such that the first bus ID departs at that time and each subsequent listed bus ID departs at that subsequent minute. 

# Lots of examples are provided: 
# 7,13,x,x,59,x,31,19: 1068781
# 17,x,13,19: 3417.
# 67,7,59,61: 754018.
# 67,x,7,59,61:  779210.
# 67,7,x,59,61: 1261476.
# 1789,37,47,1889: 1202161486.

# What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list?

# Format data for part 2
names(bus_names) <- 1:length(bus_names) - 1
bus_names <- bus_names[bus_names != "x"]
# I think the biggeset constrains we're working with are: 
# Leaving on multiples of 17, 971 bus leaving 48 minutes later, and 739 leaving 17 minutes later



# Solve 17,x,13,19:3417.
# 0/17 2/13 3/19

((17 * t) + 3) %% 19 == 0
((17 * t) + 2) %% 13 == 0



