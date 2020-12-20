# Part 1

# Each image tile has been rotated and flipped to a random orientation. Your
# first task is to reassemble the original image by orienting the tiles so they 
# fit together.
# To show how the tiles should be reassembled, each tile's image data includes a
# border that should line up exactly with its adjacent tiles. All tiles have 
# this border, and the border lines up exactly when the tiles are both oriented 
# correctly. Tiles at the edge of the image also have this border, but the 
# outermost edges won't line up with any other tiles.
# Assemble the tiles into an image. What do you get if you multiply together the
# IDs of the four corner tiles?

# Example tile orientation in solution: 
# 1951    2311    3079
# 2729    1427    2473
# 2971    1489    1171
# 
# Example answer: 951 * 3079 * 2971 * 1171 = 20899048083289.

# Read in data 
library(tidyverse)
raw_tiles <- readLines("example_input.txt")
num_tiles <- sum(grepl("Tile ", raw_tiles))
tile_ids <- raw_tiles[grepl("Tile ", raw_tiles)] %>% gsub("Tile ", "", .) %>% gsub("[:]", "", .) %>% as.numeric
raw_tiles <- raw_tiles[raw_tiles != ""]

empty_mat <- matrix(NA, ncol = 10, nrow = 10)
tile_list <- vector("list", num_tiles)
names(tile_list) <- tile_ids
for (i in 1:num_tiles) { tile_list[[i]] <- empty_mat}

mat_num <- 0 
for (i in 1:length(raw_tiles)) {
  if (grepl("Tile", raw_tiles[i])) {
    mat_num <- mat_num + 1
  } else {
    row_num <- (i - mat_num) %% 10
    if (row_num == 0) { row_num <- 10}
    tile_list[[mat_num]][row_num, ] <- raw_tiles[i] %>% strsplit(., "") %>% .[[1]]
  }
}

# Rotate matrix function

# Flip matrix function 

# Look for matches between edges
#   Each tile has 8 unique edges (as is and flipped orientation)
#   We want to compare each tile's unique edges to every other tile's unique edges
#   Some edges won't have a match --> these need to be outside edges
#   If some edges have only one match then that means that tile has to have that particular matrix partner and the space becomes more contrained. 

empty_mat <- matrix(NA, ncol = 10, nrow = 8)
tile_edge_list <- vector("list", num_tiles)
names(tile_edge_list) <- tile_ids
for (i in 1:num_tiles) { tile_edge_list[[i]] <- empty_mat}

for (i in 1:num_tiles) {
  # Top row
  tile_edge_list[[i]][1, ] <- tile_list[[i]][1, ]
  # Flipped top row
  tile_edge_list[[i]][2, ] <- tile_list[[1]][1, ][10:1]
  
  # Bottom row
  tile_edge_list[[i]][3, ] <- tile_list[[i]][10, ]
  # Flipped bottom row
  tile_edge_list[[i]][4, ] <- tile_list[[1]][10, ][10:1]
  
  # Left column
  tile_edge_list[[i]][5, ] <- tile_list[[i]][, 1]
  # Left column flipped
  tile_edge_list[[i]][6, ] <- tile_list[[i]][, 1][10:1]
  
  # Right column
  tile_edge_list[[i]][7, ] <- tile_list[[i]][, 10]
  # Right column flipped
  tile_edge_list[[i]][8, ] <- tile_list[[i]][, 10][10:1]
}

# Do the pairwise comparisons for matches
match_record_mat <- matrix(NA, 9, 9)
for (i in 1:num_tiles) {
  for (j in 1:num_tiles) {
    if (i < j) {
      match_count <- 0 
      for (k in 1:8) {
        for (l in 1:8) {
          
          if (identical(tile_edge_list[[i]][k, ], tile_edge_list[[j]][l, ])) {
            match_count <- match_count + 1
            match_record_mat[i, j] <- match_count
            print(paste(i, j, k, l))
          }
        }
      }
    }
  }
}