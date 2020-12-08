# Part 2
# Problem statement
# How many individual bags are required inside your single shiny gold bag?

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
  mutate("num" = gsub(" .*", "", value), 
         "contents" = gsub("[0-9] ", "", value)) %>% 
  select(-value)


# I got stuck 
get_contained_bags <- function(bag_name){
  df <- df %>% 
    filter(bag_type == bag_name)
}
  
get_contained_bags("shiny gold bag")  

  # tbl <- t(tbl)
  # colnames(tbl) <- "temp"
  # tbl <- tbl %>% as_tibble(tbl) 
  # tbl <- tbl %>% 
  #   filter(!is.na(temp)) %>% 
  #   mutate("num" = gsub(" .*", "", temp), 
  #          "bag_type" = gsub("[0-9] ", "", temp)) %>% 
  #   select(-temp) %>% 
  #   filter(nchar(num) > 0) %>% 
  #   mutate("parent" = bag_name)


# find_all_contained_bags <- function(tbl) {
#   len <- length(unique(tbl$bag_type))
#   unique_bags <- NULL
#   for (i in 1:len) {
#     current_bag <- tbl$bag_type[i]
#     temp <- get_contained_bags(current_bag, tbl)
#     unique_bags <- c(unique_bags, temp)
#     unique_bags <- unique(unique_bags)
#   }
#   return(unique_bags)
# }


# running_tally <- 0
# contained_bags <- get_contained_bags("shiny gold bag", df)
# new_contained_bags <- NULL
# temp <- NULL
# 
# for (i in 1:nrow(contained_bags)) {
#   new_contained_bags <- rbind(new_contained_bags, get_contained_bags(contained_bags$bag_type[i], df))
# }
# 
# all_bags <- rbind(contained_bags, new_contained_bags)
# 
# while (sum(temp$num != "0") > 0) {
#   print("here")
#   for (i in 1:nrow(new_contained_bags)) {
#     temp <- rbind(temp, get_contained_bags(new_contained_bags$bag_type[i], df))
#   }
# }
# 
