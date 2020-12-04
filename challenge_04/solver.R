make_data_tidy <- function(file_name) {
  df <- read.delim(file = file_name, header = FALSE, sep = "\n") %>% 
    mutate(indiv_id = "")
  
  # Assign individual ids to each person
  counter <- 1
  for (i in 1:nrow(df)) {
    if (str_count(df$V1[i]) <= 4) {
      counter <- counter + 1
    }
    df$indiv_id[i] <- counter
  }
  
  # Remove number char I used to assign individual ids and remove those formerly empty lines
  df <- df %>% 
    mutate(V1 = gsub(pattern = "^[0-9]+", replacement = "", x = V1)) %>% 
    filter(nchar(V1) > 1)
  
  # split V1 by spaces
  df <- df %>%
    separate(V1, into = paste0("C", 1:8), sep = " ", remove = TRUE, fill = "right")
  
  df <-
    df %>% 
    pivot_longer(!indiv_id, names_to = "column_names", values_to = "passport_entries") %>% 
    filter(!is.na(passport_entries)) %>% 
    select(-column_names) %>% 
    separate(col = passport_entries, into = c("type", "value"), sep = ":")
  return(df)
}

# Part 1
# Problem statement: 

# Count the number of valid passports - those that have all required fields.
# Treat cid as optional. In your batch file, how many passports are valid?

count_valid_passports <- function(file_name) {
  df <- make_data_tidy(file_name)
  
  # Required inputs
  required <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  
  num_valid  <- 0 
  for (i in as.numeric(unique(df$indiv_id))) {
    temp_df <- df %>% 
      filter(indiv_id == i)
    if (length(intersect(required, temp_df$type)) == 7) {
      num_valid <- num_valid + 1
    }
  }
  return(num_valid)
}

count_valid_passports("example_input2.txt")
count_valid_passports("puzzle_input2.txt")

# Part 2 
# Problem statement
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

# Looks like I finally need to get my data tidy :( 

count_valid_passports_2 <- function(file_name) {
  df <- make_data_tidy(file_name)

  num_valid_passports <- 0
  for (i in as.numeric(unique(df$indiv_id))) {
    temp_df <- df %>% 
      filter(indiv_id == i)
    
    byr_valid <- temp_df %>% 
      filter(type == "byr", nchar(value) == 4, value <= 2002 & value >= 1920) %>% 
      nrow()
    
    iyr_valid <- temp_df %>% 
      filter(type == "iyr", nchar(value) == 4, value <= 2020 & value >= 2010) %>% 
      nrow()
    
    eyr_valid <- temp_df %>% 
      filter(type == "eyr", nchar(value) == 4, value <= 2030 & value >= 2020) %>% 
      nrow()
    
    hgt_in_valid <- temp_df %>% 
      filter(type == "hgt", grepl("in", value)) %>% 
      select(value) %>% 
      mutate(value = as.numeric(gsub("in", "", value))) %>% 
      filter(value <= 76 & value >= 59) %>% 
      nrow()
    
    hgt_cm_valid <- temp_df %>% 
      filter(type == "hgt", grepl("cm", value)) %>% 
      select(value) %>% 
      mutate(value = as.numeric(gsub("cm", "", value))) %>% 
      filter(value <= 193 & value >= 150) %>% 
      nrow()
    
    hcl_valid <- temp_df %>% 
      filter(type == "hcl", nchar(value) == 7,
             grepl("[#][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]", value)) %>% 
      nrow()
    
    ecl_valid <- temp_df %>% 
      filter(type == "ecl",
             value %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>% 
      nrow()
    
    pid_valid <- temp_df %>% 
      filter(type == "pid", 
             nchar(value) == 9) %>% 
      nrow()
    
    num_valid_values <- byr_valid + iyr_valid + eyr_valid + hgt_in_valid + 
      hgt_cm_valid + hcl_valid + ecl_valid + pid_valid
    
    if (num_valid_values == 7) {
      num_valid_passports <- num_valid_passports + 1
    }
  }
  
  return(num_valid_passports)
}

count_valid_passports_2("valid_passports2.txt")
count_valid_passports_2("invalid_passports2.txt")
count_valid_passports_2("example_input2.txt")
count_valid_passports_2("puzzle_input2.txt")


