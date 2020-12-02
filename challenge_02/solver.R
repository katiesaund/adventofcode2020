# Problem statement: 
# For example, suppose you have the following list:
#
# 1-3 a: abcde 
# 1-3 b: cdefg 
# 2-9 c: ccccccccc 
# Each line gives the password policy
# and then the password. The password policy indicates the lowest and highest
# number of times a given letter must appear for the password to be valid. For
# example, 1-3 a means that the password must contain a at least 1 time and at
# most 3 times.
#
# In the above example, 2 passwords are valid. The middle password, cdefg, is
# not; it contains no instances of b, but needs at least 1. The first and third
# passwords are valid: they contain one a or nine c, both within the limits of
# their respective policies.
#
# How many passwords are valid according to their policies?
# 

library(tidyverse)
df <- read_delim("puzzle_input.tsv", col_names = FALSE, delim = " ")
df <- df %>% 
  mutate(min = as.numeric(gsub("[-].*", "", X1)), 
         max = as.numeric(gsub(".*[-]", "", X1)), 
         letter = gsub("[:]", "", X2)) %>% 
  select(min, max, letter, X3) %>% 
  rename(password = X3)


# Part 1
df %>% 
  mutate(letter_count = str_count(pattern = letter, string= password)) %>% 
  filter(letter_count <= max & letter_count >= min) %>% 
  nrow(.)

# Part 2
# Problem Statement
# Each policy actually describes two positions in the password, where 1 means
# the first character, 2 means the second character, and so on. (Be careful;
# Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of
# these positions must contain the given letter. Other occurrences of the letter
# are irrelevant for the purposes of policy enforcement.
# 
# Given the same example list from above:
#   
#   1-3 a: abcde is valid: position 1 contains a and position 3 does not.
# 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
# 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
# How many passwords are valid according to the new interpretation of the 
# policies?

df %>% 
  mutate(position1 = str_sub(password, min, min), 
         position2 = str_sub(password, max, max)) %>% 
  filter((letter == position1 & letter != position2) | 
           letter != position1 & letter == position2)

