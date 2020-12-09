# Part 1
# Problem Statement
# Immediately before any instruction is executed a second time, what value is in 
# the accumulator?
library(tidyverse)
df <- read_csv("puzzle_input.txt", col_names = FALSE)
df <- df %>% 
  separate(col = X1, into = c("command", "num"), remove = TRUE, sep = " ") %>% 
  mutate(visits = 0, 
         num = as.numeric(num))

i <- 1
acc_num <- 0
counter <- 1
while (sum(df$visits == 2) == 0) {
  if (df$command[i] == "nop") {
    df$visits[i] <- df$visits[i] + 1
    i <- i + 1    
  } else if (df$command[i] == "acc") {
    if (df$visits[i] + 1 == 2) {
      stop
    }
    df$visits[i] <- df$visits[i] + 1
    acc_num <- acc_num + df$num[i]
    i <- i + 1
  } else {
    df$visits[i] <- df$visits[i] + 1
    i <- i + df$num[i]
  }
  counter <- counter +  1
}
acc_num
