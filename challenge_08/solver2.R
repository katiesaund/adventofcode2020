# Part 2 
# Problem Statement 

# Fix the program so that it terminates normally by changing exactly one jmp (to
# nop) or nop (to jmp). What is the value of the accumulator after the program
# terminates? (the program terminates by attempting to run the instruction below
# the last instruction in the file.)
library(tidyverse)
df <- read_csv("puzzle_input.txt", col_names = FALSE)
df <- df %>% 
  separate(col = X1, into = c("command", "num"), remove = TRUE, sep = " ") %>% 
  mutate(visits = 0, 
         num = as.numeric(num))

run_program <- function(dat){
  i <- 1
  exit_num <- nrow(dat) + 1
  acc_num <- 0
  counter <- 1
  while (sum(dat$visits == 2) == 0) {
    if (i == exit_num) {
      stop("Program exited correctly; accumulator == ", acc_num)
    } else if (dat$command[i] == "nop") {
      if (dat$visits[i] + 1 == 2) {
        stop(paste0("infinite loop; nop; accumulator == ", acc_num))
      }
      dat$visits[i] <- dat$visits[i] + 1
      i <- i + 1    
    } else if (dat$command[i] == "acc") {
      if (dat$visits[i] + 1 == 2) {
        stop(paste0("infinite loop; acc; accumulator == ", acc_num))
      }
      dat$visits[i] <- dat$visits[i] + 1
      acc_num <- acc_num + dat$num[i]
      i <- i + 1
    } else { # jmp
      if (dat$visits[i] + 1 == 2) { # infinite loop
        stop(paste0("infinite loop; jmp; accumulator == ", acc_num))
      }
      dat$visits[i] <- dat$visits[i] + 1
      if (dat$num[i] == 0) { # jmp stuck in one position
        stop(paste0("infinite loop; jmp stuck in one place; accumulator == ", acc_num))
      }
      i <- i + dat$num[i]
    }
    counter <- counter +  1
  }
}

nop_indices <- which(df$command == "nop")
jmp_indices <- which(df$command == "jmp")

for (j in nop_indices) {
  temp_df <- df
  temp_df$command[j] <- "jmp"
  try({
    print(j)
    run_program(temp_df)})
}

for (k in jmp_indices) {
  temp_df <- df
  temp_df$command[k]  <- "nop"
  try({
    print(k)
    run_program(temp_df)})
}
