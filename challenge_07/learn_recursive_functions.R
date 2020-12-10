# I haven't used a recusrive function in R before so let's learn how. 


# Part A 
# Tutorial: https://www.datamentor.io/r-programming/recursion/
# Recursive function to get factorial
recursive_factorial <- function(num){
  if (num == 0) {
    return(1)
  } else {
    return(num * recursive_factorial(num - 1))
  }
}
recursive_factorial(0) # 1
recursive_factorial(1) # 1
recursive_factorial(5) # 120
recursive_factorial(10) # 3628800
# In this function when num == 0 the function terminates

# Part B
# Tutorial: https://www.geeksforgeeks.org/recursive-functions-in-r-programming/
# Recursive function to get the sum of squares of a given set of numbers

sum_of_squares <- function(vec) {
  if (length(vec) <= 1) { # terminating condition: the length of the vector
    return(vec ^ 2)
  } else {
    return(sum(vec[1]^2 + sum_of_squares(vec[-1])))
  }
}
sum_of_squares(c(2, 3, 4)) # 29
sum_of_squares(c(1, 2)) # 5
sum_of_squares(3) # 9
