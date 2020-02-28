library(tidyverse)


board <- c(5,3,0,0,7,0,0,0,0,
          6,0,0,1,9,5,0,0,0,
          0,9,8,0,0,0,0,6,0,
          8,0,0,0,6,0,0,0,3,
          4,0,0,8,0,3,0,0,1,
          7,0,0,0,2,0,0,0,6,
          0,6,0,0,0,0,2,8,0,
          0,0,0,4,1,9,0,0,5,
          0,0,0,0,8,0,0,7,9)

board <- matrix(board, nrow = 9, ncol = 9, byrow = TRUE) 
board[board == 0] <- NA


# Check here: https://gist.github.com/christlc/b2fee49627cc0a15c7f72c127239ca7e

# https://github.com/dirkschumacher/r-sudoku/blob/master/app.R
# https://www.r-orms.org/mixed-integer-linear-programming/practicals/problem-sudoku/
# https://stackoverflow.com/questions/21691135/sudoku-solution-checker-in-r



board <- rbind(c(9, NA, 6,8,NA, NA, 3, NA, NA),
               c(NA, NA, NA, NA, 7, NA, NA, NA, NA),
               c(NA, NA, 1, NA, NA, NA, 6, NA, 2),
               c(NA, NA, NA, NA, 6, NA, 1, NA, 5),
               c(NA, 7, NA, NA, 5, NA, NA, 3, NA),
               c(6, NA, 3, NA, 8, NA, NA, NA, NA),
               c(4, NA, 2, NA, NA, NA, 5, NA, NA),
               c(NA, NA, NA, NA, 1, NA, NA, NA, NA),
               c(NA, NA, 5, NA, NA, 4, 9, NA, 1))


possible_choices <- function(board, i, j){
  # Creates an all TRUE logical vector
  possible_answers <- rep(TRUE,9)
  # Lists all known numbers from the row, column, and 3x3 cell
  selected_num <- unique(c(board[i,], board[,j], board[3*((i-1) %/% 3) + 1:3, 3*((j-1) %/% 3) + 1:3]))
  # Removes NAs
  selected_num <- na.omit(selected_num)
  # Changes the logical vector to FALSE for all values currently in use for the row, column, and 3x3 cell
  possible_answers[selected_num] <- FALSE
  # Returns this logical vector for use in subsequent functions...
  return(possible_answers)
}

# The 'board' argument here provides the matrix, length 81 (9x9), to iterate through. 
# The 'progress' argument here provides a starting value to recursively iterate through. 
solve_sudoku <- function(board, progress = 81) {
  if  (0 %in% board) {
    board[board == 0] <- NA
  } else board
  # Once all cells have been assessed within the 'possible_choices' function, it stops the recursion. 
  if (progress == 0) {
    # Successfully filled in the board
    return(board)
  }
  # Get the i,j coordinates
  i <- ((progress - 1) %% 9) + 1
  j <- ((progress - 1) %/% 9) + 1
  if (is.na(board[i, j])) {
    choices <- which(possible_choices(board, i, j))
  } else{
    choices <- c(board[i, j])
  }
  for (k in choices) {
    board[i, j] <- k
    answer <- solve_sudoku(board, progress - 1)
    if (!is.null(answer)) {
      return(answer)
    }
  }
  return(NULL)
}

solve_sudoku(board)

# -------------

possible_choices(board, 1, 7)


((10 - 1) %% 9) + 1

11 %% 4


i <- ((80 - 1) %% 9) + 1
j <- ((80 - 1) %/% 9) + 1


selected_num <- unique(c(board[1,], board[,1], board[3*((1-1) %/% 3) + 1:3, 3*((1-1) %/% 3) + 1:3]))
x

possible_answers[selected_num] <- FALSE
possible_answers


if  (0 %in% board) {
  board[board == 0] <- NA
} else board