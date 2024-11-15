# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @param n Number of rows in Pascal's Triangle.
#' @return A list where each element is a row in Pascal's Triangle.
#' @export
pascals_triangle <- function(n) {
  triangle <- list()
  
  for (i in 0:(n)) {
    row <- numeric(i + 1)
    row[0] <- 1
    row[i + 1] <- 1
    if (i > 1) {
      for (j in 2:i) {
        row[j] <- triangle[[i - 1]][j - 1] + triangle[[i - 1]][j]
      }
    }
    triangle[[i]] <- row
  }
  
  return(triangle)
}

# To print Pascal's Triangle for n = 9
n <- 9
triangle <- pascals_triangle(n)
for (row in triangle) {
  print(row)
}
