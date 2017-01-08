#234567890#234567890#234567890#234567890#234567890#234567890#234567890#234567890
## Sample data
a <- 1:10
b <- 11:15
c <- numeric()
vectors <- list(a=a, b=b, c=c)

## Results vector
results <- numeric()

## Initialize start and end indices
start <- end <- 0

for(index in seq_along(vectors)) {
  values <- vectors[[index]]
  size <- length(values)
  
  if(size > 0) {
    start <- end + 1
    end <- start + size - 1
    results[start:end] <- values
  }
}
results
values <- numeric()

for(index in seq_along(frames)) {
  frame <- frames[[index]]
  value <- mean(frame$a) + mean(frame$b)
  values <- c(values, value)

  cube <- function(x, n) {
          x^3
  }
  cube(3)
  ###########
  x <- 1:10
  if(x > 5) {
          x <- 0
  }
  
  f <- function(x) {
          g <- function(y) {
                  y + z
          }
          z <- 4
          x + g(x)
  }
  z <- 10
  f(3)
  
  x <- 5
  y <- if(x < 3) {
          NA
  } else {
          10
  }
  y
  #######
  h <- function(x, y = NULL, d = 3L) {
          z <- cbind(x, d)
          if(!is.null(y))
                  z <- z + y
          else
                  z <- z + f
          g <- x + y / z
          if(d == 3L)
                  return(g)
          g <- g + 10
          g
  }
  ########
 x<-list(a=1:5, b=rnorm(10))
 lapply(x, mean)