categorize <- function(data, n) {
  a <- c(1:n)
  span <- max(data) - min(data)
  steps <- span/n
  interval <- min(data)
  for(i in 1:n) {
    a[i] <- length(data[interval <= data & data < interval + steps])
    interval <- interval + steps
  }
  return(a)
}