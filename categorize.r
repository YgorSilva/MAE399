categorize <- function(data, n) {
  a <- c(1:n)
  span <- max(data) - min(data)
  steps <- span/n
  total <- length(data)
  interval <- min(data)
  for(i in 1:n) {
    a[i] <- length(data[interval <= data & data < interval + steps]) / total
    if (a[i] > 1) {
      print('OH SHIT')
    }

    interval <- interval + steps
  }
  return(a)
}

compareWithExponencial <- function(data, n) {
  lambda <- 1 / mean(data)
  Oi <- data
  Ei <- rexp(length(data), rate=lambda)
  comparationHistogram(Oi, Ei)
}

comparationHistogram <- function(data1, data2, n) {
  p1 <- hist(data1, col=rgb(1, 0, 0, 1/2), breaks=n, plot=F)
  p2 <- hist(data2, col=rgb(0, 0, 1, 1/2), breaks=n, plot=F)
  p1$counts <- p1$counts / sum(p1$counts)
  p2$counts <- p2$counts / sum(p2$counts)
  plot(p1, freq=TRUE,ylab="Relative Frequency")
  plot(p2, freq=TRUE,add=TRUE)
}