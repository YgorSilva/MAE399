readCSVDirectory <- function(dir) {
  files <- list.files(dir)
  data <- read.csv(paste(dir, '/', files[1], sep=""))
  files <- files[seq(2:length(files))]
  for (file in files) {
    data2 <- read.csv(paste(dir, '/', file, sep=""))
    data <- rbind(data, data2)
  }
  return(data)
}