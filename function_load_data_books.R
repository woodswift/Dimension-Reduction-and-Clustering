load.data.books <- function() {
  info <- read.csv('book-info.csv')
  copurchase <- read.csv('book-copurchase.csv')
  # info[1:5,]
  # copurchase[1:5,]
  dataset <- data.frame(info,copurchase)
  # dataset[1:5,]
  dataset <- dataset[,-4]
  return(dataset)
}