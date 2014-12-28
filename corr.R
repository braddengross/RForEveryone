#Part 3
corr <- function(directory = getwd(), threshold = 0) {
  options(digits = 4)
  data_files <- list.files(directory, full.names = TRUE)
  cc_vect = vector("numeric")
  for(i in 1:332){
    csv = read.csv(data_files[i])
    completeCases = sum(complete.cases(csv))
    if(completeCases > threshold){
      cor <- cor(csv$nitrate, csv$sulfate, use="complete.obs")
      cc_vect <- c(cc_vect, cor)
    }
  }
  cc_vect
}

