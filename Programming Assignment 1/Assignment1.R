#Part 1
#Valid values are sulfate, nitrate
pollutantmean <- function(directory = getwd(), pollutant, id = 1:332){
  options(digits = 4)
  data_files <- list.files(directory, full.names = TRUE)
  pollutantReadings <- rep(0, length(id))
  mean_vector <- c()
  for(i in 1:length(id)){
    csv = read.csv(data_files[id[i]])
    file_data <- (csv[,pollutant])
    mean_vector <- c(mean_vector, file_data)
  }
  mean(mean_vector, na.rm = TRUE)
}
pollutantmean("specdata", "sulfate", 1:10)

#Part 2
complete <- function(directory = getwd(), id = 1:332) {
  data_files <- list.files(directory, full.names = TRUE)
  m <- matrix(nrow =length(id), ncol=2)
  for(i in 1:length(id)){
    csv = read.csv(data_files[id[i]])
    completeCases = sum(complete.cases(csv))
    m[i, 1] <- id[i]
    m[i, 2] <- completeCases
  }
  df <- data.frame(m)
  colnames(df) <- c("id", "nobs")
  df
}
#c <- complete("/Users/brad/Coursera/RForEveryone/specdata")

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
