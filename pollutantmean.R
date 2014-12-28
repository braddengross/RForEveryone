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