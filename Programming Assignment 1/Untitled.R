#sulfate, nitrate
pollutantmean <- function(directory, pollutant, id = 1:332){
  data_files2 <- list.files(directory)
  means <- rep(0, length(id))
  for(i in 1:length(id)){
    csv = read.csv(data_files2[i])
    mean <- mean(csv[,pollutant], na.rm = TRUE)
    if(!is.nan(mean)){means[i] <- mean}
  }
  mean(means)
}
pollutantmean("/Users/brad/Coursera/RForEveryone/specdata", "sulfate", 1)

