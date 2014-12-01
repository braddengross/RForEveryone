#Part 1
#Valid values are sulfate, nitrate
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

#Part 2
completeCases <- function(directory, id = 1:332) {
  data_files2 <- list.files(directory)
  m <- matrix(nrow =length(id), ncol=2)
  for(i in 1:length(id)){
    csv = read.csv(data_files2[i])
    completeCases = sum(complete.cases(csv))
    m[i, 1] <- i
    m[i, 2] <- completeCases
  }
  df <- data.frame(m)
  colnames(df) <- c("id", "nobs")
  df
}
c <- completeCases("/Users/brad/Coursera/RForEveryone/specdata")

#Part 3
correlationAboveThreshold <- function(directory, threshold = 0) {
  data_files2 <- list.files(directory)
  vec = numeric(0)
  for(i in 1:332){
    csv = read.csv(data_files2[i])
    completeCases = sum(complete.cases(csv))
    if(completeCases > threshold){
      print(completeCases)
      #print(cor(csv$nitrate, csv$sulfate, use="complete.obs"))
      cor <- cor(csv$nitrate, csv$sulfate, use="complete.obs")
      append(vec, cor)
    }
  }
  ## Return a numeric vector of correlations
  vec
}
v <- correlationAboveThreshold("/Users/brad/Coursera/RForEveryone/specdata", 17)

