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