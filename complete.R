complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  nobs <- (rep(0,length(id)))
  for (i in seq_along(id)){
    if(id[i]<10) {prefix <- "00"}
    else if (id[i]>=10 && id[i]<100) {prefix <- "0"}
    else {prefix <- ""}
    df <- read.csv(paste(directory,"/",prefix,id[i],".csv", sep=""))
    nobs[i] <- sum(complete.cases(df))
  }
  data.frame(id = id, nobs = nobs)
}

