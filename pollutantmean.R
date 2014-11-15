pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  sum1 <- 0
  good_d <- 0
  
  for (i in id){
    if(i<10) {prefix <- "00"}
    else if (i>=10 && i<100) {prefix <- "0"}
    else {prefix <- ""}
    df <- read.csv(paste(directory,"/",prefix,i,".csv", sep=""))
    sum1 <- sum1 + sum(df[[pollutant]],na.rm = TRUE)
    good_d <- good_d + sum(!is.na(df[[pollutant]]))
  }
  sum1/good_d
}

pollutantmean("specdata","nitrate",100)
