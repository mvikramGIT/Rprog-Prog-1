corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  comp <- complete(directory)
  comp <- comp[comp$nobs > threshold,]$id
  output <- rep(0,length(comp))
  if (length(comp) > 0){
  for (i in 1:length(comp)){
    if(comp[i]<10) {prefix <- "00"}
    else if (comp[i]>=10 && comp[i]<100) {prefix <- "0"}
    else {prefix <- ""}
    df <- read.csv(paste(directory,"/",prefix,comp[i],".csv", sep=""))
    output[i] <- cor(df$sulfate, df$nitrate, use = "complete.obs")
  }}
  output
}