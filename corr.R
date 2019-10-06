
pollutantmean <- function(directory, pollutant, id = 1:332){
  df <- data.frame()
  for(i in id){
    df <- rbind(df,read.table(sprintf("%s/%03d.csv", directory, i), head = T, sep = ","))
  }
  mean(df[,pollutant], na.rm = T)
}


complete <- function(directory, id = 1:332){
  df <- data.frame()
  for(i in id){
    temp <- read.table(sprintf("%s/%03d.csv", directory, i), head = T, sep = ",")
    df <- rbind(df, data.frame(id = i, nobs = length(which(!is.na(temp$sulfate) & !is.na(temp$nitrate)))))
  }
  df
}


corr <- function(directory, threshold = 0){
  v <- numeric()
  c <- 1
  for(i in 1:332){
    temp <- read.table(sprintf("%s/%03d.csv", directory, i), head = T, sep = ",")
    temp <- temp[complete.cases(temp),]
    if(nrow(temp) > threshold){
      v[c] <- cor(temp$sulfate, temp$nitrate)
      c = c + 1
    }
  }
  v
}