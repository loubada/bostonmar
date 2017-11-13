library(dplyr)
library(chron)
library(ggplot2)

data15 <- read.csv2("marathon_results_2015.csv", sep = ",")
data16 <- read.csv2("marathon_results_2016.csv", sep = ",")
data17 <- read.csv2("marathon_results_2017.csv", sep = ",")



data15[, c(11:19, 22)] <- data15[, c(11:19, 22)] %>% lapply(times)
index15 <- data15[, c(11:19, 22)] %>% lapply(function(X) {which(is.na(X))})
summary(data15[, c(11:19, 22)])
vec15 <- Reduce(c, index15)
vecunique15 <- unique(vec15)
data15_1 <- data15[-c(as.numeric(vecunique15)),]
duration_mins_15 <- data15_1[, c(11:19, 22)] %>% lapply(function(X) {
  mins = hours(X)*60 + minutes(X) + seconds(X)/60
  return(mins)
})
data15_1 <- c(data15_1, duration_mins_15)



data16[, c(11:19, 22)] <- data16[, c(11:19, 22)] %>% lapply(times)
index16 <- data16[, c(11:19, 22)] %>% lapply(function(X) {which(is.na(X))})
vec16 <- Reduce(c, index16)
vecunique16 <- unique(vec16)
data16_1 <- data16[-c(as.numeric(vecunique16)),]
duration_mins_16 <- data16_1[, c(11:19, 22)] %>% lapply(function(X) {
  mins = hours(X)*60 + minutes(X) + seconds(X)/60
  return(mins)
})
data16_1 <- c(data16_1, duration_mins_16)



data17[, c(11:19, 22)] <- data17[, c(11:19, 22)] %>% lapply(times)
index17 <- data17[, c(11:19, 22)] %>% lapply(function(X) {which(is.na(X))})
vec17 <- Reduce(c, index17)
vecunique17 <- unique(vec17)
data17_1 <- data17[-c(as.numeric(vecunique17)),]
duration_mins_17 <- data17_1[, c(11:19, 22)] %>% lapply(function(X) {
  mins = hours(X)*60 + minutes(X) + seconds(X)/60
  return(mins)
})
data17_1 <- c(data17_1, duration_mins_17)



data15_1 <- as.data.frame(data15_1)[ ,-c(1)]
data17_1 <- as.data.frame(data17_1)[ ,-c(1)]

data15_1 <- as.data.frame(data15_1)[ ,-c(9:18, 20,21)]
data16_1 <- as.data.frame(data16_1)[ ,-c(9:18, 20,21)]
data17_1 <- as.data.frame(data17_1)[ ,-c(9:18, 20,21)]

data_all <- rbind(data15_1,data16_1,data17_1)
saveRDS(data_all, "data_all.rds")