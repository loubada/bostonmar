data15 <- read.csv2("marathon_results_2015.csv", sep = ",")
data16 <- read.csv2("marathon_results_2016.csv", sep = ",")
data17 <- read.csv2("marathon_results_2017.csv", sep = ",")

library(dplyr)
View(data17)
class(data17$X5K)
library(stringr)
data15$year <- rep(15, length(data15$X))
data16$year <- rep(16, length(data16$X))
data17$year <- rep(17, length(data17$X))

names(data15_1)
names(data16_1)
names(data17_1)

data_all <- rbind(data15_1,data16_1,data17_1)
View(data_all)
saveRDS(data_all, "data_all.rds")



data15_1["Year"] <- rep("2015")
data16_1["Year"] <- rep("2016")
data17_1["Year"] <- rep("2017")


data_all <- rbind(data15_1, data16_1, data17_1)

View(data15)
library(chron)

data15_1 <- as.data.frame(data15_1)[ ,-c(9:18, 20,21)]
data15_1 <- as.data.frame(data15_1)[ ,-c(13)]
data16_1 <- as.data.frame(data16_1)[ ,-c(9:18, 20,21)]
data17_1 <- as.data.frame(data17_1)[ ,-c(9:18, 20,21)]
data17_1 <- as.data.frame(data15_1)[ ,-c(13)]
data15_1 <- as.data.frame(data15_1)[ ,-c(1)]
data17_1 <- as.data.frame(data17_1)[ ,-c(1)]

data_all <- rbind(data15_1,data16_1,data17_1)

View(data17_1)

data15[, c(11:19, 22)] <- data15[, c(11:19, 22)] %>% lapply(times)
index15 <- data15[, c(11:19, 22)] %>% lapply(function(X) {which(is.na(X))})
index15
summary(data15[, c(11:19, 22)])
class(index15)
vec15 <- Reduce(c, index15)
length(vec15)
length(index15$X5K)+length(index15$X10K)+length(index15$X15K)+length(index15$X20K)+length(index15$Half)+length(index15$X25K)+length(index15$X30K)+length(index15$X35K)+length(index15$X40K)+length(index15$Official.Time)
vecunique15 <- unique(vec15)
is.vector(vecunique15)
data15_1 <- data15[-c(as.numeric(vecunique15)),]
"NA" %in% data15_1[,c(11:19,22)]

data16[, c(10:18, 21)] <- data16[, c(10:18, 21)] %>% lapply(times)
index16 <- data16[, c(10:18, 21)] %>% lapply(function(X) {which(is.na(X))})
vec16 <- Reduce(c, index16)
length(vec16)
length(index16$X5K)+length(index16$X10K)+length(index16$X15K)+length(index16$X20K)+length(index16$Half)+length(index16$X25K)+length(index16$X30K)+length(index16$X35K)+length(index16$X40K)+length(index16$Official.Time)
vecunique16 <- unique(vec16)
data16_1 <- data16[-c(as.numeric(vecunique16)),]
"NA" %in% data16_1[,c(11:19,22)]


data17[, c(11:19, 22)] <- data17[, c(11:19, 22)] %>% lapply(times)
index17 <- data17[, c(11:19, 22)] %>% lapply(function(X) {which(is.na(X))})
vec17 <- Reduce(c, index17)
length(vec17)
length(index17$X5K)+length(index17$X10K)+length(index17$X15K)+length(index17$X20K)+length(index17$Half)+length(index17$X25K)+length(index17$X30K)+length(index17$X35K)+length(index17$X40K)+length(index17$Official.Time)
vecunique17 <- unique(vec17)
data17_1 <- data17[-c(as.numeric(vecunique17)),]
"NA" %in% data17_1[,c(11:19,22)]

duration_mins_15 <- data15_1[, c(11:19, 22)] %>% lapply(function(X) {
  mins = hours(X)*60 + minutes(X) + seconds(X)/60
  return(mins)
})
data15_1 <- c(data15_1, duration_mins_15)

duration_mins_16 <- data16_1[, c(10:18, 21)] %>% lapply(function(X) {
  mins = hours(X)*60 + minutes(X) + seconds(X)/60
  return(mins)
})
data16_1 <- c(data16_1, duration_mins_16)

duration_mins_17 <- data17_1[, c(11:19, 22)] %>% lapply(function(X) {
  mins = hours(X)*60 + minutes(X) + seconds(X)/60
  return(mins)
})
data17_1 <- c(data17_1, duration_mins_17)

library(ggplot2)
View(data15_1)

data15_1 <- as.data.frame(data15_1)
#data15_1[ ,c(1, 27:36)] %>% select(X5K.1, X10K.1) %>% ggplot(aes(x))

res.hc <- data15_1[ ,c(1, 27:36)] %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering
# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k =4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

library("cluster")
library("factoextra")
library("magrittr")

summary(res.hc)
plot(res.hc)
install.packages("NbClust")
library("NbClust")
inc<-fviz_nbclust(data15_1[ ,c(1, 27:36)], kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
MarathonCluster_15 <- kmeans(data15_1[ ,c(27:36)], 4, nstart = 20)
MarathonCluster_15

data15_1["cluster"] <- MarathonCluster_15$cluster
View(data15_1)

unique(data15_1$Country)
detach("package:dplyr", unload=TRUE)
library(plyr)
library(dplyr)
data15_1_Dup <- data15_1
data15_1_Dup["Freq"] <- rep(1) 

Country_15 <-arrange(as.data.frame(table(unlist(data15_1$Country))), desc(Freq))
Country_15 <- as.vector(Country_15)

Country_15_List <- Country_15[, 1]
Country_15_top10 <- as.vector(Country_15_List[1:10])
is.vector(Country_15_top10)

Country_15_top10

# the below function did not work with the specific data structure
#if_in_top10 <- function(X){
#  if (X %in% Country_15_top10 == TRUE){
#    X = as.character(X)
#  }else{
#    X = "Other"
#  }
#  return(X)
#}

#the below function is too inefficient
#if_in_top_10_iteractive <- function(X){
#  for (i in 1:length(data15_1$Country)){
#    if (X[i] %in% Country_15_top10== TRUE) { 
#      X[i] = X[i]
#    }
#    else{
#      X[i] = "Other"
#    }
#  }
#}


summary(data15_1)
data15_1$Country[7] %in% Country_15_top10
index_country_15 <- which(!data15_1_Dup$Country %in% Country_15_top10 == TRUE)
data15_1_Dup$Country <- as.character(data15_1_Dup$Country)
data15_1_Dup$Country[index_country_15] <- "Others" 
unique(data15_1_Dup$Country)
data15_1_Dup$Country_S <- NULL
data15_1_Dup$Country <- factor(data15_1_Dup$Country)
is.factor(data15_1_Dup$Country)
View(data15_1_Dup)


model.lm.country <- lm(Official.Time.1 ~ Country, data = data15_1_Dup)
summary(model.lm.country)

model.lm.city <- lm(Official.Time.1 ~ City, data = data15_1_Dup)
summary(model.lm.city)

class(data15_1_Dup$Age)
model.lm.age <- lm(Official.Time.1 ~ Age, data = data15_1_Dup)
summary(model.lm.age)

model.lm.gender <- lm(Official.Time.1 ~ M.F, data = data15_1_Dup)
summary(model.lm.gender)

typeof(data15_1$Official.Time)
mean_gender_final_time_F <- data15_1 %>% filter(M.F == "F")
mean(mean_gender_final_time_F$Official.Time.1)
var(mean_gender_final_time_F$Official.Time.1)

mean_gender_final_time_M <- data15_1 %>% filter(M.F == "M")
mean(mean_gender_final_time_M$Official.Time.1)
var(mean_gender_final_time_M$Official.Time.1)




library(FactoMineR)
PCA_15 <- PCA(data15_1[ ,c(27:36)])
cm_15 <- as.data.frame(MarathonCluster_15$centers)
cm_15["0"] <- c(0,0,0,0)

library(dplyr)
cm_15 <- cm_15 %>% select("0", everything())
cm_15



library(reshape2)
ClusterMean_15_1 <- as.data.frame(t(cm_15))


install.packages("data.table")
library(data.table)
ClusterMean_15_1 <- setDT(ClusterMean_15_1, keep.rownames = TRUE)
ClusterMean_15_1
ClusterMean_15_1$rn <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
ClusterMean_15_1_melted <- melt(ClusterMean_15_1, id.vars = "rn")
ClusterMean_15_1_melted
library(ggplot2)
ggplot(ClusterMean_15_1_melted, aes(rn, value, col = variable, group = variable)) + geom_point() + geom_line()
ggplot(ClusterMean_15_1_melted, aes(rn, value, col = variable, group = variable)) + geom_point() + stat_smooth()

data15_1 %>% ggplot(aes(x = Age)) + geom_bar()

