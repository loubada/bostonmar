library(dplyr)
library(ggplot2)

d2015<-data_2015 %>% select(-X, -X.1) %>% mutate(Year = 2015)
d2016<-data_2016 %>% select(-X) %>% mutate(Year = 2016)
d2017<-data_2017 %>% select(-X, -X.1) %>% mutate(Year = 2017)


#2015 
summary(d2015)
q1_2015 <- quantile(d2015$Age, 0.25)
q2_2015 <- quantile(d2015$Age, 0.5)
q3_2015 <- quantile(d2015$Age, 0.75)

youngest_2015 <- d2015 %>% filter(Age <= q1_2016) 
younger_2015 <- d2015 %>% filter(Age <= q1_2016) 

#2016 
summary(d2016)
q1_2016 <- quantile(d2016$Age, 0.25)
q2_2016 <- quantile(d2016$Age, 0.5)
q3_2016 <- quantile(d2016$Age, 0.75)

youngest_2016 <- d2016 %>% filter(Age <= q1_2016) 

#2017 
summary(d2015)
q1_2017 <- quantile(d2017$Age, 0.25)
q2_2017 <- quantile(d2017$Age, 0.5)
q3_2017 <- quantile(d2017$Age, 0.75)

youngest_2017<- d2017 %>% filter(Age <= q1_2017)

#combination
#youngest <- combine(youngest_2015, youngest_2016, youngest_2017)
youngest2 <- bind_rows(youngest_2015, youngest_2016, youngest_2017)
summary(yousummngest)


#test
all <- bind_rows(d2015, d2016, d2017)
dim(d2015 %>% filter(Country == "FRA"))
dim(d2016 %>% filter(Country == "FRA"))
dim(d2017 %>% filter(Country == "FRA"))


ggplot(data = all,
       aes(x = Age)) + geom_bar()
