#Script de Cyriac

library(readxl)
library(ggplot2)
library(dplyr)
library (CyCyFns)

#function to plot the output graph in the shiny app

#We apply the functions created by Ruoy on the whole dataset, and we get rid of the 1st 
#column that is "milestone km", that we already have in global avergage (redundant)
data_to_plot <- rbind(globalaverage(data_all),
                      top10percentmean(data_all),bottom20percentmean(data_all))

ggplot(data = (data_to_plot),
       aes(x = milestone_km, y = mean_time, color = Label)
) +
  geom_point() +
  geom_smooth() +
  labs(x = "Distance run", y = "Time since departure")



