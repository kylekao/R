## one-way ANOVA to compare the means of a numeric variable between groups####
library(afex)
library(emmeans)
library(dplyr)
library(ggplot2)

dat1 = read.csv('C:\\R\\aov_data_oneway.csv', header = T)

head(dat1, 10)
aov_oneway =aov_ez(id = "id", 
                   dv = "brand_loyalty",
                   between = "group",
                   data = dat1)
aov_oneway
oneway_emm <- emmeans(object = aov_oneway,
                      spec = "group")
pairs(oneway_emm, adjust = "tukey") # adjust = "tukey" is the default
afex_plot(
  object = aov_oneway, # our anova object
  x = "group" # appears on the x axis
)

