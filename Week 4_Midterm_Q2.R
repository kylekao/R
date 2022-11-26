#Q2.Is there a relationship between time spent on Facebook and amount spent at the gift shop?
library("psych")
library("ggplot2")
exam2.df <- read.csv("C://R/msmi_exam1.csv",header = T)
str(exam2.df)
#timespentFB and dollarsspent are all numeric so "2 numeric-> correlation" 
ggplot(data=exam2.df,aes(x=timespentFB,y=dollarsspent))+geom_point() + geom_smooth(method=lm, se=FALSE)
corr.test(exam2.df$timespentFB, exam2.df$dollarsspent, method = "pearson", alpha =.05)
#the plot shows relationship between the time spent on Facebook AND amount spent at the museum gift shop, it looks like the time spent on Facebook and dollar spent at the museum gift shop comes with strong relationship.(scatter but close the the line)

#then do the correlation test, the probability is 0 means there is no correlation.The estimated correlation is 0.98(Correlation matrix) suggests a strong, positive association between the time spend on the Facebook and the amount spent at the museum gift shop.
