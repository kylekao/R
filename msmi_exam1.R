#Q1.Is there a relationship between the amount spent at the museum gift shop and #satisfaction with the visit to the museum?
library("psych")
library("ggplot2")
exam1.df <- read.csv("C://R/msmi_exam1.csv",header = T)
str(exam1.df)
#dollarsspent and satisfaction are all numeric so "2 numeric-> correlation" 
ggplot(data=exam1.df,aes(x=dollarsspent,y=satisfaction))+geom_point() + geom_smooth(method=lm, se=FALSE)
corr.test(exam1.df$dollarsspent, exam1.df$satisfaction, method = "pearson", alpha =.05)
#the plot shows relationship between the amount spent at the museum gift shop and satisfaction with the visit to the museum, it looks like the more dollar spent at the museum gift shop comes with better satisfaction with the visit to the museum.

#then do the correlation test, the probability is 0 means there is no correlation.The estimated correlation is 0.92 suggests a strong, positive association between the amount spent at the museum gift shop and satisfaction with the visit to the museum.