#Business problem: 
#You’ve been hired by a local merchant in San Francisco who has a store front 
#that offers casual sandwiches, salads and soft drinks. They need your help 
#in providing insight into current customer satisfaction. 
#The client hired a survey research company to collect data, 
#and they need your expertise in testing specific questions to address their 
#current business strategies. The client is not well versed in statistical methods, 
#so it is your job to translate their business questions into a statistical analysis 
#by selecting an appropriate statistical procedure to evaluate their business questions. 
#They also need you provide interpretations for each question. Understanding the 
#client’s lack of statistical prowess, you need to generate deliverables that the 
#client can easily understand. 

#2 numeric-> correlation  #2 categories -> Chi square #1 numeric+ 1 category--> t test
#alpha = .05
#p-value <= .05 ---reject #p-value >.05 ---accept 
setwd("C://R")
library("psych")
library("ggplot2")
exam1.df <- read.csv("C://R/practiceexam1.csv",header = T)
str(exam1.df)
head(exam1.df)

#to arrange detaframe

exam1.df$bring.dog <- factor(exam1.df$bring.dog, 
                             levels=c("yesdog","nodog"), 
                             labels=c("yesdog","nodog"))

exam1.df$purchase.drink <- factor(exam1.df$purchase.drink,    
                                  levels=c("yesdrink","nodrink"), 
                                  labels=c("yesdrink","nodrink"))

exam1.df$pay.method <- factor(exam1.df$pay.method, 
                              levels=c("online","creditcard","AppleAndroidPay","cash"),
                              labels=c("online","creditcard","AppleAndroidPay","cash" ))

#Q1.	Are customers who bring a dog into the shop also likely to purchase a soft drink?
#chi square
dog_drink <- table(exam1.df$bring.dog,exam1.df$purchase.drink)
dog_drink
dog_drink_chi <- chisq.test(exam1.df$bring.dog, exam1.df$purchase.drink)
dog_drink_chi
#X-squared = 24.305, df = 1, p-value = 8.223e-07
dog_drink_chi$observed
dog_drink_chi$expected

#there is a tendency for those who bring a dog into the shop to also purchase a drink.
#Executive summary
#Customers who bring a dog into the shop are also likely to purchase a soft drink
#Perhaps we could recommend that the client consider a promotion to encourage 
#customers to bring in their dogs to help increase drink purchases.

#Q2.	For customers paying with cash, are those who bring a dog into the shop also likely to purchase a soft drink?
#TODO:get"cash","yesdog"and"yesdrink"in a subset
#chi square
cash_dog_drink = subset(exam1.df,
                        (pay.method=="cash"),
                        select = c("bring.dog","purchase.drink"))

cash_dog_drink_chi <- chisq.test(cash_dog_drink$bring.dog, cash_dog_drink$purchase.drink)
cash_dog_drink_chi
cash_dog_drink_chi$expected

cash_dog_drink_fish <- fisher.test(cash_dog_drink$bring.dog, cash_dog_drink$purchase.drink)
cash_dog_drink_fish

##This test yields p-value = 0.0612
##The p-value < alpha so do not reject the null hypothesis
##Executive summary
##For customers who pay with cash, those who bring a dog into the shop are not also likely to purchase a soft drink
##Any promotion that encourages customers to bring in their dogs (to help increase drink purchases) is not likely to be useful for customers paying with cash.

#Q3.	Is there a relationship between age and total monthly dollars spent at the shop?
#correlation
ggplot(data=exam1.df,aes(x=age,y=monthly.spend))+geom_point() + geom_smooth(method=lm, se=FALSE)
corr.test(exam1.df$age, exam1.df$monthly.spend, method = "pearson", alpha =.05)
##The estimated correlation is -.01. This is not surprising given the scatterplot produced above suggested little or no relationship between the two variables.
##The p-value is 0.81. From this, we cannot reject the null hypothesis. We conclude that there is no relationship between the two variables.
##Executive summary
##There is no relationship between age and the monthly amount spent at the shop. Any promotion to encourage customers to spend more money at the shop need not take the age of the customer into account.0

#Q4.	For customers paying with a credit card, is there a relationship between age and total monthly dollars spent at the shop?
#TODO:get"credit card","age"and"monthly.spend"in a subset
#correlation
exam1.df <- read.csv("C://R/practiceexam1.csv",header = T)
credit_age_ttm<- subset(exam1.df,(pay.method == "credit card"),)
head(credit_age_ttm)
ggplot(data=credit_age_ttm,aes(x=age,y=monthly.spend))+geom_point() + geom_smooth(method=lm, se=FALSE)
#str(credit_age_ttm)
#as.numeric(credit_age_ttm$age)
#plot(x=credit_age_ttm$age,y=credit_age_ttm$monthly.spend)
corr.test(x=credit_age_ttm$age, y=credit_age_ttm$monthly.spend, method="pearson", alpha=.05)
#-1 strong neg cor
#+1 strong pos cor
#0 non cor

#Q5.	What is the estimated mean satisfaction level according to whether or not a customer brings a dog into the store? 
#Is there an important difference in mean satisfaction between these groups?
exam1.df <- read.csv("C://R/practiceexam1.csv",header = T)
ggplot(exam1.df, aes(x = sat.overall, fill = bring.dog)) + geom_histogram()
n_dog <-  table(exam1.df$bring.dog)
n_dog              

with(exam1.df, shapiro.test(sat.overall[bring.dog == "yesdog"]))
with(exam1.df, shapiro.test(sat.overall[bring.dog == "nodog"]))

ttest_satoverall <-t.test(sat.overall ~ bring.dog, data=exam1.df, var.equal=FALSE)
print(ttest_satoverall)
#The p-value is > alpha so we do not reject the null hypothesis. We conclude that there is no difference in mean overall 
#satisfaction between those who bring in a dog and those who do not.

#Executive summary
#Customer satisfaction is not related to whether or not a customer brings a dog into the shop. Any efforts to improve 
#customer satisfaction need not take into account whether or not a customer brings a dog to the shop.


#Q6.	What is the estimated mean distance to the shop according to whether a customer brings a dog into the shop or not?  
#Is there an important mean difference in distance to the shop between these groups?
#t test
exam1.df <- read.csv("C://R/practiceexam1.csv",header = T)
ggplot(exam1.df, aes(x = distance, fill = bring.dog)) + geom_histogram()
n_bringdog <- table(exam1.df$bring.dog)
n_bringdog
with(exam1.df, shapiro.test(distance[bring.dog == "yesdog"]))
with(exam1.df, shapiro.test(distance[bring.dog == "nodog"]))

distance_dog_t <- t.test (distance ~ bring.dog, data=exam1.df, var.equal=FALSE)
distance_dog_t
#From the test, t = -0.59876, df = 178.09, p-value = 0.5501.
#The p-value is > alpha so we do not reject the null hypothesis. We conclude that there is no difference in mean distance 
#between those who bring in a dog and those who do not.

#Executive summary
#Distance is not related to whether or not a customer brings a dog into the shop. This is a bit surprising because one might 
#speculate that those who travel longer distances to the shop may be less likely to bring a dog, but this is not the case.


#Q7.	Is there a relationship between age and distance to the shop?
#correlation
ggplot(data=exam1.df, aes(x=age, y=distance)) + geom_point() + geom_smooth(method=lm, se=FALSE)
corr.test(exam1.df$age,exam1.df$distance, method = "pearson", alpha=.05)

#The estimated correlation is 0. This is not surprising given the scatterplot produced above suggested little or no relationship 
#between the two variables.
#The p-value is 0.96. From this, we cannot reject the null hypothesis. We conclude that there is no relationship between the 
#two variables.
#Executive summary
#There is no relationship between age and the distance to the shop. Any concerns to address the distance customers travel to 
#the shop need not take into account the age of the customer.
