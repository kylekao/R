#### Exam2_Qustion1
#### What is the estimated mean product satisfaction according to a customer's level of health consciousness? ----
#### Are there important differences in mean satisfaction between these groups ----
#### load libraries
library(ggplot2)
library(afex)
library(emmeans)
library(dplyr)
library(car)
library(corrplot)
####Read in the data file and generate a list of the variables contained in the file
final_exam2.df = read.csv('C:\\R\\data.csv', header = T)
str(final_exam2.df)
####check the number of levels of each of these character variables.
table(final_exam2.df$in_relationship)
table(final_exam2.df$health_conscious)
table(final_exam2.df$nut_free)
table(final_exam2.df$fruit_free)
#health_conscious got 3 levels

final_exam2.df$in_relationship <- factor(final_exam2.df$in_relationship, levels=c("yes","no"), labels = c("yes","no")) 
final_exam2.df$num_in_relationship <- factor(final_exam2.df$in_relationship, levels=c("yes","no"), labels = c("1","0")) 
final_exam2.df$num_in_relationship <- as.numeric(as.character(final_exam2.df$num_in_relationship))

final_exam2.df$nut_free <- factor(final_exam2.df$nut_free,levels=c("yes","no"), labels = c("yes","no"))
final_exam2.df$num_nut_free <- factor(final_exam2.df$nut_free,levels=c("yes","no"), labels = c("1","0"))
final_exam2.df$num_nut_free <- as.numeric(as.character(final_exam2.df$num_nut_free))

final_exam2.df$fruit_free <- factor(final_exam2.df$fruit_free,levels=c("yes","no"), labels = c("yes","no"))
final_exam2.df$num_fruit_free <- factor(final_exam2.df$fruit_free,levels=c("yes","no"), labels = c("1","0"))
final_exam2.df$num_fruit_free <- as.numeric(as.character(final_exam2.df$num_fruit_free))

final_exam2.df$health_conscious  <- factor(final_exam2.df$health_conscious, levels=c("high","medium", "low"), labels=c("high","medium", "low"))

####First, create a histogram that displays the product satisfaction according to each of the three diet types.
####Variables to analyze are health consciousness and product satisfaction
ggplot(final_exam2.df, aes(x= product_satisfaction , fill = health_conscious))+ geom_histogram()
####Now, perform the ANOVA
aov_oneway =aov_ez(id = "cust.id", 
                   dv = "product_satisfaction",
                   between = "health_conscious",
                   data = final_exam2.df)
summary(aov_oneway)
aov_oneway
#We see that the p-value for the F statistic is smaller than a significance level of .05
#This indicates that the test result is significant.
#Thus, reject the null hypothesis.
#The test result suggests that there are differences in mean satisfaction between the three health conscious groups.

#second, check a few things before we deliver the summary to our client.
table(final_exam2.df$health_conscious)
#The sample size of each diet groups is greater than 30, so we don’t need to worry about testing the assumption of normality.
#est the assumption of homogeneity of variance.
#We’ll use Levene’s test for this. Levene’s test can be carried out using the ‘car’ package for this test of homogeneity of variance.
#The null hypothesis is that the variances are equal in the respective populations
#The alternative hypothesis is that at least one variance in the respective populations differs from the others

library(car)
leveneTest(product_satisfaction ~ health_conscious, data = final_exam2.df)
#The test statistic for Levene’s test is significant, and reject the null hypothesis
#Executive Summary
#The three consumer groups based on the three health conscious types do differ on average in their product satisfaction.