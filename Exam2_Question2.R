#### Exam2_Question2
#### The client wants to understand what variables are related to product satisfaction.---- 
#### Which variables are related to product satisfaction and which are not?----
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

#First, examine the distribution of product satisfaction across to see if there are any outliers.----

g = final_exam2.df$product_satisfaction
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "x-variable", main = "observed with normal overlay") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)
#The distribution for product Satisfaction looks fairly symmetric and I don’t see any unusual values in either tail.

#next, take look at the numeric variables that we will test to see if they are related to product satisfaction. Let’s display each with a histogram and then in a scatterplot with satisfaction.----
#age
g = final_exam2.df$age
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "x-variable", main = "observed with normal overlay") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)

age_model <- lm(product_satisfaction ~ age,data=final_exam2.df)
plot(product_satisfaction ~ age, data=final_exam2.df)
abline(age_model)
#annual_income
g = final_exam2.df$annual_income
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "x-variable", main = "observed with normal overlay") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)

annual_income_model <- lm(product_satisfaction ~ annual_income,data=final_exam2.df)
plot(product_satisfaction ~ annual_income, data=final_exam2.df)
abline(annual_income_model)


g = final_exam2.df$conscientiousness_personality
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "x-variable", main = "observed with normal overlay") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)

conscientiousness_personality_model <- lm(product_satisfaction ~ conscientiousness_personality,data=final_exam2.df)
plot(product_satisfaction ~ conscientiousness_personality, data=final_exam2.df)
abline(conscientiousness_personality_model)

str(final_exam2.df)
corrplot(cor(final_exam2.df[,c(6,7:13)]), method = 'ellipse',type = 'upper')

model1 <- lm(product_satisfaction ~ age + annual_income + conscientiousness_personality + log_relationship_length + num_in_relationship + num_nut_free + num_fruit_free, data = final_exam2.df)
summary(model1)

#We see that the p-values for the t statistics for age, annual_income, conscientiousness_personality, num_in_relationship, and num_nut_free are less than a significance level of .05.
#So, for these 5 explanatory variables, we reject the null hypothesis and conclude that each relates to satisfaction ----

#We see that the p-values for the t statistics for log_relationship_length and num_fruit_free are greater than a significance level of .05.
#So, for these 2 explanatory variables, we fail to reject the null hypothesis and conclude that none of these variables relate to product satisfaction

#Let’s now see if we can simplify the model by dropping the 4 variables that were not statistically significant. We’ll compare the fit of the reduced model (I’ll call this reduced model ‘Model 2’) to the full model.
model2 <- lm(product_satisfaction ~ age + annual_income + conscientiousness_personality + num_in_relationship + num_nut_free , data = final_exam2.df)
summary(model2)

anova(model2,model1)
#From the anova test used to compare the fit of these two models, we see that the test statistic has a p value that is greater than .05, so the fit of the full model is not statistically different from the fit of the reduced model. In fact, the adjusted R squared value is still at about 18%.
#we want to convey to our client that Product satisfaction is related to their level of being health conscious .whether the length of the relationship, whether or not the person is in relationship ,whether they prefer nut-free chocolate, or whether they prefer fruit-free chocolate are not related to product satisfaction.
####Executive Summary====
#Product satisfaction is related to level of being health conscious.