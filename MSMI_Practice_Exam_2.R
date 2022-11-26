library(ggplot2)
library(afex)
library(emmeans)
library(dplyr)
library(car)
library(corrplot)
#### Read in the data file and generate a list of the variables contained in the file----
exam2.df = read.csv('C:\\R\\week6\\exam2.csv', header = T)
str(exam2.df)
####  check the number of levels of each of these character variables.---- 
table(exam2.df$assemble_meals)
table(exam2.df$live_with_others)
table(exam2.df$gluten_free)
table(exam2.df$diet)
#### diet has 3 levels,declare each of these four variables as a ‘factor’ variable
exam2.df$assemble_meals <- factor(exam2.df$assemble_meals, levels=c("yes","no"), labels=c("yes","no"))
exam2.df$num_assemble_meals <- factor(exam2.df$assemble_meals, levels=c("yes","no"), labels=c(1,0))
exam2.df$num_assemble_meals <- as.numeric(as.character(exam2.df$num_assemble_meals))

exam2.df$live_with_others <- factor(exam2.df$live_with_others, levels=c("yes","no"), labels=c("yes","no"))
exam2.df$num_live_with_others <- factor(exam2.df$live_with_others, levels=c("yes","no"), labels=c(1,0))
exam2.df$num_live_with_others <- as.numeric(as.character(exam2.df$num_live_with_others))

exam2.df$gluten_free <- factor(exam2.df$gluten_free, levels=c("yes","no"), labels=c("yes","no"))
exam2.df$num_gluten_free <- factor(exam2.df$gluten_free, levels=c("yes","no"), labels=c(1,0))
exam2.df$num_gluten_free <- as.numeric(as.character(exam2.df$num_gluten_free))

exam2.df$diet <- factor(exam2.df$diet, levels=c("AllFoodTypes","Vegan", "Vegetarian"), labels=c("AllFoodTypes","Vegan", "Vegetarian"))

####Q1====
ggplot(exam2.df, aes(x = satisfaction, fill = diet )) + geom_histogram()

aov_oneway =aov_ez(id = "cust.id", 
                   dv = "satisfaction",
                   between = "diet",
                   data = exam2.df)
summary(aov_oneway)
aov_oneway

table(exam2.df$diet)
library(car)

leveneTest(satisfaction ~ diet, data = exam2.df)
#### Q2====
g = exam2.df$satisfaction
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "x-variable", main = "observed with normal overlay") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)
#### age----
g = exam2.df$age
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "x-variable", main = "observed with normal overlay") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)

age_model <- lm(satisfaction ~ age,data=exam2.df)
plot(satisfaction ~ age, data=exam2.df)
abline(age_model)

####annual_income----
g = exam2.df$annual_income
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "x-variable", main = "observed with normal overlay") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)

annual_income_model <- lm(satisfaction ~ annual_income,data=exam2.df)
plot(satisfaction ~ annual_income, data=exam2.df)
abline(annual_income_model)

####commute_time----
g = exam2.df$commute_time
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "x-variable", main = "observed with normal overlay") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)

commute_time_model <- lm(satisfaction ~ commute_time,data=exam2.df)
plot(satisfaction ~ commute_time, data=exam2.df)
abline(commute_time_model)

#####open_personality----

g = exam2.df$open_personality
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "x-variable", main = "observed with normal overlay") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)

open_personality_model <- lm(satisfaction ~ open_personality,data=exam2.df)
plot(satisfaction ~ open_personality, data=exam2.df)
abline(open_personality_model)

str(exam2.df)

corrplot(cor(exam2.df[ , c(6,7:13)]),method='ellipse',type = 'upper')

model1 <- lm(satisfaction ~ age + annual_income + commute_time + open_personality + num_assemble_meals + num_live_with_others + num_gluten_free + diet, data=exam2.df)

summary(model1)

model2 <- lm(satisfaction ~ age + annual_income + commute_time + open_personality, data=exam2.df)

summary(model2)

anova(model2,model1)
