sat.df <- read.csv("http://goo.gl/HKnl74")
#######                   
str(sat.df)

library(lattice)
prop.table(table(sat.df$weekend))
barchart(prop.table(table(sat.df$weekend)))

hist(sat.df$num.child)
hist(sat.df$distance)
hist(sat.df$rides)
hist(sat.df$games)
hist(sat.df$wait)
hist(sat.df$clean)
####p25----
aggregate(sat.df$overall, by=list(sat.df$weekend),mean)
####p29----
lm(overall ~ weekend, data=sat.df)

sat.df$num_wkend    <- factor(sat.df$weekend,    levels=c("no","yes"), labels=c(0,1))
sat.df$num_wkend    <- as.numeric(as.character(sat.df$num_wkend))
####p31----
wkendmodel <- lm(overall ~ weekend, data=sat.df)
# to see all 4 plots in a single window, use par()
par(mfrow=c(2,2))
plot(wkendmodel)

####p32----
wkendmodel <- lm(overall ~ sat.df$weekend, data=sat.df)
summary(wkendmodel)

wkendmodel <- lm(overall ~ sat.df$weekend, data=sat.df)
summary(wkendmodel)
sat.df$fitted <- wkendmodel$fitted.values
tt <- cor(sat.df$overall,sat.df$fitted)
tt*tt
####p36----
g = sat.df$overall
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "x-variable", main = "observed with normal overlay") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)

shapiro.test(sat.df$overall)
####p39----
rides_model <- lm(overall~rides,data=sat.df)
plot(overall ~ rides, data=sat.df)
abline(rides_model)
####p40----
clean_model <- lm(overall~clean,data=sat.df)
plot(overall ~ clean, data=sat.df)
abline(clean_model)
####p40----
games_model <- lm(overall~games,data=sat.df)
plot(overall ~ games, data=sat.df)
abline(games_model)
####p41----
g = sat.df$distance
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "x-variable", main = "observed with normal overlay") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 
lines(xfit, yfit, col = "black", lwd = 2)

distance_model <- lm(overall~distance,data=sat.df)
plot(overall ~ distance, data=sat.df)
abline(distance_model)
####p42----
sat.df$logdistance <- log(sat.df$distance)

logdist_model <- lm(overall~logdistance,data=sat.df)
plot(overall ~ logdistance, data=sat.df)
abline(logdist_model)
####p44----
num.child_model <- lm(overall~num.child,data=sat.df)
plot(overall ~ num.child, data=sat.df)
abline(num.child_model)
####p45----
wait_model <- lm(overall~wait,data=sat.df)
plot(overall ~ wait, data=sat.df)
abline(wait_model)

####p46----
####install.packages("corrplot")
library(corrplot)
corrplot(cor(sat.df[ , c(8,2:7,9)]),method='ellipse',type = 'upper')

sat.df$num_wkend    <- factor(sat.df$weekend,    levels=c("no","yes"), labels=c(0,1))
sat.df$num_wkend    <- as.numeric(as.character(sat.df$num_wkend))
####p47----
corrplot(cor(sat.df[ , c(8,2:7,9)]),method='number',type = 'upper')

####p49----
model1 <- lm(overall ~ weekend + num.child + distance + rides + games + wait + clean, data=sat.df)
summary(model1)
####P50----
####1.Compare model estimates, especially the estimate for ‘clean
model2 <- lm(overall~ weekend + num.child + distance + games + wait + clean, data=sat.df)
summary(model2)
####p51----
####2.  Compare the adjusted R-squared values of model 1 and model 2
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
####P52----
anova(model2,model1)
####3.Perform a test of whether ‘rides’ contributes significantly to the model, above and beyond what the other variables contributed together:
####P54----
model3 <- lm(overall~ num.child + distance + rides + games + wait + clean, data=sat.df)
summary(model3)
anova(model3,model1)
####P55----
plot(sat.df$overall, fitted(model3),col='blue',
     xlim=c(0,100),ylim=c(0,100),
     xlab="observed overall satisfaction",
     ylab="fitted overall satisfaction")