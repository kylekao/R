library(ggplot2)
library(afex)
library(emmeans)
library(dplyr)
library(car)
library(corrplot)
library(psych)
library(palmerpenguins)
netflix.df = read.csv('/Users/kyle/Downloads/net.csv', header = T)
str(netflix.df)

modelnet1 <- lm(sbsci ~ GDP + Inflat + OTT + HBO + Hulu + Basic + Stand + Prem + Award + OriCnt + CntSpn + MktExp, data = netflix.df)
summary(modelnet1)

model2 <- lm(sbsci ~ OTT + MktExp, data=netflix.df)
summary(model2)

anova(modelnet1, model2)
result1 <- cor.test(netflix.df$sbsci,netflix.df$OTT, method = "pearson")
result2 <- cor.test(netflix.df$sbsci,netflix.df$MktExp, method = "pearson")
result1
result2

corrplot(cor(netflix.df[,c(2,3:13)]),method = 'ellipse', type = 'upper')
corrplot(cor(netflix.df[,c(2,5,13)]),method = 'ellipse', type = 'upper')
