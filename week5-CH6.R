seg.df <- read.csv("C:/R/rintro-chapter5.csv",header = T)  
summary(seg.df)

###1-6.2
tmp.tab <- table(rep(c(1:4), times=c(25,25,25,20)))
tmp.tab
chisq.test(tmp.tab)

tmp.tab <- table(rep(c(1:4), times=c(25,25,25,10)))
tmp.tab
chisq.test(tmp.tab)

tmp.tab <- tmp.tab/5
tmp.tab
chisq.test(tmp.tab)

###one way chi-square
chisq.test(table(seg.df$Segment)) 

###two-way chi-square
table(seg.df$subscribe, seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))

###two-way chi-square without correction (matches traditional formula)
chisq.test(table(seg.df$subscribe, seg.df$ownHome), correct=FALSE)

###two-way with simulation to establish p value
chisq.test(table(seg.df$subscribe, seg.df$ownHome), sim=TRUE, B=10000)


###6.3binomial
binom.test(12, 20, p=0.5)
binom.test(120, 200, p=0.5)

sum(dbinom(8:12, 20, 0.5))
###agresti-coull might be more applicable for small N
library(binom)
binom.confint(12, 20, method="ac")

binom.confint(0, 20, method="ac")


###2-6.4 t-test
###2-a plot-one for all consumers together
hist(seg.df$income)
###plot-only home owners
with(seg.df, hist(income[ownHome=="ownYes"]))
###plot-do not own a home
with(seg.df, hist(income[ownHome=="ownNo"]))
###Do you think the sample data support the assumption that income values are normally distributed in the populations?
###YES

###2-b
t.test(income ~ ownHome, data=seg.df)
###Interpret the results
###
###2-c
t.test(income ~ ownHome, data=subset(seg.df, Segment=="Travelers"))
###Interpret the results

###3-6.5 ANOVA
seg.aov.own <- aov(income ~ ownHome, data=seg.df)
anova(seg.aov.own)

seg.aov.seg <- aov(income ~ Segment, data=seg.df)
anova(seg.aov.seg)

# two-way aov
anova(aov(income ~ Segment + ownHome, data=seg.df))

anova(aov(income ~ Segment * ownHome, data=seg.df))

### 6.5.1 compare models
anova(aov(income ~ Segment, data=seg.df),
      aov(income ~ Segment + ownHome, data=seg.df))


### 6.5.2 Visualize ANOVA group means
library(multcomp)
# create an aov model
seg.aov <- aov(income ~ Segment, data=seg.df)
glht(seg.aov)

# make new AOV without intercept
seg.aov <- aov(income ~ -1 + Segment, data=seg.df)

glht(seg.aov)

par(mar=c(6,10,2,2))   # adjusts margins to preserve axis labels
plot(glht(seg.aov), 
     xlab="Income", main="Average Income by Segment (95% CI)")


###6.5.3 Stepwise ANOVA
seg.aov.step <- step(aov(income ~ ., data=seg.df))
anova(seg.aov.step)

### clean up -- not shown in book
### rm(seg.aov.own, seg.aov.seg, seg.aov.step, seg.aov)


#### 6.6.2 Bayesian ANOVA
set.seed(96761)
library(BayesFactor)
seg.bf1 <- lmBF(income ~ Segment, data=seg.df)
seg.bf2 <- lmBF(income ~ Segment + ownHome, data=seg.df)
seg.bf1 / seg.bf2

seg.bf.chain <- posterior(seg.bf1, 1, iterations = 10000)

### plot the trace for posterior draw chain
plot(seg.bf.chain[ , 1:6])   #may need to hit <Return> to see all

###6.6.3 inspecting posterio draws
summary(seg.bf.chain)

head(seg.bf.chain)

seg.bf.chain[1:4, 1:5]
seg.bf.chain[1:4, 2:5] + seg.bf.chain[1:4, 1]

seg.bf.chain.total <- seg.bf.chain[, 2:5] + seg.bf.chain[, 1]
seg.bf.ci <- t(apply(seg.bf.chain.total, 2, 
                     quantile, pr=c(0.025, 0.5, 0.975)))
seg.bf.ci


###6.6.4 Bayesian Credible Intervals
library(ggplot2)
seg.bf.df <- data.frame(seg.bf.ci)
seg.bf.df$Segment <- rownames(seg.bf.df)

### basic plot with segment, 50% and 95% limits
p <- ggplot(seg.bf.df, aes(x=Segment, y=X50., ymax=X97.5., ymin=X2.5.))
### add the points and error bars
p <- p + geom_point(size=4) + geom_errorbar(width=0.2) + ylab("Income")

### plot it, adding a title, and flipping to horizontal

p + ggtitle("95% CI for Mean Income by Segment") + coord_flip()