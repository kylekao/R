store.df <- read.csv("http://goo.gl/QPDdMl")
k.stores <- 20 
k.weeks <- 104
store.df <- data.frame(matrix(NA , ncol =10, nrow=k.stores*k.weeks))
names(store.df) <- c("storeNum","Year","Week","p1sales","p2sales","p1price",
                     "p2price", "p1prom" , "p2prom" , "country")
dim(store.df)
store.num <- 101:(100+k.stores)
(store.cty <- c(rep("US",3), rep("DE",5), rep("GB",3), rep("BR",2),rep("JP", 4),
                rep("AU" , 1) , rep("CN" , 2)))
length(store.cty)
store.df$storeNum <- rep(store.num , each=k.weeks)
store.df$country <- rep(store.cty , each=k.weeks)
#rm(store.num , store.cty)
(store.df$Week <- rep (1:52 , times=k.stores*2))
(store.df$Year <- rep(rep (1:2 , each=k.weeks/2) , times=k.stores))
str(store.df)
store.df$storeNum <- factor(store.df$storeNum)
store.df$country <- factor(store.df$country)
str(store.df)
head(store.df)
head(store.df,120)
tail(store.df,120)
set.seed (98250)
store.df$p1prom <- rbinom(n=nrow(store.df) , size=1, p=0.1)
store.df$p2prom <- rbinom(n=nrow(store.df) , size=1, p=0.15)
head(store.df)
store.df$p1price <- sample(x=c(2.19 , 2.29 , 2.49 , 2.79 , 2.99),
                           size=nrow(store.df) , replace=TRUE)
store.df$p2price <- sample(x=c(2.29 , 2.49 , 2.59 , 2.99 , 3.19),
                           size=nrow(store.df) , replace=TRUE)
head(store.df)
tmp.sales1 <- rpois(nrow(store.df), lambda =120)
tmp.sales2 <- rpois(nrow(store.df), lambda =100)
tmp.sales1 <- tmp.sales1 *log(store.df$p2price) / log(store.df$p1price)
tmp.sales2 <- tmp.sales2 *log(store.df$p1price) / log(store.df$p2price)
store.df$p1sales <- floor(tmp.sales1 * (1 + store.df$p1prom*0.3))
store.df$p2sales <- floor(tmp.sales2 * (1 + store.df$p2prom*0.4))
head(store.df)
#install.packages("car")
library(car)
some(store.df , 10)
#3.2
table(store.df$p1price)
p1.table <- table(store.df$p1price)
p1.table
str(p1.table)
plot(p1.table)
table(store.df$p1price,store.df$p1prom)
p1.table2 <- table(store.df$p1price, store.df$p1prom)
p1.table2[, 2]/(p1.table2[, 1]+ p1.table2[, 2])
min(store.df$p1sales)
max(store.df$p2sales)
mean(store.df$p1prom)
median(store.df$p2sales)
var(store.df$p1sales)
sd(store.df$p1sales)
IQR(store.df$p1sales)
mad(store.df$p1sales)
quantile(store.df$p1sales , probs=c(0.25 , 0.5 , 0.75))
quantile(store.df$p1sales , probs=c(0.05 , 0.95))
quantile(store.df$p1sales , probs =0:10/10)
mysummary.df <- data.frame(matrix(NA , nrow =2, ncol =2))
names(mysummary.df) <- c("Median Sales" , "IQR")
rownames(mysummary.df) <- c("Product 1" , "Product 2")
mysummary.df["Product 1" , "Median Sales"] <- median(store.df$p1sales)
mysummary.df["Product 2" , "Median Sales"] <- median(store.df$p2sales)
mysummary.df["Product 1" , "IQR"] <- IQR(store.df$p1sales)
mysummary.df["Product 2" , "IQR"] <- IQR(store.df$p2sales)
mysummary.df
#3.3
summary(store.df)
#install.packages("psych")
library(psych)
describe(store.df)
describe(store.df[ , c(2 , 4:9)])
apply(store.df[ ,2:9], MARGIN =2, FUN=mean)
apply(store.df[ ,2:9] , 1 , mean)
apply(store.df[ ,2:9] , 2 , sum)
apply(store.df[ ,2:9] , 2 , sd)
apply(store.df[ ,2:9] , 2 , function(x) { mean(x) - median(x) } )
mysummary2.df <- data.frame(matrix(NA , nrow=2, ncol =2))
names(mysummary2.df) <- c("Median Sales" , "IQR")
rownames(mysummary2.df) <- names(store.df)[4:5] # names from the data frame
mysummary2.df[, "Median Sales"] <- apply(store.df[ , 4:5] , 2 , median)
mysummary2.df[, "IQR"] <- apply(store.df[ , 4:5] , 2 , IQR)
mysummary2.df
#3.4
hist(store.df$p1sales)
hist(store.df$p1sales ,main="Product 1 Weekly Sales Frequencies , All Stores",xlab="Product 1 Sales ( Units)",ylab="Count" )
hist(store.df$p1sales ,main="Product 1 Weekly Sales Frequencies , All Stores",xlab="Product 1 Sales ( Units)",ylab="Count",breaks =30 ,col="lightblue")
hist(store.df$p1sales ,main="Product 1 Weekly Sales Frequencies , All Stores",xlab="Product 1 Sales ( Units)",ylab="Relative frequency", breaks =30,col="lightblue",freq=FALSE ,xaxt="n")
axis(side=1, at=seq (60 , 300 , by =20))
lines(density(store.df$p1sales , bw =10) ,type="l" , col="darkred" , lwd =2)
boxplot(store.df$p2sales , xlab="Weekly sales" , ylab="P2",main="Weekly sales of P2 , All stores" , horizontal=TRUE)
boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE, ylab="Store", xlab="Weekly unit sales", las=1,main="Weekly Sales of P2 by Store")
boxplot(p2sales ~ p2prom , data=store.df,horizontal=TRUE,yaxt="n",ylab="P2 promoted in store?" , xlab="Weekly sales",main="Weekly sales of P2 with and without promotion")
axis(side=2, at=c(1,2) , labels=c("No", "Yes"))

#install.packages("beanplot")
library(beanplot)
beanplot(p2sales ~ p2prom , data=store.df , horizontal=TRUE , yaxt="n",
         what=c(0,1,1,0) , log="" , side="second",
         ylab="P2 promoted in store?" , xlab="Weekly sales",
         main="Weekly sales of P2 with and without promotion")
axis(side=2, at=c(1,2) , labels=c("No", "Yes"))
qqnorm(store.df$p1sales)
qqline(store.df$p1sales)

qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))

#my.object <- FUNCTION(my.data)
#plot(my.object)
#plot(FUNCTION(my.data))
plot(ecdf(store.df$p1sales),
     main="Cumulative distribution of P1 Weekly Sales",
     ylab="Cumulative Proportion",
     xlab=c("P1 weekly sales , all stores" , "90% of weeks sold <= 171 units"
     ),
      yaxt="n")
axis(side=2, at=seq(0, 1, by =0.1) , las=1,
     labels=paste(seq(0,100,by =10) , "%" , sep=""))
abline(h=0.9 , lty =3) 
abline(v=quantile(store.df$p1sales , pr =0.9) , lty =3)

by(store.df$p1sales , store.df$storeNum , mean)
by(store.df$p1sales , list(store.df$storeNum , store.df$Year) , mean)
aggregate(store.df$p1sales , by=list(country=store.df$country) , sum)
p1sales.sum <- aggregate(store.df$p1sales ,
                         by=list(country=store.df$country) , sum)
p1sales.sum

p1sales.sum <- aggregate(store.df$p1sales ,
                         by=list(country=store.df$country) , sum)
#install.packages(c("rworldmap" , "RColorBrewer"))
library(rworldmap)
library(RColorBrewer)
p1sales.map <- joinCountryData2Map(p1sales.sum , joinCode = "ISO2",
                                   nameJoinColumn = "country")
mapCountryData(p1sales.map , nameColumnToPlot="x",
               mapTitle="Total P1 sales by Country",
               colourPalette=brewer.pal(7, "Greens"),
               catMethod="fixedWidth" , addLegend=FALSE)
