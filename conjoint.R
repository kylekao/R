library(robustbase)
library(ggplot2)
library(dlstats)
library(conjoint)
prefer <- read.csv("/Users/kyle/preferences.csv",header = T)
prof <- read.csv("/Users/kyle/profiles.csv",header = T)
imp <- caImportance (prefer, prof)
print("Importance summary: ", quote=FALSE)
print(imp)


#### Example
####Conjoint Analysis - Samsung
#library(conjoint)
#### Loading in the data
#data(tea)
#str(tprof)
#expand.grid(price=1:3, variety=1:3, kind=1:3, aroma=1:3)

#head(tprof) #Matrix of profiles (4 attributes and 13 profiles).
#head(tpref) #Vector of preferences (length 1300).
#head(tprefm) #Matrix of preferences (100 respondents and 13 profiles).
#head(tlevn) #Character vector of names for the attributes' levels.
#head(tsimp) #Matrix of simulation profiles.
#dim(tprof)
#tprefm[1,]
#caModel(y=tprefm[1,], x=tprof)
#caUtilities(y=tprefm[1,], x=tprof, z=tlevn)
#caImportance(y=tpref, x=tprof)

####Utility for first 10 customers
#caPartUtilities(y=tprefm[1:10,], x=tprof, z=tlevn)

#Conjoint(y=tpref, x=tprof, z=tlevn)

#caSegmentation(y=tpref, x=tprof, c=3)
#clusters=caSegmentation(y=tpref, x=tprof, c=3)
#clusters

####library(cluster)
#plot(clusters)
#### More complex
#clusplot(tprof, clusters$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#ShowAllSimulations(sym=tsimp, y=tpref, x=tprof)
