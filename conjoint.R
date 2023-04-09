#Task:You are the marketing manager for Acme Espresso Machine Small manufacturer of premium coffee makers Competitors: Breville, DeLonghi, Gaggia, Rancilio
## Step1. Attribute Selection(Select relevant product characteristics)	-> Speed, Capacity, Price
## Step2. Attribute Level Selection(Identify levels of characteristics)	-> Speed: S1-> Fast S2->Slow, Capacity: C1-> Single-Cup C2-> Multi-Cup, Price: P1-> Budget P2-> Premium
## Step3. Product Bundle Formation(Create sample products by bundling)  -> 2levels ^ 3 Arttibutes = 8
## Step4. Data Collection Method Selection(Decide how to collect data)  -> Pairwise Comparison | Rank Ordering | Rating Scale
## Step5. Data Collection(Ask responders to rate products)              -> Record Likert Scale respondences in to preference
## Step6. Data Preparation(Get data ready for conjoint)                 -> loading data into CSV file(Preferences, Profiles) and install conjoint package in R

library(conjoint)
prefer <- read.csv("/Users/kyle/preferences.csv",header = T)
prof <- read.csv("/Users/kyle/profiles.csv",header = T)

## Step7. Importance Weights Calculation(Assess importance of each attribute)
imp <- caImportance (prefer, prof)
print("Importance summary: ", quote=FALSE)
print(imp)

## Step8. Part-Worth Calculation(Assess importance of each level)
partntil <- caPartUtilities(preferences,profiles,levelnames)
print(partntil)

## Step9. Market Share Estimattion(Predict preferences of products)
simul <- read.csv("/Users/kyle/profiles.csv",header = T)
maxutil <- caMaxUtility(simul,preferences,profiles)
print(maxutil)
## Step9. Market Share Estimation(Bradley-Terry-Luce (BTL))
btl <- caBTL(simul, preferences, profiles)
print(btl)
## Step10. Data Interpretation
### Findings
#### From the part worth calculations, we note that speed was the most important attribute, followed by price as the second most important. Capacity was third. 

### Conclusion
#### We also interpret the result by Part Worth Calculations and Market Share Estimation that 
#### card 3 (fast, multi-cup, and higher price) and card 1 (fast. Single-cup, and lower price) covered all the preference. This means speed of coffee maker machines dominate the market.

### Recommendations
#### In summary, the interpretation results in 3 recommendations:
#### 1.Develop card3 and card1 to maximize sales
#### 2.Emphasize the speed of the machine in marketing messages
#### 3.Emphasize the value that the machine delivers, and how its price represents a bargain.

### Reference



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
