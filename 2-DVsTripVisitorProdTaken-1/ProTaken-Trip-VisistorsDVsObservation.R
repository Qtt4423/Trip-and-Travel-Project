#Load Libraries
install.packages("Hmisc")
install.packages("ggstatsplot")

library("car")
library("dplyr")
library("effects")
library("Hmisc")
library("rcompanion")
library(tidyr)
library("mvnormtest")
library("multcomp")
library ("IDPmisc")
library("psych")
library(ggstatsplot)




#Explore data:
head(Travel)

#DV1= NumberOfPersonVisiting
#DV2 = NumberOfTrips

#Data Wrangling: 


##Creating Subset with all customers who bought the Products.
#remove missing data
TravilNA <- na.omit (Travel)

#Check value for ProdTaken
table(TravelNA$ProdTaken)
### only 797 customers bought product, 3331 customers did not.

table(TravelNA$CityTier)


#Subset data select only value of ProdTaken = 1
TravelProd <- subset(Travel, ProdTaken == 1)

#Check value for NumberOfTrips
table(TravelProd$NumberOfTrips)
### Since there is only 1 value of 19 and 20 trips, we are going to remove these values.

## Remove outliers 
ProdTrip <- subset(TravelProd, NumberOfTrips <= 10)
ProdTrip <- na.omit (ProdTrip)
#Check value for NumberOfTrips
table(ProdTrip$NumberOfTrips)
head(ProdTrip)

# Converting categorical variables into numeric values
TravelConvert <- ProdTrip
TravelConvert[sapply(TravelConvert, is.factor)] <- data.matrix(TravelConvert[sapply(TravelConvert, is.factor)])

# Correlation Matrix
TravelProdMatrix <- rcorr(as.matrix(TravelConvert))
TravelProdMatrix

## Matrix without ProTaken
ProdDrop <- subset(ProdTrip, select = -c(ProdTaken))
#Removing NA values:
ProdDropNA <- na.omit (ProdDrop)

# Converting categorical variables into numeric values
ProdConvert <- ProdDropNA
ProdConvert[sapply(ProdConvert, is.factor)] <- data.matrix(ProdConvert[sapply(ProdConvert, is.factor)])

# Correlation Matrix
Prodmatrix <- cor(ProdConvert)
View(round(Prodmatrix, 2))

chart.Correlation(Prodmatrix, histogram=FALSE, method="pearson")



#Categorical IVs correlated with both DVs: TypeofContact, CityTier, Gender, MaritalStatus, Passport, OwnCar, Designation 
#Continuous IVs correlated with both DVs:PitchSatisfactionScore
#IVs correlated with NumberofTrips only: DurationPitch and PrefferedPropertyStar
#IVs correlated with NumberOfPersonVisiting only is Occupation

#Exam means of the above variable:

#Group by Type of Contact
TypeofContactM <- ProdTrip %>% group_by(TypeofContact) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
TypeofContactM
ggplot(ProdTrip, aes(x = factor(TypeofContact), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Type of Contact") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Type of Contact vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(TypeofContact), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Type of Contact") + 
  ylab("Number Of Trips") + 
  ggtitle("Type of Contact vs. Number Of Trips")


#Group by CityTier
CityTierM <- ProdTrip %>% group_by(CityTier) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
CityTierM

ggplot(ProdTrip, aes(x = factor(CityTier), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("City Tier") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("City Tier vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(CityTier), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("City Tier") + 
  ylab("Number Of Trips") + 
  ggtitle("City Tier vs. Number Of Trips")


#Group by Gender
GenderM <- ProdTrip %>% group_by(Gender) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
GenderM

ggplot(ProdTrip, aes(x = factor(Gender), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Gender") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Gender vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(Gender), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Gender") + 
  ylab("Number Of Trips") + 
  ggtitle("Gender vs. Number Of Trips")


#Group by MaritalStatus
MaritalStatusM <- ProdTrip %>% group_by(MaritalStatus) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
MaritalStatusM
ggplot(ProdTrip, aes(x = factor(MaritalStatus), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Marital Status") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Marital Status vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(MaritalStatus), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Marital Status") + 
  ylab("Number Of Trips") + 
  ggtitle("Marital Status vs. Number Of Trips")

#Group by Passport
PassportsM <- ProdTrip %>% group_by(Passport) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
PassportsM
ggplot(ProdTrip, aes(x = factor(Passport), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Passport") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Passport vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(Passport), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Passport") + 
  ylab("Number Of Trips") + 
  ggtitle("Passport vs. Number Of Trips")

#Group by OwnCar
OwnCarM <- ProdTrip %>% group_by(OwnCar) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
OwnCarM
ggplot(ProdTrip, aes(x = factor(OwnCar), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("OwnCar") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("OwnCar vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(OwnCar), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("OwnCar") + 
  ylab("Number Of Trips") + 
  ggtitle("OwnCar vs. Number Of Trips")

#Group by Designation
DesignationM <- ProdTrip %>% group_by(Designation) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
DesignationM
ggplot(ProdTrip, aes(x = factor(Designation), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Designation") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Designation vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(Designation), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Designation") + 
  ylab("Number Of Trips") + 
  ggtitle("Designation vs. Number Of Trips")

#Group by PitchSatisfactionScore
ScoreM <- ProdTrip %>% group_by(PitchSatisfactionScore) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
ScoreM

ggplot(ProdTrip, aes(x = factor(PitchSatisfactionScore), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Pitch Satisfaction Score") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Pitch Satisfaction Score vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(PitchSatisfactionScore), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Pitch Satisfaction Score") + 
  ylab("Number Of Trips") + 
  ggtitle("Pitch Satisfaction Score vs. Number Of Trips")

#Group by PrefferedPropertyStar
PropStarM <- ProdTrip %>% group_by(PreferredPropertyStar) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
PropStarM

#Group by Occupation
OccupationM <- ProdTrip %>% group_by(Occupation) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
OccupationM
ggplot(ProdTrip, aes(x = factor(Occupation), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Occupation") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Occupation vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(Occupation), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Occupation") + 
  ylab("Number Of Trips") + 
  ggtitle("Occupation vs. Number Of Trips")

#DurationPitch
table(TravelProd$DurationOfPitch)
table(TravelProd$NumberOfTrips)

TravelProd <- na.omit (TravelProd)

#Convert DurationOfPitch into range
TravelProd$cat_Duration <- as.factor(ifelse(TravelProd$DurationOfPitch < 10, 'Under 10',
                                 ifelse(TravelProd$DurationOfPitch < 20, '10s', 
                                        ifelse(TravelProd$DurationOfPitch < 30, '20s', 
                                        ifelse(TravelProd$DurationOfPitch < 40, '30s', '40s and above')))))

head(TravelProd)
str(TravelProd$cat_Duration)
PitchM <- TravelProd %>% group_by(cat_Duration) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Duration Average" = mean))
PitchM
max(PitchM$Average)
### Customer receive duration pitch around 30s make the most trip with the most number of visitor.

ggplot(TravelProd, aes(x = factor(cat_Duration), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Duration Pitch") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Duration Pitch vs. Number Of Person Visiting")


ggplot(TravelProd, aes(x = factor(cat_Duration), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Duration Of Pitch") + 
  ylab("Number Of Trips") + 
  ggtitle("Duration Pitch vs. Number Of Trips")


###Age
ggplot(ProdTrip, aes(x = factor(Age), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Age") + 
  ylab("Number Of Trips") + 
  ggtitle("Age v.s. Number Of Trips")  

ggplot(ProdTrip, aes(x = factor(NumberOfTrips), y = Age)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Number Of Trips") + 
  ylab("Age") + 
  ggtitle("Age vs. Number Of Trips")  

#Group by PreferredPropertyStar
ScorePS <- ProdTrip %>% group_by(PreferredPropertyStar) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
ScorePS

ggplot(ProdTrip, aes(x = factor(PreferredPropertyStar), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Preferred Property Star") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Preferred Property Star vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(PreferredPropertyStar), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Preferred Property Star") + 
  ylab("Number Of Trips") + 
  ggtitle("Preferred Property Star vs. Number Of Trips")

##Group by NumberOfFollowups
ScoreFUps <- ProdTrip %>% group_by(NumberOfFollowups) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
ScoreFUps

ggplot(ProdTrip, aes(x = factor(NumberOfFollowups), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Number Of Follow-ups") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Number Of Follow-ups vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(NumberOfFollowups), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Number Of Follow-ups") + 
  ylab("Number Of Trips") + 
  ggtitle("Number Of Follow-ups vs. Number Of Trips")

##Group by NumberOfChildrenVisiting
ScoreChildren <- ProdTrip %>% group_by(NumberOfChildrenVisiting) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
ScoreChildren

ggplot(ProdTrip, aes(x = factor(NumberOfChildrenVisiting), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Number Of Children Visiting") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Number Of Children Visiting vs. Number Of Person Visiting")

ggplot(ProdTrip, aes(x = factor(NumberOfChildrenVisiting), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Number Of Children Visiting") + 
  ylab("Number Of Trips") + 
  ggtitle("Number Of Children Visiting vs. Number Of Trips")
