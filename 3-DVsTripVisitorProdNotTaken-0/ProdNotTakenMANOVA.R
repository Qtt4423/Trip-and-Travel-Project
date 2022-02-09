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


#Question Set Up
#How does the following IVs influence DVs among customers who did not made a purchase?

#IV1 = TypeofContact
#IV2 = Occupation
#IV3 = Gender
#IV4 = MaritalStatus
#IV5 = Passport

#DV1 = NumberOfPersonVisiting
#DV2 = NumberOfTrips

#Data Wrangling: 


#Check value for ProdTaken
table(Travel$ProdTaken)
### only 920 customers bought product, 3968 customers did not.

#Subset data select only value of ProdTaken = 0
NotTakenProd <- subset(Travel, ProdTaken == 0)

#remove missing data
NoProdNA <- na.omit (NotTakenProd)

#Check value for NumberOfTrips
table(NoProdNA$NumberOfTrips)
### Since there is only 1 value of 19 and 20 trips, we are going to remove these values.

## Remove outliers 
NOProdTrip <- subset(NoProdNA, NumberOfTrips <= 10)
#Check value for NumberOfTrips
table(NOProdTrip$NumberOfTrips)
head(NOProdTrip)

## make sure DVs are numeric
str(NOProdTrip$NumberOfPersonVisiting)
str(NOProdTrip$NumberOfTrips)

#### make sure Passport is categorical
###Check datatype
class(NOProdTrip$Passport)
table(NOProdTrip$Passport)
###Convert data
PassportCat <- as.factor(NOProdTrip$Passport) 
###Double check datatype
class(PassportCat)


## Check normality of DVs

#Number of Person Visiting
plotNormalHistogram(NOProdTrip$NumberOfPersonVisiting, main = "Number of Visitors Original Histogram")

NOProdTrip$NumberOfPersonVisitingSquare <- NOProdTrip$NumberOfPersonVisiting ^ 2
plotNormalHistogram(NOProdTrip$NumberOfPersonVisitingSquare, main = "Number of Visitors Square Histogram")

NOProdTrip$NumberOfPersonVisitingCUBE <- NOProdTrip$NumberOfPersonVisiting ^ 3
plotNormalHistogram(NOProdTrip$NumberOfPersonVisitingCUBE, main = "Number of Visitors Cube Histogram")

NOProdTrip$NumberOfPersonVisitingLOG <- log(NOProdTrip$NumberOfPersonVisiting)
plotNormalHistogram(NOProdTrip$NumberOfPersonVisitingLOG, main = "Number of Visitors LOG Histogram")
####Log model is most normally distributed

##Number of Trips
plotNormalHistogram(NOProdTrip$NumberOfTrips, main = "Number of Trips Original Histogram")

NOProdTrip$NumberOfTripsSquare <- NOProdTrip$NumberOfTrips ^ 2
plotNormalHistogram(NOProdTrip$NumberOfTripsSquare, main = "Number of Trips Square Histogram")

NOProdTrip$NumberOfTripsSquareCUBE <- NOProdTrip$NumberOfTripsSquare ^ 3
plotNormalHistogram(NOProdTrip$NumberOfTripsSquareCUBE, main = "Number of Trips Cube Histogram")

NOProdTrip$NumberOfTripsLOG <- log(NOProdTrip$NumberOfTrips)
plotNormalHistogram(NOProdTrip$NumberOfTripsLOG, main = "Number of Trips LOG Histogram")
####Log model is most normally distributed


###Subset data
keeps <- c("NumberOfPersonVisitingLOG", "NumberOfTripsLOG")
TravelDVs <- NOProdTrip[keeps]

### Turn data into matrix
TravelDVsM <- as.matrix(TravelDVs)
TravelDVsM

#Test Assumptions
## Sample size:  with  906 entries our data exceed requirement

## Multivariate Normality
mshapiro.test(t(TravelDVsM))
###p-value < 2.2e-16 --> significant -> violate assumption.
###So unfortunately, these data do not meet the assumption for MANOVAs.
###However, our Histogram resemble normal distribution. We are proceed with analysis.



### Type of Contact
##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingLOG ~ TypeofContact, data=NOProdTrip)
#P-value of 0.4865 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ TypeofContact, data=NOProdTrip)
#P-value of 0.0456 < 0.05 means this is a significant. --> did not pass the assumption

##Absence of Multicollinearity
cor.test(NOProdTrip$NumberOfPersonVisitingLOG, NOProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .228 < 0.7, you have an absence of multicollinearity.

##The Analysis
ContMANOVA <- manova(cbind(NumberOfPersonVisitingLOG, NumberOfTripsLOG) ~ TypeofContact, data = NOProdTrip)
summary(ContMANOVA)
###With Pr(>F) value of 0.8183, this is not a significant test.
###There is not a significant difference in NumberOfPersonVisitingSquare and NumberOfTripsLOG by TypeofContact.

##ANOVAs as Post Hocs
summary.aov(ContMANOVA, test = "wilks") 

###NumberOfPersonVisiting Pr(>F) value of 0.6963 is not significant.
###NumberOfTrips Pr(>F) value of 0.6919 is not significant.
###There is not a significant difference in Number Of Visitors nor trips by TypeofContact


### Occupation
##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingLOG ~ Occupation, data=NOProdTrip)
#P-value of 0.5551 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ Occupation, data=NOProdTrip)
#P-value of 0.1669 > 0.05 means this is not a significant. --> pass the assumption

##Absence of Multicollinearity
cor.test(NOProdTrip$NumberOfPersonVisitingLOG, NOProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .228 < 0.7, you have an absence of multicollinearity.

##The Analysis
OcMANOVA <- manova(cbind(NumberOfPersonVisitingLOG, NumberOfTripsLOG) ~ Occupation, data = NOProdTrip)
summary(OcMANOVA)
###With Pr(>F) value of 0.9062, this is not a significant test.
###There is not a significant difference in NumberOfPersonVisitingSquare and NumberOfTripsLOG by Occupation.

##ANOVAs as Post Hocs
summary.aov(OcMANOVA, test = "wilks") 

###NumberOfPersonVisiting Pr(>F) value of 0.7551 is not significant.
###NumberOfTrips Pr(>F) value of 0.7687 is not significant.
###There is not a significant difference in Number Of Visitors nor trips by Occupation.



### Gender
##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingLOG ~ Gender, data=NOProdTrip)
#P-value of 0.4671 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ Gender, data=NOProdTrip)
#P-value of 0.5349 > 0.05 means this is not a significant. --> pass the assumption

##Absence of Multicollinearity
cor.test(NOProdTrip$NumberOfPersonVisitingLOG, NOProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .228 < 0.7, you have an absence of multicollinearity.

##The Analysis
GenMANOVA <- manova(cbind(NumberOfPersonVisitingLOG, NumberOfTripsLOG) ~ Gender, data = NOProdTrip)
summary(GenMANOVA)
###With Pr(>F) value of 0.7006, this is not a significant test.
###There is not a significant difference in NumberOfPersonVisitingSquare and NumberOfTripsLOG by Gender

##ANOVAs as Post Hocs
summary.aov(GenMANOVA, test = "wilks") 

###NumberOfPersonVisiting Pr(>F) value of 0.7646 is not significant.
###NumberOfTrips Pr(>F) value of 0.403 is not significant.
###There is not a significant difference in Number Of Visitors nor trips by Gender




### MaritalStatus
##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingLOG ~ MaritalStatus, data=NOProdTrip)
#P-value of 5.119e-06 < 0.05 means this is a significant. --> did not pass the assumption

leveneTest(NumberOfTripsLOG ~ MaritalStatus, data=NOProdTrip)
#P-value of 0.4096 > 0.05 means this is not a significant. --> pass the assumption

##Absence of Multicollinearity
cor.test(NOProdTrip$NumberOfPersonVisitingLOG, NOProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .228 < 0.7, you have an absence of multicollinearity.

##The Analysis
MartMANOVA <- manova(cbind(NumberOfPersonVisitingLOG, NumberOfTripsLOG) ~ MaritalStatus, data = NOProdTrip)
summary(MartMANOVA)
###With Pr(>F) value of 1.636e-15, this is a significant test.
###There is a significant difference in NumberOfPersonVisitingSquare and NumberOfTripsLOG by Marital Status

##ANOVAs as Post Hocs
summary.aov(MartMANOVA, test = "wilks") 

###NumberOfPersonVisiting Pr(>F) value of 2.289e-15  is significant although did not pass assumption.
###NumberOfTrips Pr(>F) value of 2.036e-05  is  significant.
###There is a significant difference in Number Of Visitors nor trips by Marital Status.




### Passport
##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingLOG ~ PassportCat, data=NOProdTrip)
#P-value of 0.9871 > 0.05 means this a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ PassportCat, data=NOProdTrip)
#P-value of 0.01301 < 0.05 means this is a significant. --> did not pass the assumption

##Absence of Multicollinearity
cor.test(NOProdTrip$NumberOfPersonVisitingLOG, NOProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .228 < 0.7, you have an absence of multicollinearity.

##The Analysis
PMANOVA <- manova(cbind(NumberOfPersonVisitingLOG, NumberOfTripsLOG) ~ PassportCat, data = NOProdTrip)
summary(PMANOVA)
###With Pr(>F) value of 0.2561, this is not a significant test.
###There is a significant difference in NumberOfPersonVisitingSquare and NumberOfTripsLOG by Passport

##ANOVAs as Post Hocs
summary.aov(PMANOVA, test = "wilks") 

###NumberOfPersonVisiting Pr(>F) value of 0.9958  is not significant.
###NumberOfTrips Pr(>F) value of 0.1083  is not significant.
###There is a significant difference in Number Of Visitors nor trips by Passport.

