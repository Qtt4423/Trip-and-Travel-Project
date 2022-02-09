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



### ProductPitched
##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingLOG ~ ProductPitched, data=NOProdTrip)
#P-value of 0.0005802 < 0.05 means this is a significant. --> did not pass the assumption

leveneTest(NumberOfTripsLOG ~ ProductPitched, data=NOProdTrip)
#P-value of 3.334e-07 < 0.05 means this is a significant. --> did not pass the assumption

##Absence of Multicollinearity
cor.test(NOProdTrip$NumberOfPersonVisitingLOG, NOProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .228 < 0.7, you have an absence of multicollinearity.

##The Analysis
ProMANOVA <- manova(cbind(NumberOfPersonVisitingLOG, NumberOfTripsLOG)
                    ~ ProductPitched, data = NOProdTrip)
summary(ProMANOVA)
###With Pr(>F) value of 2.2e-16 , this is a significant test.
###There is  a significant difference in NumberOfPersonVisitingSquare and NumberOfTripsLOG by ProductPitched

##ANOVAs as Post Hocs
summary.aov(ProMANOVA, test = "wilks") 

###NumberOfPersonVisiting Pr(>F) value of 2.2e-16 is  significant.
###NumberOfTrips Pr(>F) value of 0.002365 is significant.
###There is a significant difference in Number Of Visitors and trips by ProductPitched

###Exame Mean:
ProductPitchedM <- NOProdTrip %>% group_by(ProductPitched) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
ProductPitchedM
ggplot(NOProdTrip, aes(x = factor(ProductPitched), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Product Pitched") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Product Pitched vs. Number Of Person Visiting")

ggplot(NOProdTrip, aes(x = factor(ProductPitched), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Product Pitched") + 
  ylab("Number Of Trips") + 
  ggtitle("Product Pitched vs. Number Of Trips")

