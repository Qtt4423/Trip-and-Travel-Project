#Load Libraries
install.packages("Hmisc")
install.packages("ggstatsplot")
install.packages("GGally")
install.packages("CCA")
install.packages("CRAN")
install.packages("CCP")


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
library(GGally)
library(CCA)
library (CCP)


#Explore data:
head(Travel)


#Question Set Up
#How does the following IVs influence Numbers of Visistors per trip and number of Trips among customers who did not made a purchase?

#IV1 = Age
#IV2 = NumberOfFollowups 
#IV3 = NumberOfChildrenVisiting 
#IV4 = MonthlyIncome

#DV1 = NumberOfPersonVisiting
#DV2 = NumberOfTrips

#Data Wrangling: 


##Creating Subset with all customers who did not taken a the Products.

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
NOProdTrip <- na.omit (NOProdTrip)

#Check value for NumberOfTrips
table(NOProdTrip$NumberOfTrips)
head(NOProdTrip)

## make sure variables are numeric
str(NOProdTrip$NumberOfPersonVisiting)
str(NOProdTrip$NumberOfTrips)
str(NOProdTrip$Age)
str(NOProdTrip$NumberOfFollowups)
str(NOProdTrip$NumberOfChildrenVisiting)
str(NOProdTrip$MonthlyIncome)



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
#IVsSubset: X
IVskeeps <- c("Age","NumberOfFollowups", "NumberOfChildrenVisiting", "MonthlyIncome")
IVs <- NOProdTrip[IVskeeps]

#DVsSubset: Y
DVskeeps <- c("NumberOfPersonVisitingLOG", "NumberOfTripsLOG")
DVs <- NOProdTrip[DVskeeps]

#Exam the correlation within each subset
ggpairs(IVs)
ggpairs(DVs)

#Exam the correlation between the two sets of variables using matcor from CCA
# correlations
cormat<-matcor(DVs, IVs)

#Extracting the within study correlations for IVs and DVs and between set cor
round(cormat$Ycor, 4)

#The associations between the two sets can be extracted as
#Between set associations
cormat<-matcor(DVs,IVs)
round(cormat$XYcor, 4)

#Obtain the canonical correlations
can_cor1=cc(DVs, IVs)
can_cor1$cor

#Obtain raw canonical coefficients
can_cor1[3:4]

#computes the canonical loadings
can_cor2=comput(DVs,IVs,can_cor1)
#displays the canonical loadings
can_cor2[3:6] 

#obtain the statistical significance of the dimensions
#test of canonical dimensions
rho=can_cor1$cor
##defining the number of observations, no of variables in first set,
#and number of variables in second set
n=dim(DVs)[1]
p=length(DVs)
q=length(IVs)
##Calculating the F approximations using different test statistics
p.asym(rho,n,p,q,tstat="Wilks")
p.asym(rho,n,p,q,tstat="Hotelling")
p.asym(rho,n,p,q,tstat="Pillai")
p.asym(rho,n,p,q,tstat="Roy")

#Calculating standardized canonical coefficients using R

#standardizing the first set of 
 #canonical coefficients(DVs)
std_coef1<-diag(sqrt(diag(cov(DVs))))
std_coef1%*%can_cor1$xcoef

#standardizing the coeficents of 
 ##the second set (IVs)
std_coef2<-diag(sqrt(diag(cov(IVs))))
std_coef2%*%can_cor1$ycoef

### Exam Means
#Age
#Convert Age to AgeRange
NOProdTrip$cat_Age <- as.factor(ifelse(NOProdTrip$Age < 30, '18-29',
                                 ifelse(NOProdTrip$Age < 40, '30s', 
                                        ifelse(NOProdTrip$Age < 50, '40s', 
                                               ifelse(NOProdTrip$Age < 60, '50s','60s')))))

AgeM <- NOProdTrip %>% group_by(cat_Age) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
AgeM
ggplot(NOProdTrip, aes(x = factor(cat_Age), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Age") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Age vs. Number Of Person Visiting")

ggplot(NOProdTrip, aes(x = factor(cat_Age), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Age") + 
  ylab("Number Of Trips") + 
  ggtitle("Age vs. Number Of Trips")

#NumberOfFollowups 
FollowupsM <- NOProdTrip %>% group_by(NumberOfFollowups) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
FollowupsM
ggplot(NOProdTrip, aes(x = factor(NumberOfFollowups), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Number Of Followups") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Number Of Followups vs. Number Of Person Visiting")

ggplot(NOProdTrip, aes(x = factor(NumberOfFollowups), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Number Of Followups") + 
  ylab("Number Of Trips") + 
  ggtitle("Number Of Followups vs. Number Of Trips")

#NumberOfChildrenVisiting 
ChildrenM <- NOProdTrip %>% group_by(NumberOfChildrenVisiting) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
ChildrenM
ggplot(NOProdTrip, aes(x = factor(NumberOfChildrenVisiting), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Number Of Children Visiting ") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Number Of Children Visiting vs. Number Of Person Visiting")

ggplot(NOProdTrip, aes(x = factor(NumberOfChildrenVisiting), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Number Of Children Visiting ") + 
  ylab("Number Of Trips") + 
  ggtitle("Number Of Children Visiting vs. Number Of Trips")


#MonthlyIncome

#Convert MonthlyIncome to IncomeRange
NOProdTrip$IncomeRange <- as.factor(ifelse(NOProdTrip$MonthlyIncome < 17000, '16s thousands', 
                                    ifelse(NOProdTrip$MonthlyIncome < 18000, '17s thousands', 
                                    ifelse(NOProdTrip$MonthlyIncome < 19000, '18s thousands',
                                    ifelse(NOProdTrip$MonthlyIncome < 20000, '19s thousands',
                                    ifelse(NOProdTrip$MonthlyIncome < 30000, '20s thousands','30s thousands'))))))
table(NOProdTrip$IncomeRange)
IncomeRangeM <- NOProdTrip %>% group_by(IncomeRange) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
IncomeRangeM
ggplot(NOProdTrip, aes(x = factor(IncomeRange), y = NumberOfPersonVisiting)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Income Range") + 
  ylab("Number Of Person Visiting") + 
  ggtitle("Income Range vs. Number Of Person Visiting")

ggplot(NOProdTrip, aes(x = factor(IncomeRange), y = NumberOfTrips)) + 
  stat_summary(fun = "mean", geom = "bar")+
  xlab("Income Range") + 
  ylab("Number Of Trips") + 
  ggtitle("Income Range vs. Number Of Trips")
