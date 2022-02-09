#Load Libraries
install.packages("Hmisc")
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

#Removing NA values:
TravelNA <- na.omit (Travel)
head(TravelNA)

#Creating new columns SaleIndex by number of trips, visitors and proTaken:
TravelNA$SaleIndex <-TravelNA$NumberOfPersonVisiting*TravelNA$NumberOfTrips+TravelNA$ProdTaken

#Delete column
TravelNA$NumberOfPersonVisiting <- NULL 
TravelNA$NumberOfTrips <- NULL 
TravelNA$ProdTaken <- NULL 



head(TravelNA)



# Exploring Dataset:

# Converting categorical variables into numeric values
TravelConvert <- TravelNA
TravelConvert[sapply(TravelConvert, is.factor)] <- data.matrix(TravelConvert[sapply(TravelConvert, is.factor)])

# Correlation Matrix
Travelmatrix <- cor(TravelConvert)
View(round(Travelmatrix, 2))

chart.Correlation(Travelmatrix, histogram=FALSE, method="pearson")


##Matrix Correlation observation:
#DVs: SaleIndex
#Highly Correlated IVs:
        ###Age
        ###NumberOfFollowups
        ###NumberOfChildrenVisiting
        ###MonthlyIncome
     

## Exam Categorical variables that are highly correlated with ProdTaken:
keeps <- c ("Age", "NumberOfFollowups","NumberOfChildrenVisiting","MonthlyIncome")
ProdCat <- TravelNA[keeps] 



###Examine Means

#Income:
h <- ggplot(TravelNA, aes(x = MonthlyIncome))
IncomeHis<- h + geom_histogram(binwidth =5000, aes(y = ..count../sum(..count..))) +
        ggtitle("MonthlyIncome histogram") + xlab("Monthly Income") +
        ylab("Relative frequency")
IncomeHis

#Convert MonthlyIncome into range
TravelNA$cat_Income <- as.factor(ifelse(TravelNA$MonthlyIncome < 10000, '0-10k',
                                        ifelse(TravelNA$MonthlyIncome < 15000, '10-15k', 
                                        ifelse(TravelNA$MonthlyIncome < 20000, '15-20k', 
                                        ifelse(TravelNA$MonthlyIncome < 25000, '20k-25k',
                                        ifelse(TravelNA$MonthlyIncome < 30000, '25-30k',
                                        ifelse(TravelNA$MonthlyIncome < 35000, '30-35k',
                                        ifelse(TravelNA$MonthlyIncome < 40000, '35-40k', '35k-100k'))))))))

str(TravelNA$cat_Income)
IncomeM <- TravelNA %>% group_by(TravelNA$cat_Income) %>% summarise_at(vars("SaleIndex"), list("Duration Average" = mean))
IncomeM


SaleIndex_Plot<-ggplot(TravelNA, aes(x = factor(cat_Income), y = SaleIndex)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Monthly Income") + 
        ylab("SaleIndex") + 
        ggtitle("Monthly Income vs. SaleIndex")
SaleIndex_Plot
table(TravelNA$cat_Income)

#eliminate Outliner:
IncomeTrim <- subset(TravelNA, MonthlyIncome >= 15000, MonthlyIncome <= 40000 )
IncomeTrim <- na.omit (IncomeTrim)

#Convert MonthlyIncome into range
IncomeTrim$cat_Income <- as.factor(ifelse(IncomeTrim$MonthlyIncome < 10000, '0-10k',
                                   ifelse(TravelNA$MonthlyIncome < 15000, '10-15k', 
                                   ifelse(TravelNA$MonthlyIncome < 20000, '15-20k', 
                                   ifelse(TravelNA$MonthlyIncome < 25000, '20k-25k',
                                   ifelse(TravelNA$MonthlyIncome < 30000, '25-30k',
                                   ifelse(TravelNA$MonthlyIncome < 35000, '30-35k',
                                   ifelse(TravelNA$MonthlyIncome < 40000, '35-40k', '35k-100k'))))))))

SaleIndex_Plot<-ggplot(IncomeTrim, aes(x = factor(cat_Income), y = SaleIndex)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Monthly Income") + 
        ylab("Sale Index") + 
        ggtitle("Monthly Income vs. Sale Index")
SaleIndex_Plot


#Age
table(TravilNA$Age)
table(TravilNA$SaleIndex)

#Age
#Convert Age to AgeRange
TravelNA$cat_Age <- as.factor(ifelse(TravelNA$Age < 30, '18-29',
                              ifelse(TravelNA$Age < 40, '30s', 
                              ifelse(TravelNA$Age < 50, '40s', 
                              ifelse(TravelNA$Age < 60, '50s','60s')))))

AgeM <- TravelNA %>% group_by(cat_Age) %>% summarise_at(vars("SaleIndex"), list("Average" = mean))
AgeM
ggplot(TravelNA, aes(x = factor(cat_Age), y = SaleIndex)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Age") + 
        ylab("Sale Index") + 
        ggtitle("Age vs. Sale Index")




#NumberOfFollowups

#Check Mean
FollowupsM <- TravelNA %>% group_by(NumberOfFollowups) %>% summarise_at(vars("SaleIndex"), list("NumberOfFollowups Average" = mean))
FollowupsM

ggplot(TravelNA, aes(x = factor(NumberOfFollowups), y = SaleIndex)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("NumberOfFollowups") + 
        ylab("Sale Index") + 
        ggtitle("NumberOfFollowups vs. Sale Index")


#NumberOfChildrenVisiting

#Check Mean
ChildrenM <- TravelNA %>% group_by(NumberOfChildrenVisiting) %>% summarise_at(vars("SaleIndex"), list("Duration Average" = mean))
ChildrenM

ggplot(TravelNA, aes(x = factor(NumberOfChildrenVisiting), y = SaleIndex)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("NumberOfChildrenVisiting") + 
        ylab("Sale Index") + 
        ggtitle("Number Of Children Visiting vs. Sale Index")

#Question Set Up
#How does the following IVs influence Sale Index?
#DVs: SaleIndex
#Highly Correlated IVs:
###Age (cat_Age)
###NumberOfFollowups
###NumberOfChildrenVisiting
###MonthlyIncome (cat_Income)

#Check value for NumberOfTrips
table(TravelNA$SaleIndex)
head(TravelNA)

## make sure variables are numeric
str(TravelNA$SaleIndex)
str(TravelNA$Age)
str(TravelNA$NumberOfFollowups)
str(TravelNA$NumberOfChildrenVisiting)
str(TravelNA$MonthlyIncome)

### Check normality of DVs

#Number of Person Visiting
plotNormalHistogram(TravelNA$SaleIndex, main = "Sale Index Original Histogram")

TravelNA$SaleIndexSquare <- TravelNA$SaleIndex ^ 2
plotNormalHistogram(TravelNA$SaleIndexSquare, main = "SaleIndex Square Histogram")

TravelNA$SaleIndexCUBE <- TravelNA$SaleIndex ^ 3
plotNormalHistogram(TravelNA$SaleIndexCUBE, main = "SaleIndex Cube Histogram")

TravelNA$SaleIndexLOG <- log(TravelNA$SaleIndex)
plotNormalHistogram(TravelNA$SaleIndexLOG, main = "SaleIndex LOG Histogram")
####Log model is most normally distributed

#Test Assumptions
## Sample size:  with  4,128 entries our data exceed requirement

## Multivariate Normality - no need to test, only 1 DV

###CANONICAL CORRELATION ANALYSIS
###Subset data
#IVsSubset: X
IVskeeps <- c("Age","NumberOfFollowups", "NumberOfChildrenVisiting", "MonthlyIncome")
IVs <- TravelNA[IVskeeps]

#DVsSubset: Y
DVskeeps <- c("SaleIndexLOG")
DVs <- TravelNA[DVskeeps]

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
#standardizing the first set of canonical coefficients(DVs)
std_coef1<-diag(sqrt(diag(cov(DVs))))
std_coef1%*%can_cor1$xcoef
##standardizing the coeficents of the second set (IVs)
std_coef2<-diag(sqrt(diag(cov(IVs))))
std_coef2%*%can_cor1$ycoef
