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
library("gmodels")
library("PerformanceAnalytics")
library(writexl)

#Removing NA values:
TravelNA <- na.omit (Travel)
head(TravelNA)


# Exploring Dataset:

# Converting categorical variables into numeric values
TravelConvert <- TravelNA
TravelConvert[sapply(TravelConvert, is.factor)] <- data.matrix(TravelConvert[sapply(TravelConvert, is.factor)])

# Correlation Matrix
Travelmatrix <- cor(TravelConvert)
View(round(Travelmatrix, 2))

chart.Correlation(Travelmatrix, histogram=FALSE, method="pearson")


##Matrix Correlation observatioN:
#DVs: ProdTaken
#Highly Correlated IVs:
        ###Passport 0.271
        ###MaritalStatus 0.147
        ###NumberOfFollowups 0.11
        ###Designation -0.107
        ###MonthlyIncome -0.133
        ###Age -0.15
        ###ProductPitched  -0.15

## Exam Categorical variables that are highly correlated with ProdTaken:
keeps <- c ("ProdTaken", "Passport","MaritalStatus","Designation", "ProductPitched")
ProdCat <- TravelNA[keeps] 
write_xlsx(ProdCat, 'C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\ProdTakenAll.xlsx')

#Independent Chi-Squares analysis

##Passport
CrossTable(ProdCat$ProdTaken, ProdCat$Passport, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
###Since all of our expected values are above 5, our data pass the assumption.
###With a p value of 4.848102e-68, this analysis is significant, meaning that having a passport or not does in fact make a difference in whether potential customer makes a purchased.

##Maritial Status
CrossTable(ProdCat$ProdTaken, ProdCat$MaritalStatus, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
###Since all of our expected values are above 5, our data pass the assumption.
###With a p value of 1.729159e-40, this analysis is significant, meaning that martial status in fact make a difference in whether potential customer makes a purchased.

##Designation
CrossTable(ProdCat$ProdTaken, ProdCat$Designation, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
###Since all of our expected values are above 5, our data pass the assumption.
###With a p value of 8.401618e-44, this analysis is significant, meaning that Designation in fact make a difference in whether potential customer makes a purchased.

##ProductPitched
CrossTable(ProdCat$ProdTaken, ProdCat$ProductPitched, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")

## Exam Continuous variables that are highly correlated with ProdTaken:
## We want to know information among those customers who made a purchase.

##Creating Subset with all customers who bought the Products.
TravelProd <- subset(Travel, ProdTaken == 1)
ProdNA <- na.omit (TravelProd)
head(ProdNA)
table(TravelNA$ProdTaken)
### only 797 customers bought product, 3331 customers did not.

## Drop Categorical variable
Conkeeps <- c ("ProdTaken", "MonthlyIncome","Age","NumberOfFollowups")
ProdCon <- TravelNA[Conkeeps] 


#Stepwise Regression Analysis for Income Iv and DVs of NumberOfTrips and ProdTaken
##Question Set Up
###Does the Monthly Income influence the number of trips and customer decision to make a purchase?

##Data Wrangling: Ensure dependent Variables are Numeric
str(ProdCon$NumberOfTrips)
str(ProdCon$ProdTaken)
### Since both variable are int, we can proceed.

##Subsetting keep only your two dependent variabes
ConDVskeeps <- c("NumberOfTrips", "ProdTaken")
ProdConDVs <- ProdCon[ConDVskeeps]

##Although the test for normality can only handle 5,000 records, with 4,128 entries we are good to go.

##Format as a Matrix
ProdConIncome <- as.matrix(ProdConDVs)

##Test Assumptions:
###Sample Size: with a dataset of 323,746, our dataset fullfil the at least 20 cases per independent variable and that there must be more cases then dependent variables in every cell. 


###Examine Means

#Income:
h <- ggplot(TravelNA, aes(x = MonthlyIncome))
IncomeHis<- h + geom_histogram(binwidth = 5000, aes(y = ..count../sum(..count..))) +
        ggtitle("MonthlyIncome for Travelers") + xlab("Monthly Income") +
        ylab("Relative frequency")
IncomeHis

#Convert MonthlyIncome into range
TravelNA$cat_Income <- as.factor(ifelse(TravelNA$MonthlyIncome < 10000, '0-10k',
                                        ifelse(TravelNA$MonthlyIncome < 15000, '10-15k', 
                                        ifelse(TravelNA$MonthlyIncome < 20000, '15-20k', 
                                        ifelse(TravelNA$MonthlyIncome < 25000, '20k-25k',
                                        ifelse(TravilNA$MonthlyIncome < 30000, '25-30k',
                                        ifelse(TravilNA$MonthlyIncome < 35000, '30-35k',
                                        ifelse(TravilNA$MonthlyIncome < 40000, '35-40k', '35k-100k'))))))))

str(TravelNA$cat_Income)
IncomeM <- TravelNA %>% group_by(TravelNA$cat_Income) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Duration Average" = mean))
IncomeM

### Customer receive duration pitch around 30s make the most trip with the most number of visitor.

PersonVisiting_Plot<-ggplot(TravelNA, aes(x = factor(cat_Income), y = NumberOfPersonVisiting)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Monthly Income") + 
        ylab("Number Of Person Visiting") + 
        ggtitle("Monthly Income vs. Number Of Person Visiting")
PersonVisiting_Plot
NumberOfTrips_Plot<-ggplot(TravelNA, aes(x = factor(cat_Income), y = NumberOfTrips)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Monthly Income") + 
        ylab("Number Of Trips") + 
        ggtitle("Monthly Income vs. Number Of Trips")
NumberOfTrips_Plot

table(TravelNA$cat_Income)

#eliminate Outliner:
IncomeTrim <- subset(TravelNA, MonthlyIncome >= 15000, MonthlyIncome <= 40000 )
IncomeTrim <- na.omit (IncomeTrim)

#Convert MonthlyIncome into range
IncomeTrim$cat_Income <- as.factor(ifelse(IncomeTrim$MonthlyIncome < 10000, '0-10k',
                                        ifelse(TravelNA$MonthlyIncome < 15000, '10-15k', 
                                               ifelse(TravelNA$MonthlyIncome < 20000, '15-20k', 
                                                      ifelse(TravelNA$MonthlyIncome < 25000, '20k-25k',
                                                             ifelse(TravilNA$MonthlyIncome < 30000, '25-30k',
                                                                    ifelse(TravilNA$MonthlyIncome < 35000, '30-35k',
                                                                           ifelse(TravilNA$MonthlyIncome < 40000, '35-40k', '35k-100k'))))))))

PersonVisiting_Plot<-ggplot(IncomeTrim, aes(x = factor(cat_Income), y = NumberOfPersonVisiting)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Monthly Income") + 
        ylab("Number Of Person Visiting") + 
        ggtitle("Monthly Income vs. Number Of Person Visiting")
PersonVisiting_Plot
NumberOfTrips_Plot<-ggplot(IncomeTrim, aes(x = factor(cat_Income), y = NumberOfTrips)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Monthly Income") + 
        ylab("Number Of Trips") + 
        ggtitle("Monthly Income vs. Number Of Trips")
NumberOfTrips_Plot

#Age
table(TravilNA$Age)

ggplot(TravilNA, aes(x = factor(Age), y = NumberOfTrips)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Age") + 
        ylab("Number Of Trips") + 
        ggtitle("Age vs. Number Of Trips")  

ggplot(TravilNA, aes(x = factor(NumberOfTrips), y = Age)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Number Of Trips") + 
        ylab("Age") + 
        ggtitle("Age vs. Number Of Trips")  

ggplot(TravilNA, aes(x = factor(Age), y = NumberOfPersonVisiting)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Age") + 
        ylab("Number Of Person Visiting") + 
        ggtitle("Age vs. Number Of Person Visiting")  

ggplot(TravilNA, aes(x = factor(NumberOfPersonVisiting), y = Age)) + 
        stat_summary(fun = "mean", geom = "bar")+
        ylab("Age") + 
        xlab("Number Of Person Visiting") + 
        ggtitle("Age vs. Number Of Person Visiting") 

#Pitch Duration

#Convert DurationOfPitch into range
TravilNA$cat_Duration <- as.factor(ifelse(TravilNA$DurationOfPitch < 10, 'Under 10',
                                   ifelse(TravilNA$DurationOfPitch < 20, '10s', 
                                   ifelse(TravilNA$DurationOfPitch < 30, '20s', 
                                   ifelse(TravilNA$DurationOfPitch < 40, '30s', '40s and above')))))
#Check Mean
PitchM <- TravilNA %>% group_by(cat_Duration) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Duration Average" = mean))
PitchM

ggplot(TravilNA, aes(x = factor(cat_Duration), y = NumberOfPersonVisiting)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Duration Pitch") + 
        ylab("Number Of Person Visiting") + 
        ggtitle("Duration Pitch vs. Number Of Person Visiting")


ggplot(TravilNA, aes(x = factor(cat_Duration), y = NumberOfTrips)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Duration Of Pitch") + 
        ylab("Number Of Trips") + 
        ggtitle("Duration Pitch vs. Number Of Trips")
