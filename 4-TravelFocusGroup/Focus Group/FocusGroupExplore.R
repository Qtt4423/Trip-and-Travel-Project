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
library(ggplot2)
library(writexl)

# Exploring Dataset:
head(TravelFocusGroup)

###Examine Means

#Income:

CityTierM <- TravelFocusGroup %>% group_by(TravelFocusGroup$CityTier) %>% summarise_at(vars("SaleIndex", "Age", "MonthlyIncome", "NumberOfChildrenVisiting", "DurationOfPitch", "NumberOfFollowups", "PreferredPropertyStar", "PitchSatisfactionScore"), list("Average" = mean))
write_xlsx(CityTierM, 'C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\CityTierMean.xlsx')


TravelFocusGroup %>% group_by(CityTier, TypeofContact) %>% count()
TravelFocusGroup %>% group_by(CityTier, Occupation) %>% count()
TravelFocusGroup %>% group_by(CityTier, Gender) %>% count()
TravelFocusGroup %>% group_by(CityTier, ProductPitched) %>% count()
TravelFocusGroup %>% group_by(CityTier, MaritalStatus) %>% count()
TravelFocusGroup %>% group_by(CityTier, Passport) %>% count()
TravelFocusGroup %>% group_by(CityTier, OwnCar) %>% count()
TravelFocusGroup %>% group_by(CityTier, Designation) %>% count()
TravelFocusGroup %>% group_by(CityTier, PreferredPropertyStar) %>% count()
TravelFocusGroup %>% group_by(CityTier, PitchSatisfactionScore) %>% count()
TravelFocusGroup %>% group_by(CityTier, NumberOfFollowups) %>% count()
TravelFocusGroup %>% group_by(CityTier, NumberOfChildrenVisiting) %>% count()
TravelFocusGroup %>% group_by(CityTier, cat_Age) %>% count()
TravelFocusGroup %>% group_by(CityTier, cat_Income) %>% count()
TravelFocusGroup %>% group_by(CityTier) %>% summarise_at(vars(SaleIndex), list(name = mean))
TravelFocusGroup %>% group_by(CityTier) %>% summarise_at(vars(MonthlyIncome), list(name = mean))
TravelFocusGroup %>% group_by(CityTier) %>% summarise_at(vars(Age), list(name = mean))
TravelFocusGroup %>% group_by(CityTier) %>% summarise_at(vars(NumberOfFollowups), list(name = mean))
TravelFocusGroup %>% group_by(CityTier, OwnCar) %>% count()
table(TravelFocusGroup$CityTier)


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
