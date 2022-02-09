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

#to export result dataset to Excel package:
install.packages('writexl')
library(writexl)

#Removing NA values:
TravelNA <- na.omit (Travel)
head(TravelNA)

#Creating new columns SaleIndex by number of trips, visitors and proTaken:
TravelNA$SaleIndex <-TravelNA$NumberOfPersonVisiting*TravelNA$NumberOfTrips+TravelNA$ProdTaken

#Delete column
TravelNA$NumberOfPersonVisiting <- NULL 
TravelNA$NumberOfTrips <- NULL 
TravelNA$ProdTaken <- NULL 

##to export result dataset to Excel code:
write_xlsx(TravelNA, 'C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\SaleIndexSub.xlsx')

head(TravelNA)



# Exploring Dataset:

# Converting categorical variables into numeric values
TravelConvert <- TravelNA
TravelConvert[sapply(TravelConvert, is.factor)] <- data.matrix(TravelConvert[sapply(TravelConvert, is.factor)])

# Correlation Matrix
Travelmatrix <- cor(TravelConvert)
View(round(Travelmatrix, 2))
IndexMatrix2 <- round(Travelmatrix, 2)
write.csv(Travelmatrix, "C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\IndexMatrix.csv")


chart.Correlation(Travelmatrix, histogram=FALSE, method="pearson")


##Matrix Correlation observatioN:
#DVs: SaleIndex
#Highly Correlated IVs:
        ###Age
        ###NumberOfFollowups
        ###NumberOfChildrenVisiting
        ###MonthlyIncome
     

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
                                        ifelse(TravilNA$MonthlyIncome < 30000, '25-30k',
                                        ifelse(TravilNA$MonthlyIncome < 35000, '30-35k',
                                        ifelse(TravilNA$MonthlyIncome < 40000, '35-40k', '35k-100k'))))))))

str(TravelNA$cat_Income)
IncomeM <- TravelNA %>% group_by(TravelNA$cat_Income) %>% summarise_at(vars("SaleIndex"), list("Duration Average" = mean))
IncomeM


SaleIndex_Plot<-ggplot(TravelNA, aes(x = factor(cat_Income), y = SaleIndex)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("Monthly Income") + 
        ylab("SaleIndex") + 
        ggtitle("Monthly Income v.s. SaleIndex")
SaleIndex_Plot
table(TravelNA$cat_Income)

#eliminate Outliner:
IncomeTrim <- subset(TravelNA, MonthlyIncome >= 15000, MonthlyIncome <= 40000 )
TravelNA <- na.omit (IncomeTrim)

#Convert MonthlyIncome into range
TravelNA$cat_Income <- as.factor(ifelse(IncomeTrim$MonthlyIncome < 10000, '0-10k',
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
        ggtitle("Monthly Income v.s. Sale Index")
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
        ggtitle("Age v.s. Sale Index")




#NumberOfFollowups

#Check Mean
FollowupsM <- TravelNA %>% group_by(NumberOfFollowups) %>% summarise_at(vars("SaleIndex"), list("Duration Average" = mean))
FollowupsM

ggplot(TravelNA, aes(x = factor(NumberOfFollowups), y = SaleIndex)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("NumberOfFollowups") + 
        ylab("Sale Index") + 
        ggtitle("NumberOfFollowups v.s. Sale Index")

#NumberOfChildrenVisiting

#Check Mean
ChildrenM <- TravelNA %>% group_by(NumberOfChildrenVisiting) %>% summarise_at(vars("SaleIndex"), list("Duration Average" = mean))
ChildrenM

ggplot(TravelNA, aes(x = factor(NumberOfChildrenVisiting), y = SaleIndex)) + 
        stat_summary(fun = "mean", geom = "bar")+
        xlab("NumberOfChildrenVisiting") + 
        ylab("Sale Index") + 
        ggtitle("Number Of Children Visiting v.s. Sale Index")

#Export result:
write_xlsx(TravelNA, 'C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\SaleIndexCATSub.xlsx')


## Investigate why group that has 3 followups perform poorly

table(TravelNA$NumberOfFollowups)
Followups3 <- subset(TravelNA, NumberOfFollowups == 3)

table(Followups3$Designation)
table(Followups3$SaleIndex)
plotNormalHistogram(Followups3$SaleIndex, main = "SaleIndex for customer with 3 Followups Histogram")
### Majority of customer who receive 3 followups take less than 10 trips

plotNormalHistogram(Followups3$DurationOfPitch, main = "DurationOfPitch for customer with 3 Followups Histogram")
plotNormalHistogram(Followups3$MonthlyIncome, main = "Monthly Income for customer with 3 Followups Histogram")
plotNormalHistogram(Followups3$Age, main = "Age for customer with 3 Followups Histogram")


#Compare to customer who did not receive 3 followups
FollowupsNOT3 <- subset(TravelNA, NumberOfFollowups != 3)
plotNormalHistogram(FollowupsNOT3$SaleIndex, main = "SaleIndex for customer did not receive 3 Followups Histogram")
plotNormalHistogram(FollowupsNOT3$DurationOfPitch, main = "DurationOfPitch for customer did not receive 3 Followups Histogram")
NOT3TripTrim <- subset(FollowupsNOT3, MonthlyIncome < 40000)
plotNormalHistogram(NOT3TripTrim$MonthlyIncome, main = "Monthly Income for customer did not receive 3 Followups Histogram")

plotNormalHistogram(NOT3TripTrim$Age, main = "Age for customer did not receive 3 Followups Histogram")





#Create a dataset that only contain desire data base on our analysis

## Focus on income group above 20,000 and bellow 40,000 monthly, above 30 years old and travel with children.
IndexSubsetFilter <- subset(TravelNA, MonthlyIncome > 20000 &
                                      MonthlyIncome < 40000 &
                                      Age > 29 &
                                      NumberOfChildrenVisiting > 0)
                          

write_xlsx(IndexSubsetFilter, 'C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\TravelFocusGroup.xlsx')

#Create Tier 1 subset
Focus1 <- subset(IndexSubsetFilter, CityTier == 1)
write_xlsx(Focus1, 'C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\Focus1.xlsx')

#Create Tier 2 only
Focus2 <- subset(IndexSubsetFilter, CityTier == 2)
write_xlsx(Focus2, 'C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\Focus2.xlsx')




#Create Tier 3 only
Focus3 <- subset(IndexSubsetFilter, CityTier == 3)
write_xlsx(Focus3, 'C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\Focus3.xlsx')

