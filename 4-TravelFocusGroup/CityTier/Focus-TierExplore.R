#Load Libraries
install.packages("Hmisc")
install.packages('writexl')

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
library(writexl)

#Data wrangling

#Removing NA values:
TravelNA <- na.omit (Travel)



#eliminate Outliner:
IncomeTrim <- subset(TravelNA, MonthlyIncome >= 15000, MonthlyIncome <= 40000 )
TravelNA <- na.omit (IncomeTrim)

IndexSubsetFilter <- subset(TravelNA, MonthlyIncome > 20000 &
                              MonthlyIncome < 40000 &
                              Age > 29 &
                              NumberOfChildrenVisiting > 0)
#Creating new columns SaleIndex by number of trips, visitors and proTaken:
TravelNA$SaleIndex <-TravelNA$NumberOfPersonVisiting*TravelNA$NumberOfTrips+TravelNA$ProdTaken

#Subset Tier1:
Focus1 <- subset(TravelNA, CityTier == 1)
#Removing NA values:
Focus1NA <- na.omit (Focus1)
table (Focus1$CityTier)
write_xlsx(Focus1NA, 'C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\Focus1.xlsx')


#Subset Tier2:
Focus2 <- subset(TravelNA, CityTier == 2)
#Removing NA values:
Focus2NA <- na.omit (Focus2)
table(Focus2$CityTier)
write_xlsx(Focus2NA, 'C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\Focus2.xlsx')

#Subset Tier3:
Focus3 <- subset(TravelNA, CityTier == 3)
#Removing NA values:
Focus3NA <- na.omit (Focus3)
table(Focus3$CityTier)
head(Focus3NA)
write_xlsx(Focus3NA, 'C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\Focus3.xlsx')


#Delete column
Focus1NA[ , c('cat_Age', 'cat_Income', 'CityTier', 'SaleIndex')] <- list(NULL)
Focus2NA[ , c('cat_Age', 'cat_Income', 'CityTier', 'SaleIndex')] <- list(NULL)
Focus3NA[ , c('cat_Age', 'cat_Income', 'CityTier', 'SaleIndex')] <- list(NULL)


# Exploring Dataset:

# Converting categorical variables into numeric values

#Tier 1 Correlation Matrix
Focus1Convert <- Focus1NA
Focus1Convert[sapply(Focus1Convert, is.factor)] <- data.matrix(Focus1Convert[sapply(Focus1Convert, is.factor)])

# Correlation Matrix
MatrixT1 <- cor(Focus1Convert)
View(round(MatrixT1, 2))
write.csv(MatrixT1, "C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\MatrixT1.csv")


# Tier2 Correlation Matrix:
# Converting categorical variables into numeric values
Focus2Convert <- Focus2NA
Focus2Convert[sapply(Focus2Convert, is.factor)] <- data.matrix(Focus2Convert[sapply(Focus2Convert, is.factor)])

# Correlation Matrix
MatrixT2 <- cor(Focus2Convert)
write.csv(MatrixT2, "C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\MatrixT2.csv")
View(round(MatrixT2, 2))

chart.Correlation(MatrixT2, histogram=FALSE, method="pearson")

#Tier 3 Correlation Matrix
# Converting categorical variables into numeric values
Focus3Convert <- Focus3NA
Focus3Convert[sapply(Focus3Convert, is.factor)] <- data.matrix(Focus3Convert[sapply(Focus3Convert, is.factor)])

# Correlation Matrix
MatrixT3 <- cor(Focus3Convert)
View(round(MatrixT3, 2))
write.csv(MatrixT3, "C:\\Users\\Quy\\Data-Python-Documents\\Final-Project\\MatrixT3.csv")

chart.Correlation(MatrixT3, histogram=FALSE, method="pearson")


