# R-code-for-Marketing-Analysis-Dataset
R code for Marketing Analysis Dataset

library(dplyr)
library(mlbench)
library(caret)

#EDA

#Summary of the dataset 
summary(ifood_df)

#now as we have understood the data and made it according to our understanding we can clean the data set by clearing the unwanted variables.

#removing unwanted variables
final = select(ifood_df, -Kidhome, -Teenhome)
final

#check the null values
not_applicable = sum(is.na(ifood_df))
not_applicable
#there seems to be no null values in the dataset

#individual boxplot for univariant

boxplot(ifood_df$Age, main="AGE box plot",ylab="AGE",col="red")

boxplot(ifood_df$Income, main="INCOME box plot",ylab="INCOME",col="red")

boxplot(ifood_df$Kidhome, main="KIDS AT HOME box plot",ylab="KIDS AT HOME",col="red")

boxplot(ifood_df$Teenhome, main="TEENS AT HOME plot",ylab="TEENS AT HOME",col="red")

boxplot(ifood_df$Recency, main="DAYS FROM THE LAST PURCHASE box plot",ylab="DAYS FROM THE LAST PURCHASE",col="red")

boxplot(ifood_df$MntWines, main="AMOUNT SPENT ON WINES box plot",ylab="AMOUNT SPENT ON WINES",col="red")

boxplot(ifood_df$MntFruits, main="AMOUNT SPENT ON FRUITS box plot",ylab="AMOUNT SPENT ON FRUITS",col="red")

boxplot(ifood_df$MntMeatProducts, main="AMOUNT SPENT ON MEAT PRODUCTS box plot",ylab="AMOUNT SPENT ON MEAT PRODUCTS",col="red")

boxplot(ifood_df$MntFishProducts, main="AMOUNT SPENT ON FISH PRODUCTS box plot",ylab="AMOUNT SPENT ON FISH PRODUCTS",col="red")

boxplot(ifood_df$MntGoldProds, main="AMOUNT SPENT ON GOLD box plot",ylab="AMOUNT SPENT ON GOLD",col="red")

boxplot(ifood_df$MntSweetProducts, main="AMOUNT SPENT ON SWEET PRODUCTS box plot",ylab="AMOUNT SPENT ON SWEET PRODUCTS",col="red")

boxplot(ifood_df$NumDealsPurchases, main="NUMBER OF DEALS THAT WERE GIVEN DISCOUNT box plot",ylab="NUMBER OF DEALS THAT WERE GIVEN DISCOUNT",col="red")

boxplot(ifood_df$NumWebPurchases, main="NUMBER OF PURCHASES DONE THROUGH COMPANY WEBSITE box plot",ylab="NUMBER OF PURCHASES DONE THROUGH COMPANY WEBSITE",col="red")

boxplot(ifood_df$NumCatalogPurchases, main="NUMBER OF PURCHASES MADE BY CATALOGUES box plot",ylab="NUMBER OF PURCHASES MADE USING CATALOGUE",col="red")

boxplot(ifood_df$NumStorePurchases, main="NUMBER OF PURCHASES MADE DIRECTLY FROM STORES box plot",ylab="NUMBER OF PURCHASES MADE DIRECTLY FROM STORES",col="red")

boxplot(ifood_df$NumWebVisitsMonth, main="NUMBER OF VISITS TO COMPANY WEBSITE IN LAST MONTH box plot",ylab="NUMBER OF VISITS TO COMPANY WEBSITE IN LAST MONTH",col="red")

#boxplot for bivariant data on the basis of age of kids

boxplot(ifood_df[,c("Kidhome", "Teenhome")],  # plots columns between the homes with teens and kids
        main="Box-Plots for homes with teens and kids", # gives plot main label
        col= c("red","orange"), # gives color for each plot
        horizontal = TRUE, #orientates the plots horizontally
        border = "blue", # colors the border of the plots
        notch = TRUE, # rather than boxes make notched boxes
        xlab="Number of People"
)

#boxplot for multivariant data on marital basis

boxplot(ifood_df[,c("marital_Divorced", "marital_Married", "marital_Single", "marital_Together", "marital_Widow")],  # plots columns across the different marital status
        main="Box-Plots for people with Different marital status", # gives plot main label
        col= c("red","orange","yellow","green","blue"), # gives color for each plot
        horizontal = TRUE, #orientates the plots horizontally
        border = "brown", # colors the border of the plots
        notch = TRUE, # rather than boxes make notched boxes
        xlab="Number of People"
)

#boxplot for multivariant data on the basis of the amount spent

boxplot(ifood_df[,c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntGoldProds","MntSweetProducts")],  # plots columns across the differnet expenses
        main="Box-Plots for people and their Different expenses", # gives plot main label
        col= c("red","orange","yellow","green","blue","black"), # gives color for each plot
        horizontal = TRUE, #orientates the plots horizontally
        border = "brown", # colors the border of the plots
        notch = TRUE, # rather than boxes make notched boxes
        xlab="Amount spent in dollar,$"
)

###

#to find the correlation the variables have with the consequent other 

cor(ifood_df$Income,ifood_df$MntWines)

#income has a positive correlation with amount spend on wine; cor=0.7304952

cor(ifood_df$Income,ifood_df$MntFruits)

#income has a positive correlation with amount spend on fruits; cor=0.5379203

cor(ifood_df$Income,ifood_df$MntMeatProducts)

#income has a positive correlation with amount spend on fruits; cor=0.7024996

cor(ifood_df$Income,ifood_df$MntFishProducts)

#income has a positive correlation with amount spend on fruits; cor=0.551758

cor(ifood_df$Income,ifood_df$MntSweetProducts)

#income has a positive correlation with amount spend on fruits; cor=0.555601

cor(ifood_df$Income,ifood_df$MntGoldProds)

#income has a positive correlation with amount spend on fruits; cor=0.4176529

cor(ifood_df$Income,ifood_df$MntRegularProds)

#income has a positive correlation with amount spend on fruits; cor=0.8168792

cor(ifood_df$Income,ifood_df$MntTotal)

#income has a positive correlation with amount spend on fruits; cor=0.823066

#Plotting with scatterplot diagrams

plot(ifood_df$Income,ifood_df$MntWines)

abline(lm(ifood_df$MntWines~ifood_df$Income),col="red")
#

plot(ifood_df$Income,ifood_df$MntFruits)

abline(lm(ifood_df$MntFruits~ifood_df$Income),col="red")
#

plot(ifood_df$Income,ifood_df$MntMeatProducts)

abline(lm(ifood_df$MntMeatProducts~ifood_df$Income),col="red")
#

plot(ifood_df$Income,ifood_df$MntFishProducts)

abline(lm(ifood_df$MntFishProducts~ifood_df$Income),col="red")
#

plot(ifood_df$Income,ifood_df$MntSweetProducts)

abline(lm(ifood_df$MntSweetProducts~ifood_df$Income),col="red")
#

plot(ifood_df$Income,ifood_df$MntGoldProds)

abline(lm(ifood_df$MntGoldProds~ifood_df$Income),col="red")
#

plot(ifood_df$Income,ifood_df$MntRegularProds)

abline(lm(ifood_df$MntRegularProds~ifood_df$Income),col="red")
#

plot(ifood_df$Income,ifood_df$MntTotal)

abline(lm(ifood_df$MntWines~ifood_df$Income),col="red")
#

plot(ifood_df$Income,ifood_df$Age)

abline(lm(ifood_df$Age~ifood_df$Income),col="red")
#

############## R E G R E S S I O N ################

#simple linear regression

regi = lm(Income~Age, data=ifood_df)
summary(regi)
#
reg = lm(MntWines~Income, data=ifood_df)
summary(reg)
#
reg = lm(MntFruits~Income, data=ifood_df)
summary(reg)
#
reg = lm(MntMeatProducts~Income, data=ifood_df)
summary(reg)
#
reg = lm(MntWines~Income, data=ifood_df)
summary(reg)
#
reg = lm(MntFruits~Income, data=ifood_df)
summary(reg)
#
reg = lm(MntMeatProducts~Income, data=ifood_df)
summary(reg)
#
reg = lm(MntWines~Income, data=ifood_df)
summary(reg)
#
reg = lm(MntFruits~Income, data=ifood_df)
summary(reg)
#

## Multiple linear regression model

multi.fit= lm(Income~ Age+MntFishProducts+MntFruits+MntGoldProds+MntMeatProducts+MntRegularProds+MntSweetProducts+MntWines+MntTotal,
              data = ifood_df)

summary(multi.fit)
plot(multi.fit)
