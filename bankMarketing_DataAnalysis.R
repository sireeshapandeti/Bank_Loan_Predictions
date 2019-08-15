rm(list=ls())
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(ggplot2)
library(gridExtra)
setwd("/Users/sireeshapandeti/Documents/Rfiles")
getwd()
bank<-read.csv("bank-additional-full.csv",header=T,sep=";")

### Data Cleaning ###
table(is.na(bank))
bank$age <- as.numeric(bank$age)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)
bank$emp.var.rate <- as.numeric(bank$emp.var.rate)
bank$cons.price.idx <- as.numeric(bank$cons.price.idx)
bank$cons.conf.idx <- as.numeric(bank$cons.conf.idx)
bank$euribor3m <- as.numeric(bank$euribor3m)
bank$nr.employed <- as.numeric(bank$nr.employed)
summary(bank)

### Histogram of Bank$age ###
hist(bank$age, col = "light green",  xlab = "Age")

### Graph on Loan and age to the number of seconds on call using ggplot ###
ggplot(bank, aes(x= age, y= duration, colour= loan, group= loan))+
  geom_line()+ggtitle("Relation between Loan, Age and duration(No of sec on call)")

### Pairs Plot ###
pairs(~emp.var.rate+ cons.price.idx + cons.conf.idx + euribor3m + nr.employed,
      data = bank, lty.smooth = 2, main = "Relationship between Economic numerical variables")

### Box Charts of Different Marital Status ###

plotMarried <- ggplot(subset(bank, marital=="married"),aes(x= job, y= duration))+
                geom_boxplot()+ ggtitle(" Married people")+
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
plotMarried
  
plotDivorced <- ggplot(subset(bank, marital=="divorced"),aes(x= job, y= duration))+
  geom_boxplot()+ ggtitle("Divorced people")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plotDivorced

plotSingle <- ggplot(subset(bank, marital=="single"),aes(x= job, y= duration))+
  geom_boxplot()+ ggtitle("Singles")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plotSingle

plotUnknown <- ggplot(subset(bank, marital=="unknown"),aes(x= job, y= duration))+
  geom_boxplot()+ ggtitle("Unknown ")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plotUnknown

### Grid View ###
grid.arrange(plotMarried,
             plotSingle, plotDivorced, plotUnknown,
             ncol=2)

### Box Chart of Duration versus Y ###
ggplot(bank, aes(x=y , y= duration))+
  geom_boxplot(aes(fill = bank$y))+
  ggtitle("Box Chart of Duration versus Y")

### Decision Tree ###
bank.rpart <- rpart(y ~ ., data = bank, method = "class")
bank.rpart
rpart.plot(bank.rpart, type = 4, extra = 101)


