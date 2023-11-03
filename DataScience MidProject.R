mydataset<- read.csv("D:/DATA SCIENCE/Dataset_midterm.csv",header = TRUE,sep = ",")
mydataset

names(mydataset)
str(mydataset)
sapply (mydataset, class)
summary(mydataset)
sample_n(mydataset,30)



library(matrixStats)
library(dplyr)
mydataset_cleaned <- distinct(mydataset)
mydataset
mydataset %>% summarise_if(is.numeric, sd)

mydataset$Heart = rowSds(as.matrix(mydataset[,c(2,3)]))
mydata$Heart

colSums(is.na(mydataset))
which(is.na(mydataset$Age))
which(is.na(mydataset$Caesarian))
which(is.na(mydataset$Delivery_number))

mydataset$Blood <- factor(mydataset$Blood, levels = c("low","high","normal"), labels = c(0,1,2))
mydataset

boxplot(mydataset)
boxplot(mydataset$Age, main="Boxplot of Age")

mydataset$Age[mydataset$Age > 60] = NA
mydataset$weight.kg.[mydataset$weight.kg. > 80] = NA
mydataset$Delivery_number[mydataset$Delivery_number > 3] = NA
mydataset

mydatasetomit <-  na.omit(mydataset)
mydatasetomit

mydatasetMean<- mydataset
mydatasetMean$weight.kg.[is.na(mydatasetMean$weight.kg.)]<-mean(mydatasetMean$weight.kg.,na.rm=TRUE)
mydatasetMean$Age[is.na(mydatasetMean$Age)]<-mean(mydatasetMean$Age,na.rm=TRUE)
mydatasetMean$Delivery_number[is.na(mydatasetMean$Delivery_number)]<-mean(mydatasetMean$Delivery_number,na.rm=TRUE)
mydatasetMean$Delivery_time [is.na(mydatasetMean$Delivery_time )]<-mean(mydatasetMean$Delivery_time ,na.rm=TRUE)
mydatasetMean$Caesarian[is.na(mydatasetMean$Caesarian)]<-mean(mydatasetMean$Caesarian,na.rm=TRUE)
mydatasetMean

mydatasetMedian<- mydataset
mydatasetMedian$weight.kg.[is.na(mydatasetMedian$weight.kg.)]<-median(mydatasetMedian$weight.kg.,na.rm=TRUE)
mydatasetMedian$Age[is.na(mydatasetMedian$Age)]<-median(mydatasetMedian$Age,na.rm=TRUE)
mydatasetMedian$Delivery_number[is.na(mydatasetMedian$Delivery_number)]<-median(mydatasetMedian$Delivery_number,na.rm=TRUE)
mydatasetMedian$Delivery_time [is.na(mydatasetMedian$Delivery_time )]<-median(mydatasetMedian$Delivery_time ,na.rm=TRUE)
mydatasetMedian$Caesarian[is.na(mydatasetMedian$Caesarian)]<-median(mydatasetMedian$Caesarian,na.rm=TRUE)
mydatasetMedian


Age <- mydataset$Age
getAgeMode <- function(Age){
  uniqv <- unique(Age)
  uniqv[which.max(tabulate(match(Age, uniqv)))]
}
Age.mode <- getAgeMode(Age)
mydataset$Age[is.na(mydataset$Age)]<-Age.mode
mydataset

weight <- mydataset$weight.kg.
getweight.kg.Mode <- function(weight){
  uniqv <- unique(weight)
  uniqv[which.max(tabulate(match(weight, uniqv)))]
}
weight.mode <- getweight.kg.Mode(weight)
mydataset$weight.kg.[is.na(mydataset$weight.kg.)]<-weight.mode
mydataset

Delinum<- mydataset$Delivery_number 
getDelivery_numberMode <- function(Delinum){
  uniqv <- unique(Delinum)
  uniqv[which.max(tabulate(match(Delinum, uniqv)))]
}
Delinum.mode <- getDelivery_numberMode(Delinum)
mydataset$Delivery_number[is.na(mydataset$Delivery_number )]<-Delinum.mode
mydataset

Delitime<- mydataset$Delivery_time 
getDelivery_timeMode <- function(Delitime){
  uniqv <- unique(Delitime)
  uniqv[which.max(tabulate(match(Delitime, uniqv)))]
}
Delitime.mode <- getDelivery_numberMode(Delitime)
mydataset$Delivery_time[is.na(mydataset$Delivery_time )]<-Delitime.mode
mydataset

Caesar<- mydataset$Caesarian 
getCaesarianMode <- function(Caesar){
  uniqv <- unique(Caesar)
  uniqv[which.max(tabulate(match(Caesar, uniqv)))]
}
Caesar.mode <- getCaesarianMode(Caesar)
mydataset$Caesarian[is.na(mydataset$Caesarian )]<-Caesar.mode
mydataset

mean(mydataset$Age, na.rm = TRUE)
mean(mydataset$weight.kg., na.rm = TRUE)

median(mydataset$Age, na.rm = TRUE)
median(mydataset$weight.kg., na.rm = TRUE)

val <- as.numeric(mydataset$Age)
getAgeMode <- function(val){
  uniqv <- unique(val)
  uniqv[which.max(tabulate(match(val, uniqv)))]
}
Age.mode <- getAgeMode(val)
Age.mode

val <- as.numeric(mydataset$weight.kg.)
getweight.kg.Mode <- function(val){
  uniqv <- unique(val)
  uniqv[which.max(tabulate(match(val, uniqv)))]
}
weight.kg.mode <- getweight.kg.Mode(val)
weight.kg.mode


#hist(mydataset$Age, main = "Distribution of Age", xlab = "Age")

