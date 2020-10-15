#This R analysis is for genus Danaus (Tiger milkweed butterflies) from iNaturalist
#Kate Skocelas October 12, 2020

# 1. Read in raw data
setwd("~/Desktop/stats/Project1/IBIO830---Statistical-Project")
butterfly_raw <- read.csv("Danaus.csv", header = TRUE)#,row.names=1)

# 2. clean data for next task
butterfly_data <- butterfly_raw
butterfly_data <- butterfly_data[!is.na(butterfly_data$phyenotype..metamorphosis.stage.),]
butterfly_data <- butterfly_data[!is.na(butterfly_data$observed_on),]

# 3. Explore data with basic plots
library(dplyr)
library(ggplot2)

# Create bar plot of metamorphic stage
ggplot(butterfly_data, aes(x=phyenotype..metamorphosis.stage.))+
  geom_bar(col="Black", fill="Navy")+
  labs(x="Metamorphic Stage", y="Number of Samples", title="Metamorphic Stage of Samples")+
  theme_classic()

#plot stage against date of observation
butterfly_data$month <- substr(butterfly_data$observed_on, 1,2)
for(i in 1:nrow(butterfly_data)) {
  month <- butterfly_data$month[i]
  last_letter <- substr(month, 2,2)
  if(last_letter == "/") {
    month <- substr(month, 1,1)
  }
  butterfly_data$month[i] <- month
}

phenotype <- butterfly_data$phyenotype..metamorphosis.stage.
butterfly_data$month <- factor(butterfly_data$month, levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))

ggplot(butterfly_data, aes(x=month, fill=phenotype))+
  geom_bar()+
  labs(x="Month", y="Number of Samples", title="Metamorphic Stage of Samples by Month")+
  theme_classic()
