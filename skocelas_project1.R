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
#ggplot(butterfly_data, aes(x=phyenotype..metamorphosis.stage.))+
#  geom_bar(col="Black", fill="Navy")+
#  labs(x="Metamorphic Stage", y="Number of Samples", title="Metamorphic Stage of Samples")+
#  theme_classic()

#TODO plot stage against date of observation
butterfly_data$month <- substr(butterfly_data$observed_on, 1,2)
for(i in 1:nrow(butterfly_data)) {
  month <- butterfly_data$month[i]
  last_letter <- substr(month, 2,2)
  if(last_letter == "/") {
    month <- substr(month, 1,1)
  }
  butterfly_data$month[i] <- month
}

#read in date info in format 'm/d/y'
#butterfly_data$date <- as.Date(butterfly_data$observed_on, "%m/%d/%y")

# make a 12 value vector for each life stage where each entry is the # of observations of that life stage in that month
egg <- integer(12)
chrysalis <- integer(12)
caterpillar <- integer(12)
butterfly <- integer(12)

phenotype <- butterfly_data$phyenotype..metamorphosis.stage.
for(r in 1:nrow(butterfly_data)) {
  phenotype <- butterfly_data[r, 15]
  month <- as.numeric(butterfly_data$month[r])
  if(phenotype == "egg") {
    egg[month] <- egg[month] + 1
  }
  else if(phenotype == "chrysalis"){
    chrysalis[month] <- chrysalis[month] + 1
  }
  else if(phenotype == "caterpillar"){
    caterpillar[month] <- caterpillar[month] + 1
  }
  else if(phenotype == "butterfly"){
    butterfly[month] <- butterfly[month] + 1
  }
  else {
    print("Error: No phenotype match!")
  }
}

# make a blank plot, then add the 4 lines (1 for each life stage)
months <- c(1,2,3,4,5,6,7,8,9,10,11,12)
df <- data.frame(months, egg, chrysalis, caterpillar, butterfly)

ggplot(df, aes(months))+
  geom_line(aes(y=egg, color="orange"))+
  geom_line(aes(y=chrysalis, color="red"))+
  geom_line(aes(y=caterpillar, color="blue"))+
  geom_line(aes(y=butterfly, color="purple"))+
  labs(x="Month", y="Number of Samples", title="Metamorphic Stage of Samples by Month")+
  theme_classic()
