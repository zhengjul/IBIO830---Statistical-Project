#This R analysis is for genus Danaus (Tiger milkweed butterflies) from iNaturalist
#Kate Skocelas October 12, 2020

# 1. Read in raw data
setwd("~/Desktop/stats/Project1/IBIO830---Statistical-Project")
butterfly_raw <- read.csv("Danaus.csv", header = TRUE)#,row.names=1)

# 2. clean data for next task
butterfly_data <- butterfly_raw
butterfly_data <- butterfly_data[!is.na(butterfly_data$phyenotype..metamorphosis.stage.),]

# 3. Explore data with basic plots
library(dplyr)
library(ggplot2)

#phenotype <- butterfly_data$phyenotype..metamorphosis.stage.
#counts <- count(butterfly_data, butterfly_data$phyenotype..metamorphosis.stage.)
#print(counts)

# Create bar plot of metamorphic stage
ggplot(butterfly_data, aes(x=phyenotype..metamorphosis.stage.))+
  geom_bar(col="Black", fill="Navy")+
  labs(x="Metamorphic Stage", y="Number of Samples", title="Metamorphic Stage of Samples")+
  theme_classic()