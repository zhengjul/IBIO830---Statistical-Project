#This R analysis is for genus Danaus (Tiger milkweed butterflies) from iNaturalist
#Julia Zheng October 12, 2020

setwd("C:\\Dropbox\\Classes\\Classes\\IBIO 830 stats\\project1")
butterfly_raw <- read.csv("Danaus.csv", header = TRUE)#,row.names=1)
butterfly_raw
str(butterfly_raw)

#clean data for next task
butterfly_data <- butterfly_raw
butterfly_data <- butterfly_data[!is.na(butterfly_data$latitude),]
butterfly_data <- butterfly_data[!is.na(butterfly_data$longitude),]
butterfly_data <- butterfly_data[!is.na(butterfly_data$scientific_name),]

# SECTION 1, plotting -----------------------------------
#adding colours qualitatively with a library
if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)
butterfly_data$scientific_name <- as.factor(butterfly_data$scientific_name)
jColors <- with(butterfly_data,
                data.frame(scientific_name = levels(scientific_name),
                           color = I(brewer.pal(nlevels(scientific_name), name = 'Accent'))))

butterfly_data_scatter <- data.frame(subset(butterfly_data, select = c(latitude, longitude, scientific_name)),
           matchRetVal = match(butterfly_data$scientific_name, jColors$scientific_name))


#Let's take a look at species distribution across the world in our dataset
#plot world map
pdf("Danaus_observed_across_the_world.pdf")
if (!require(rworldmap)) install.packages('rworldmap')
library(rworldmap)
plot(getMap(), 
     xlab = "Latitude",     
     ylab = "Longitude",     
     main = 'Genus Danaus (Tiger MilkWeed Butterfly) Observed Across the World')
#add points from our dataset
library(scales)
points(butterfly_data_scatter$longitude, butterfly_data_scatter$latitude, 
       col=alpha(jColors$color[match(butterfly_data_scatter$scientific_name, jColors$scientific_name)],0.4), 
       pch=19,cex=1)
legend(x = 'bottomright', 
       legend = as.character(jColors$scientific_name),
       col = jColors$color, pch = 19, bty = 'n', cex=0.7)
dev.off()

# SECTION 2: statistical chi-squared goodness of fit -------------------------------------------
#Testing the Chi-squared goodness of fit on our categorical data describing the 4 metamorphic stage of genus Danaus butterflies
#From the following link, I learned about average duration of butterflies' metamorphic stages. 
#https://www.joyfulbutterfly.com/life-cycle-of-a-butterfly/
#I make the assumption that the following information applies for genus Danaus across the world, time, and region.
###butterfly life cycle###
#Egg:               5 days                              5/57.5 = 0.08695652
#Caterpillar:       3.5 weeks = 24.5 days               24.5/57.5 = 0.426087
#Chrysalis/Pupa:    1.5 weeks = 10.5 days               10.5/57.5 = 0.1826087
#Adult butterfly:   2.5 weeks = 17.5 days               17.5/57.5 = 0.3043478
#Total lifecycle:   5+24.5+10.5+17.5 = 57.5 days

chisq.test(table(butterfly_data$phyenotype..metamorphosis.stage.), p = c(5, 24.5, 10.5, 17.5)/57.5)
#X-squared = 1056.7, df = 3, p-value < 2.2e-16
#based on the chi-square goodness of fit test, our p-value 2.2e-16 < 0.005, strongly rejecting the null hypothesis of no fit.
#thus, I conclude that our observed dataset for genus Danaus matches with my predicted average butterfly lifecycle durations.

