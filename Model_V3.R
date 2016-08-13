#Author: Daniel Woodrich
#d.woodrich1@gmail.com

#Making models

#DCS <- read.csv("E:/Whatcom Creek Project/Scripts and Analysis/R scipt csv files/R_output_DCS.csv")

path_Drive <- paste(readLines("~/Drive_path.txt"), collapse=" ")

DCS <- read.csv(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/R_output_DCS.csv",sep=""))
DCS$Month <- as.factor(DCS$Month)
DCS$Year <- as.factor(DCS$Year)

#draw pois dist over to check for too many zeros: really looks like a lot of zeros. 

#create model: 

library(lme4)
library(GGally)
library(ggplot2)
library(grid)
library(car)
library(MASS)
library(lattice)

DCS_NNA <- DCS[which(is.na(DCS$num_seals)==FALSE),]
DCS_NNA[44,names(DCS)=="num_seals"] <- 3


DCS_Fall <- DCS_NNA[DCS_NNA$Season=="fal",]
DCS_3Sea <- DCS_NNA[DCS_NNA$Season!="fal",]

# only month

#708.4
fit1 <- glmer(DCS_3Sea$num_seals ~ Month + (1|Year), data=DCS_3Sea,family=poisson(link = "log"))
summary(fit1)

#only season 

#741.7
fit2 <- glmer(DCS_3Sea$num_seals ~ Season + (1|Year), data=DCS_3Sea,family=poisson(link = "log"))
summary(fit2)

# Month is better

#add other variables

#702.1
fit3 <- glmer(DCS_3Sea$num_seals ~ Month + Time_block + (1|Year), data=DCS_3Sea,family=poisson(link = "log"))
summary(fit3)

#679.7
fit4 <- glmer(DCS_3Sea$num_seals ~ Month + Time_block + Tide_height_category2 + (1|Year), data=DCS_3Sea,family=poisson(link = "log"))
summary(fit4)

#680.9 (better to remove time_block)
fit5 <- glmer(DCS_3Sea$num_seals ~ Month +  Tide_height_category2 + (1|Year), data=DCS_3Sea,family=poisson(link = "log"))
summary(fit5)

#Take away: tide hieght, but not time of day predicts seal presence. Could time of day be tied to salmon behavior? (no way to tell from our data, only daily counts)


#1555.7
fit1b <- glmer(DCS_Fall$num_seals ~ Month + (1|Year), data=DCS_Fall,family=poisson(link = "log"))
summary(fit1b)

#only season 

#Error
fit2b <- glmer(DCS_Fall$num_seals ~ Season + (1|Year), data=DCS_Fall,family=poisson(link = "log"))
summary(fit2b)

#add other variables

#1511.3
fit3b <- glmer(DCS_Fall$num_seals ~ Month + Time_block + (1|Year), data=DCS_Fall,family=poisson(link = "log"))
summary(fit3b)

#1509.8
fit4b <- glmer(DCS_Fall$num_seals ~ Month + Time_block + Tide_height_category2 + (1|Year), data=DCS_Fall,family=poisson(link = "log"))
summary(fit4b)

#1555.4
fit5b <- glmer(DCS_Fall$num_seals ~ Month +  Tide_height_category2 + (1|Year), data=DCS_Fall,family=poisson(link = "log"))
summary(fit5b)

#take away: in fall, seal abundance predicted by time block, but not by tide height

