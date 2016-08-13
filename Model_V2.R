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

#ggpairs(DCS [,c("num_chum","num_coho","num_chinook","num_pink","num_steal.head","num_seals","Month","Season")])



#qqp(DCS$num_seals,"norm")
#qqp(DCS$num_seals,"lnorm")

#following functions don't allow NAs in data
DCS_NNA <- DCS[which(is.na(DCS$num_seals)==FALSE),]
DCS_NNA[44,names(DCS)=="num_seals"] <- 3

#character string for fish season
fish_sea <- as.character(seq(as.Date("2000/8/19"),as.Date("2001/2/15"),by="day"))
fish_mon <- t(as.data.frame(strsplit(fish_sea,split="-"))[2,])
fish_day <- t(as.data.frame(strsplit(fish_sea,split="-"))[3,])

fall <- paste(fish_mon,fish_day,sep="/")

DCS_NNA$Date <- as.character(DCS_NNA$Date)
Month <- t(as.data.frame(strsplit(DCS_NNA$Date,split="/"))[1,])
Day <- t(as.data.frame(strsplit(DCS_NNA$Date,split="/"))[2,])
DCS_NNA$Month_and_Day <- paste(Month,Day,sep="/")

DCS$fish_season <- ""
for(n in 1:length(DCS_NNA$Month_and_Day)){
if (any(grepl(DCS_NNA$Month_and_Day[n],fall),na.rm=FALSE))
  DCS_NNA$fish_seasons[n] <- "fal"
if (any(grepl(DCS_NNA$Month_and_Day[n],fall),na.rm=FALSE)==FALSE)
  DCS_NNA$fish_seasons[n] <- "other"
}

#traditional season ranges (solstice)
DCS_Fall <- DCS_NNA[DCS_NNA$Season=="fal",]
DCS_3Sea <- DCS_NNA[DCS_NNA$Season!="fal",]

#Fish season (fish or no fish, ranges set on historic presence)
#DCS_Fall <- DCS_NNA[DCS_NNA$fish_seasons=="fal",]
#DCS_3Sea <- DCS_NNA[DCS_NNA$fish_seasons!="fal",]


DCS_Fall$Year <- as.character(DCS_Fall$Year)
DCS_Fall$Year <- as.numeric(DCS_Fall$Year)
DCS_Fall_O <- DCS_Fall[DCS_Fall$Year%%2==1,]
DCS_Fall_E <- DCS_Fall[DCS_Fall$Year%%2==0,]
DCS_Fall_O$Year <- as.factor(DCS_Fall_O$Year)
DCS_Fall_E$Year <- as.factor(DCS_Fall_E$Year)

DCS_Fall_O$E_O_Year <- "Odd"
DCS_Fall_E$E_O_Year <- "Even"
DCS_Fall <- rbind(DCS_Fall_O,DCS_Fall_E)
DCS$Month <- as.factor(DCS$Month)
DCS$Year <- as.factor(DCS$Year)

#all year long
DCS_NNA$Year <- as.character(DCS_NNA$Year)
DCS_NNA$Year <- as.numeric(DCS_NNA$Year)
DCS_NNA_O <- DCS_NNA[DCS_NNA$Year%%2==1,]
DCS_NNA_E <- DCS_NNA[DCS_NNA$Year%%2==0,]
DCS_NNA_O$Year <- as.factor(DCS_NNA_O$Year)
DCS_NNA_E$Year <- as.factor(DCS_NNA_E$Year)

#New round: just fall season

# only month
#839.8
fit1a <- glmer(DCS_Fall_O$num_seals ~ Month + (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit1a)

#716.5
fit1b <- glmer(DCS_Fall_E$num_seals ~ Month + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit1b)

# month and time of day
#814.9
fit2a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit2a)


#693.4
fit2b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit2b)

#explore potential differences between years

#relatively close
barchart(num_seals~E_O_Year,data=DCS_Fall,groups=Time_block)

#more pronounced difference in Odd years
barchart(num_seals~E_O_Year,data=DCS_Fall,groups=Tide_height_category2)

#Tide height category

#785.2 : big enough change (>1%)
fit3a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit3a)

#690.2 : Not big enough change to count (<1%)
fit3b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block + Tide_height_category2 + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit3b)

#Fish (pink)

#786.2 (model got worse with addition of pink)
fit4a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_pink+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit4a)

# (rank defficient column not included)
fit4b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_pink + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit4b)

#Fish (Chinook)

#786.9 (got worse)
fit5a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_chinook+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit5a)

#694.7 (got worse)
fit5b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_chinook + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit5b)

#Fish (Coho)

#787.1 (got worse)
fit6a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_coho+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit6a)

#695.4 (got worse)
fit6b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_coho + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit6b)

#Fish (Steelhead)

#786.2 (got worse)
fit7a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_steal.head+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit7a)

#693.7 (got worse)
fit7b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_steal.head + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit7b)

#Fish (Chum)

#782.6 (slightly better, but not >1% better) (eigenvalue warning)
fit8a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_chum+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit8a)

#695.4 (got worse) (eigenvalue warning)
fit8b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_chum + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit8b)

#Fish (all)

#786.0 (got worse) (eigenvalue warning) (chum only significan fish var)
fit9a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_chum+num_chinook+num_steal.head+num_coho+num_pink+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit9a)

#699.0 (got worse) (eigenvalue warning) (No significant fish)
fit9b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_chum+num_chinook+num_steal.head+num_coho + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit9b)

#best model for fall season of each year:

#785.2 : 3 points lower than model that included chum. 
fit3a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit3a)

#693.4 : emperically best model
fit2b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit2b)

#Take away: only abiotic factors! Could be due to the fact that there is response lab in seal presence after peaks of fish runs that aren't accounted for in daily counts. Rows corresponding to weekly averages of seals/fish might be more telling. Splitting this data up also decreases predicting power of fish count variable, which are continuous and have zeros.Fact that fish counts variables are good predictors in overall model may be due to colinearity or better predictive power due to higher instances.  
#variables to test in future? (Guesses) 1. water temp 2. river flow 3. phase of moon . Literature would help inform selections.


#change season 

#modify season ranges

# only month
#1013.1 (convergence warning)
fit1a <- glmer(DCS_Fall_O$num_seals ~ Month + (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit1a)

#955.7
fit1b <- glmer(DCS_Fall_E$num_seals ~ Month + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit1b)

# month and time of day

#980.2 (convergence warning)
fit2a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit2a)


#938.8
fit2b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit2b)

#explore potential differences between years

#relatively close
barchart(num_seals~E_O_Year,data=DCS_Fall,groups=Time_block)

#more pronounced difference in Odd years
barchart(num_seals~E_O_Year,data=DCS_Fall,groups=Tide_height_category2)

#Tide height category

#945.2 : big enough change (>1%) (convergence warning)
fit3a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit3a)

#937.3 : Not big enough change to count (<1%)
fit3b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block + Tide_height_category2 + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit3b)

#Fish (pink)

#944.3 (model got only slightly better with addition of pink) (convergence warning) (pink not significant)
fit4a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_pink+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit4a)

# (rank defficient column not included)
fit4b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_pink + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit4b)

#Fish (Chinook)

#946.4 (got worse)
fit5a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_chinook+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit5a)

#940.8 (got worse)
fit5b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_chinook + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit5b)

#Fish (Coho)

#947.2 (got worse)
fit6a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_coho+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit6a)

#940.8 (got worse)
fit6b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_coho + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit6b)

#Fish (Steelhead)

#943.9 (got slightly better)
fit7a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_steal.head+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit7a)

#935.2 (got slightly better)
fit7b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_steal.head + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit7b)

#Fish (Chum)

#942.2 (slightly better, but not >1% better) (eigenvalue warning)
fit8a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_chum+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit8a)

#940.7  (got worse) (eigenvalue warning)
fit8b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_chum + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit8b)

#Fish (all)

#944.4 (got worse) (eigenvalue warning) (chum only significan fish var)
fit9a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + num_chum+num_chinook+num_steal.head+num_coho+num_pink+ (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit9a)

#941.0 (got worse) (eigenvalue warning) (No significant fish)
fit9b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block  + num_chum+num_chinook+num_steal.head+num_coho + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit9b)

#best model for fall season of each year:

#945.2 : 3 points lower than model that included chum. 
fit3a <- glmer(DCS_Fall_O$num_seals ~ Month + Time_block + Tide_height_category2 + (1|Year), data=DCS_Fall_O,family=poisson(link = "log"))
summary(fit3a)

#938.8 : emperically best model
fit3b <- glmer(DCS_Fall_E$num_seals ~ Month + Time_block + Tide_height_category2 + (1|Year), data=DCS_Fall_E,family=poisson(link = "log"))
summary(fit3b)

# same take away: pink in odd years is nonsignificant factor, steelhead in even years. Mostly unimportant. 





#fish comparison with whole year dataset:

# 1154.2
fit3a <- glmer(DCS_NNA_O$num_seals ~  Month +(1|Year), data=DCS_NNA_O,family=poisson(link = "log"))
summary(fit3a)

# 1107.2
fit3b <- glmer(DCS_NNA_E$num_seals ~  Month + (1|Year), data=DCS_NNA_E,family=poisson(link = "log"))
summary(fit3b)



#Fish (pink)

#1145.4 (convergence warning)
fit4a <- glmer(DCS_NNA_O$num_seals ~  Month + num_pink+ (1|Year), data=DCS_NNA_O,family=poisson(link = "log"))
summary(fit4a)

# (rank defficient column not included)
fit4b <- glmer(DCS_NNA_E$num_seals ~  Month + num_pink + (1|Year), data=DCS_NNA_E,family=poisson(link = "log"))
summary(fit4b)

#Fish (Chinook)

# 1153.2 
fit5a <- glmer(DCS_NNA_O$num_seals ~ Month + num_chinook+ (1|Year), data=DCS_NNA_O,family=poisson(link = "log"))
summary(fit5a)

#1109.1 
fit5b <- glmer(DCS_NNA_E$num_seals ~ Month + num_chinook + (1|Year), data=DCS_NNA_E,family=poisson(link = "log"))
summary(fit5b)

#Fish (Coho)

# 1155.2 
fit6a <- glmer(DCS_NNA_O$num_seals ~ Month  + num_coho+ (1|Year), data=DCS_NNA_O,family=poisson(link = "log"))
summary(fit6a)

#1108.8 (got worse)
fit6b <- glmer(DCS_NNA_E$num_seals ~ Month  + num_coho + (1|Year), data=DCS_NNA_E,family=poisson(link = "log"))
summary(fit6b)

#Fish (Steelhead)

#1155.4 
fit7a <- glmer(DCS_NNA_O$num_seals ~ Month  + num_steal.head+ (1|Year), data=DCS_NNA_O,family=poisson(link = "log"))
summary(fit7a)

#1101.7 (got slightly better)
fit7b <- glmer(DCS_NNA_E$num_seals ~ Month + num_steal.head + (1|Year), data=DCS_NNA_E,family=poisson(link = "log"))
summary(fit7b)

#Fish (Chum)

#1147.9 
fit8a <- glmer(DCS_NNA_O$num_seals ~ Month  + num_chum+ (1|Year), data=DCS_NNA_O,family=poisson(link = "log"))
summary(fit8a)

#1109.1 
fit8b <- glmer(DCS_NNA_E$num_seals ~ Month + num_chum + (1|Year), data=DCS_NNA_E,family=poisson(link = "log"))
summary(fit8b)

#Fish (all)

#1139.2 
fit9a <- glmer(DCS_NNA_O$num_seals ~ Month + num_chum+num_chinook+num_steal.head+num_coho+num_pink+ (1|Year), data=DCS_NNA_O,family=poisson(link = "log"))
summary(fit9a)

#1107.3
fit9b <- glmer(DCS_NNA_E$num_seals ~ Month   + num_chum+num_chinook+num_steal.head+num_coho + (1|Year), data=DCS_NNA_E,family=poisson(link = "log"))
summary(fit9b)

#1139.2
fit10a <- glmer(DCS_NNA_O$num_seals ~ Month  + num_chum+ num_pink + (1|Year), data=DCS_NNA_O,family=poisson(link = "log"))
summary(fit10a)

cor.test(DCS_NNA_O$num_seals,DCS_NNA_O$num_chum)
cor.test(DCS_NNA_O$num_seals,DCS_NNA_O$num_pink)
cor.test(DCS_NNA_O$num_chum,DCS_NNA_O$num_pink)
