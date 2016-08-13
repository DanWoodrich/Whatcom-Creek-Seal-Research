#Author: Daniel Woodrich
#d.woodrich1@gmail.com

#this script will upload the DCS, format it and integrate external environmental variable.  

#General Analysis for establishing seasonality
#------------------------------------

###################################RUN TO STOP#####################
#DCS stands for data compilation sheet
#HighLow is the data sheet for tide classification and times for each date
#Sundata is the data sheet for sunrise and sunset times for 1 year (which year not important)
#SBD is seal behavior data sheet

#Order of imput: Highlow,DCS,Sundata,SBD

#packages to use later
library(ggplot2)
library(stringr)
library(grid)

#functions used later

#a function to produce a Julian Date style column based on minutes since a start day).
DateTimeToMin <- function(HighLow,Date,Time){
  for(n in 1:nrow(HighLow)){
    HighLow$DateMinutes[n] <- ((length(seq(as.Date(HighLow$Date[1],format="%m/%d/%y"),as.Date(HighLow$Date[n],format="%m/%d/%y"),by=1))-1) * 1440)
  }
  
  #time to minutes 
  HighLow$Time <- as.character(HighLow$Time)
  Mt <- strsplit(HighLow$Time,split=":")
  Mt <- as.data.frame(t(as.data.frame(Mt)))
  Mt1 <- Mt[,1]
  Mt2 <- Mt[,2]
  Mt1 <- as.character(Mt1)
  Mt2 <- as.character(Mt2)
  Mt1 <- as.numeric(Mt1)
  Mt2 <- as.numeric(Mt2)
  
  HighLow$TimeMinutes <- ((Mt1*60)+Mt2)
  HighLow$Minutes_from_start <- (HighLow$TimeMinutes + HighLow$DateMinutes)
}

#a function to perform repetitive task of making season column
SeasonFromDate <- function(x){
  b <- as.character(x$Date)
  Month <- t(as.data.frame(strsplit(b,split="/"))[1,])
  Day <- t(as.data.frame(strsplit(b,split="/"))[2,])
  x$Month_and_Day <- paste(Month,Day,sep="/")
  x$Month <- Month
  x$Season <- ""
  x$SeasonNum <- ""
  
  for(n in 1:length(x$Month_and_Day)){
    if (any(grepl(x$Month_and_Day[n],summer),na.rm=FALSE))
      x$Season[n] <- "sum"
    if (any(grepl(x$Month_and_Day[n],winter),na.rm=FALSE))
      x$Season[n] <- "win"
    if (any(grepl(x$Month_and_Day[n],fall),na.rm=FALSE))
      x$Season[n] <- "fal" 
    if (any(grepl(x$Month_and_Day[n],spring),na.rm=FALSE))
      x$Season[n] <- "spr"}
  
  for(n in 1:length(x$Month_and_Day)){
    if (any(grepl(x$Month_and_Day[n],summer),na.rm=FALSE))
      x$SeasonNum[n] <- 3
    if (any(grepl(x$Month_and_Day[n],winter),na.rm=FALSE))
      x$SeasonNum[n] <- 1
    if (any(grepl(x$Month_and_Day[n],fall),na.rm=FALSE))
      x$SeasonNum[n] <- 4 
    if (any(grepl(x$Month_and_Day[n],spring),na.rm=FALSE))
      x$SeasonNum[n] <- 2}
  
  return(x)
}
################ function to insert a row
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r+1,] <- newrow
  existingDF
}

########################
#preprocessing: 
########################

#define the file path for the hard drive
path_Drive <- "/Volumes/Seagate Backup Plus Drive/"

#save drive path, referenced in other scripts
fileConn<-file(paste("~/Drive_path.txt",sep=""))
writeLines(path_Drive, fileConn)
close(fileConn)

#define the file path for the tide data
path_Tides <- paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/HIGHLOWS_Bellingham_Bay_R_Format.CSV",sep="")


#define the file path for the DCS
path_DCS <- paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/05.05.2016 Current data compilation sheet.csv",sep="")


#define the file path for the Sunlight data
path_Sundata <- paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/Sundata_w_DST_R_edit.csv",sep="")

#define the file path for the fish count data
path_Fishdata <- paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/Fishdata_all.csv",sep="")

#to further update this, get a new dataset from Alejandro with same format
HighLow <- read.csv(path_Tides)
DCS <- read.csv(path_DCS)

HighLow$X1<-NULL
#"value" column in HighLow is in feet. 

HighLow$Minutes_from_start <- DateTimeToMin(HighLow)
DCS$Minutes_from_start <- DateTimeToMin(DCS)

HighLow$Minutes_from_DCS_start <- (HighLow$Minutes_from_start - ((length(seq(as.Date(HighLow$Date[1],format="%m/%d/%y"),as.Date(DCS$Date[1],format="%m/%d/%y"),by=1))-1) * 1440))

HighLow$Value_Type <- as.character(HighLow$Value_Type)


DCS$Tide_height_category2 <- ""



#new column with tide value (high/low)
for(n in 1:nrow(DCS)){
  DCS$Tide_height_category2[n] <- HighLow$Value_Type[which(abs(HighLow$Minutes_from_DCS_start-DCS$Minutes_from_start[n])==min(abs(HighLow$Minutes_from_DCS_start-DCS$Minutes_from_start[n])))]
}

#remove old tide column
DCS$Tide_height_category <- NULL

#replace original tide height (feet)
for(n in 1:nrow(DCS)){
  DCS$Tide_height._at_slack_ft[n] <- HighLow$Value[which(abs(HighLow$Minutes_from_DCS_start-DCS$Minutes_from_start[n])==min(abs(HighLow$Minutes_from_DCS_start-DCS$Minutes_from_start[n])))]
}

medtide <- median(DCS$Tide_height._at_slack_ft) 
#make column that bases high or low off of tide height
DCS$Water_height <- ""
DCS$Water_height[DCS$Tide_height._at_slack_ft<medtide] <- "L"
DCS$Water_height[DCS$Tide_height._at_slack_ft>medtide] <- "H"
if(length(which(DCS$Water_height=="L"))>length(which(DCS$Water_height=="H"))){
  DCS$Water_height[DCS$Tide_height._at_slack_ft==medtide] <- "H"
}else{
  DCS$Water_height[DCS$Tide_height._at_slack_ft==medtide] <- "L"
}
  

#Define bounds of seasons 

#character string for summer season
summer <- as.character(seq(as.Date("2000/6/21"),as.Date("2000/9/22"),by="day"))
summer_month <- t(as.data.frame(strsplit(summer,split="-"))[2,])
summer_day <- t(as.data.frame(strsplit(summer,split="-"))[3,])

summer <- paste(summer_month,summer_day,sep="/")

#character string for fall season
fall <- as.character(seq(as.Date("2000/9/23"),as.Date("2000/12/21"),by="day"))
fall_month <- t(as.data.frame(strsplit(fall,split="-"))[2,])
fall_day <- t(as.data.frame(strsplit(fall,split="-"))[3,])

fall <- paste(fall_month,fall_day,sep="/")

#character string for winter season
winter <- as.character(seq(as.Date("2000/12/22"),as.Date("2001/3/19"),by="day"))
winter_month <- t(as.data.frame(strsplit(winter,split="-"))[2,])
winter_day <- t(as.data.frame(strsplit(winter,split="-"))[3,])

winter <- paste(winter_month,winter_day,sep="/")

#character string for spring season, compared to Month_and_Day to make new "Season Column"
spring <- as.character(seq(as.Date("2000/3/20"),as.Date("2000/6/20"),by="day"))
spring_month <- t(as.data.frame(strsplit(spring,split="-"))[2,])
spring_day <- t(as.data.frame(strsplit(spring,split="-"))[3,])

spring <- paste(spring_month,spring_day,sep="/")

#Give DCS season column 
DCS$Date <- as.character(DCS$Date)
DCS$Year <- t(as.data.frame(strsplit(DCS$Date,split="/"))[3,])
DCS$Year <- paste("20",DCS$Year,sep="")
DCS <- SeasonFromDate(DCS)

#Give each day sunrise and sunset times:

Sundata <- read.csv(path_Sundata)

#input sunrise time (in minutes from midnight) to DCS
DCS$Sunrise <- ""
for(n in 1:length(DCS$Date)){
    DCS$Sunrise[n]<- Sundata[which(Sundata$Date==DCS$Date[n]),which(names(Sundata)=="Sunrise_time")]
}

DCS$Sunrise <- as.numeric(as.character(DCS$Sunrise))
                          
#input sunset time (in minutes from midnight) to DCS
DCS$Sunset <- ""
for(n in 1:length(DCS$Date)){
  DCS$Sunset[n]<- Sundata[which(Sundata$Date==DCS$Date[n]),which(names(Sundata)=="Sunset_time")]
}

DCS$Sunset <- as.numeric(as.character(DCS$Sunset))

#classify each observation block as belonging to a certain time period. Time period duration will get shorter with each season, which is consistent with the way twilight works (but not exact) due to the shortening of daylight hours in winter. 1 ~= dawn, 5~= dusk. Day divided into 5ths.


DCS$Time <- as.character(DCS$Time)
Mt <- strsplit(DCS$Time,split=":")
Mt <- as.data.frame(t(as.data.frame(Mt)))
Mt1 <- Mt[,1]
Mt2 <- Mt[,2]
Mt1 <- as.character(Mt1)
Mt2 <- as.character(Mt2)
Mt1 <- as.numeric(Mt1)
Mt2 <- as.numeric(Mt2)

DCS$Minute_in_day <- ((Mt1*60)+Mt2)

DCS$Time_block_inc <- ((DCS$Sunset-DCS$Sunrise)/5)

for(i in 1:nrow(DCS)){
  for(n in 1:5){
    if((DCS$Sunrise[i]+(n-1)*(DCS$Time_block_inc[i])) < DCS$Minute_in_day[i] & DCS$Minute_in_day[i] < (DCS$Sunrise[i]+(n*(DCS$Time_block_inc[i])))){
      DCS$Time_block[i] <- n
    }
  }
}
DCS$Time_block <- as.numeric(DCS$Time_block)
for(i in 1:nrow(DCS)){
  if(DCS$Time_block[i]>5){
    DCS$Time_block[i] <- 99
  }
}
DCS$Time_block <- as.character(DCS$Time_block)
for(i in 1:nrow(DCS)){
  if(DCS$Time_block[i]=="99"){
    DCS$Time_block[i] <- "Dark"
  }
}
#good check for innacurate times (not in military time errors) would be to go through all the "dark" times. test <- DCS[DCS$Time_block=="Dark",]


#reorder DCS by date (necessary for "Observation_ID " and "Minutes_from_start" column generated later):

DCS$Date2 <- as.Date(DCS$Date,format="%m/%d/%y")
DCS <- DCS[order(DCS$Date2,DCS$Minute_in_day),]

DCS$Observation_ID <- 1
DCS$Time_integer <- ((as.numeric(t(as.data.frame(strsplit(as.character(DCS$Time),split=":"))[1,]))*60) + (as.numeric(t(as.data.frame(strsplit(as.character(DCS$Time),split=":"))[2,]))))
for(n in 1:(length(DCS$Date)-1)){
  if(DCS$Date[n+1] != DCS$Date[n]){
    DCS$Observation_ID[n+1] <- DCS$Observation_ID[n]+1
  }
  else{
    DCS$Observation_ID[n+1] <- DCS$Observation_ID[n]
  }
  if((DCS$Date[n+1] == DCS$Date[n]) & ((DCS$Time_integer[n+1])-(DCS$Time_integer[n]) >= 35)){
    DCS$Observation_ID[n+1] <- DCS$Observation_ID[n]+1
  }
}


#add in fish data. 
fishdata <- read.csv(path_Fishdata)

#chum
for(n in 1:nrow(fishdata)){
  if(any(fishdata$Date[n]==DCS$Date)){
    DCS$num_chum[c(which(fishdata$Date[n]==DCS$Date))] <- fishdata$Chum[n]
  }
}

#steelhead
for(n in 1:nrow(fishdata)){
  if(any(fishdata$Date[n]==DCS$Date)){
    DCS$num_steal.head[c(which(fishdata$Date[n]==DCS$Date))] <- fishdata$Steelhead[n]
  }
}

#pink
for(n in 1:nrow(fishdata)){
  if(any(fishdata$Date[n]==DCS$Date)){
    DCS$num_pink[c(which(fishdata$Date[n]==DCS$Date))] <- fishdata$Pink[n]
  }
}

#Coho
for(n in 1:nrow(fishdata)){
  if(any(fishdata$Date[n]==DCS$Date)){
    DCS$num_coho[c(which(fishdata$Date[n]==DCS$Date))] <- fishdata$Coho[n]
  }
}

#chinook
for(n in 1:nrow(fishdata)){
  if(any(fishdata$Date[n]==DCS$Date)){
    DCS$num_chinook[c(which(fishdata$Date[n]==DCS$Date))] <- fishdata$Chinook[n]
  }
}
#Assuming days of no sampling from fish data represent fish = 0. 

DCS[which(is.na(DCS$num_chum)),names(DCS)=="num_chum"] <- 0
DCS[which(is.na(DCS$num_steal.head)),names(DCS)=="num_steal.head"] <- 0
DCS[which(is.na(DCS$num_pink)),names(DCS)=="num_pink"] <- 0
DCS[which(is.na(DCS$num_coho)),names(DCS)=="num_coho"] <- 0
DCS[which(is.na(DCS$num_chinook)),names(DCS)=="num_chinook"] <- 0

#sum fish data.
DCS$num_total.fish <- DCS$num_chum+DCS$num_steal.head+DCS$num_pink+DCS$num_coho+DCS$num_chinook

#idea: calculate shannon diversity index in another column for fish

#get presence column
DCS$num_seals <- as.character(DCS$num_seals)
DCS$num_seals <- as.numeric(DCS$num_seals)
DCS$seal_pre[DCS$num_seals>0] <- 1
DCS$seal_pre[DCS$num_seals==0] <- 0

#Check if observations are on slack tides, remove those that aren't:
tide_eff <- as.data.frame(table(DCS$Observation_ID,DCS$Tide_height_category2))

tide_eff$Freq <- as.numeric(as.character(tide_eff$Freq))
tide_eff <- tide_eff[-c(which(tide_eff$Freq==0)),]

htide <- tide_eff[which(tide_eff$Var2=="H"),]
ltide <- tide_eff[which(tide_eff$Var2=="L"),]

names(ltide) <- c("Var1","2","3")

tide_eff <- merge(htide,ltide)
removevec_tide <- as.numeric(as.character(tide_eff$Var1))

removevec_expanded <- ""
for( n in 1:length(removevec_tide)){
  removevec_expanded <- c(removevec_expanded,which(DCS$Observation_ID==removevec_tide[n]))
}

removevec_expanded <- as.numeric(removevec_expanded[2:length(removevec_expanded)])

DCS <- DCS[-removevec_expanded,]

#number of total legit observations:

total_observations <- length(unique(DCS$Observation_ID))

tide_eff <- as.data.frame(table(DCS$Observation_ID,DCS$Tide_height_category2))

tide_eff$Freq <- as.numeric(as.character(tide_eff$Freq))
tide_eff <- tide_eff[-c(which(tide_eff$Freq==0)),]

htide <- tide_eff[which(tide_eff$Var2=="H"),]
ltide <- tide_eff[which(tide_eff$Var2=="L"),]

#remove inital observation (so we can assume 30min periods), add the number of seals seen at time 0 to the first 30 minute period. 

DCS$num_fishermen <- as.character(DCS$num_fishermen)
DCS$num_fishermen <- as.numeric(DCS$num_fishermen)

DCS$num_fish_by_seals <- as.character(DCS$num_fish_by_seals)
DCS$num_fish_by_seals <- as.numeric(DCS$num_fish_by_seals)

DCS$num_fish_by_fishermen <- as.character(DCS$num_fish_by_fishermen)
DCS$num_fish_by_fishermen <- as.numeric(DCS$num_fish_by_fishermen)

removevec <- c(1)
for(n in 2:nrow(DCS)){
  if(DCS$Observation_ID[n]!=DCS$Observation_ID[n-1]){
    removevec <- c(removevec,n)
    DCS$num_seals[n] <- DCS$num_seals[n] + DCS$num_seals[n-1]
  }
}
DCS <- DCS[-removevec,]

DCS$Month <- as.factor(DCS$Month)


write.csv(DCS, paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/R_output_DCS.csv",sep=""))



