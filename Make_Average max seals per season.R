#Author: Daniel Woodrich
#d.woodrich1@gmail.com

#import DCS
path_Drive <- singleString <- paste(readLines("~/Drive_path.txt"), collapse=" ")

DCS <- read.csv(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/R_output_DCS.csv",sep=""))

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

########

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

############################

#make dataframe for plot


SealMax <- as.data.frame(aggregate(DCS$num_seals, list(DCS$Observation_ID,DCS$Date,DCS$Tide_height_category2,DCS$Tide_height._at_slack_ft), max))
names(SealMax) <- c("Ob_ID","Date","Tide_cat","Tide_value","Seals")

SealMax$Date <- as.character(SealMax$Date)
SealMax <- SeasonFromDate(SealMax)

SealSeasonSummary <- tapply(SealMax$Seals, list(SealMax$Season), mean,na.rm=TRUE)
SealSeasonSummaryOrdered <- c(SealSeasonSummary[4],SealSeasonSummary[2],SealSeasonSummary[3],SealSeasonSummary[1])


jpeg(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R script outputs/Average max seals per season.jpg",sep=""),width=9,height=9,units="in",res=300)

#barplot that compares average max seals seen per sighting over all seasons
barplot(SealSeasonSummaryOrdered, ylab="Average Maximum Seals per Observation",cex.lab=1.5,space=0,xaxt="n")
Axis(side=1,at=c(0.5,1.5,2.5,3.5),labels=c("Winter","Spring","Summer","Fall"))

dev.off()