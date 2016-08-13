#Author: Daniel Woodrich
#d.woodrich1@gmail.com

addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

#Plots seals per tide height per season
path_Drive <- paste(readLines("~/Drive_path.txt"), collapse=" ")

DCS <- read.csv(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/R_output_DCS.csv",sep=""))

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




#######################Load packages

library(plyr)
library(stats)
library(lattice)

####################### manipulate data

#ALT: data table for: max seals per day over experiment.
Seals_exp <- as.data.frame(ddply(DCS,c("Date"),summarise,max_seals=max(num_seals,na.rm=TRUE)))
Seals_exp$Date <- as.Date(Seals_exp$Date, "%m/%d/%y")
Seals_exp <- Seals_exp[order(Seals_exp$Date),]

DCS$Date2 <- as.Date(DCS$Date2, "%Y-%m-%d")

#vector for when season changes happen
DateVec <- as.data.frame(seq(as.Date(DCS$Date2[1]), max(DCS$Date2), "day"))
names(DateVec) <- "Date" 
DateVec$Date <- format(DateVec$Date, "%m/%d/%y")
DateVec <- SeasonFromDate(DateVec)
DateVec[136,4] <- "win"
DateVec[136,5] <- 1
DateVec[1597,4] <- "win"
DateVec[1597,5] <- 1
seasonchangerow <- 0
for(n in 1:(nrow(DateVec)-1)){
  if(DateVec$Season[n+1]!=DateVec$Season[n]){
    seasonchangerow <- c(seasonchangerow,DateVec$Date[n])
  }
}
seasonchangerow <- seasonchangerow[-1]
seasonchangerow <- as.Date(seasonchangerow,"%m/%d/%y")

fall_plot <- (seasonchangerow[seq(4,length(seasonchangerow),4)])
#length(seq(from=as.Date(seasonchangerow[4]),to=as.Date(seasonchangerow[5]),'day'))/2
midfall <- (seasonchangerow[seq(4,length(seasonchangerow),4)])+45.5

win_plot <- (seasonchangerow[seq(1,length(seasonchangerow),4)])
#length(seq(from=as.Date(seasonchangerow[1]),to=as.Date(seasonchangerow[2]),'day'))/2
midwin <- (seasonchangerow[seq(1,length(seasonchangerow),4)])+45

spr_plot <- (seasonchangerow[seq(2,length(seasonchangerow),4)])
#length(seq(from=as.Date(seasonchangerow[2]),to=as.Date(seasonchangerow[3]),'day'))/2
midspr <- (seasonchangerow[seq(2,length(seasonchangerow),4)])+47

sum_plot <- (seasonchangerow[seq(3,length(seasonchangerow),4)])
#length(seq(from=as.Date(seasonchangerow[3]),to=as.Date(seasonchangerow[4]),'day'))/2
midsum <- (seasonchangerow[seq(3,length(seasonchangerow),4)])+47.5

#find observation effort over experiment
effort_tab <- as.data.frame(ddply(DCS,c("Season","SeasonNum","Year"),summarise,number_of_observations=length(unique(Observation_ID))))
effort_tab$SeasonNum <- as.numeric(effort_tab$SeasonNum)
effort_tab <- effort_tab[order(effort_tab$Year,effort_tab$SeasonNum),]
effort_tab$SeasonYear <- paste(effort_tab$Season,effort_tab$Year,sep=" ")


effort_tab <- effort_tab[order(effort_tab$SeasonNum,effort_tab$Year),]

midfall2 <- midfall-365
allmidsum <- c(midwin,midspr,midsum,midfall2,midfall[4])
#ALT: max seals per day over experiment
#par(mai=c(1.25,1.25,0.5,1.25))
#plot(max_seals ~ Date,Seals_exp, type = "p",ylab="Maximum seals/30 minutes period/observation",lwd=1.75)
#par(new=T)
#plot(num_chum ~ Date2,DCS,type = 'l',yaxt="n",ylab="",xlab="",col="blue",xaxt = "n",ylim=c(0,max(DCS$num_chum)))
#par(new=T)
#plot(num_pink ~ Date2,DCS,type = 'l',yaxt="n",ylab="",xlab="",col="pink",xaxt = "n",ylim=c(0,max(DCS$num_chum)),lwd=5)
#par(new=T)
#plot(num_steal.head ~ Date2,DCS,type = 'l',yaxt="n",ylab="",xlab="",col="brown",xaxt = "n",ylim=c(0,max(DCS$num_chum)))
#par(new=T)
#plot(num_chinook ~ Date2,DCS,type = 'l',yaxt="n",ylab="",xlab="",col="red",xaxt = "n",ylim=c(0,max(DCS$num_chum)))
#par(new=T)
#plot(num_coho ~ Date2,DCS,type = 'l',yaxt="n",ylab="",xlab="",col="green",xaxt = "n",ylim=c(0,max(DCS$num_chum)))
#par(new=T)
#plot(num_total.fish ~ Date2,DCS,type = 'l',yaxt="n",ylab="",xlab="",col="blue",xaxt = "n",ylim=c(0,max(DCS$num_chum)))
#axis(4)
#mtext(side = 4, line = 3, 'Adult salmon abundance')
#NEXT two x axes for season and year, highlight year 


#axis(1, Seals_exp$Date, format(Seals_exp$Date, "%Y"), cex.axis = .7)

#scatter.smooth(Seals_exp$Date,Seals_exp$max_seals,0.02)

#par(new=T)
#scatter.smooth(Seals_exp$Date,Seals_exp$max_seals,0.02,xaxs='i',yaxs='i',yaxt='n',ylim=c(-1,9),xaxt='n')

#convert salmon abundance to logarithmic scale 
DCS$lnum_pink <- DCS$num_pink+1
DCS$lnum_chum <- DCS$num_chum+1
DCS$lnum_coho <- DCS$num_coho+1
DCS$lnum_chinook <- DCS$num_chinook+1
DCS$lnum_steal.head <- DCS$num_steal.head+1

DCS$log_pink <- log10(DCS$lnum_pink)
DCS$log_chum <- log10(DCS$lnum_chum)
DCS$log_coho <- log10(DCS$lnum_coho)
DCS$log_chinook <- log10(DCS$lnum_chinook)
DCS$log_steal.head <- log10(DCS$lnum_steal.head)

#plot
jpeg(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R script outputs/Max seals over experiment_wFish.jpg",sep=""),width=28,height=8,units="in",res=300)

par(mai=c(1.25,1.25,0.5,1.75))

plot(log_chum ~ Date2,DCS,type = 'n',yaxt="n",ylab="",xlab="",col=addTrans("purple",200),xaxt = "n",ylim=c(0,max(DCS$log_chum)+0.5),lwd=2,xaxs='i',yaxs='i',cex=2,bty="n")

#plot boxes and lines
rect(0,0,win_plot[1],max(Seals_exp$max_seals)+0.5,col=addTrans("gray60",75),border=NA)
for(n in 1:length(fall_plot)){
  rect(win_plot[n+1],0,fall_plot[n],max(Seals_exp$max_seals)+0.5,col=addTrans("gray60",75),border=NA)
}

segments(seasonchangerow,0,seasonchangerow,max(Seals_exp$max_seals)+1,lty=5,lwd=2,col=addTrans("gray60",200))

par(new=T)
plot(log_chum ~ Date2,DCS,type = 'l',yaxt="n",ylab="",xlab="",col=addTrans("purple",200),xaxt = "n",ylim=c(0,max(DCS$log_chum)+0.5),lwd=2,xaxs='i',yaxs='i',cex=2)

par(new=T)
plot(log_pink ~ Date2,DCS,type = 'l',yaxt="n",ylab="",xlab="",col=addTrans("red",200),xaxt = "n",ylim=c(0,max(DCS$log_chum)+0.5),lwd=2,xaxs='i',yaxs='i',cex=2)


par(new=T)
plot(log_coho ~ Date2,DCS,type = 'l',yaxt="n",ylab="",xlab="",col=addTrans("darkgreen",225),xaxt = "n",ylim=c(0,max(DCS$log_chum)+0.5),lwd=2,xaxs='i',yaxs='i',cex=2)

par(new=T)
plot(log_chinook ~ Date2,DCS,type = 'l',yaxt="n",ylab="",xlab="",col=addTrans("darkorange",225),xaxt = "n",ylim=c(0,max(DCS$log_chum)+0.5),lwd=2,xaxs='i',yaxs='i',cex=2)

par(new=T)
plot(log_steal.head ~ Date2,DCS,type = 'l',yaxt="n",ylab="",xlab="",col=addTrans("darkblue",200),xaxt = "n",ylim=c(0,max(DCS$log_chum)+0.5),lwd=2,xaxs='i',yaxs='i',cex=2)
axis(4,cex.axis=2,las=1)
mtext(side = 4, line = 4.5, expression('Log'[10]*' of adult salmon abundance'),cex=2)

par(new=T)

plot(max_seals ~ Date,Seals_exp, type = "p",ylab="Maximum seals/30 minutes period/observation",pch=19,lwd=1.5,xaxt='n',xaxs='i',ylim=c(0,max(Seals_exp$max_seals)+1.5),xlab="",yaxs='i',cex.lab=2,cex.axis=2,las=1)

#plot top bar 
abline(h=max(Seals_exp$max_seals)+0.5)
text(midwin[1]-89.5,max(Seals_exp$max_seals)+1.25,labels="Fall",cex=2)

text(midfall,max(Seals_exp$max_seals)+1.25,labels="Fall",cex=2)

text(midwin,max(Seals_exp$max_seals)+1.25,labels="Win",cex=2)

text(midspr,max(Seals_exp$max_seals)+1.2,labels="Spr",cex=2)

text(midsum,max(Seals_exp$max_seals)+1.25,labels="Su",cex=2)

for(n in 1:length(allmidsum)){
  text(allmidsum[n],max(Seals_exp$max_seals)+0.75,labels=paste("n =",as.character(effort_tab$number_of_observations[n])),cex=2)
}

#expression("n ="+c(effort_tab$number_of_observations)[n])

#x axis labels
axis.Date(1, at=seq(as.Date(DCS$Date2[1]+15), max(DCS$Date2), "month"),labels = FALSE, tcl = -0.3,cex=2)

axis.Date(1, at=seq(as.Date("2012-01-01"), max(DCS$Date2), "year"),cex.axis=2)

legend(DCS$Date2[1],max(Seals_exp$max_seals)+0.5,c("Seals","Chum","Pink","Chinook","Coho","Steelhead"),ncol = 3,pch=c(19,NA,NA,NA,NA,NA),lty=c(NA,1,1,1,1,1),lwd=c(1,3,3,3,3,3),col=c("black",addTrans("purple",175),addTrans("red",175),addTrans("darkorange",200),addTrans("darkgreen",175),addTrans("darkblue",175)),cex=2)

dev.off()
#add in legend



