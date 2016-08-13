#Author: Daniel Woodrich
#d.woodrich1@gmail.com

#import DCS
path_Drive <- paste(readLines("~/Drive_path.txt"), collapse=" ")

DCS <- read.csv(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/R_output_DCS.csv",sep=""))
#Find the average max amount of seals per observation in fall
DCSfall <- subset(DCS, DCS$Season == "fal")

FallMax <- as.data.frame(aggregate(DCSfall$num_seals, list(DCSfall$Observation_ID,DCSfall$Date,DCSfall$Year), max, na.rm=TRUE))
names(FallMax) <- c("Ob_ID","Date","Year","Seals")
SealFallSummary <- tapply(FallMax$Seals, list(FallMax$Year), mean,na.rm=TRUE)
#Average seals per observation for fall of each year
barplot(SealFallSummary, ylab="Average Maximum Seals per Observation",cex.lab=1.5,xlab="Fall of year ___",space=0)
abline(h=0)

jpeg(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R script outputs/Average max seals per year (fall season).jpg",sep=""),width=12,height=8,units="in",res=300)


barplot(SealFallSummary, ylab="Average Maximum Seals per Observation",cex.lab=1.5,xlab="Fall of year ___",space=0)
abline(h=0)

dev.off()
