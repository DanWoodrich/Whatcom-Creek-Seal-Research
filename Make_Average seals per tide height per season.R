#Author: Daniel Woodrich
#d.woodrich1@gmail.com

#inport DCS
path_Drive <- paste(readLines("~/Drive_path.txt"), collapse=" ")

DCS <- read.csv(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis
/R scipt csv files/R_output_DCS.csv",sep=""))

library(ggplot2)
library(plyr)
library(lattice)
library(grid)
library(Rmisc)

#Could make an argument for averaging max seals per observation instead of averaging max seals per 30 minute period (as is done below).   


#Data table for: Seals per tide class, per season
tide_tab <- as.data.frame(ddply(DCS,c("Season","Tide_height_category2"),summarise,max_seals=mean(num_seals,na.rm=TRUE),N    = length(is.na(num_seals)==FALSE),mean = mean(num_seals,na.rm=TRUE),sd   = sd(num_seals,na.rm=TRUE),se   = sd / sqrt(N)))
tide_tab$Season <- factor(tide_tab$Season,levels = c("win","spr", "sum", "fal"))
levels(tide_tab$Season) <- c("Winter", "Spring", "Summer","Fall")
tide_tab$Tide_height_category2 <- factor(tide_tab$Tide_height_category2,levels = c("H","L"))
levels(tide_tab$Tide_height_category2) <- c("High","Low")

Tide_test <-tide_tab[3:6,]
Tide_test <- rbind(tide_tab[7:8,],Tide_test,tide_tab[1:2,])

#Seals per tide height, per season
plot1 <- ggplot(tide_tab, aes(x=factor(Tide_height_category2), y=max_seals)) +
  geom_bar(stat="identity",fill="gray60",width=1,colour="gray80") + facet_grid(.~Season) +
  geom_errorbar(aes(ymin=mean+se, ymax=mean+se), width=.2)+
  geom_linerange(aes(ymin = mean, ymax = mean+se)) +
  xlab("Tide height category") + ylab("Average seals/30 minutes period")+
  theme_bw() + 
  theme(text = element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 2))+
  geom_text(size=6,aes(label=N, y=(Tide_test$mean+Tide_test$se+0.1)))

jpeg(paste(path_Drive,"/Whatcom Creek Project/Scripts and Analysis/R script outputs/Average seals per tide height per season.jpg",sep=""),width=13.5,height=5.5,units="in",res=300)

plot1

dev.off()