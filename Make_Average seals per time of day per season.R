#Author: Daniel Woodrich
#d.woodrich1@gmail.com

#import DCS
path_Drive <- paste(readLines("~/Drive_path.txt"), collapse=" ")

DCS <- read.csv(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/R_output_DCS.csv",sep=""))

library(ggplot2)
library(plyr)
library(lattice)
library(grid)
library(Rmisc)

Seals_day_sea <- as.data.frame(ddply(DCS,c("Season","SeasonNum","Time_block"),summarise,avg_seals=mean(num_seals,na.rm=TRUE),N    = length(is.na(num_seals)==FALSE),mean = mean(num_seals,na.rm=TRUE),sd   = sd(num_seals,na.rm=TRUE),se   = sd / sqrt(N)))
Seals_day_sea <- Seals_day_sea[Seals_day_sea$Time_block!="Dark",] 

Seals_day_sea$Season <- factor(Seals_day_sea$Season,levels = c("win","spr", "sum", "fal"))
levels(Seals_day_sea$Season) <- c("Winter", "Spring", "Summer","Fall")

Seals_day_sea$Time_block <- factor(Seals_day_sea$Time_block,levels = c("1","2", "3", "4","5"))
levels(Seals_day_sea$Time_block) <- c("EM", "LM", "MD","LA","EV")
#remove row with "dark" column (it is innacurately placed after #5 with respect to the figure: it really could be early morning too). Remove "dark" name so that it will not show up in graph

Seals_day_sea[11,4] <- NA

Seals_test <-Seals_day_sea[6:15,]
Seals_test <- rbind(Seals_day_sea[16:20,],Seals_test,Seals_day_sea[1:5,])

#line version
plot1 <- ggplot(Seals_day_sea, aes(x=factor(Time_block), y=avg_seals)) +
  geom_point(aes(group=Season),size=2)+
  geom_line(aes(group=Season)) + facet_grid(.~Season) +
  geom_errorbar(aes(ymin=avg_seals-se, ymax=avg_seals+se), width=.2)+
  xlab(expression('Time of day')) + 
  ylab("Average seals/30 minutes period") +
  theme_bw() +
  theme(text = element_text(size=25)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(size=6,aes(label=N, y=(Seals_test$mean+Seals_test$se+0.1)))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 2.3))

plot1

jpeg(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R script outputs/Average seals per time of day per season.jpg",sep=""),width=12,height=8,units="in",res=300)

plot1

dev.off()