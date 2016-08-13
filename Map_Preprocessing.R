#Map Pre-processing

#Written by Nolan Newman and Daniel Woodrich
#d.woodrich1@gmail.com

########################################################################################################################################################################

#Obtain excel sheets
#SBD <- read.csv("H:/Whatcom Creek Seals/Scripts and Analysis/R scipt csv files/SBD.csv")

path_Drive <- paste(readLines("~/Drive_path.txt"), collapse=" ")

SBD <- read.csv(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/Current Seal behavior data Nolan R 2-18-2016.csv",sep=""))

DCS <- read.csv(paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R scipt csv files/R_output_DCS.csv",sep=""))

#Makes season column
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

      #Character string for summer season
      summer <- as.character(seq(as.Date("2000/6/21"),as.Date("2000/9/22"),by="day"))
      summer_month <- t(as.data.frame(strsplit(summer,split="-"))[2,])
      summer_day <- t(as.data.frame(strsplit(summer,split="-"))[3,])
      summer <- paste(summer_month,summer_day,sep="/")
      
      #Character string for fall season
      fall <- as.character(seq(as.Date("2000/9/23"),as.Date("2000/12/21"),by="day"))
      fall_month <- t(as.data.frame(strsplit(fall,split="-"))[2,])
      fall_day <- t(as.data.frame(strsplit(fall,split="-"))[3,])
      fall <- paste(fall_month,fall_day,sep="/")
      
      #Character string for winter season
      winter <- as.character(seq(as.Date("2000/12/22"),as.Date("2001/3/19"),by="day"))
      winter_month <- t(as.data.frame(strsplit(winter,split="-"))[2,])
      winter_day <- t(as.data.frame(strsplit(winter,split="-"))[3,])
      winter <- paste(winter_month,winter_day,sep="/")
      
      #Character string for spring season, compared to Month_and_Day to make new "Season Column"
      spring <- as.character(seq(as.Date("2000/3/20"),as.Date("2000/6/20"),by="day"))
      spring_month <- t(as.data.frame(strsplit(spring,split="-"))[2,])
      spring_day <- t(as.data.frame(strsplit(spring,split="-"))[3,])
      spring <- paste(spring_month,spring_day,sep="/")
      
#Give SBD a season column
      SBD$Date <- as.character(SBD$Date)
      SBD$Year <- t(as.data.frame(strsplit(SBD$Date,split="/"))[3,])
      SBD$Year <- paste("20",SBD$Year,sep="")
      SBD <- SeasonFromDate(SBD)

#Binary system for location, used to add up total
      SBD$Upper_River_BI <- ""
      SBD$Lower_River_BI <- ""
      SBD$Mid_River_BI <- ""
      SBD$Stairs_Eddy_BI <- ""
      
      #change to grep style parsing on wednesday May 18th! test <- as.data.frame(table(SBD$Location))
      SBD$Upper_River_BI[grepl("up", SBD$Location, ignore.case = TRUE) ] <- 1
      SBD$Upper_River_BI[SBD$Upper_River_BI!=1] <- 0
      
      SBD$Mid_River_BI[grepl("mid", SBD$Location, ignore.case = TRUE) ] <- 1
      SBD$Mid_River_BI[SBD$Mid_River_BI!=1] <- 0
      
      SBD$Lower_River_BI[grepl("low", SBD$Location, ignore.case = TRUE) ] <- 1
      SBD$Lower_River_BI[SBD$Lower_River_BI!=1] <- 0
      
      SBD$Stairs_Eddy_BI[grepl("ed", SBD$Location, ignore.case = TRUE) ] <- 1
      SBD$Stairs_Eddy_BI[SBD$Stairs_Eddy_BI!=1] <- 0
      
      
#Find the sum of the number of binary number of seals that surfaced at each location in the creek
      #This code takes how many seals are in the num_seals column in the DCS and the amount of seals seen per season,
      #then adds them up and gives the total amount of seals seen per season 
      SBD$Upper_River_BI <- as.numeric(SBD$Upper_River_BI) 
      Sum_UpperRiver_SBD <- aggregate(SBD$Upper_River_BI,list(SBD$Season), sum, na.rm = TRUE)
      UR_Sum <- as.data.frame(t(Sum_UpperRiver_SBD))
      #Number of times seals popped up in the season in upper river
          
      SBD$Mid_River_BI <- as.numeric(SBD$Mid_River_BI)
      Sum_MidRiver_SBD <- aggregate(SBD$Mid_River_BI,list(SBD$Season), sum, na.rm = TRUE)
      MR_Sum <- as.data.frame(t(Sum_MidRiver_SBD))
      #Number of times seals popped up in the season in mid river
          
      SBD$Lower_River_BI <- as.numeric(SBD$Lower_River_BI)
      Sum_LowerRiver_SBD <- aggregate(SBD$Lower_River_BI,list(SBD$Season), sum, na.rm = TRUE)
      LR_Sum <- as.data.frame(t(Sum_LowerRiver_SBD))
      #Number of times seals popped up in the season in lower river
          
      SBD$Stairs_Eddy_BI <- as.numeric(SBD$Stairs_Eddy_BI)
      Sum_StairsEddy_SBD <- aggregate(SBD$Stairs_Eddy_BI,list(SBD$Season), sum, na.rm = TRUE)
      SE_Sum <- as.data.frame(t(Sum_StairsEddy_SBD))
      #Number of times seals popped up in the season in stairs eddy
          
          
#Calculates the sum of how many surfacing events per season over the course of the WHOLE project and stores as a data frame
      mergeDF <- rbind(SE_Sum, UR_Sum, MR_Sum, LR_Sum)
      mergeDF <- mergeDF[-c(1,3,5,7),]
      mergeDF$V1 <- as.numeric(as.character(mergeDF$V1))
      mergeDF$V2 <- as.numeric(as.character(mergeDF$V2))
      mergeDF$V3 <- as.numeric(as.character(mergeDF$V3))
      mergeDF$V4 <- as.numeric(as.character(mergeDF$V4))
          
          
      propSum <- as.data.frame(c(sum(as.numeric(as.character(mergeDF$V1))), sum(as.numeric(as.character(mergeDF$V2))), sum(as.numeric(as.character(mergeDF$V3))), sum(as.numeric(as.character(mergeDF$V4)))))
      propSum <- t(propSum)
      surfacingEvents <- rbind(mergeDF,propSum)
      
#Calculate the proportion of surfacing events in each part of the river
      VecSE <- ""
        for (x in 1:length(surfacingEvents)){
            num <- surfacingEvents[1,x] / surfacingEvents[5,x]
            VecSE <- c(VecSE, num)
        }
      VecSE <- t(as.data.frame(VecSE))
          
          
      VecUR <- ""
        for (x in 1:length(surfacingEvents)){
          num <- surfacingEvents[2,x] / surfacingEvents[5,x]
          VecUR <- c(VecUR, num)
        }
      VecUR <- t(as.data.frame(VecUR))
          
          
      VecMR <- ""
        for (x in 1:length(surfacingEvents)){
          num <- surfacingEvents[3,x] / surfacingEvents[5,x]
          VecMR <- c(VecMR, num)
        }
      VecMR <- t(as.data.frame(VecMR))
        
      VecLR <- ""
        for (x in 1:length(surfacingEvents)){
          num <- surfacingEvents[4,x] / surfacingEvents[5,x]
          VecLR <- c(VecLR, num)
        }
      VecLR <- t(as.data.frame(VecLR))

#Add the proportion data to the surfacingEvents data frame
      surfacingEvents <- rbind(mergeDF)
      colnames(propSum) <- c("Fall", "Win", "Spring", "Sum")
      colnames(surfacingEvents) <- c("Fall", "Win", "Spring", "Sum")
      rownames(surfacingEvents) <- c("SE", "UR", "MR", "LR")
      surfacingEvents <- rbind(surfacingEvents, propSum)
      rownames(surfacingEvents) <- c("SE", "UR", "MR", "LR", "Total")
          
#make a data frame for the proportion of surfacing events of all years for each season
      propSurfacingEvents <- rbind(VecSE, VecUR, VecMR, VecLR)
      colnames(propSurfacingEvents) <- c("x", "Fall", "Win", "Spring", "Sum")
      rownames(propSurfacingEvents) <- c("Proportion of surfacing events SE", "Proportion of surfacing events UR", "Proportion of surfacing events MR", "Proportion of surfacing events LR")
          
#Combine the three data frames
      Table1 <- rbind(surfacingEvents, propSurfacingEvents[,2:5])
      propSurfacingEvents <- t(propSurfacingEvents)
    
      write.csv(Table1, paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R script outputs/Table1.csv",sep=""))
####################################################################################################################
####################################################################################################################          
              #This concludes the first approach. I hope you've enjoyed your stay
####################################################################################################################
####################################################################################################################  
      
      #Add the proportion data to the surfacingEvents data frame
      surfacingEvents2 <- rbind(mergeDF)
      colnames(propSum) <- c("Fall", "Spring", "Win", "Sum")
      colnames(surfacingEvents2) <- c("Fall", "Spring", "Win", "Sum")
      rownames(surfacingEvents2) <- c("SE", "UR", "MR", "LR")
      surfacingEvents2 <- rbind(surfacingEvents2, propSum)
      rownames(surfacingEvents2) <- c("SE", "UR", "MR", "LR", "Total")
      

          
#This second approach will calculate the number of surfacing events per seal at each location in the river
      #Find the (approximate) number of seals that appeared in the season using the DCS
      Sum_of_Seals_DCS <- as.data.frame(aggregate(DCS$num_seals,list(DCS$Season), sum, na.rm = TRUE))
      Sum_of_Seals_DCS <- rbind(Sum_of_Seals_DCS[1,],Sum_of_Seals_DCS[2,],Sum_of_Seals_DCS[4,],Sum_of_Seals_DCS[3,])
      
      
      Sum_of_Seals_DCS <- t(Sum_of_Seals_DCS)
      colnames(Sum_of_Seals_DCS) <- c("Fall", "Spring", "Win", "Sum")
          
      #Combine the surfacing events data frame and sum from DCS together
      perSeal <- rbind(surfacingEvents2[1:4,], Sum_of_Seals_DCS)
      perSeal$Fall <- as.numeric(perSeal$Fall)
      perSeal$Win <- as.numeric(perSeal$Win)
      perSeal$Spring <- as.numeric(perSeal$Spring)
      perSeal$Sum <- as.numeric(perSeal$Sum)
      perSeal <- perSeal[-c(5),]
      rownames(perSeal) <- c("Total Surfacing Events SE", "Total Surfacing Events UR", "Total Surfacing Events MR", "Total Surfacing Events LR", "Total number of seals per season")
          
          
#Calculate the number of surfacing events per seal in each part of the river
      VecSE2 <- ""
        for (x in 1:length(surfacingEvents2)){
          num <- perSeal[1,x] / perSeal[5,x]
          VecSE2 <- c(VecSE2, num)
        }
      VecSE2 <- t(as.data.frame(VecSE2))
          
          
      VecUR2 <- ""
        for (x in 1:length(surfacingEvents2)){
          num <- perSeal[2,x] / perSeal[5,x]
          VecUR2 <- c(VecUR2, num)
        }
      VecUR2 <- t(as.data.frame(VecUR2))
          
          
      VecMR2 <- ""
        for (x in 1:length(surfacingEvents2)){
          num <- perSeal[3,x] / perSeal[5,x]
          VecMR2 <- c(VecMR2, num)
        }
      VecMR2 <- t(as.data.frame(VecMR2))
          
      VecLR2 <- ""
        for (x in 1:length(surfacingEvents2)){
          num <- perSeal[4,x] / perSeal[5,x]
          VecLR2 <- c(VecLR2, num)
        }
      VecLR2 <- t(as.data.frame(VecLR2))
          
      
      eventsPerSeal <- rbind(VecSE2, VecUR2, VecMR2, VecLR2)
      colnames(eventsPerSeal) <- c("", "Fall", "Spring", "Win", "Sum")
      rownames(eventsPerSeal) <- c("Surfacing events per seal SE", "Surfacing events per seal UR", "Surfacing events per seal MR", "Surfacing events per seal LR")

      #Create table 2. This will serve as the end of the second part and be hopefully the most useful data            
      Table2 <- rbind(surfacingEvents2[1:4,], Sum_of_Seals_DCS[2,], eventsPerSeal[,2:5])
     
 
      Table2 <-cbind(Table2$Fall,Table2$Win,Table2$Spring,Table2$Sum)
      rownames(Table2) <- c("SE", "UR", "MR", "LR", "Sum", "Surfacing events per seal SE", "Surfacing events per seal UR", "Surfacing events per seal MR", "Surfacing events per seal LR")
      colnames(Table2) <- c("Fall","Winter","Spring","Summer")
      write.csv(Table2, paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R script outputs/Table2.csv",sep=""))
####################################################################################################################
####################################################################################################################          
                #This concludes the second approach. Please come again soon and have a beautiful day
####################################################################################################################
####################################################################################################################                                        
          

#This begins the section for the third table, which is a little more complex. This table will be like Table 1 
#but instead of comparing the surfacing events for every year all at once, it instead compares each year individually.
      #Things we'll need
        #  
    
                                   
#testing, 1, 2, 3

      #Find the sum of the number of binary number of seals that surfaced at each location in the creek
      #This code takes how many seals are in the num_seals column in the DCS and the amount of seals seen per season,
      #then adds them up and gives the total amount of seals seen per season 
      SBD$Upper_River_BI <- as.numeric(SBD$Upper_River_BI) 
      Sum_UpperRiver_SBD <- aggregate(SBD$Upper_River_BI,list(SBD$Season, SBD$Year), sum, na.rm = TRUE)
      UR_ArcGIS_Data <- as.data.frame(t(Sum_UpperRiver_SBD))
      #Number of times seals popped up in the season in upper river
      
      SBD$Mid_River_BI <- as.numeric(SBD$Mid_River_BI)
      Sum_MidRiver_SBD <- aggregate(SBD$Mid_River_BI,list(SBD$Season, SBD$Year), sum, na.rm = TRUE)
      MR_ArcGIS_Data <- as.data.frame(t(Sum_MidRiver_SBD))
      #Number of times seals popped up in the season in mid river
      
      SBD$Lower_River_BI <- as.numeric(SBD$Lower_River_BI)
      Sum_LowerRiver_SBD <- aggregate(SBD$Lower_River_BI,list(SBD$Season, SBD$Year), sum, na.rm = TRUE)
      LR_ArcGIS_Data <- as.data.frame(t(Sum_LowerRiver_SBD))
      #Number of times seals popped up in the season in lower river
      
      SBD$Stairs_Eddy_BI <- as.numeric(SBD$Stairs_Eddy_BI)
      Sum_StairsEddy_SBD <- aggregate(SBD$Stairs_Eddy_BI,list(SBD$Season, SBD$Year), sum, na.rm = TRUE)
      SE_ArcGIS_Data <- as.data.frame(t(Sum_StairsEddy_SBD))
      #Number of times seals popped up in the season in stairs eddy
      
      Table3 <- rbind(SE_ArcGIS_Data[3,], UR_ArcGIS_Data[3,], MR_ArcGIS_Data[3,], LR_ArcGIS_Data[3,])
      colnames(Table3) <- c("fall 2011", "fall 2012", "spring 2012", "summer 2012", "winter 2012", "fall 2013", "spring 2013", "summer 2013", "winter 2013", "fall 2014", "spring 2014", "summer 2014", "winter 2014", "fall 2015", "spring 2015", "summer 2015", "winter 2015", "winter 2016")
      rownames(Table3) <- c("SE", "UR", "MR", "LR")
      write.csv(Table3, paste(path_Drive,"Whatcom Creek Project/Scripts and Analysis/R script outputs/Table3.csv",sep=""))
      
      

      