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

#ggpairs(DCS [,c("num_chum","num_coho","num_chinook","num_pink","num_steal.head","num_seals","Month","Season")])



#qqp(DCS$num_seals,"norm")
#qqp(DCS$num_seals,"lnorm")

#following functions don't allow NAs in data
DCS_NNA <- DCS[which(is.na(DCS$num_seals)==FALSE),]
DCS_NNA[44,names(DCS)=="num_seals"] <- 3

hist(DCS_NNA$num_seals)
dotchart(DCS_NNA$num_seals)
#qqnorm(DCS_NNA$num_seals)
#qqline(DCS_NNA$num_seals)


nbinom <- fitdistr(DCS_NNA$num_seals,"Negative Binomial")
qqp(DCS_NNA$num_seals,"nbinom",nbinom$estimate[[1]],mu=nbinom$estimate[[2]])

poissondist <- fitdistr(DCS_NNA$num_seals,"Poisson")
qqp(DCS_NNA$num_seals,"pois",poissondist$estimate)

gammadist <- fitdistr(DCS_NNA$num_seals,"gamma")
qqp(DCS_NNA$num_seals,"gamma",gammadist$estimate)

expdist <- fitdistr(DCS_NNA$num_seals,"Exponential")
qqp(DCS_NNA$num_seals,"exp",expdist$estimate)




#just date variables:===========================================

#AIC 2269.2, No warnings
fit1d <- glmer(DCS_NNA$num_seals ~ Month+(1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit1d)
anova(fit1d)

#AIC 2193.6, one convergence warning
fit2d <- glmer(DCS_NNA$num_seals ~ Month+(1|Year)+(1|Month_and_Day), data=DCS_NNA,family=poisson(link = "log"))

summary(fit2d)

#AIC 2248.8 , one convergence warning
fit3d <- glmer(DCS_NNA$num_seals ~ Season+(1|Year)+(1|Month_and_Day), data=DCS_NNA,family=poisson(link = "log"))

summary(fit3d)

#AIC 2559.2 , one convergence warning
fit4d <- glmer(DCS_NNA$num_seals ~ Season+(1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit4d)

#AIC 2559.2 , one convergence warning
fit4d <- glmer(DCS_NNA$num_seals ~ Season+(1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit4d)

#================================================================

#just fish:===========================================

#AIC 3114
fit1f <- glmer(DCS_NNA$num_seals ~ num_chum +(1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit1f)

#AIC 3383.3
fit2f <- glmer(DCS_NNA$num_seals ~ num_pink +(1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit2f)

#AIC 3078.7 
fit3f <- glmer(DCS_NNA$num_seals ~ num_pink +num_chum + num_coho + num_chinook + num_steal.head +(1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit3f)

#AIC 3077.9
fit4f <- glmer(DCS_NNA$num_seals ~ num_chum + num_coho + num_chinook + num_steal.head +(1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit4f)

#AIC 3358.4
fit5f <- glmer(DCS_NNA$num_seals ~ num_coho + num_chinook + num_steal.head +(1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit5f)

#just tide and sun:=========================================================================

#AIC 3317.1 
fit1s <- glmer(DCS_NNA$num_seals ~ Time_block+ (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit1s)

#AIC 3093.1
fit2s <- glmer(DCS_NNA$num_seals ~ Tide_height._at_slack_ft + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit2s)

#AIC 2524.9 : warnings
fit3s <- glmer(DCS_NNA$num_seals ~ Time_block*Season + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit3s)

#AIC 2500.9
fit4s <- glmer(DCS_NNA$num_seals ~ Tide_height._at_slack_ft*Season + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit4s)

#AIC 2474.5 : warnings
fit5s <- glmer(DCS_NNA$num_seals ~ Time_block*Season + Tide_height._at_slack_ft*Season + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit5s)

#AIC 2468.1 
fit6s <- glmer(DCS_NNA$num_seals ~ Time_block + Tide_height._at_slack_ft*Season + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit6s)

#AIC 2480.4 ; warnings  
fit7s <- glmer(DCS_NNA$num_seals ~ Time_block*Season + Tide_height._at_slack_ft + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit7s)

#AIC 3041.4  
fit8s <- glmer(DCS_NNA$num_seals ~ Time_block + Tide_height._at_slack_ft + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit8s)

#AIC 2481.3 
fit9s <- glmer(DCS_NNA$num_seals ~ Time_block + Tide_height._at_slack_ft + Season + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit9s)

#AIC 3384.8
fit10s <- glmer(DCS_NNA$num_seals ~ Tide_height_category2 + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit10s)

#AIC 2539.3 : warnings
fit11s <- glmer(DCS_NNA$num_seals ~ Tide_height_category2*Season + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit11s)


#AIC 2526.1
fit12s <- glmer(DCS_NNA$num_seals ~ Time_block + Season+ (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit12s)


#AIC 2510.3
fit13s <- glmer(DCS_NNA$num_seals ~ Tide_height._at_slack_ft + Season + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

summary(fit13s)

#best models from each group:====================================

#AIC 2193.6, one convergence warning
fit2d <- glmer(DCS_NNA$num_seals ~ Month+(1|Year)+(1|Month_and_Day), data=DCS_NNA,family=poisson(link = "log"))

#AIC 3077.9
fit4f <- glmer(DCS_NNA$num_seals ~ num_chum + num_coho + num_chinook + num_steal.head +(1|Year), data=DCS_NNA,family=poisson(link = "log"))

#AIC 2468.1  
fit6s <- glmer(DCS_NNA$num_seals ~ Time_block + Tide_height._at_slack_ft*Season + (1|Year), data=DCS_NNA,family=poisson(link = "log"))

#add them all together==============================================
#AIC 2122.4 : 1 warning
fit_all <- glmer(DCS_NNA$num_seals ~ Month+(1|Year)+(1|Month_and_Day)+ num_chum + num_coho + num_chinook + num_steal.head+Time_block + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"))

summary(fit_all)

outlierTest(fit_all)

#try increasing maximum evaluations
#AIC 2122.4 : 1 warning
fit_all_opt <- glmer(DCS_NNA$num_seals ~ Month+(1|Year)+(1|Month_and_Day)+ num_chum + num_coho + num_chinook + num_steal.head+Time_block + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"),control=glmerControl(optCtrl=list(maxfun=30000)))

summary(fit_all_opt)

#try changing optimizer function. changed warning message but did not eliminate
fit_all_opt <- glmer(DCS_NNA$num_seals ~ Month+(1|Year)+(1|Month_and_Day)+ num_chum + num_coho + num_chinook + num_steal.head+Time_block + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"),control=glmerControl(optimizer="bobyqa"))

summary(fit_all_opt)


#test: steal.head and season not significant in last model, try removing
#AIC 2132.3 ; 1 warning
fit_all2 <- glmer(DCS_NNA$num_seals ~ Month+(1|Year)+(1|Month_and_Day)+ num_chum + num_coho + num_chinook +Time_block + Tide_height._at_slack_ft, data=DCS_NNA,family=poisson(link = "log"))

summary(fit_all2)

#AIC 2125.2: 1 warning
fit_all3 <- glmer(DCS_NNA$num_seals ~ (1|Year)+(1|Month_and_Day)+ num_chum + num_coho + num_chinook + num_steal.head+Time_block + Tide_height._at_slack_ft*Month, data=DCS_NNA,family=poisson(link = "log"))

summary(fit_all3)

#AIC 2146.9 : remove tide height 1 warning
fit_all4 <- glmer(DCS_NNA$num_seals ~ Month+(1|Year)+(1|Month_and_Day)+ num_chum + num_coho + num_chinook + num_steal.head+Time_block, data=DCS_NNA,family=poisson(link = "log"))

summary(fit_all4)

#AIC 2160.3 : remove time block 1 warning
fit_all5 <- glmer(DCS_NNA$num_seals ~ Month+(1|Year)+(1|Month_and_Day)+ num_chum + num_coho + num_chinook + num_steal.head+ Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"))

summary(fit_all5)


#AIC 2125.8
fit_2_1 <- glmer(DCS_NNA$num_seals ~ Month+(1|Year)+(1|Month_and_Day)+Time_block + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"))

summary(fit_2_1)

#AIC 2387.6
fit_2_2 <- glmer(DCS_NNA$num_seals ~ (1|Year)+ num_chum + num_coho + num_chinook + num_steal.head+Time_block + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"))

summary(fit_2_2)

#AIC 2183.5
fit_2_3 <- glmer(DCS_NNA$num_seals ~ Month+(1|Year)+(1|Month_and_Day)+ num_chum + num_coho + num_chinook + num_steal.head, data=DCS_NNA,family=poisson(link = "log"))

summary(fit_2_3)


#================================================================

#AIC: 2243.7
fit_erin1 <- glmer(DCS_NNA$num_seals ~ num_pink + num_chum + num_coho + num_chinook + num_steal.head + (1|Tide_height_category2)+(Year|Month), data=DCS_NNA,family=poisson(link = "log"))

summary(fit_erin1)

#AIC: 2243.7
fit_erin2 <- glmer(DCS_NNA$num_seals ~ (num_pink + num_chum + num_coho + num_chinook + num_steal.head) + (1|Tide_height_category2)+(Year|Month), data=DCS_NNA,family=poisson(link = "log"))

summary(fit_erin2)

#AIC: 2256.2
fit_erin3 <- glmer(DCS_NNA$num_seals ~ num_chum + num_coho + (1|Tide_height_category2)+(Year|Month), data=DCS_NNA,family=poisson(link = "log"))

summary(fit_erin3)

#AIC: 2270.0
fit_erin4 <- glmer(DCS_NNA$num_seals ~ num_coho + (1|Tide_height_category2)+(Year|Month), data=DCS_NNA,family=poisson(link = "log"))

summary(fit_erin4)

#AIC: 2254.5
fit_erin5 <- glmer(DCS_NNA$num_seals ~ num_chum + (1|Tide_height_category2)+(Year|Month), data=DCS_NNA,family=poisson(link = "log"))

summary(fit_erin5)

#AIC: 2268
fit_erin6 <- glmer(DCS_NNA$num_seals ~  (1|Tide_height_category2)+(Year|Month), data=DCS_NNA,family=poisson(link = "log"))

summary(fit_erin6)

plot(fit_erin6)



#======================================
#AIC 2436.1
fit1 <- glmer(DCS_NNA$num_seals ~ Time_block*Season + (1|Year) + num_chum + num_chinook +num_steal.head + num_pink + num_coho+ (1|Tide_height_category2), data=DCS_NNA,family=poisson(link = "log"))

summary(fit1)

#AIC 2315.5 Tide height at slack ft better than tide height category?
fit2 <- glmer(DCS_NNA$num_seals ~ Time_block*Season + (1|Year) + num_chum + num_chinook +num_steal.head + num_pink+ num_coho + (1|Tide_height._at_slack_ft), data=DCS_NNA,family=poisson(link = "log"))

summary(fit2)

#AIC 3078.7  fish as predictors
fit3 <- glmer(DCS_NNA$num_seals ~  (1|Year) + num_chum + num_chinook +num_steal.head + num_pink+ num_coho, data=DCS_NNA,family=poisson(link = "log"))

summary(fit3)

#AIC 2435.0 Apparently, this one converged!! Only 1 so far without convergence/eigenvalue/hesien warnings
fit4 <- glmer(DCS_NNA$num_seals ~  Time_block*Season + (1|Year) + num_chum + num_chinook +num_steal.head + num_pink+ num_coho, data=DCS_NNA,family=poisson(link = "log"))

summary(fit4)

#AIC  3006.7 
fit5 <- glmer(DCS_NNA$num_seals ~  Time_block + (1|Year) + num_chum + num_chinook +num_steal.head + num_pink+ num_coho, data=DCS_NNA,family=poisson(link = "log"))

summary(fit5)

#AIC 2319.0: model likes (Tide_height_category2|Tide_height._at_slack_ft)
fit6 <- glmer(DCS_NNA$num_seals ~ Time_block*Season + (1|Year) + num_chum + num_chinook +num_steal.head + num_pink+ num_coho + (Tide_height_category2|Tide_height._at_slack_ft), data=DCS_NNA,family=poisson(link = "log"))

summary(fit6)

#AIC 2396.3: slightly worse fit with season interaction
fit7 <- glmer(DCS_NNA$num_seals ~ Time_block*Season + (1|Year) + num_chum + num_chinook +num_steal.head + num_pink+ num_coho + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"))

summary(fit7)


#AIC 2412.8 other fish besides chum taken out
fit8 <- glmer(DCS_NNA$num_seals~ Time_block*Season + (1|Year) + num_chum + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"))

summary(fit8)


############Explore fit 2 and fit 4

#AIC 2313.6: Remove pink salmon. Makes it slightly better
fit2a <- glmer(DCS_NNA$num_seals ~ Time_block*Season + (1|Year) + num_chum + num_chinook +num_steal.head +  num_coho + (1|Tide_height._at_slack_ft), data=DCS_NNA,family=poisson(link = "log"))

summary(fit2a)

#AIC 2312.6: 
fit2b <- glmer(DCS_NNA$num_seals ~ Season + (1|Year) + num_chum + num_chinook +num_steal.head +  num_coho + (1|Tide_height._at_slack_ft), data=DCS_NNA,family=poisson(link = "log"))

summary(fit2b)

#AIC 2401.4 
fit2c <- glmer(DCS_NNA$num_seals ~ Time_block*Season + (1|Year) + num_chum + num_chinook +num_steal.head +  num_coho + Tide_height._at_slack_ft, data=DCS_NNA,family=poisson(link = "log"))

summary(fit2c)

##################Ok, gonna say models MUST include Time_block*Season, Tide_height._at_slack_ft*Season, (1|Year), all fish except pink as fixed effects.  

#AIC 2394
fit9 <- glmer(DCS_NNA$num_seals~ Time_block*Season + (1|Year) + num_chum + num_chinook +num_steal.head +num_coho + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"))

summary(fit9)

#AIC  2174.2 : best one yet!
fit10 <- glmer(DCS_NNA$num_seals~ Time_block*Season + (1|Year) + (1|Month_and_Day)+ num_chum + num_chinook +num_steal.head +num_coho + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"))

summary(fit10)

#AIC   2176.0 : add pink
fit11 <- glmer(DCS_NNA$num_seals~ Time_block*Season + (1|Year) + (1|Month_and_Day)+ num_chum + num_chinook +num_steal.head +num_coho +num_pink + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"))

summary(fit11)

#AIC     2178.0 : remove bad fish
fit12 <- glmer(DCS_NNA$num_seals~ Time_block*Season + (1|Year) + (1|Month_and_Day)+ num_chum +num_steal.head +num_coho + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"))

summary(fit12)

#AIC   2196.2  : like 10, month as fixed effect instead of Month_day random effect
fit13 <- glmer(DCS_NNA$num_seals~ Time_block*Season + (1|Year) + Month+ num_chum + num_chinook +num_steal.head +num_coho + Tide_height._at_slack_ft*Season, data=DCS_NNA,family=poisson(link = "log"))

summary(fit13)





#AIC 2953.1 Tide height at slack ft still better than tide height category
fit9 <- glmer(DCS_NNA$num_seals~ Time_block*Season + (1|Year) + num_chum + Tide_height_category2*Season, data=DCS_NNA,family=poisson(link = "log"))



#AIC 2821: Best way to do tide height! (Tide_height_category2|Tide_height._at_slack_ft)*Season

fit10 <- glmer(DCS_NNA$num_seals~ Time_block*Season + (1|Year) + num_chum + (Tide_height_category2|Tide_height._at_slack_ft)*Season, data=DCS_NNA,family=poisson(link = "log"))


#AIC 2839.9: tried removing Time_block*Season, AIC went down slightly
fit11 <- glmer(DCS_NNA$num_seals~ (1|Year) + num_chum + (Tide_height_category2|Tide_height._at_slack_ft)*Season, data=DCS_NNA,family=poisson(link = "log"))

#AIC 2806.3 Same as 6, but with season interaction for tide height
fit12 <- glmer(DCS_NNA$num_seals ~ Time_block*Season + (1|Year) + num_chum + num_chinook +num_steal.head + num_pink + (Tide_height_category2|Tide_height._at_slack_ft)*Season, data=DCS_NNA,family=poisson(link = "log"))

#AIC 2821: Without season interaction with tide
fit13 <- glmer(DCS_NNA$num_seals~ Time_block*Season + (1|Year) + num_chum + (Tide_height_category2|Tide_height._at_slack_ft), data=DCS_NNA,family=poisson(link = "log"))

#2798.9 : Like 12, time block without season interaction
fit14 <- glmer(DCS_NNA$num_seals ~ Time_block + (1|Year) + num_chum + num_chinook +num_steal.head + num_pink + (Tide_height_category2|Tide_height._at_slack_ft)*Season, data=DCS_NNA,family=poisson(link = "log"))

#3070.2 : like 12, but with season interaction for tide height removed
fit15 <- glmer(DCS_NNA$num_seals ~ Time_block + (1|Year) + num_chum + num_chinook +num_steal.head + num_pink + (Tide_height_category2|Tide_height._at_slack_ft), data=DCS_NNA,family=poisson(link = "log"))

# 2855.9 like 14 but with tide split
fit16 <- glmer(DCS_NNA$num_seals ~ Time_block + (1|Year) + num_chum + num_chinook +num_steal.head + num_pink + Tide_height_category2*Season + Tide_height._at_slack_ft, data=DCS_NNA,family=poisson(link = "log"))

#2935.7 : Like 14, but with THASF removed
fit17 <- glmer(DCS_NNA$num_seals ~ Time_block + (1|Year) + num_chum + num_chinook +num_steal.head + num_pink + Tide_height_category2*Season, data=DCS_NNA,family=poisson(link = "log"))

#2797.3 : Like 14, but with no pink salmon
fit18 <- glmer(DCS_NNA$num_seals ~ Time_block + (1|Year) + num_chum + num_chinook +num_steal.head + (Tide_height_category2|Tide_height._at_slack_ft)*Season, data=DCS_NNA,family=poisson(link = "log"))

#2798.4: Like 14, but with no chinook
fit19 <- glmer(DCS_NNA$num_seals ~ Time_block + (1|Year) + num_chum +num_steal.head + (Tide_height_category2|Tide_height._at_slack_ft)*Season, data=DCS_NNA,family=poisson(link = "log"))

#2805.9: like 19, but with season interaction
fit20 <- glmer(DCS_NNA$num_seals ~ Time_block*Season + (1|Year) + num_chum +num_steal.head + (Tide_height_category2|Tide_height._at_slack_ft)*Season, data=DCS_NNA,family=poisson(link = "log"))

#2859 random effect for year removed. Still get same warning messages
fit21 <- glmer(DCS_NNA$num_seals ~ Time_block*Season + num_chum +num_steal.head + (Tide_height_category2|Tide_height._at_slack_ft)*Season, data=DCS_NNA,family=poisson(link = "log"))

#2823.1: like 20, but time block removed
fit22 <- glmer(DCS_NNA$num_seals ~ (1|Year) + num_chum +num_steal.head + (Tide_height_category2|Tide_height._at_slack_ft)*Season, data=DCS_NNA,family=poisson(link = "log"))

#2813.8: like 20, but with Season\year inserted
fit23 <- glmer(DCS_NNA$num_seals ~ Time_block*Season + (Season|Year) + num_chum +num_steal.head + (Tide_height_category2|Tide_height._at_slack_ft)*Season, data=DCS_NNA,family=poisson(link = "log"))

#3122.9 
fit24 <- glmer(DCS_NNA$num_seals ~  (Season|Year), data=DCS_NNA,family=poisson(link = "log"))



#chose lowest AIC score
