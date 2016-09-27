#--------------------------------------------------------------------------------------
# Preliminary
#--------------------------------------------------------------------------------------
setwd("C:/Users/Karim/Documents/Uni/World Population Program/Finale Version")
load("./ENVIRONMENT_Beta-LC.RDATA")
library(foreign)
library(plyr)
library(data.table)
#measure time 
ptm <- proc.time()
#-------------------------------------------------------------------------------------
#
#
#
#  Poverty Clock (based on Beta Lorenzcurves)
#
#
#
#--------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------#
#                             Specifiy Key Variables                                 #
#------------------------------------------------------------------------------------#
# for time specific poverty outputs we assume GDP data to be launched by 31st of Dec. .
# Therefore the inter year period spans from 31st of Dec. to 31st of Dec of the following year.
# Intra-year time points we just project linearily to the respective date. (say 15.mio sec to June)


                            # Specify Poverty Line
                               z <- 365*1.90

                               
                               
       # Specify desired mean-anchor (GDP/Capita or Survey Mean)

#GDP/Capita
data$anchor.2012 <- data$gdp.capita.2012; data$anchor.2017 <- data$gdp.capita.2017
data$anchor.2013 <- data$gdp.capita.2013; data$anchor.2018 <- data$gdp.capita.2018
data$anchor.2014 <- data$gdp.capita.2014; data$anchor.2019 <- data$gdp.capita.2019
data$anchor.2015 <- data$gdp.capita.2015; data$anchor.2020 <- data$gdp.capita.2020
data$anchor.2016 <- data$gdp.capita.2016; data$anchor.2021 <- data$gdp.capita.2021

#Survey Mean
# data$anchor.2012 <- data$svy.mean.2012; data$anchor.2017 <- data$svy.mean.2017
# data$anchor.2013 <- data$svy.mean.2013; data$anchor.2018 <- data$svy.mean.2018
# data$anchor.2014 <- data$svy.mean.2014; data$anchor.2019 <- data$svy.mean.2019
# data$anchor.2015 <- data$svy.mean.2015; data$anchor.2020 <- data$svy.mean.2020
# data$anchor.2016 <- data$svy.mean.2016; data$anchor.2021 <- data$svy.mean.2021

#cons. Mean (ipc data)
#data$anchor.2012 <- data$cons.2012; data$anchor.2017 <- data$cons.2017
#data$anchor.2013 <- data$cons.2013; data$anchor.2018 <- data$cons.2018
#data$anchor.2014 <- data$cons.2014; data$anchor.2019 <- data$cons.2019
#data$anchor.2015 <- data$cons.2015; data$anchor.2020 <- data$cons.2020
#data$anchor.2016 <- data$cons.2016; data$anchor.2021 <- data$cons.2021



#------------------------------------------------------------------------------------#
#                           Start of Analysis                                        #
#------------------------------------------------------------------------------------#

# Poverty Headcounts
x<-subset(data, select=c("TID","anchor.2012","anchor.2013","anchor.2014",
                           "anchor.2015","anchor.2016","anchor.2017","anchor.2018",
                           "anchor.2019","anchor.2020","anchor.2021","pop.12","pop.13",
                           "pop.14","pop.15","pop.16","pop.17","pop.18","pop.19",
                           "pop.20","pop.21","gamma","delta","theta"))

x <- as.data.frame(x[!duplicated(x),])



#--------------------------
# Headcount Estimation
#--------------------------

#Coefficients
gamma<-x$gamma; delta<-x$delta; theta<-x$theta #Beta


#Anchors
mean.12<-x$anchor.2012; mean.17<-x$anchor.2017
mean.13<-x$anchor.2013; mean.18<-x$anchor.2018
mean.14<-x$anchor.2014; mean.19<-x$anchor.2019
mean.15<-x$anchor.2015; mean.20<-x$anchor.2020
mean.16<-x$anchor.2016; mean.21<-x$anchor.2021



#Prepare Beta Headcount estimation
beta<-as.data.frame(x$TID); names(beta)<-c("TID")
beta<-data.frame(country=(substr(beta$TID, 1,3)), year=(substr(beta$TID,5,8)), beta$TID)
names(beta)[3]<-c("TID")

beta$hci.12<-NA; beta$hci.13<-NA; beta$hci.14<-NA; beta$hci.15<-NA
beta$hci.16<-NA; beta$hci.17<-NA; beta$hci.18<-NA; beta$hci.19<-NA
beta$hci.20<-NA; beta$hci.21<-NA




# Headcountindex----------------------------------------------------------------------

#-2012----------------------
for(i in 1:length(gamma)){
  f<-function(H) 
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/mean.12[i])
  try(x<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  beta$hci.12[i]<-x}

beta$hci.12 <- ifelse(is.na(mean.12)==TRUE, NA, beta$hci.12)


#-2013----------------------
for(i in 1:length(gamma)){
  f<-function(H) 
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/mean.13[i])
  try(x<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  beta$hci.13[i]<-x}

beta$hci.13 <- ifelse(is.na(mean.13)==TRUE, NA, beta$hci.13)


#-2014----------------------
for(i in 1:length(gamma)){
  f<-function(H) 
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/mean.14[i])
  try(x<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  beta$hci.14[i]<-x}

beta$hci.14 <- ifelse(is.na(mean.14)==TRUE, NA, beta$hci.14)


#-2015----------------------
for(i in 1:length(gamma)){
  f<-function(H) 
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/mean.15[i])
  try(x<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  beta$hci.15[i]<-x}

beta$hci.15 <- ifelse(is.na(mean.15)==TRUE, NA, beta$hci.15)

#-2016----------------------
for(i in 1:length(gamma)){
  f<-function(H) 
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/mean.16[i])
  try(x<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  beta$hci.16[i]<-x}

beta$hci.16 <- ifelse(is.na(mean.16)==TRUE, NA, beta$hci.16)


#-2017----------------------
for(i in 1:length(gamma)){
  f<-function(H) 
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/mean.17[i])
  try(x<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  beta$hci.17[i]<-x}

beta$hci.17 <- ifelse(is.na(mean.17)==TRUE, NA, beta$hci.17)


#-2018----------------------
for(i in 1:length(gamma)){
  f<-function(H) 
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/mean.18[i])
  try(x<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  beta$hci.18[i]<-x}

beta$hci.18 <- ifelse(is.na(mean.18)==TRUE, NA, beta$hci.18)


#-2019----------------------
for(i in 1:length(gamma)){
  f<-function(H) 
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/mean.19[i])
  try(x<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  beta$hci.19[i]<-x}

beta$hci.19 <- ifelse(is.na(mean.19)==TRUE, NA, beta$hci.19)


#-2020----------------------
for(i in 1:length(gamma)){
  f<-function(H) 
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/mean.20[i])
  try(x<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  beta$hci.20[i]<-x}

beta$hci.20 <- ifelse(is.na(mean.20)==TRUE, NA, beta$hci.20)

#-2021----------------------
for(i in 1:length(gamma)){
  f<-function(H) 
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/mean.21[i])
  try(x<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  beta$hci.21[i]<-x}

beta$hci.21 <- ifelse(is.na(mean.21)==TRUE, NA, beta$hci.21)



# Headcounts-----------------------------------------------------------------------

# Population Data
pop <- subset(data, select=c("pop.12","pop.13","pop.14","pop.15",
                                           "pop.16","pop.17","pop.18","pop.19",
                                           "pop.20","pop.21","TID","source","data"))
pop <- pop[!duplicated(pop$TID),]
poverty.clock <- merge(beta, pop, by="TID")

# rounded to the nearest integer number
poverty.clock$hc.12 <- round(poverty.clock$hci.12 * poverty.clock$pop.12, digits=0)
poverty.clock$hc.13 <- round(poverty.clock$hci.13 * poverty.clock$pop.13, digits=0) 
poverty.clock$hc.14 <- round(poverty.clock$hci.14 * poverty.clock$pop.14, digits=0)
poverty.clock$hc.15 <- round(poverty.clock$hci.15 * poverty.clock$pop.15, digits=0) 
poverty.clock$hc.16 <- round(poverty.clock$hci.16 * poverty.clock$pop.16, digits=0) 
poverty.clock$hc.17 <- round(poverty.clock$hci.17 * poverty.clock$pop.17, digits=0) 
poverty.clock$hc.18 <- round(poverty.clock$hci.18 * poverty.clock$pop.18, digits=0) 
poverty.clock$hc.19 <- round(poverty.clock$hci.19 * poverty.clock$pop.19, digits=0) 
poverty.clock$hc.20 <- round(poverty.clock$hci.20 * poverty.clock$pop.20, digits=0) 
poverty.clock$hc.21 <- round(poverty.clock$hci.21 * poverty.clock$pop.21, digits=0) 


poverty.clock <- subset(poverty.clock, select=c("country","year","source","hc.12","hc.13","hc.14",
                                                "hc.15","hc.16","hc.17","hc.18","hc.19","hc.20",
                                                "hc.21"))
rm(list=setdiff(ls(), c("poverty.clock","ptm")))


#Pick most recent base year
poverty.clock$country <- paste(poverty.clock$country, poverty.clock$source, sep="_") #
poverty.clock$year <- as.numeric(as.character(poverty.clock$year))
poverty.clock <- poverty.clock[order(poverty.clock$country, -abs(poverty.clock$year)),]
poverty.clock <- poverty.clock[!duplicated(poverty.clock$country),]

poverty.clock$country <- substring(poverty.clock$country, 1, 3)
colnames(poverty.clock)[2] <- "base.year"


#-----------------------------------------------------------------------------------------------------
# Evaluation and Results
#-----------------------------------------------------------------------------------------------------

# Poverty reduction (2016-2017)
poverty.clock$change.16.17 <- poverty.clock$hc.17 - poverty.clock$hc.16        #absolute change
poverty.clock$change.target <- round(poverty.clock$hc.16/15  * (-1),digits=0)  # 15 years 'till 2030

sec <- 366*24*60*60
poverty.clock$tik.tak.16.17 <- poverty.clock$change.16.17/sec                  #change per second
poverty.clock$tik.tak.target <- poverty.clock$change.target/sec


# Performance 2016 - 2017
# increase or decrease?
poverty.clock$performance.1 <- ifelse(poverty.clock$change.16.17 > 0, "increase", 
                                    ifelse(poverty.clock$change.16.17 < 0, "decrease", NA) )

# if decrease: underperformer or overperformer?
# decreases are NEGATIVE NUMBERS
poverty.clock$performance.2 <- ifelse(
  poverty.clock$change.16.17 < poverty.clock$change.target, "overperformer", "underperformer")

poverty.clock$performance.2 <- ifelse(poverty.clock$performance.1 != "decrease", 
                                      NA,poverty.clock$performance.2) 


# global results
sum(poverty.clock$hc.16, na.rm=T)           #Number of poor people in 2016

sum(poverty.clock$tik.tak.16.17, na.rm=T)   #Reduction per Second 2016-2017
sum(poverty.clock$tik.tak.target, na.rm=T)  #Target Reduction


poverty.clock <- subset(poverty.clock, !is.na(poverty.clock$hc.16))
rm(sec)


proc.time() - ptm
write.csv(poverty.clock, file="poverty.clock.csv")
