
# Set working directory and choose between home and work
t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock")
rm(list=ls())



load("./ENVIRONMENT_Beta-LC.RDATA")
library(foreign)
library(plyr)
library(tidyr)
library(data.table)





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

                               
                               


 fun.set.anchor <- function(anchor,dataset = data){
   x <- subset(dataset,select = paste0(anchor,2012:2021))
   names(x) <- paste0("anchor.",2012:2021)
   data <- cbind(data,x)}

 
 
 #########################################################
 # Specify desired mean-anchor (GDP/Capita or Survey Mean)
 ######################################################### 
 
 # # #GDP/Capita
 # data <- fun.set.anchor("gdp.capita.",data)
 
 #Survey Mean
 data <- fun.set.anchor("svy.mean.",data)
 # 
 # #cons. Mean (ipc data)
 # data <- fun.set.anchor("cons.",data)
 
 
 

 # add population projections
 load("./Data/population.io.RData")
 a <- subset(population.io, year %in% c(2022:2030), select=c("ccode","poptotal","year"))
 a$year <- paste0("pop.",substr(a$year,3,4))
 a <- spread(a,key = "year",value = "poptotal")
 names(a)[1] <- "country"
 data <- merge(data, a, by="country");rm(a)
 


#project the anchor to 2030 using the avg. growth rate of the last 5 years
 
avg.anchor.g <- with(data,(anchor.2021/anchor.2015)^(1/5))
x <- data.frame(matrix(NA,nrow = nrow(data),ncol = 9))
x[,1] <- avg.anchor.g*data$anchor.2021
for(i in 2:9){x[,i] <- avg.anchor.g*x[,(i-1)]}
names(x) <- paste0("anchor.",2022:2030)
data <- data.frame(data,x);rm(x,avg.anchor.g)



#------------------------------------------------------------------------------------#
#                           Start of Analysis                                        #
#------------------------------------------------------------------------------------#

# get the first and the latest anchor year
end   <- as.numeric(substr(names(data)[max(grep("anchor.",names(data)))],10,11))
start <- as.numeric(substr(names(data)[min(grep("anchor.",names(data)))],10,11))

# Poverty Headcounts

x <- subset(data,select = c("TID",
                            paste0("anchor.",(start:end + 2000)),
                            paste0("pop.",start:end),
                            "gamma","delta","theta"))


x <- as.data.frame(x[!duplicated(x),])



#--------------------------
# Headcount Estimation
#--------------------------

#Coefficients
gamma<-x$gamma; delta<-x$delta; theta<-x$theta #Beta


#Anchors
for(i in start:end){
assign(paste0("mean.",i), unlist(subset(x,select = paste0("anchor.",2000+i)), use.names = F) )}
meanlist <- data.frame(matrix(NA,nrow=nrow(x)))

for(i in start:end){
  meanlist[,(i-11)] <-subset(x,select = paste0("anchor.",2000+i))}
names(meanlist) <- paste0("mean.",start:end)


#Prepare Beta Headcount estimation
beta<-as.data.frame(x$TID); names(beta)<-c("TID")
beta<-data.frame(country=(substr(beta$TID, 1,3)), year=(substr(beta$TID,5,8)), beta$TID)
names(beta)[3]<-c("TID")


a <- data.frame(matrix(NA,ncol = end-start+1))
names(a) <- paste0("hci.",start:end)
beta <- data.frame(beta,a);rm(a)


# Headcountindex----------------------------------------------------------------------


fun.fitbeta <- function(anchor){
  output <- rep(NA,length(gamma))
  for(i in 1:length(gamma)){
    f<-function(H) 
      (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/anchor[i])
    try(temp<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
    output[i]<-temp}
    return(ifelse(is.na(anchor)==TRUE, NA,output))}


for(j in 1:ncol(meanlist)){
  beta[,(3+j)] <- fun.fitbeta(meanlist[,j])}


# Headcounts-----------------------------------------------------------------------

# Population Data

pop <- subset(data,select = c(paste0("pop.",start:end),"TID","source","data"))

pop <- pop[!duplicated(pop$TID),]
poverty.clock <- merge(beta, pop, by="TID")



# rounded to the nearest integer number  --> rounding removed! don't round outside of output tables

a <- do.call(cbind,  Map(function(x,y) {  poverty.clock[,x]   *poverty.clock[,y]},x = paste0("hci.",start:end),y = paste0("pop.",start:end) ))
colnames(a)<- paste0("hc.",start:end)
poverty.clock <- data.frame(poverty.clock,a);rm(a)



#Pick most recent base year
poverty.clock$country <- paste(poverty.clock$country, poverty.clock$source, sep="_") #
poverty.clock$year <- as.numeric(as.character(poverty.clock$year))
poverty.clock <- poverty.clock[order(poverty.clock$country, -abs(poverty.clock$year)),]
poverty.clock <- poverty.clock[!duplicated(poverty.clock$country),]

poverty.clock$country <- substring(poverty.clock$country, 1, 3)
colnames(poverty.clock)[colnames(poverty.clock)=="year"] <- "base.year"

pop <- subset(poverty.clock,select = c("pop.16","pop.30"))
poverty.clock <- subset(poverty.clock, select=c("country","base.year","source",paste0("hc.",start:end)))

rm(list=setdiff(ls(), c("poverty.clock","pop")))


#-----------------------------------------------------------------------------------------------------
# Evaluation and Results
#-----------------------------------------------------------------------------------------------------


poverty.clock$change.16.17 <- poverty.clock$hc.17 - poverty.clock$hc.16    #absolute change reduction (2016-2017)
poverty.clock$change.12.17 <- (poverty.clock$hc.17 - poverty.clock$hc.12)/5  #mean change reduction   (2012-2016)
poverty.clock$change.target <- poverty.clock$hc.16/15  * (-1) # 15 years 'till 2030

sec <- 366*24*60*60
poverty.clock$tik.tak.16.17 <- poverty.clock$change.16.17/sec                  #change per second
poverty.clock$tik.tak.12.17 <- poverty.clock$change.12.17/sec                  #change per second
poverty.clock$tik.tak.target <- poverty.clock$change.target/sec

# Performance
poverty.clock$performance.16.17 <- poverty.clock$change.16.17/poverty.clock$change.target
poverty.clock$performance.12.17 <- poverty.clock$change.12.17/poverty.clock$change.target

poverty.clock$track.16.17 <- as.character(cut(poverty.clock$performance.16.17,breaks = c(-Inf,0,1,Inf),labels = c("wrong","off","on")))

poverty.clock$track.12.17 <- as.character(cut(poverty.clock$performance.12.17,breaks = c(-Inf,0,1,Inf),labels = c("wrong","off","on")))

poverty.clock$track.16.17[poverty.clock$hc.16/pop$pop.16 < 0.015] <- "no"
poverty.clock$track.12.17[poverty.clock$hc.16/pop$pop.16 < 0.015] <- "no"


#poverty.clock$check <- as.numeric(poverty.clock$track.12.17) - as.numeric(poverty.clock$track.16.17) 


# Another Track measure. This time based on the poverty headcount forecast of 2030

poverty.clock$SGD_met <- as.character(poverty.clock$hc.30/pop$pop.30 < 0.015)

poverty.clock$SGD_met[poverty.clock$SGD_met=="FALSE" & poverty.clock$track.12.17 == "no"] <- "Massive Increase"
poverty.clock$SGD_met[poverty.clock$SGD_met=="TRUE" & poverty.clock$track.12.17 == "no"] <- "trivial"

# global results
sum(poverty.clock$hc.16, na.rm=T)           #Number of poor people in 2016

sum(poverty.clock$tik.tak.16.17, na.rm=T)   #Reduction per Second 2016-2017
sum(poverty.clock$tik.tak.12.17, na.rm=T)   #avg Reduction per Second 2012-2017
sum(poverty.clock$tik.tak.target, na.rm=T)  #Target Reduction

########Disect leaving and entering poverty

with(poverty.clock,  data.frame(entering = sum(tik.tak.16.17[tik.tak.12.17>0],na.rm = T),
                                leaving = sum(tik.tak.16.17[tik.tak.12.17<0],na.rm = T))       )

with(poverty.clock,  data.frame(entering = sum(tik.tak.12.17[tik.tak.12.17>0],na.rm = T),
                               leaving = sum(tik.tak.12.17[tik.tak.12.17<0],na.rm = T))       )


require(countrycode)

m <- subset(poverty.clock, is.na(poverty.clock$hc.16))
poverty.clock <- subset(poverty.clock, !is.na(poverty.clock$hc.16))


write.csv(poverty.clock, file="poverty.clock.csv")
require(xlsx)

poverty.clock <- data.frame(cname = countrycode(poverty.clock$country,origin = "iso3c","country.name"), poverty.clock)

write.xlsx2(poverty.clock,"poverty.clock.xlsx",row.names = F)
