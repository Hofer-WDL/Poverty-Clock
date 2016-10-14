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

                               
                               



 #########################################################
 # Specify desired mean-anchor (GDP/Capita or Survey Mean)
 ######################################################### 
                               
fun.set.anchor <- function(hierarchy,dataset = data){
  # get the first and the latest anchor year
  extended.anchor <- paste0(hierarchy[1],"20")
  substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
  assign("end", as.numeric( substrRight(names(dataset)[max(grep(extended.anchor,names(dataset)))],2)),envir=globalenv())
  assign("start",as.numeric( substrRight(names(dataset)[min(grep(extended.anchor,names(dataset)))],2)),envir=globalenv() )
  varlist <- expand.grid(hierarchy,start:end+2000)
  varlist <- paste0(varlist$Var1,varlist$Var2)
  if ( sum(varlist %in% names(dataset) ) != length(varlist)) warning("Not all Anchor Variables for all years in dataframe")
  output <- data.frame(matrix(NA,nrow=nrow(dataset),ncol=length(start:end)))
  names(output) <- paste0("anchor.",start:end+2000)
  for(i in 1:length(hierarchy)){
    output[is.na(output)] <- subset(dataset,select = paste0(hierarchy[i],start:end+2000))[is.na(output)] }
  return(cbind(dataset,output))}
 

# the function fun.set.anchor can receive a anchor hierarchy in the form of a vector like c("svy.mean.","cons.","gdp.capita.")
# Starting with the first anchor the function uses the given variable as anchor for the lorenz curves if this data is not available it uses the next anchor in the hieararchy
#note the function could use some work since as of now it could happen that we mix within a country which will produce spikes for all forecasts due to a rapid change of the anchor (we forecast wherever we have some data but if something slips this could happen)

data <- fun.set.anchor(c("svy.mean.","cons.","gdp.capita."),data)


 # # #GDP/Capita
 # data <- fun.set.anchor("gdp.capita.",data)
 
 #Survey Mean
 #data <- fun.set.anchor("svy.mean.",data)
 # 
 # #cons. Mean (ipc data)
 # data <- fun.set.anchor("cons.",data)
 
 
 


#------------------------------------------------------------------------------------#
#                           Start of Analysis                                        #
#------------------------------------------------------------------------------------#



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


meanlist <- subset(x,select = paste0("anchor.",start:end+2000))
names(meanlist) <- paste0("mean.",start:end)

# old meanlist
# meanlist <- data.frame(matrix(NA,nrow=nrow(x)))
# for(i in start:end){
#   meanlist[,(i-11)] <-subset(x,select = paste0("anchor.",2000+i))}
# names(meanlist) <- paste0("mean.",start:end)


#Prepare Beta Headcount estimation
beta<-as.data.frame(x$TID); names(beta)<-c("TID")
beta<-data.frame(country=(substr(beta$TID, 1,3)), year=(substr(beta$TID,5,8)), beta$TID)
names(beta)[3]<-c("TID")


a <- data.frame(matrix(NA,ncol = end-start+1))
names(a) <- paste0("hci.",start:end)
beta <- data.frame(beta,a);rm(a)


# Headcountindex----------------------------------------------------------------------

# 
# fun.fitbeta <- function(anchor){
#   output <- rep(NA,length(gamma))
#   for(i in 1:length(gamma)){
#     f<-function(H)
#       (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/anchor[i])
#     try(temp<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
#     output[i]<-temp}
#     return(ifelse(is.na(anchor)==TRUE, NA,output))}
# 
# 
# for(j in 1:ncol(meanlist)){
#   beta[,(3+j)] <- fun.fitbeta(meanlist[,j])}

#####Alternative Better mechanism implement at times

fun.fitbeta <- function(anchor){
 # output <- "failed"  # using "failed instead of NA you can check wheter the root failed or there is just no data but the rest of the code is not built to handle this change
  output <- NA
  f<-function(H)
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/anchor)
  try(output<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  return(ifelse(is.na(anchor)==TRUE, NA,output))}


for(j in 1:ncol(meanlist)){
  for(i in 1: length(gamma)){
    beta[i,(3+j)] <- fun.fitbeta(meanlist[i,j])}}


##########HCI after GQL
load("./Data/GQL_coeff.RData")
GQL_coeff <- GQL_coeff[match(x$TID,GQL_coeff$TID),]

gql.hci <- function(mean,z){with(GQL_coeff,
                                 (-1/(2*m)) * (n + r*(b + 2*z/mean)*
                                                 (((b + 2*z/mean)^2 - m)^(-1/2) ) ))}


GQL.hci <-  data.frame(TID = x$TID)
GQL.hci <- data.frame(country=(substr(GQL.hci$TID, 1,3)), year=(substr(GQL.hci$TID,5,8)), TID =  GQL.hci$TID)
GQL.hci <- data.frame(GQL.hci,apply(meanlist, 2, gql.hci,1.9*365))
GQL.hci[,-c(1:3)] <- apply(GQL.hci[,-c(1:3)],2,function(x){x[x<0]<-0;x})
GQL.hci[,-c(1:3)] <- apply(GQL.hci[,-c(1:3)],2,function(x){x[x>1]<-1;x})


selected <- beta
for(i in 1:nrow(meanlist)){if (  any(  is.na( selected[i,] )  )  ){ selected[i,] <- GQL.hci[i,]  }  }


# Headcounts-----------------------------------------------------------------------

# Population Data

pop <- subset(data,select = c(paste0("pop.",start:end),"TID","source","data"))

pop <- pop[!duplicated(pop$TID),]
pop <- pop[match(selected$TID,pop$TID),]
a <- data.frame(subset(selected,select = c("TID","country","year")) ,subset(pop,select = paste0("pop.",start:end)) * subset(selected,select = paste0("hci.",start:end)))
names(a)[-c(1:3)] <- paste0("hc.",start:end)

poverty.clock <- merge(a, pop, by="TID")


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

# 
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

poverty.clock <- subset(poverty.clock, !is.na(poverty.clock$hc.16))

write.csv(poverty.clock, file="poverty.clock.csv")
require(xlsx)

poverty.clock <- data.frame(cname = countrycode(poverty.clock$country,origin = "iso3c","country.name"), poverty.clock)

write.xlsx2(poverty.clock,"poverty.clock.xlsx",row.names = F)
