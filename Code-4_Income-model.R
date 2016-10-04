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








#########################################################
# Specify desired mean-anchor (GDP/Capita or Survey Mean)
######################################################### 

fun.set.anchor <- function(anchor,dataset = data){
  # get the first and the latest anchor year
  extended.anchor <- paste0(anchor,"20")
  substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
  assign("end", as.numeric( substrRight(names(data)[max(grep(extended.anchor,names(data)))],2)),envir=globalenv())
  assign("start",as.numeric( substrRight(names(data)[min(grep(extended.anchor,names(data)))],2)),envir=globalenv() )
  x <- subset(dataset,select = paste0(anchor,start:end+2000))
  names(x) <- paste0("anchor.",start:end+2000)
  return(cbind(data,x))}


# # #GDP/Capita
# data <- fun.set.anchor("gdp.capita.",data)

#Survey Mean
#data <- fun.set.anchor("svy.mean.",data)
# 
# #cons. Mean (ipc data)
# data <- fun.set.anchor("cons.",data)
 
 # # # Household expenditure per capita
 data <- fun.set.anchor("HH.ex.capita.",data)




#------------------------------------------------------------------------------------#
#                           Start of Analysis                                        #
#------------------------------------------------------------------------------------#



# Poverty Headcounts

x <- subset(data,select = c("TID",
                            paste0("anchor.",(start:end + 2000)),
                            paste0("pop.",start:end),
                            "gamma","delta","theta","p"))


x <- as.data.frame(x[!duplicated(x),])

country=(substr(x$TID, 1,3)); year=as.numeric(substr(x$TID,5,8))

x <- x[order(country, -abs(year)),]
x <- x[!duplicated(country),];rm(country,year)


#--------------------------
# Headcount Estimation
#--------------------------

#Coefficients
gamma<-x$gamma; delta<-x$delta; theta<-x$theta #Beta


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

fun.fitbeta <- function(anchor,z){
  output <- rep(NA,length(gamma))
  for(i in 1:length(gamma)){
    f<-function(H) 
      (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/anchor[i])
    try(temp<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
    output[i]<-temp}
  return(ifelse(is.na(anchor)==TRUE, NA,output))}

# for(j in 1:ncol(meanlist)){
#   beta[,(3+j)] <- fun.fitbeta(meanlist[,j],z=365*1.9)}


# Headcounts-----------------------------------------------------------------------

# Population Data

pop <- subset(data,select = c(paste0("pop.",start:end),"TID","source","data"))
pop$ccode <- substr(pop$TID,1,3)
pop <- melt(pop[,-match(c("source","data","TID"),names(pop))],c("ccode"))
pop <- pop[!duplicated(paste0(pop$ccode,pop$variable)),]
pop$time <- paste0(20,substr(pop$variable,5,6))
#pop <- spread(pop,key="variable",value = "value")


income <- c(10,30,50,100)
#income <- seq(0,100,by = 0.2 )
require(countrycode)

index <- list(country = data.frame(ccode=substr(x$TID,1,3),
                                   cname=countrycode(substr(x$TID,1,3), "iso3c","country.name")),
              time=2000+start:end,
              income_level = income,
              variable = c("HC_index","Pov_gap"))


ptm <- Sys.time()

b <- array(NA,dim = c(country = nrow(x),time = end-start+1, income_level = length(income),variable = 2 ))

for(k in 1:length(income)){
  for(j in 1:ncol(meanlist)){
    b[,j,k,1] <- fun.fitbeta(meanlist[,j],365*income[k])}}
Sys.time()-ptm



#save(b,file = "pdf.RData")
#load("pdf.RData")









###for those graphics I reduce the dimension since I did it bevor the array got the 4th
b <- b[,,,1]
# between l and h$ for all years
h=100;l=50
medium.inc <- melt(data.frame(index$country,range = paste0("[",l, "," ,h, ")"),b[,,match(h,index[[3]])]-b[,,match(l,index[[3]])]),c("ccode","cname","range"))

h=50;l=10
medium.inc <- rbind(medium.inc,
                    melt(data.frame(index$country,range = paste0("[",l, "," ,h, ")"),b[,,match(h,index[[3]])]-b[,,match(l,index[[3]])]),c("ccode","cname","range"))        )

names(medium.inc)[4] <- "time"
medium.inc$time <- index[[2]][as.numeric(substr(medium.inc$time,2,3))]

medium.inc$id <- paste(medium.inc$ccode,medium.inc$range,sep = "_")

medium.inc$pop <- pop$value[match(paste0(medium.inc$ccode,medium.inc$time), paste0(pop$ccode,pop$time))]
medium.inc$value.pop <- medium.inc$pop*medium.inc$value

#ID <- paste(medium.inc$ccode,medium.inc$time,sep = "_")

require(ggplot2)
library(scales)
countries <- c("BRA","NGA","IDN")

#popshare 10 50
ggplot(medium.inc[medium.inc$ccode %in% countries & medium.inc$range == "[10,50)",],aes(x=time,y=value,group=id ,color = cname )) +
  geom_line(size=1.3,linetype =2 )+ ggtitle("Population share with a daily income between 10 and 50  2011 PPP USD")+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ scale_y_continuous(name = "",labels = percent)+ scale_x_continuous(name = "Time")

# popshare 50 100
ggplot(medium.inc[medium.inc$ccode %in% countries & medium.inc$range == "[50,100)",-1],aes(x=time,y=value,group=id ,color = cname )) +
  geom_line(size=1,linetype =1)+ ggtitle("Population share with a daily income between 50 and 100  2011 PPP USD") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ scale_y_continuous(name = "",labels = percent)+ scale_x_continuous(name = "Time")

## combined popshare
ggplot(medium.inc[medium.inc$ccode %in% countries,-1],aes(x=time,y=value,group=id ,color = cname,linetype = range )) +
  geom_line(size=1)+ ggtitle("Population share for variing levels of daily income") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ scale_y_continuous(name = "Population",labels = percent)+ scale_x_continuous(name = "Time")+ guides(color=guide_legend(title="Country"),linetype = guide_legend(title="Range"))


# poplevel 10 50
ggplot(medium.inc[medium.inc$ccode %in% countries & medium.inc$range == "[10,50)",],aes(x=time,y=value.pop,group=id ,color = cname )) +
  geom_line(size=1.3,linetype =2 )+ ggtitle("Total Population with a daily income between 10 and 50  2011 PPP USD")+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ scale_y_continuous(labels = function(x){x/10e6},name = "Population in Millions")+ scale_x_continuous(name = "Time")+ guides(color=guide_legend(title="Country"))

# poplevel 50 100
ggplot(medium.inc[medium.inc$ccode %in% countries & medium.inc$range == "[50,100)",],aes(x=time,y=value.pop,group=id ,color = cname )) +
  geom_line(size=1,linetype =1 )+ ggtitle("Total Population with a daily income between 50 and 100  2011 PPP USD")+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ scale_y_continuous(labels = function(x){x/10e6},name = "Population in Millions")+ scale_x_continuous(name = "Time")+ guides(color=guide_legend(title="Country"))



  ggplot(medium.inc[medium.inc$ccode %in% countries,-1],aes(x=time,y=value.pop,group=id ,color = cname,linetype = range )) +
    geom_line(size=1.3)+ ggtitle("") + 
    scale_y_continuous(labels = function(x){x/10e6},name = "Population in Millions")+ scale_x_continuous(name = "Time")+ guides(color=guide_legend(title="Country"),linetype = guide_legend(title="Range"))+
    theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()#+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("check.png")
  
  

#pdf of Uganda in 2016
plot(diff(b[177,5,])~index[[3]][-1],type="l")
#pdf of spain in 2016
plot(diff(b[match("ESP",index[[1]][[1]]),5,])~index[[3]][-1],type="l",ylab = "pdf",xlab = "PPP income per Day",main = index[[1]][index[[1]][[1]]=="ESP",2])





#solution for Z i.e incomethreshold given the headcount index ie percentage of pop
#z=
i=7 # austria
fun.test <-  function(H) anchor[i]*(1-t(1-H)^delta[i]*H^gamma[i] * (delta[i]/(H-1) + gamma[i]/H )  )
fun.test(0.6)
curve(fun.test,0.001,0.9999)



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
