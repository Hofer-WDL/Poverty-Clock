t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())

require(foreign)
require(tidyr)
#---------------------------------------------------------------------------------------
#
# POVCAL
#
#---------------------------------------------------------------------------------------
distrib <- read.dta(("../Data/povcal.dta"))

x<-aggregate(distrib$year, by = list(distrib$actloc, distrib$survey), max)
colnames(x)<-c("actloc","survey", "year")

x$loc.id<-paste(x$actloc, x$year,x$survey, sep="_")
x<-as.data.frame(x$loc.id); colnames(x)<-"loc.id"

distrib$loc.id<-paste(distrib$actloc, distrib$year,distrib$survey, sep="_")
distrib<-merge(distrib, x, by="loc.id", all.x=F, all.y=T)


distrib$ID <-paste(distrib$ccode, distrib$year, sep="_")
distrib <- distrib[order(distrib$ID, distrib$headcount),] 
w <- reshape(distrib, 
             timevar = "headcount",
             idvar = c("actloc", "year", "survey", 
                       "ccode", "iso3n", "region", 
                       "loc.id", "country", "ID"),
             direction = "wide")

w$year <- floor(w$year)

foo <- data.frame(do.call('rbind', strsplit(as.character(w$actloc),'--',fixed=TRUE)))
#unique(foo$X2)
foo$X1 <- as.character(foo$X1)
foo$X2 <- as.character(foo$X2)
foo$X2 <- ifelse(foo$X2 != foo$X1 , foo$X2, "total")
names(foo)[2]<- "coverage"
names(foo)[1]<- "country"

w <- cbind(w, foo$coverage); 
w <- w[,c(1,110,2:109)]
names(w)[2]<- "coverage"
w <- w[,c(9, 5:6 ,2,11:110)] ;

dat <- w 
dat$ID <- paste(dat$ccode, dat$year, sep = "_")
dat <- dat[, c(105, 1:104)]

colnames(dat)[2]<-"country"
dat$TID <- paste(dat$country, dat$year, dat$survey, dat$coverage, sep = "_") # <- mind country-years in which 
#   multiple "nested" surveys were 
#   conducted (india- urban&rural e.g.)
dat <- dat[, c(106, 1:105)]
length(unique(dat$TID)) == nrow(dat) #check if observations occur more than once


# Subset for total coverage
povcal.total <- subset(dat, dat$coverage=="total")

#reshape total
povcal.total<-  reshape(povcal.total, 
                        varying = c(7:106), 
                        v.names = "monthly.inc.thresh", 
                        timevar = "p", 
                        times = c(7:106), 
                        direction = "long")
povcal.total <- povcal.total[order(povcal.total$ID, povcal.total$year, povcal.total$p),] 
povcal.total$p <- (povcal.total$p-6) / 100
povcal.total$pop.share <- 0.01
x<-aggregate(povcal.total$monthly.inc.thresh, by = list(povcal.total$TID), max)
colnames(x)<-c("TID","max")

povcal.total <- merge(povcal.total, x , by="TID", all.x = T)

#----------------------------------------------------------------------------------------
# Survey Means 
#----------------------------------------------------------------------------------------

z <- subset(povcal.total, select= c("TID", "monthly.inc.thresh"))
z$svy.type <- substr(z$TID,10,10)
z$ccode <- substr(z$TID, 1,3)
z$svy.mean <- ave(z$monthly.inc.thresh, z$TID, FUN=mean)
z <- subset(z, select = c("TID","ccode", "svy.mean","svy.type"))

# Since Povcal Data is in USD 2005ppp and the 1,90 poverty line is in USD 2011ppp
# we need to apply a conversion:

#compute the 2005 ppp convertion rates
#income surveys
load("../Data/pppconv.Rdata")

pppconv$I <- pppconv$pppconv_GDP_2005/pppconv$pppconv_GDP_2011 
pppconv$C <- pppconv$pppconv_C_2005/pppconv$pppconv_C_2011 

#subset of ratio of convertions
pppconv <- subset(pppconv, select = c("ccode","I","C"), pppconv$year==2005)

#Correct Survey means
z <- merge(z, pppconv, by="ccode", all.x=T)

z$svy.mean <- ifelse(z$svy.type=="C" & !is.na(z$C)==T, 
                     z$svy.mean*z$C,
                     z$svy.mean*z$I)
z$svy.mean <- ifelse(z$svy.type=="I" & is.na(z$I)==T,
                     z$svy.mean*z$C,
                     z$svy.mean)


z <- subset(z, select=c("TID","svy.mean"))





povcal.total$inc.share <- povcal.total$monthly.inc.thresh / 
  ave(povcal.total$monthly.inc.thresh,
      povcal.total$TID, FUN = sum)

povcal.total <- povcal.total[order(povcal.total$TID, povcal.total$p),]
povcal.total$inc.share <- ave(povcal.total$inc.share,
                              povcal.total$TID, FUN=cumsum)

povcal <- subset(povcal.total, select=c("TID","ID","p","inc.share"))

rm(list=setdiff(ls(), 
                c("gdp","population.io","povcal", "z","pppconv")))



# we need povcal in quintile format to match Poverty Equity and UNUWIDER:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
povcal <- subset(povcal,  povcal$p==0.2 |
                   povcal$p==0.4 |
                   povcal$p==0.6 |
                   povcal$p==0.8 |
                   povcal$p==1)



#Get rid of consumption survey if consumption and income surveys are both contained
# (POVCAL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Consumption or Income Survey?
povcal$survey.type <- substr(povcal$TID, 10, 10)

#How many surveys do we have for each ID
povcal$survey.no <- ave(povcal$ID, povcal$ID, FUN=length)

povcal$survey.no <- ifelse(povcal$survey.no==5, 1, 2)

#Now we select the income Surveys! (gets a 1 instead of a 2)
povcal$survey.no <- ifelse(povcal$survey.no==2 & 
                             povcal$survey.type=="I",
                           1,povcal$survey.no)

povcal <- subset(povcal, povcal$survey.no==1)



#Income Shares
#Present income shares are incorrect since they are not incomeshares but a cumsum
#Hence we substract the observation [i-1] from [i] and set [i-1]=0 wherever it equals 1

x<-povcal$inc.share; x<-as.data.frame(x)
y<-x[c(nrow(x), 1:(nrow(x)-1)), ]; y<-as.data.frame(y)

#to obtain inc shares, set b=0 if b=1 and substract a-b
x<-cbind(x,y); colnames(x)<-c("a","b"); rm(y)
x$b<-ifelse(x$a - x$b <0, 0, x$b)
x$inc.share <- x$a - x$b

#Correct Povcal Inc Shares
povcal$quintile.share <- x$inc.share
povcal <- povcal[,-c(4)]




#Year and Country
povcal$year <- substr(povcal$ID,5,8)
povcal$country <- substr(povcal$ID,1,3)
povcal$popshare <- 0.01
povcal <- povcal[,c("TID","ID","country","year","p","popshare","quintile.share")]



#Population Base Year
load("../Data/population.io.Rdata")
pop <- subset(population.io, select=c("ID","poptotal"))
povcal <- merge(povcal, pop, by="ID", all.x=T, all.y=F); rm(pop)
colnames(povcal)<-c("ID","TID","country","year","quintile","popshare","inc.share","pop")


#Population Projections

a <- subset(population.io, year %in% c(2012:2021), select=c("ccode","poptotal","year"))
a$year <- paste0("pop.",substr(a$year,3,4))
a <- spread(a,key = "year",value = "poptotal")
names(a)[1] <- "country"
povcal <- merge(povcal, a, by="country");rm(a)


# Add GDP Data and Projection

load("../Data/gdp.Rdata")

anchor<-subset(gdp, select=c("ID","gdp.imf","gdp.2012","gdp.2013","gdp.2014",
                             "gdp.2015","gdp.2016","gdp.2017",
                             "gdp.2018","gdp.2019","gdp.2020","gdp.2021"))
povcal<- merge(povcal, anchor, by="ID", all.x=T , all.y=F)

povcal$gdp.capita.imf <- povcal$gdp.imf / povcal$pop
povcal$gdp.capita.2012 <- povcal$gdp.2012 / povcal$pop.12
povcal$gdp.capita.2013 <- povcal$gdp.2013 / povcal$pop.13
povcal$gdp.capita.2014 <- povcal$gdp.2014 / povcal$pop.14
povcal$gdp.capita.2015 <- povcal$gdp.2015 / povcal$pop.15
povcal$gdp.capita.2016 <- povcal$gdp.2016 / povcal$pop.16
povcal$gdp.capita.2017 <- povcal$gdp.2017 / povcal$pop.17
povcal$gdp.capita.2018 <- povcal$gdp.2018 / povcal$pop.18
povcal$gdp.capita.2019 <- povcal$gdp.2019 / povcal$pop.19
povcal$gdp.capita.2020 <- povcal$gdp.2020 / povcal$pop.20
povcal$gdp.capita.2021 <- povcal$gdp.2021 / povcal$pop.21



#order
povcal <- povcal[order(povcal$ID, povcal$quintile),]
rm(anchor,x)



#Correct population shares
povcal$popshare <- 0.2
povcal<-povcal[order(povcal$ID,povcal$quintile),]




# P and L Values:
povcal$p <- ave(povcal$popshare, povcal$TID, FUN=cumsum)
povcal$L <- ave(povcal$inc.share, povcal$TID, FUN=cumsum)




#Insert Survey Mean into POVCAL datasets 

z <- z[!duplicated(z$TID),]
povcal <- merge(povcal, z, by="TID", all.x = T, all.y = F)
# SVY means are monthly - multiply by 12 to obtain yearly value
povcal$svy.mean <- povcal$svy.mean*12



# svy.mean projection (same growth rate as in imf projections)
# Note that the gdp.capita variable that is used for this projection already refears to
# the year the survey took place
end <- strsplit(names(povcal)[tail(grep("gdp.20",names(povcal)),1)],".",fixed = T)[[1]][2] %>% as.numeric
start <- strsplit(names(povcal)[head(grep("gdp.20",names(povcal)),1)],".",fixed = T)[[1]][2] %>% as.numeric

a <- subset(povcal,select = grep("gdp.capita.20",names(povcal))) 
a <- replicate(ncol(a),povcal$svy.mean) * (a/replicate(ncol(a), povcal$gdp.capita.imf) )

names(a) <- paste0("svy.mean.",start:end)

povcal <- data.frame(povcal, a);rm(a)

 ### the above 6 lines replaced this bottom part
# #2012
# povcal$svy.mean.2012 <- povcal$svy.mean * (
#   povcal$gdp.capita.2012/povcal$gdp.capita.imf)
# 
# #2013
# povcal$svy.mean.2013 <- povcal$svy.mean * (
#   povcal$gdp.capita.2013/povcal$gdp.capita.imf)



#####################################################################

# Merge IPC Data with Povcal

load("../Data/ipcdata.Rdata")
povcal <- merge(povcal, ipc.data[, c(1,3)], by="country",all.x = T)

povcal$cons.2012 <- ifelse(is.na(povcal$cons.2012), povcal$svy.mean.2012, povcal$cons.2012)
povcal$cons.2013 <- povcal$cons.2012 * (povcal$gdp.capita.2013/povcal$gdp.capita.2012)
povcal$cons.2014 <- povcal$cons.2012 * (povcal$gdp.capita.2014/povcal$gdp.capita.2012)
povcal$cons.2015 <- povcal$cons.2012 * (povcal$gdp.capita.2015/povcal$gdp.capita.2012)
povcal$cons.2016 <- povcal$cons.2012 * (povcal$gdp.capita.2016/povcal$gdp.capita.2012)
povcal$cons.2017 <- povcal$cons.2012 * (povcal$gdp.capita.2017/povcal$gdp.capita.2012)
povcal$cons.2018 <- povcal$cons.2012 * (povcal$gdp.capita.2018/povcal$gdp.capita.2012)
povcal$cons.2019 <- povcal$cons.2012 * (povcal$gdp.capita.2019/povcal$gdp.capita.2012)
povcal$cons.2020 <- povcal$cons.2012 * (povcal$gdp.capita.2020/povcal$gdp.capita.2012)
povcal$cons.2021 <- povcal$cons.2012 * (povcal$gdp.capita.2021/povcal$gdp.capita.2012)


# use consumption data instead of survey mean IF survey mean is not available
povcal$svy.mean.2012 <- ifelse(is.na(povcal$svy.mean.2012), povcal$cons.2012, povcal$svy.mean.2012)
povcal$svy.mean.2013 <- ifelse(is.na(povcal$svy.mean.2013), povcal$cons.2013, povcal$svy.mean.2013)
povcal$svy.mean.2014 <- ifelse(is.na(povcal$svy.mean.2014), povcal$cons.2014, povcal$svy.mean.2014)
povcal$svy.mean.2015 <- ifelse(is.na(povcal$svy.mean.2015), povcal$cons.2015, povcal$svy.mean.2015)
povcal$svy.mean.2016 <- ifelse(is.na(povcal$svy.mean.2016), povcal$cons.2016, povcal$svy.mean.2016)
povcal$svy.mean.2017 <- ifelse(is.na(povcal$svy.mean.2017), povcal$cons.2017, povcal$svy.mean.2017)
povcal$svy.mean.2018 <- ifelse(is.na(povcal$svy.mean.2018), povcal$cons.2018, povcal$svy.mean.2018)
povcal$svy.mean.2019 <- ifelse(is.na(povcal$svy.mean.2019), povcal$cons.2019, povcal$svy.mean.2019)
povcal$svy.mean.2020 <- ifelse(is.na(povcal$svy.mean.2020), povcal$cons.2020, povcal$svy.mean.2020)
povcal$svy.mean.2021 <- ifelse(is.na(povcal$svy.mean.2021), povcal$cons.2021, povcal$svy.mean.2021)


rm(list=setdiff(ls(), c("povcal","gdp","population.io","pppconv")))


save(povcal,file="../Data/povcal.Rdata")
