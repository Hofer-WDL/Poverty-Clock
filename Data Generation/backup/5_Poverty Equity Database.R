t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())

require(foreign)
#--------------------------------------------------------------------------------------
#
#Poverty Equity Database
#
#--------------------------------------------------------------------------------------

poverty.equity <- read.dta("../Data/poverty-equity.dta")
code.correction <- data.frame(old = c("KSV","ROM","TMP","WBG","ZAR"),new = c("KSV","ROU","TLS","PSE","COD"))
new <- as.character( code.correction$new[match(poverty.equity$ccode,code.correction$old)])
poverty.equity$ccode[!is.na(new)] <- new[!is.na(new)]
rm(new)


poverty.equity <- with(poverty.equity, poverty.equity[!(is.na(Q1)), ])
poverty.equity$ID <- paste(poverty.equity$ccode, poverty.equity$year, sep="_")


#DATAFRAME
total <- poverty.equity
#getting rid of everything except most recent year
x<-aggregate(total$year, by = list(total$ccode), max); colnames(x)<-c("ccode","year")
x$ID<-paste(x$ccode, x$year, sep="_"); x<-as.data.frame(x$ID); colnames(x)<-"ID"
total<-merge(total, x, by="ID", all.x=F, all.y=T)


dat<-subset(total, select = c("ID", "ccode","Q1","Q2","Q3","Q4","Q5"))
colnames(dat)[2]<-"country"

length(unique(dat$ID)) == nrow(dat) #check if observations occur more than once
duplicates<-subset(dat, duplicated(dat$ID)==TRUE)
dat<-subset(dat, duplicated(dat$ID)==FALSE)

q1<-subset(dat, select=c("country","ID","Q1")); q1$quintile<-"q1"; colnames(q1)[3]<-"x"
q2<-subset(dat, select=c("country","ID","Q2")); q2$quintile<-"q2"; colnames(q2)[3]<-"x"
q3<-subset(dat, select=c("country","ID","Q3")); q3$quintile<-"q3"; colnames(q3)[3]<-"x"
q4<-subset(dat, select=c("country","ID","Q4")); q4$quintile<-"q4"; colnames(q4)[3]<-"x"
q5<-subset(dat, select=c("country","ID","Q5")); q5$quintile<-"q5"; colnames(q5)[3]<-"x"
dat<-rbind(q1,q2,q3,q4,q5) ; rm(q1,q2,q3,q4,q5)

colnames(dat)[3]<-"quintile.share"
dat<-dat[order(dat[,2], dat[,4]),]

poverty.equity <- dat


#Year and Country
poverty.equity$year <- substr(poverty.equity$ID,5,8)
poverty.equity$quintile.share <- poverty.equity$quintile.share/100
poverty.equity$popshare <- 0.2
poverty.equity <- poverty.equity[,c("ID","country","year","quintile","popshare","quintile.share")]


#Population Base Year
load("../Data/population.io.Rdata")

pop <- subset(population.io,year %in% poverty.equity$year, select=c("ID","poptotal"))

poverty.equity <- merge(poverty.equity, pop, by="ID", all.x=T, all.y=F); rm(pop)

colnames(poverty.equity)<-c("ID","country","year","quintile","popshare","inc.share","pop")


a <- subset(population.io, year %in% c(2012:2021), select=c("ccode","poptotal","year"))
a$year <- paste0("pop.",substr(a$year,3,4))
a <- spread(a,key = "year",value = "poptotal")
names(a)[1] <- "country"
poverty.equity <- merge(poverty.equity, a, by="country");rm(a)


#GDP
load("../Data/gdp.Rdata")

anchor<-subset(gdp, select=c("ID","gdp.imf","gdp.2012","gdp.2013","gdp.2014","gdp.2015",
                             "gdp.2016","gdp.2017","gdp.2018","gdp.2019","gdp.2020",
                             "gdp.2021"))


poverty.equity<- merge(poverty.equity, anchor, by="ID", all.x=T , all.y=F)




poverty.equity$gdp.capita.imf <- poverty.equity$gdp.imf / poverty.equity$pop


yearset <- as.numeric( substr(names(poverty.equity)[ grep("gdp.2",names(poverty.equity))] ,5,8))
gdp.capita <- subset(poverty.equity,select =  paste0("gdp.",yearset))/subset(poverty.equity,select = paste0("pop.",(yearset-2000)))
names(gdp.capita) <- paste0("gdp.capita.",yearset)
poverty.equity <- cbind(poverty.equity,gdp.capita)

# 
# ##########################################
# # Survey mean substitudes
# ##########################################
# poverty.equity$svy.mean <- NA
# svy.means <- data.frame(matrix(NA,nrow = nrow(poverty.equity),ncol = (21-12+1)))
# names(svy.means) <- paste0("svy.mean.",2012:2021)
# #poverty.equity <- data.frame(poverty.equity,a);rm(a)
# 
# 
# #########Household expenditure
# 
# load("../Data/household_expenditure.RData")
# HH_ex$ccode <- substr(HH_ex$ID,1,3)
# HH_ex$year <- substr(HH_ex$ID,5,8)
# HH_ex$var <- paste0("HH_ex_cap.",HH_ex$year)
# 
# poverty.equity$svy.mean[is.na(poverty.equity$svy.mean)] <- HH_ex$HH_expenditure_2011PPP_percap[match(poverty.equity$ID,HH_ex$ID)][is.na(poverty.equity$svy.mean)]
# 
# # # We could use the avg growth rates of hh expenditure as of now we use the same growthrate as the IMF gdp/cap projections. This way it resembles the same procedure like the survey means in povcal
# # HH_ex <- subset(HH_ex,year>2011,select = c("ccode","var","HH_expenditure_2011PPP_percap"))
# # HH_ex <- spread(HH_ex,key = "var",value = "HH_expenditure_2011PPP_percap")
# # poverty.equity<- merge(poverty.equity, HH_ex, by.x="country",by.y = "ccode", all.x=T , all.y=F)
# 
# 
# fun.svy.means <- function(x){ 
#   poverty.equity$svy.mean * (x/poverty.equity$gdp.capita.imf)}
# 
# svy.means[is.na(svy.means)]  <-  apply(gdp.capita,2,fun.svy.means)[is.na(svy.means)]
# 


#order
poverty.equity <- poverty.equity[order(poverty.equity$ID, poverty.equity$quintile),]


poverty.equity$p <- ave(poverty.equity$popshare, poverty.equity$ID, FUN=cumsum)
poverty.equity$L <- ave(poverty.equity$inc.share, poverty.equity$ID, FUN=cumsum)




rm(list=setdiff(ls(), c("povcal","gdp",
                        "poverty.equity","gdp","population.io","pppconv")))


# Since we cannot rely on Survey means using the poverty equity database, we need
# to use gdp.capita (idealy we would use gni instead of gdp). We have to specify these
# gdp/capita variables as survey mean in order for the two data frames to have the same
# columns so we can rbind
# the is.na command is in there in the case we use hh expenditure. In that case we only want to use GDP where we  dont't have HH expenditure data
poverty.equity$TID <- paste(poverty.equity$ID, "P_total", sep="_")

poverty.equity$svy.mean <- poverty.equity$gdp.capita.imf
poverty.equity$svy.mean.2012 <- poverty.equity$gdp.capita.2012
poverty.equity$svy.mean.2013 <- poverty.equity$gdp.capita.2013
poverty.equity$svy.mean.2014 <- poverty.equity$gdp.capita.2014
poverty.equity$svy.mean.2015 <- poverty.equity$gdp.capita.2015
poverty.equity$svy.mean.2016 <- poverty.equity$gdp.capita.2016
poverty.equity$svy.mean.2017 <- poverty.equity$gdp.capita.2017
poverty.equity$svy.mean.2018 <- poverty.equity$gdp.capita.2018
poverty.equity$svy.mean.2019 <- poverty.equity$gdp.capita.2019
poverty.equity$svy.mean.2020 <- poverty.equity$gdp.capita.2020
poverty.equity$svy.mean.2021 <- poverty.equity$gdp.capita.2021

poverty.equity$quintile <- as.numeric(as.factor(poverty.equity$quintile))*0.2

# merge consumption data (con/capita) to poverty equity database
load("../Data/ipcdata.Rdata")

poverty.equity <- merge(poverty.equity, ipc.data[, c(1,3)], by="country")

# if consumption data is not available we use the survey mean instead.
poverty.equity$cons.2012  <- 
  ifelse(is.na(poverty.equity$cons.2012), poverty.equity$svy.mean.2012, poverty.equity$cons.2012)

poverty.equity$cons.2013 <- 
  poverty.equity$cons.2012 * (poverty.equity$gdp.capita.2013/poverty.equity$gdp.capita.2012)

poverty.equity$cons.2014 <- 
  poverty.equity$cons.2012 * (poverty.equity$gdp.capita.2014/poverty.equity$gdp.capita.2012)

poverty.equity$cons.2015 <- 
  poverty.equity$cons.2012 * (poverty.equity$gdp.capita.2015/poverty.equity$gdp.capita.2012)

poverty.equity$cons.2016 <- 
  poverty.equity$cons.2012 * (poverty.equity$gdp.capita.2016/poverty.equity$gdp.capita.2012)

poverty.equity$cons.2017 <- 
  poverty.equity$cons.2012 * (poverty.equity$gdp.capita.2017/poverty.equity$gdp.capita.2012)

poverty.equity$cons.2018 <- 
  poverty.equity$cons.2012 * (poverty.equity$gdp.capita.2018/poverty.equity$gdp.capita.2012)

poverty.equity$cons.2019 <- 
  poverty.equity$cons.2012 * (poverty.equity$gdp.capita.2019/poverty.equity$gdp.capita.2012)

poverty.equity$cons.2020 <-
  poverty.equity$cons.2012 * (poverty.equity$gdp.capita.2020/poverty.equity$gdp.capita.2012)

poverty.equity$cons.2021 <- 
  poverty.equity$cons.2012 * (poverty.equity$gdp.capita.2021/poverty.equity$gdp.capita.2012)


save(poverty.equity,file = "../Data/poverty.equity.Rdata")
