t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(t)

require(foreign)

#--------------------------------------------------------------------------------------
#
# UNUWIDER
#
#--------------------------------------------------------------------------------------

unuwider <- read.dta("../Data/unuwider.dta")
unuwider <- with(unuwider, unuwider[!(is.na(Q1)), ])
unuwider$ID <- paste(unuwider$ccode, unuwider$year, sep="_")


#DATAFRAME
total <- unuwider
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

unuwider <- dat


#Year and Country
unuwider$year <- substr(unuwider$ID,5,8)
unuwider$quintile.share <- unuwider$quintile.share/100
unuwider$popshare <- 0.2
unuwider <- unuwider[,c("ID","country","year","quintile","popshare","quintile.share")]



#Population Base Year
load("../Data/population.io.Rdata")

pop <- subset(population.io, select=c("ID","poptotal"))
unuwider <- merge(unuwider, pop, by="ID", all.x=T, all.y=F); rm(pop)
colnames(unuwider)<-c("ID","country","year","quintile","popshare","inc.share","pop")



#Population Projections
pop12<-subset(population.io, year==2012, select=c("ccode","poptotal"))
colnames(pop12) <- c("country","pop.12")
unuwider <- merge(unuwider, pop12, by="country")

pop13<-subset(population.io, year==2013, select=c("ccode","poptotal"))
colnames(pop13) <- c("country","pop.13")
unuwider <- merge(unuwider, pop13, by="country")

pop14<-subset(population.io, year==2014, select=c("ccode","poptotal"))
colnames(pop14) <- c("country","pop.14")
unuwider <- merge(unuwider, pop14, by="country")

pop15<-subset(population.io, year==2015, select=c("ccode","poptotal"))
colnames(pop15) <- c("country","pop.15")
unuwider <- merge(unuwider, pop15, by="country")

pop16<-subset(population.io, year==2016, select=c("ccode","poptotal"))
colnames(pop16) <- c("country","pop.16")
unuwider <- merge(unuwider, pop16, by="country")

pop17<-subset(population.io, year==2017, select=c("ccode","poptotal"))
colnames(pop17) <- c("country","pop.17")
unuwider <- merge(unuwider, pop17, by="country")

pop18<-subset(population.io, year==2018, select=c("ccode","poptotal"))
colnames(pop18) <- c("country","pop.18")
unuwider <- merge(unuwider, pop18, by="country")

pop19<-subset(population.io, year==2019, select=c("ccode","poptotal"))
colnames(pop19) <- c("country","pop.19")
unuwider <- merge(unuwider, pop19, by="country")

pop20<-subset(population.io, year==2020, select=c("ccode","poptotal"))
colnames(pop20) <- c("country","pop.20")
unuwider <- merge(unuwider, pop20, by="country")

pop21<-subset(population.io, year==2021, select=c("ccode","poptotal"))
colnames(pop21) <- c("country","pop.21")
unuwider <- merge(unuwider, pop21, by="country")
rm(pop16, pop17, pop12,pop13,pop14,pop15,pop18,pop19,pop20,pop21)


#GDP

load("../Data/gdp.Rdata")

anchor<-subset(gdp, select=c("ID","gdp.imf","gdp.2012","gdp.2013","gdp.2014","gdp.2015",
                             "gdp.2016","gdp.2017","gdp.2018","gdp.2019","gdp.2020",
                             "gdp.2021"))
unuwider<- merge(unuwider, anchor, by="ID", all.x=T , all.y=F)

unuwider$gdp.capita.imf <- unuwider$gdp.imf / unuwider$pop

unuwider$gdp.capita.2012 <- unuwider$gdp.2012 / unuwider$pop.12
unuwider$gdp.capita.2013 <- unuwider$gdp.2013 / unuwider$pop.13
unuwider$gdp.capita.2014 <- unuwider$gdp.2014 / unuwider$pop.14
unuwider$gdp.capita.2015 <- unuwider$gdp.2015 / unuwider$pop.15
unuwider$gdp.capita.2016 <- unuwider$gdp.2016 / unuwider$pop.16
unuwider$gdp.capita.2017 <- unuwider$gdp.2017 / unuwider$pop.17
unuwider$gdp.capita.2018 <- unuwider$gdp.2018 / unuwider$pop.18
unuwider$gdp.capita.2019 <- unuwider$gdp.2019 / unuwider$pop.19
unuwider$gdp.capita.2020 <- unuwider$gdp.2020 / unuwider$pop.20
unuwider$gdp.capita.2021 <- unuwider$gdp.2021 / unuwider$pop.21



#order
unuwider <- unuwider[order(unuwider$ID, unuwider$quintile),]
rm(anchor)

unuwider$p <- ave(unuwider$popshare, unuwider$ID, FUN=cumsum)
unuwider$L <- ave(unuwider$inc.share, unuwider$ID, FUN=cumsum)





rm(list=setdiff(ls(), c("povcal","poverty.equity",
                        "unuwider","gdp","population.io","pppconv")))

unuwider$TID <- paste(unuwider$ID, "P_total", sep="_")

unuwider$quintile <- as.numeric(as.factor(unuwider$quintile))*0.2



# Since we cannot rely on Survey means using the UNUWIDER database, we need
# to use gdp.capita (idealy we would use gni instead of gdp). We have to specify these
# gdp/capita variables as survey mean in order for the two data frames to have the same
# columns so we can rbind

unuwider$svy.mean <- NA
svy.means <- data.frame(matrix(NA,nrow = nrow(unuwider),ncol = (21-12+1)))
names(svy.means) <- paste0("svy.mean.",2012:2021)

unuwider <- cbind(unuwider,svy.means)



# merge consumption data (con/capita) to poverty equity database
load("../Data/ipcdata.Rdata")

unuwider <- merge(unuwider, ipc.data[, c(1,3)], by="country")

# if consumption data is not available we use the survey mean instead.
unuwider$cons.2012  <- 
  ifelse(is.na(unuwider$cons.2012), unuwider$svy.mean.2012, unuwider$cons.2012)

unuwider$cons.2013 <- 
  unuwider$cons.2012 * (unuwider$gdp.capita.2013/unuwider$gdp.capita.2012)

unuwider$cons.2014 <- 
  unuwider$cons.2012 * (unuwider$gdp.capita.2014/unuwider$gdp.capita.2012)

unuwider$cons.2015 <- 
  unuwider$cons.2012 * (unuwider$gdp.capita.2015/unuwider$gdp.capita.2012)

unuwider$cons.2016 <- 
  unuwider$cons.2012 * (unuwider$gdp.capita.2016/unuwider$gdp.capita.2012)

unuwider$cons.2017 <- 
  unuwider$cons.2012 * (unuwider$gdp.capita.2017/unuwider$gdp.capita.2012)

unuwider$cons.2018 <- 
  unuwider$cons.2012 * (unuwider$gdp.capita.2018/unuwider$gdp.capita.2012)

unuwider$cons.2019 <- 
  unuwider$cons.2012 * (unuwider$gdp.capita.2019/unuwider$gdp.capita.2012)

unuwider$cons.2020 <-
  unuwider$cons.2012 * (unuwider$gdp.capita.2020/unuwider$gdp.capita.2012)

unuwider$cons.2021 <- 
  unuwider$cons.2012 * (unuwider$gdp.capita.2021/unuwider$gdp.capita.2012)


rm(list=setdiff(ls(), c("povcal","poverty.equity","unuwider","gdp")))

save(unuwider,file = "../Data/unuwider.Rdata")
