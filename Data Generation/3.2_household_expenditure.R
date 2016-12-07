t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())
require(tidyr)



#load household expenditure data from WB
HH_expenditure <- read.csv("../Data/HH_expenditure_Data.csv",na.strings = "..")
load("../Data/countrycodes_pop.io_WB.RData")

#rename the columns
names(HH_expenditure) <- c("Series.Name","Series.Code","cname","ccode",substr(names(HH_expenditure)[5:ncol(HH_expenditure)],1,5))

#only take the PPP expenditure series
HH_ex <- subset(HH_expenditure,HH_expenditure$Series.Name=="Household final consumption expenditure, PPP (constant 2011 international $)",-c(1,2));rm(HH_expenditure)

# adjust the countrycodes
HH_ex <- merge(HH_ex,countrycodes_pop.io_WB,by.x = "ccode",by.y="ccode_WB",all = T)
HH_ex$ccode <- HH_ex$ccode_pop.io
HH_ex <- HH_ex[,1:(ncol(HH_ex)-2)]

require(reshape)
HH_ex <- melt(HH_ex,id.vars = c("ccode","cname"))
names(HH_ex)[c(3,4)]<- c("year","HH_expenditure_2011PPP")
HH_ex$year <- substr(HH_ex$year,2,5)
HH_ex$ID <- paste(HH_ex$ccode,HH_ex$year,sep = "_")


#####################
# require(imputeTS)
# #maybe use logs? linearity in logs should be linearity in growthrates
# 
# na.interpolation(log(HH_ex[HH_ex$ccode=="NPL","HH_expenditure_2011PPP"]), option = "spline")
# 
# l <- split(HH_ex,HH_ex$ccode)
# timelength <- nrow(l[[1]])
# temp <- lapply(l,function(x) tryCatch(na.interpolation(x$HH_expenditure_2011PPP),error = function(e) rep(NA,timelength))   )
# l <- Map(cbind,l,temp)
# 
# a <- unsplit(l,HH_ex$ccode)




# reshape the data for easier access
temp <- subset(HH_ex,select = c("HH_expenditure_2011PPP","ccode","year"))
temp <- spread(temp,key = year,value = HH_expenditure_2011PPP)


# return the last year we have data on
last <- data.frame(ccode = temp$ccode,year = apply(temp,1,function(x)   max(as.numeric(names(temp)[!is.na(x)]) ,na.rm=T ) ))
last$year[last$year == -Inf] <- NA

# if row is all NA tail returns the countryname. as.numeric gets rid of those with a warning
last$HH_ex.last <- apply(temp,1,function(x){as.numeric(x[tail(which(!is.na(x)),1)])})


#####use gdp growth to determine forecast household expenditure to 2021

#load gdp data and match the rows (like merge)
load("../Data/gdp.RData")
last$gdp.last <- gdp$gdp.imf[match(paste(temp$ccode,last$year,sep = "_"),gdp$ID)]

# calculate the growthrates of gdp from the last year we have data to various years between 12 and 15. With those estimate the corresponding household expenditures
last$HH_ex.2012 <-    gdp$gdp.2012[match(last$ccode,gdp$ccode)]/last$gdp.last* last$HH_ex.last
last$HH_ex.2013 <-    gdp$gdp.2013[match(last$ccode,gdp$ccode)]/last$gdp.last* last$HH_ex.last
last$HH_ex.2014 <-    gdp$gdp.2014[match(last$ccode,gdp$ccode)]/last$gdp.last* last$HH_ex.last
last$HH_ex.2015 <-    gdp$gdp.2015[match(last$ccode,gdp$ccode)]/last$gdp.last* last$HH_ex.last
temp$`2012` <- last$HH_ex.2012
temp$`2013` <- last$HH_ex.2013
temp$`2014` <- last$HH_ex.2014
temp$`2015` <- last$HH_ex.2015
rm(last)

# Now we have 2015 for all years do the same estimation for 2015 to 2021
#maybe we can do this in one step actually

a <- subset(gdp,year==2015) # since each row is duplicated for each year it doesn't matter which year we take
a <- a[match(temp$ccode,a$ccode),]
b <- a[,paste0("gdp.",2015:2021)]

#b contains the gdp for the years 2015 to 21 devide this by gdp 2015 and multiply with expenditure of 2015
b <- replicate(ncol(b),temp$`2015`) * (b/replicate(ncol(b), b$gdp.2015) )
b <- b[,-1]
names(b) <- paste0("X",2016:2021)

temp <- data.frame(temp,b)
rm(a,b)

temp <- melt(temp,"ccode")

names(temp)[match("variable",names(temp))] <- "year"
temp$year <- substr(temp$year,2,5)

HH_ex <- merge(HH_ex,temp,by=c("ccode","year"),all=T)
rm(temp)
#Use the estimates only where we don't have data already
HH_ex$HH_expenditure_2011PPP[is.na(HH_ex$HH_expenditure_2011PPP)] <- HH_ex$value[is.na(HH_ex$HH_expenditure_2011PPP)]

HH_ex$ID <- paste(HH_ex$ccode,HH_ex$year,sep = "_")



load("../Data/population.io.Rdata")
population_io<- population.io;rm(population.io)

population_io$ccode <- as.character(population_io$ccode)
population_io$ccode <- factor(population_io$ccode)
population_io$ID <- paste(population_io$ccode,population_io$year,sep = "_")
population_io <- subset(population_io,population_io$year %in% HH_ex$year,-c(1,3))



HH_ex <- merge(subset(HH_ex,select = c("ID","HH_expenditure_2011PPP")),population_io,by="ID",all.y=T)
HH_ex$HH_expenditure_2011PPP_percap <- with(HH_ex,HH_expenditure_2011PPP/poptotal)




temp <- subset(HH_ex,substr(HH_ex$ID,5,8) %in% 2012:2021,select = c("ID","HH_expenditure_2011PPP_percap","ccode3"))
temp$year <- paste("HH.ex.capita",substr(temp$ID,5,8),sep=".")
temp$ID <- substr(temp$ID,1,3)
temp <- temp[,-match("ID",names(temp))]
temp <- spread(temp,key = year,value = HH_expenditure_2011PPP_percap)


HH_ex <- merge(subset(HH_ex,select = c("ID","HH_expenditure_2011PPP_percap","ccode3")),temp,by.y="ccode3",by.x = "ccode3")
HH_ex <- HH_ex[,-match("ccode3",names(HH_ex))]






save(HH_ex,file = "../Data/household_expenditure.RData")
