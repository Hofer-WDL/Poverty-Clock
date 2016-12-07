t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())



library(foreign)
library(plyr)
library(data.table)


population.io <- read.dta("../Data/population_data.dta")
population.io$ccode[population.io$location=="Channel Islands"] <- "CHA"

names(population.io)[names(population.io)=="location"] <- "country"

groups <- population.io

population.io <- 
  aggregate(poptotal ~ ccode+ ccode3 + year + country, data=population.io, FUN=sum)

population.io$ID <- paste(population.io$ccode, population.io$year, sep = "_")

population.io$poptotal <- population.io$poptotal*1000

save(population.io,file = "../Data/population.io.RData")


kidage = 16
require(tidyr)
require(reshape2)

groups$kid <- cut(groups$age,breaks = c(-Inf,kidage,Inf),labels = c("Kid","Adult"))
male <- aggregate(popmale~ccode+country+ year + kid,FUN = sum,data=groups)
male <- spread(male,key=kid,value = popmale)
names(male)[4:5] <- paste0("male_",names(male)[4:5])

female <- aggregate(popfemale~ccode+ year+kid,FUN = sum,data=groups)
female <- spread(female,key=kid,value = popfemale)
names(female)[3:4] <- paste0("female_",names(female)[3:4])

groups <- merge(male,female,by=c("ccode","year"))
total <- with(groups,male_Kid+male_Adult+female_Kid+female_Adult)
groups[-c(1:3)] <- groups[-c(1:3)] / total

save(groups,file = "../Data/popshares.RData")

#a <- merge(groups,population.io,by=c("ccode","year"))
