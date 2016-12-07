t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())



sat <- read.csv("E:/Drive/WDL_Data/Poverty Clock/Data/night_time_lights_mean.csv")



sat$year <- substr(sapply(strsplit(as.character(sat$system.index), "_"), "[",1),4,7)
