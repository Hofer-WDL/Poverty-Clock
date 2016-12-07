t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())

require(reshape2)
require(tidyr)

GNI <- read.csv("../Data/GNI_cap_PPP11_Data.csv",na.strings = "..",stringsAsFactors = F)
GNI <- subset(GNI,Series.Code=="NY.GNP.MKTP.PP.KD")

GNI <- GNI[,-c(1,2)]
names(GNI)[-c(1,2)] <- paste0("gni.cap.",substr(names(GNI)[-c(1,2)],2,5))
X <- data.frame(matrix(NA,nrow = nrow(GNI),ncol=5));names(X) <- paste0("gni.cap.",2017:2021)
GNI <- data.frame(GNI,X);rm(X)

GNI <- melt(GNI,c("Country.Name","Country.Code"))
names(GNI) <- c("cname","ccode","time","GNI.cap")

# Subsetting for the years we need
GNI <- subset(GNI,time %in% paste0("gni.cap.",2012:2021))


l <- split(GNI,GNI$ccode)

my.forecast <- function(x){
  if(any(!is.na(x$GNI.cap))) {
    a <- subset(x,!is.na(GNI.cap),-c(1,2))
    a$time <- substr(a$time,9,12)
    fst <- head(a,1) %>%  as.numeric()
    lst <- tail(a,1) %>%  as.numeric()
    diff <- lst[1] - fst[1]
    growth <- (lst[2] / fst[2])^(1/diff)
    b <- tail(x$GNI.cap[!is.na(x$GNI.cap)],1)*(growth^ (1:nrow(x)- which(x$GNI.cap==tail(x$GNI.cap[!is.na(x$GNI.cap)],1))))
    x$GNI.cap[is.na(x$GNI.cap)] <- b[is.na(x$GNI.cap)]
  }
  return(x) }

l <- lapply(l, my.forecast)

GNI <- unsplit(l,GNI$ccode)

#spread(GNI,key = "time",value = "GNI.cap")
save(GNI,file = "../Data/GNI.RData")
