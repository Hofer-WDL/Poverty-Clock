t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())

require(xlsx)

code.correction <- data.frame(old = c("KSV","ROM","TMP","WBG","ZAR"),new = c("KSV","ROU","TLS","PSE","COD"))

class <- read.xlsx("../Data/WB_CLASS.xls",1)
class <- class[c(6:223),c(3,4,6,7)]
names(class) <- c("cname","ccode","region","inc")
class$ccode <- as.character(class$ccode)

new <- as.character( code.correction$new[match(class$ccode,code.correction$old)])
class$ccode[!is.na(new)] <- new[!is.na(new)]
rm(new,code.correction)

class$region <- factor(as.character(class$region))

class <- data.frame(apply(class,2,function(x) factor(as.character(x))))


save(class,file = "../Data/WB_class.RData")