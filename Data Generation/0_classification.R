t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())

require(xlsx)
continents <- read.csv("../Data/continents.csv")

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

class$region2 <- as.character(class$region)

#add the regional dummies to each country
west <- c("ADO","AUT","BEL","CHI","DNK","FRO","FIN","FRA","DEU","GIB","GRL","ISL","IRL","IMY","ITA","LIE","LUX","MCO","NLD","NOR","PRT","SMR","ESP","SWE","CHE","GBR")

east <- class$ccode[class$region=="Europe & Central Asia" & !class$ccode %in% west]

class$region2[class$ccode %in% west] <- "Western Europe"
class$region2[class$ccode %in% east] <- "Eastern Europe"
class$region2[class$region2 %in% c("South Asia", "East Asia & Pacific")] <- "Asia Pacific"

class$region2[class$ccode %in% "MEX"] <- "North America"


class <- merge(class,continents,"ccode",all=T)
class$continent[class$ccode %in% c("ADO","CHI","IMY","KSV")]<- "Europe"

class$region2[class$continent == "Asia" & class$region2 == "Eastern Europe"] <- "Asia Pacific"


class$region2 <- factor(class$region2)

save(class,file = "../Data/WB_class.RData") 
