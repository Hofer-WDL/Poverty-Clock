t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())

require(foreign)
require(tidyr)
# consumption ------------------------------------------

ipc.data <- read.dta("../Data/consumptiondata.dta")
ipc.data <- ipc.data[!duplicated(ipc.data),]

ipc.data$ID <- paste(ipc.data$country, "2011", sep = "_")

load("../Data/gdp.Rdata")

gdp.IMF <- subset(gdp, year=="2011" | year=="2012" , select =  c("ccode", "year", "country", "gdp.imf"))
# the change of name to gdpimf is an artefact of changing the code afterwards
names(gdp.IMF)[match("gdp.imf",names(gdp.IMF))] <- "gdpimf"

gdp.IMF$year <- as.character(gdp.IMF$year)


a <- spread(gdp.IMF[,-match("ccode",names(gdp.IMF))],key = year,value = gdpimf)
a$changerate <- a$`2012`/a$`2011`
ipc.data <- merge(ipc.data,  subset(a, select=c("country","changerate")), by="country", all.y=T) 
rm(a)



ipc.data$cons.2012 <- ipc.data$cons * ipc.data$changerate
ipc.data <- ipc.data[,-c(2,4)]
ipc.data$ID <- paste0(ipc.data$country,"_2011")

# We don't have any other datapoint for bahamas other than 2012 we delete it here as it causes problems lateron
ipc.data$cons.2012[ipc.data$country=="BHS"] <- NA

save(ipc.data,file= "../Data/ipcdata.Rdata")

load("../Data/ipcdata.Rdata")
