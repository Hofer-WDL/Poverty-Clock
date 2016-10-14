t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())

require(foreign)
# consumption ------------------------------------------

ipc.data <- read.dta("../Data/consumptiondata.dta")
ipc.data <- ipc.data[!duplicated(ipc.data),]

ipc.data$ID <- paste(ipc.data$country, "2011", sep = "_")


# gdp data to project cons data from 2011 to 2012
gdp.IMF <- read.dta("../Data/gdpimf.dta")

gdp.IMF$gdpimf <- gsub(",","",gdp.IMF$gdpimf)
gdp.IMF$gdpimf[gdp.IMF$gdpimf %in% c("n/a","--")] <- NA
gdp.IMF$gdpimf <- as.numeric(gdp.IMF$gdpimf)*10e8

gdp.IMF <- subset(gdp.IMF, gdp.IMF$year=="2011" | gdp.IMF$year=="2012" , select =  c("ccode", "year", "country", "gdpimf"))
gdp.IMF$ID <-  paste(gdp.IMF$ccode, gdp.IMF$year, sep = "_")
gdp.IMF$year <- as.character(gdp.IMF$year)

# reshape
x <-  subset(gdp.IMF, gdp.IMF$year=="2011", select=c("ccode","gdpimf"))
y <-  subset(gdp.IMF, gdp.IMF$year=="2012",  select=c("ccode","gdpimf"))
z <- merge(x,y, by="ccode")
rm(gdp.IMF, x, y)
z$changerate <- z$gdpimf.y/z$gdpimf.x
z <- subset(z, select=c("ccode","changerate"))
names(z)[1] <- "country"
ipc.data <- merge(ipc.data, z, by="country", all.y=T) 
rm(z)

ipc.data$cons.2012 <- ipc.data$cons * ipc.data$changerate
ipc.data <- ipc.data[,-c(2,4)]

# We don't have any other datapoint than 2012 we delete it here as it causes problems lateron
ipc.data$cons.2012[ipc.data$country=="BHS"] <- NA

save(ipc.data,file= "../Data/ipcdata.Rdata")
