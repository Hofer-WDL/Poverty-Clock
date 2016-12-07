t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())
require(tidyr)

# # #  source all datagenerations
# file.sources = list.files(getwd(), pattern="*.R$", full.names=TRUE,
#                             ignore.case=TRUE)
# #don't source the files that contain zzz they are either not finished or not relevant here
# file.sources <- file.sources[-grep("zzz",file.sources)]
# ####exclude our own projections?
# ### file.sources <- file.sources[-grep("our_projection",file.sources)]
# sapply(file.sources,source,.GlobalEnv)



# Merging POVCAL, Poverty Equity Database and UNUWIDER into one Dataframe:
# (i) if we have POVCAL-Data we use POVCAL, else Poverty-Equity
# (ii) if we have neither POVCAL nor Poverty-Equity data, we use UNUWIDER

load("../Data/povcal.Rdata")
load("../Data/poverty.equity.Rdata")
load("../Data/unuwider.Rdata")
load("../Data/household_expenditure.RData")
load("../Data/GNI.Rdata")
load("../Data/SSD_SOM.Rdata")

povcal$source <- "povcal";        povcal$data <- 1
poverty.equity$source <- "poverty equity";  poverty.equity$data <- 2
unuwider$source <- "unuwider"; unuwider$data<- 3
S$source <- "Utz"; S$data <- 4




# (i)
# which countries in poverty equity are contained in povcal? (and are thus droped)
x <- subset(povcal, select=c("country", "data"))
x<-x[!duplicated(x$country),]
y <- subset(poverty.equity, select=c("country", "data"))
y<-y[!duplicated(y$country),]

# generate dummy: 1=not in povcal; 0=in povcal
z <- merge(x,y, by="country", all.x=T, all.y=T)
z$dummy <- ifelse(is.na(z$data.x),1,0)
z <- z[,-c(2,3)]

poverty.equity <- merge(poverty.equity, z, by="country", all.x = T, all.y = F)
x <- subset(poverty.equity, poverty.equity$dummy==1)
x <- x[,-c(66)]

data <- rbind(povcal, x)
rm(x,y,z)


# (ii)
# which countries in unuwider are contained in povcal OR poverty equity? (and are thus droped)
x <- subset(data, select=c("country", "data"))
x<-x[!duplicated(x$country),]
y <- subset(unuwider, select=c("country", "data"))
y<-y[!duplicated(y$country),]

# generate dummy: 1=not in povcal; 0=in povcal
z <- merge(x,y, by="country", all.x=T, all.y=T)
z$dummy <- ifelse(is.na(z$data.x),1,0)
z <- z[,-c(2,3)]

unuwider <- merge(unuwider, z, by="country", all.x = T, all.y = F)
x <- subset(unuwider, unuwider$dummy==1)
x <- x[,-c(66)]

data <- rbind(data, x)
rm(x,y,z)

data <- rbind(data,S)


# Correct L values (cumulative income share) if L has minor deviations from 1
data$L <- ifelse(data$L>0.9, 1, data$L)


# order dataframe 
data <- data[order(data$TID, data$quintile),]






#####merge data and household expenditure

data <- merge(data,HH_ex,by="ID",all.x = T)

#####merge data and GNI

GNI <- spread(GNI,key = time,value = GNI.cap)

###data <- merge(GNI,HH_ex,by="ID",all.x = T)



#######################################
# Projections further than 2021
#######################################


## add population projections
load("../Data/population.io.RData")
a <- subset(population.io, year %in% c(2022:2030), select=c("ccode","poptotal","year"))
a$year <- paste0("pop.",substr(a$year,3,4))
a <- spread(a,key = "year",value = "poptotal")
names(a)[1] <- "country"
data <- merge(data, a, by="country");rm(a)



#project the anchor to 2030 using the avg. growth rate of the last n years



projection <- function(dataframe = data ,variable, avg.timeframe=9,horizon =2030){
  var <- paste0(variable,".20")
  index <- grep(var,x = names(dataframe))
  time <- as.numeric(substr(names(dataframe), nchar(names(dataframe))-4+1, nchar(names(dataframe)))[index])
  forecast.time <- horizon-tail(time,1)
  avg.g <- (dataframe[,max(index)]/dataframe[,(max(index)-(avg.timeframe))])^(1/avg.timeframe)
    y <- data.frame(matrix(NA,nrow = nrow(dataframe),ncol=forecast.time))
  y[,1] <- avg.g*dataframe[,tail(index,1)]
  for(i in 2:forecast.time){y[,i] <- avg.g*y[,(i-1)]}
  names(y) <- paste0(var,(horizon-forecast.time+1):horizon  -2000 )
  y
    }


data <- data.frame(data, (projection(data,"svy.mean"))  )
data <- data.frame(data, (projection(data,"gdp"))       )
data <- data.frame(data, (projection(data,"gdp.capita"))       )
data <- data.frame(data, (projection(data,"cons"))      )
data <- data.frame(data, (projection(data,"HH.ex.capita")))







#####
#write a complete gdp and gdp/capita dataframe
# maybe move this into an extra script as it isn't actually a merge
# maybe include CIA data here
load("../Data/gdp.Rdata")

a <- subset(population.io, year %in% c(2012:2030), select=c("ccode","poptotal","year"))
a$year <- paste0("pop.20",substr(a$year,3,4))
a <- spread(a,key = "year",value = "poptotal")
names(a)[1] <- "country"
gdp_complete <- merge(gdp, a, by="country",all.x = T);rm(a)
gdp_complete <- gdp_complete[!duplicated(gdp_complete$ccode),-match(c("country","year","ID","gdp.imf"),names(gdp_complete))]
gdp_complete <- melt(gdp_complete,"ccode")
gdp_complete  <- separate(gdp_complete,"variable",into = c("var","time"))
gdp_complete <- spread(gdp_complete,key = var,value = value)
gdp_complete$gdp.capita <- with(gdp_complete,gdp/pop)
gdp_complete -> gdp_capita
#gdp_complete <- gdp_complete %>% melt(c("ccode","time")) %>%  unite(vartime,variable,time, remove = TRUE,sep = ".") %>%  spread(key =vartime,value = value)


# gdp/capita
# a <- subset(data,select = paste0("gdp.",2022:2030))/subset(data,select = paste0("pop.",22:30))
# names(a) <- paste0("gdp.capita.",2022:2030)
# data <- cbind(data,a)

save(data,file = "../Data/merged_surveys.Rdata")
save(gdp_capita,file = "../Data/gdp_capita.Rdata")

save.image("../ENVIRONMENT_Dataframes.RData")
