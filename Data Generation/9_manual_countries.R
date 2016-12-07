t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())


require(tidyr)



#These numbers came from Utz Pape per mail 

S <- read.csv("../Data/Utz_Pape_SSD_SOM.csv", sep=";",stringsAsFactors = F)

aggregate(mean.inc~TID,S,mean)

S$svy.mean <- ave(S$mean.inc, S$TID, FUN=mean) * 12*365


S$inc.share <- S$mean.inc/ave(S$mean.inc, S$TID, FUN=sum)
S$popshare <- 0.2
S$p        <- S$quintile

S$L <- ave(S$inc.share, S$TID, FUN=cumsum)

#Population Projections and GDP Data and Projection

# bring it into the same format as previous work 
load("../Data/gdp_capita.Rdata")
gdp_capita$ID <- paste(gdp_capita$ccode,gdp_capita$time,sep = "_")
gdp <- melt(gdp_capita,c("ccode","time","ID"))
names(gdp_capita) <-c( "ccode","time","gdp.imf","pop","gdp.capita.imf","ID")

gdp$variable <- paste(gdp$variable, gdp$time, sep = ".")
gdp <- gdp[,c("ccode","variable","value")]
gdp <- spread(gdp, variable, value)
gdp <- merge(gdp_capita,gdp,by = "ccode"); rm(gdp_capita)

names(gdp) <-   gsub("pop.20",replacement = "pop.",names(gdp))
  

S <- merge(S,gdp,by = "ID")


# svy.mean projection (same growth rate as in imf projections)
# Note that the gdp.capita variable that is used for this projection already refers to
# the year the survey took place
end <- strsplit(names(S)[tail(grep("gdp.20",names(S)),1)],".",fixed = T)[[1]][2] %>% as.numeric
start <- strsplit(names(S)[head(grep("gdp.20",names(S)),1)],".",fixed = T)[[1]][2] %>% as.numeric

#extract all colums of gdp per capita except the base survey year column
a <- subset(S,select = grep("gdp.capita.20",names(S))) 
# replicate: creates a matrix that has the survey mean of the base year in every column that corresponds to the subset above i.e one survey mean for every year
# Then replicate gdp.capita from teh base year the same way 
a <- replicate(ncol(a),S$svy.mean) * (a/replicate(ncol(a), S$gdp.capita.imf) )

names(a) <- paste0("svy.mean.",start:end)

S <- data.frame(S, a);rm(a)


load("../Data/ipcdata.Rdata")

S <- merge(S, ipc.data[, c(1,3)], by="country",all.x = T)

end <- strsplit(names(S)[tail(grep("gdp.20",names(S)),1)],".",fixed = T)[[1]][2] %>% as.numeric
start <- strsplit(names(S)[head(grep("gdp.20",names(S)),1)],".",fixed = T)[[1]][2] %>% as.numeric

#extract all colums of gdp per capita except the base survey year column
a <- subset(S,select = grep("gdp.capita.20",names(S))) 
# replicate: creates a matrix that has the survey mean of the base year in every column that corresponds to the subset above i.e one survey mean for every year
# Then replicate gdp.capita from teh base year the same way 
a <- replicate(ncol(a),S$cons.2012) * (a/replicate(ncol(a), S$gdp.capita.2012) )

names(a) <- paste0("cons.",start:end)

S <- data.frame(S, a[,-1]);rm(a)


load("../Data/povcal.Rdata")

setdiff(names(povcal),names(S))

S <- S[,names(povcal)]

# View(rbind(head(povcal),S))

save(S,file = "../Data/SSD_SOM.Rdata")
