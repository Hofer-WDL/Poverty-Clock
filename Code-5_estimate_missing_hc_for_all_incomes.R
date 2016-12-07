#####################################################################
# Estimate missings
#####################################################################
# Set working directory and choose between home and work
t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Google Drive/WDL_Data/Poverty Clock")
rm(list=ls())

require(reshape2)
require(tidyr)
# maybe with MICE package for multiple imputations?
library(mice)

ptm <- Sys.time()

#contains our distributional estimates for some countries no NAs but does not contain every country
load("pdf.RDATA")
index <- dimnames(b)
#contains a list of all countries in the world
load("./Data/population.io.RData")
#contains our gdp estimates
#load("./Data/merged_surveys.Rdata")
load("./Data/gdp_capita.Rdata")
names(gdp_capita)[match("ccode",names(gdp_capita))] <- "country"

full_set <- unique(population.io$ccode)
no_dist  <- full_set[!full_set %in% index$country]



#extract from the distribution array only the headcount and total expenditure but for all times, countries and incomes
d <- b[,,,c("HC","Total_exp")]
# the dimension INF contains the whole population of a country
d_inf <- b[,,match("INF",dimnames(b)$income_level),c("HC","Total_exp")]

#melt the array into a dataframe
d <- melt(d)
# spread by HC and Total expenditure
d <- spread(d,key=variable,value=value)
# add an X to the icnome levels as the plain numbers sometimes cause problems when they are used as names
d$income_level <- paste0("X",d$income_level)


unique(d$time)
unique(d$income_level)

e <- expand.grid(no_dist,unique(d$time),unique(d$income_level),stringsAsFactors = F)
names(e) <- c("country","time","income_level")
e <- data.frame(e,HC = NA,Total_exp = NA )
d <- rbind(d,e);rm(e)


# d <- merge(subset(d,!income_level %in% c("X0","XInf")),subset(d,income_level =="XInf",c("country","time","HC","Total_exp")), by = c("country","time"))
# names(d) <- c("country" , "time","income_level", "HC","Total_exp"  ,"pop"  ,"Total_exp_inf")

d <- subset(d,!income_level %in% c("X0","XInf"))

d <- merge(d,gdp_capita[,-match(c("gdp"),names(gdp_capita))],all.x = T,by = c("country","time"))

require(countrycode)
countrycode(setdiff(unique(d$country),unique(gdp_capita$country)),"iso3c","country.name")

names(d)[match("gdp.capita",names(d))] <- "gdp.cap"
d$HC_sh <- d$HC/d$pop
d$time <- factor(d$time)
# is the split necesary? we coud use subsets, this would complicate apply commands though.
d <- split(d,d$income_level)


formula6 <- ~log(pop)* log(gdp.cap) *time
reg <- function(dataset){
lm(update(formula6,log(HC)~.), data = dataset[dataset$HC>1,])}

#list of all regressions
#l <- lapply( d[c("X11","X110")]   ,reg)
#lapply(l,summary)

pred <- function(data){
  regression <- reg(data)
  x <- data
  m <- is.na(x$HC) & !is.na(x$gdp.cap)
  x$HC[m] <- exp(model.matrix( formula6 , data=x[m,])  %*% coef(regression))
  return(x)}

t <- lapply( d   ,pred)
t <- do.call("rbind", t)

save(t,file = "imputedpdf.R")
load( "imputedpdf.R")
require(abind)
t <- subset(t,income_level %in% c("X11","X110"))
t <- melt(t,1:3)
t <- t[order(t$variable,t$income_level,t$time,t$country),]

index <- list(country = unique(as.character(t$country)),time = unique(as.character(t$time)), income_level = unique(t$income_level),variable = unique(as.character(t$variable)))

a <- array(t$value,dim = unlist(lapply(index,length)),dimnames = index)
names(dim(a)) <- names(index)
dimnames(a) <- index

test <- cbind(t,as.numeric(a))
a["SUR","2016","X110","HC"]
subset(t,country == "SUR"&time =="2016"&variable =="HC")
save(a,file = "imputedpdf.R")
ptm - Sys.time()


# 
# load("imputedpdf.R")
# require(abind)
# t <- subset(t,income_level %in% c("X11","X110"))
# 
# abind("HC" = lower,selected, INF=upper,along = match("income_level",names(dim(selected))),new.names =  ID)
# 
