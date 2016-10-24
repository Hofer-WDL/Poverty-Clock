t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())

# always run 2.1 GDP first!!

load("../Data/gdp.RData")
load("../Data/population.io.RData")

#Check which countries are in the poplulation set but not in the gdp set
a <- gdp[!duplicated(gdp$country),]

# CHeck which countries have incomplete data in the gdp set
incomplete_IMF_ccode <- a$country[ apply(a,1,function(x) { any(is.na(x[-c(1:5)]))})]

# return the countries that have holes in the gdp time series but not those with open ends ie. argentina. Write the latter into a vector of ccodes
incomplete_IMF <- a[a$ccode %in% incomplete_IMF_ccode,-c(1:4)]
incomplete_IMF_ccode <- incomplete_IMF$ccode[is.na(incomplete_IMF$gdp.2012)]
incomplete_IMF <- incomplete_IMF[!is.na(incomplete_IMF$gdp.2012),]

#using the imputeTS package we can impose linear imputations from the two observastions adjacent to the NAs. Using logs we can change this to average growth rates
require(imputeTS)
z <- apply(incomplete_IMF,1, function(x){
  x <- log( as.numeric(x[-1]))
  try(exp(na.interpolation(x , option = "linear")))} )
z <- data.frame(incomplete_IMF[,1],t(z))
names(z) <- names(incomplete_IMF)
rm(incomplete_IMF)

z <- merge(melt(gdp,c("country","ID","year","ccode")) , melt(z,c("ccode")),by = c("ccode","variable"),all=T)

z$value.x[is.na(z$value.x)] <- z$value.y[is.na(z$value.x)]

gdp <- spread(z[,-c(7)],key=variable,value = value.x)
rm(a,z)


a <- gdp[!duplicated(gdp$country),c("ccode","country")]
b <- population.io[!duplicated(population.io$ccode),c("ccode","country")]
a <- merge(a,b,by = "ccode",all = T); rm(b)
no_IMF_gdp <- a[is.na(a$country.x),]

WB_GDP_Data <- read.csv("../Data/WB_GDP_Data.csv", stringsAsFactors=FALSE,na.strings = "..")


code.correction <- data.frame(old = c("KSV","ROM","TMP","WBG","ZAR"),new = c("KSV","ROU","TLS","PSE","COD"))

new <- as.character( code.correction$new[match(WB_GDP_Data$Country.Code,code.correction$old)])
WB_GDP_Data$Country.Code[!is.na(new)] <- new[!is.na(new)]
rm(new,code.correction)

WB_GDP_Data <- head(WB_GDP_Data,-5)
WB_GDP_Data <- subset(WB_GDP_Data,Series.Name=="GDP, PPP (constant 2011 international $)")


no_IMF_gdp <- merge(no_IMF_gdp,WB_GDP_Data,by.x = "ccode",by.y = "Country.Code",all.x = T)
no_IMF_gdp <- no_IMF_gdp [!is.na(no_IMF_gdp$Series.Code),-c(2,4:6)]
names(no_IMF_gdp)[3:ncol(no_IMF_gdp)] <- paste0("gdp.",substr(names(no_IMF_gdp)[3:ncol(no_IMF_gdp)],2,5))

x <- data.frame(matrix(NA,nrow=nrow(no_IMF_gdp),ncol=6))
names(x) <-  paste0("gdp.",2016:2021)
no_IMF_gdp <- cbind(no_IMF_gdp,x);rm(x)
## Forecast CUBA gdp based on the last 5 years of growth
cub_growth <- (no_IMF_gdp$gdp.2013[no_IMF_gdp$ccode=="CUB"]  /no_IMF_gdp$gdp.2008[no_IMF_gdp$ccode=="CUB"] )^(1/5)
for(i in 26:33){no_IMF_gdp[no_IMF_gdp$ccode=="CUB",(i)] <- cub_growth * no_IMF_gdp[no_IMF_gdp$ccode=="CUB",(i-1)]}

## Forecast Gaza gdp based on the last 5 years of growth
gaza_growth <- (no_IMF_gdp$gdp.2015[no_IMF_gdp$ccode=="PSE"]  /no_IMF_gdp$gdp.2010[no_IMF_gdp$ccode=="PSE"] )^(1/5)

for(i in 28:33){no_IMF_gdp[no_IMF_gdp$ccode=="PSE",(i)] <- gaza_growth * no_IMF_gdp[no_IMF_gdp$ccode=="PSE",(i-1)]}
rm(cub_growth,gaza_growth)

frame <- expand.grid(no_IMF_gdp$ccode,1982:2016,stringsAsFactors = F)
names(frame) <- c("ccode","year")
frame$ID <- paste(frame$ccode,frame$year,sep="_")

require(reshape)
a <- melt(no_IMF_gdp,id.vars = c("ccode","country.y"))
a$variable <- substr(a$variable,5,8)
a$ID <- paste(a$ccode,a$variable,sep = "_")


frame <-merge(frame,a[,-c(1,2)],by = "ID",all.x = T)
frame$gdp.imf <- frame$value
frame <- subset(frame,select = c("ID","ccode","year","gdp.imf"))
frame <- merge(frame,   subset(no_IMF_gdp,select = -c(2:23))     ,by="ccode")
frame$country <- frame$ccode
gdp <- rbind(gdp,frame)
rm(a,no_IMF_gdp,frame,WB_GDP_Data)

x <- (gdp[gdp$ccode %in% incomplete_IMF_ccode,])
# only keep the lines that have at least one datapoint
x <- x[apply(x[,-c(1:5)],1,function(y) any(!is.na(y)) ),]


my.fun <- function(x){
  x <- as.numeric(x[grep("20",names(gdp))])
  no_na <- !is.na(x)
  lst <- (1:length(x)) [no_na][length(x[no_na])]  # return the index of the last entry that isn't NA
  fst <- (1:length(x)) [no_na][1]                 # return the index of the first entry that isn't NA
  index <- 1:length(x) - fst                      # we use the entry of fst as base. Index gives us the relative distance to the base in years. We can use that in combination with growth rates to impute 
  growth <-  (x[no_na][length(x[no_na])] /  x[no_na][1])^(1/(lst-fst))
  prediction <- unlist(x[no_na][1]) * growth^index
  y <- x
  y[is.na(x)] <- prediction[is.na(x)]
  y}

x <- data.frame(x[,c(1:5)]  ,t(apply(x,1,my.fun)));rm(my.fun)
names(x) <- names(gdp)
x <- melt(x,c(1:4))
x <- merge(melt(gdp,c(1:4)),x,by = c( "ccode","country" , "ID","year","variable"),all=T)
x$value.x[is.na(x$value.x)] <- x$value.y[is.na(x$value.x)]
names(x) <- c( "ccode","country" , "ID","year","variable","value","value.y")
x <- x[, c( "ccode","country" , "ID","year","variable","value")]
gdp <- spread(x,key=variable,value=value)

projection <- function(dataframe = data ,variable, avg.timeframe=9,horizon =2030){
  var <- paste0(variable,".20")
  index <- grep(var,x = names(dataframe))
  time <- as.numeric(substr(names(dataframe), nchar(names(dataframe))-4+1, nchar(names(dataframe)))[index])
  forecast.time <- horizon-tail(time,1)
  if (forecast.time == 0) stop("Horizon is already in the dataframe")
  avg.g <- (dataframe[,max(index)]/dataframe[,(max(index)-(avg.timeframe))])^(1/avg.timeframe)
  y <- data.frame(matrix(NA,nrow = nrow(dataframe),ncol=forecast.time))
  y[,1] <- avg.g*dataframe[,tail(index,1)]
  for(i in 2:forecast.time){y[,i] <- avg.g*y[,(i-1)]}
  names(y) <- paste0(var,(horizon-forecast.time+1):horizon  -2000 )
  y
}

gdp <- data.frame(gdp,projection(gdp,"gdp"))

save(gdp,file = "../Data/gdp.Rdata") 
