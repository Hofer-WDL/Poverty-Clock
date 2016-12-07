t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())

require(xlsx)
require(reshape2)
require(tidyr)

WEO_data <- read.delim("../Data/WEO/WEO_April_2016.xls",stringsAsFactors = F,dec = ".",na.strings = c("--","n/a",""))
names(WEO_data)
unique(WEO_data$Subject.Descriptor)



WEO_data[,grep("X[[:digit:]]",names(WEO_data))] <- apply(WEO_data[,grep("X[[:digit:]]",names(WEO_data))] ,2,
                                                         function(x){as.numeric(gsub(",","",x))})


########################################
##############PPP Conversion#
########################################
# This method is based on this question: https://www.imf.org/external/dforum/ListMessages.aspx?messageid=878&forumid=13
# It is also in the FAQ's of the World economic outlook. 

# Method applied to our case
# 1) Select a base year, 2011
# 
# 2) Take the GDP in current PPP dollars for your sample for this base year. (PPPGDP)
# 2011 Level of Series: Gross domestic product based on purchasing-power-parity (PPP) per capita GDP (Current international dollar)
# 
# 3) Apply the growth rates of GDP per capita in constant LCU (NGDP_R) forwards and backwards to the GDP per capita in current PPP dollars to extend the series.
# Growth rates of Series: Gross domestic product per capita, constant prices (National currency)


# set base
baseyear <- 2011

#get the baseyear for PPP GDP
WEO_base <- subset(WEO_data,
                   WEO.Subject.Code %in% c("PPPGDP"),c("WEO.Country.Code","ISO","Country",paste0("X",baseyear)  ))




# Get the constant local currency gdp data for all years
WEO <- subset(WEO_data,
              WEO.Subject.Code %in% c("NGDP_R"),c("WEO.Country.Code","ISO","Country",paste0("X",1980:2021)  ))



rm(WEO_data)

# for each year of constant local currency and country do the following:
# devide it by the level of constant LCU of the baseyear to get for each year and country the growthrate with respect to the base
# multiply this growthrate with the PPP GDP level of the baseyear
WEO[,grep("X[[:digit:]]",names(WEO))]  <- apply(WEO[,grep("X[[:digit:]]",names(WEO))] , 2 ,
                                          function(x){(x/WEO[,paste0("X",baseyear)] )  *WEO_base[,paste0("X",baseyear)] })

rm(WEO_base)

#average growthrate between 2012 and 2021
avg_growth <- with(WEO,(X2021/X2012)^(1/9))

projection_end <- 2035
# create a matrix that gives just the years past since 2021. Take the growthrate to the power of this matrix elementwise. Multiply each column of this matrix with the gdp of 2021 to get a matrix of the projected gdp for 2022 to projection_end 
a <- data.frame(WEO$X2021  *     avg_growth^matrix(rep(1:(projection_end -2021),nrow(WEO)),nrow=nrow(WEO),byrow = T) )
names(a) <- paste0("X",2022:projection_end )

WEO <- data.frame(WEO,a)


WEO <- melt(WEO,1:3)
WEO$year <- substr(WEO$variable,2,5)
WEO$miss <- is.na(WEO$value)
aggregate(miss~year,FUN = sum,data = WEO)

WEO$gdp <- WEO$value *1e9
WEO$ccode <- WEO$ISO
WEO$cname <- WEO$Country
WEO <- WEO[,c("ccode","cname","year","gdp")]






load("../Data/population.io.RData")

unique(population.io$country[population.io$ccode %in% setdiff(population.io$ccode,WEO$ccode)])
unique(WEO$cname[WEO$ccode %in% setdiff(WEO$ccode,population.io$ccode)])

WEO <- merge(WEO,population.io)

WEO$gdp.capita <- with(WEO,gdp/ poptotal)

WEO <- WEO[,c("ID","ccode","year","cname","gdp","poptotal","gdp.capita")]
WEO <- melt(WEO,1:4)


##################################################################################################################
##################################################################################################################
#replicate old gdp file
##################################################################################################################
##################################################################################################################

gdp <- subset(WEO,variable =="gdp")

X <- gdp
X$variable <- paste(X$variable,X$year,sep = ".")
X <- spread(X[,-c(1,3,4)],variable,value)


names(gdp)[c(4,6)] <- c("country","gdp.imf")
gdp <- gdp[,c("ccode","country","ID","year","gdp.imf")]

gdp$country <- gdp$ccode


gdp <- merge(gdp,X,by = "ccode")

save(gdp,file ="../Data/gdp.Rdata" )
