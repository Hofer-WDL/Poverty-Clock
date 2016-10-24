t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock/Data Generation"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock/Data Generation")
rm(list=ls())

require(foreign)

gdp.IMF <- read.dta("../Data/gdpimf.dta")
pppconv <- read.csv("../Data/ppp-conversion-rates.csv", na.strings = "..")
pppconv$Country.Code <- as.character(pppconv$Country.Code)

code.correction <- data.frame(old = c("KSV","ROM","TMP","WBG","ZAR"),new = c("KSV","ROU","TLS","PSE","COD"))
new <- as.character( code.correction$new[match(pppconv$Country.Code,code.correction$old)])
pppconv$Country.Code[!is.na(new)] <- new[!is.na(new)]
rm(new)


#conversion pppconv to full & punct
gdp.IMF$gdpimf <- gsub(",","",gdp.IMF$gdpimf)
gdp.IMF$gdpimf[gdp.IMF$gdpimf %in% c("n/a","--")] <- NA
gdp.IMF$gdpimf <- as.numeric(gdp.IMF$gdpimf)*10e8

gdp.IMF <- subset(gdp.IMF, select =  c("ccode", "year", "country", "gdpimf"))

gdp.IMF$ID <-  paste(gdp.IMF$ccode, gdp.IMF$year, sep = "_")


#conversion pppconversion to full & punct
pppconv <- pppconv[,-c(1,4)]

colnames(pppconv) <- c("ccode","year","pppconv_GDP_2011","pppconv_C_2005",
                       "pppconv_GDP_2005", "pppconv_C_2011")

pppconv$ID <-  paste(pppconv$ccode, pppconv$year, sep = "_")


# apply PPP conversion
IMF <- merge(gdp.IMF, pppconv[, c("ID", "pppconv_GDP_2011")], by="ID", all.x=T)
colnames(IMF)[6] <- "pppconv"
IMF$pppconv <- as.numeric(IMF$pppconv)

IMF$gdp <- IMF$gdpimf/IMF$pppconv
gdp.IMF <- subset(IMF, select=c("ID","gdp"))
colnames(gdp.IMF) <- c("ID","gdp.imf")
rm(IMF)

gdp.IMF$year <- substr(gdp.IMF$ID, 5, 8);   gdp.IMF$ccode <- substr(gdp.IMF$ID, 1, 3)

gdp.IMF.cons.rate <- subset(gdp.IMF, (gdp.IMF$year == "2011" | gdp.IMF$year== "2012"), select = c("ccode", "gdp.imf", "year"))

gdp.IMF.cons.rate2011 <- subset(gdp.IMF.cons.rate, gdp.IMF.cons.rate$year =="2011")
gdp.IMF.cons.rate2012 <- subset(gdp.IMF.cons.rate, gdp.IMF.cons.rate$year =="2012")

gdp.IMF.cons.rate <- merge(gdp.IMF.cons.rate2011, gdp.IMF.cons.rate2012, by="ccode")
gdp.IMF.cons.rate <- subset(gdp.IMF.cons.rate, select = c("ccode", "gdp.imf.x", "gdp.imf.y"))
names(gdp.IMF.cons.rate)[1:3] <- c("country", "gdp.imf.2011", "gdp.imf.2012")
# GDP - PROJECTIONS (IMF)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IMF Projections for 2016 and 2017
imf<-read.dta("../Data/imf-projections.dta")
imf.update <- read.dta("../Data/imfgdpfull.dta")
imf.update <- subset(imf.update, select=c("ccode","y_2012","y_2013","y_2014","y_2015"), 
                     imf.update$units=="National currency")

my.convert <- function(x){
  a <- gsub(",","",x)
  a[a %in% c("n/a","--")] <- NA
  a <- as.numeric(a)*10e2
}

imf.update[,-1] <- apply(imf.update[-1],2,my.convert)
names(imf.update)[-1] <- paste0("gdp.",2012:2015)

imf[,-c(1:3)] <- apply(imf[,-c(1:3)],2,my.convert)

names(imf)[-c(1:3)] <- paste0("gdp.",2016:2021)



#ppp conversion for 2012 - 2015
x<-subset(pppconv, select=c("ccode","pppconv_GDP_2011"), pppconv$year==2012)
colnames(x)<- c("ccode","pppconv2012")
imf.update <- merge(imf.update, x, by="ccode", all.x=T)

x<-subset(pppconv, select=c("ccode","pppconv_GDP_2011"), pppconv$year==2013)
colnames(x) <- c("ccode","pppconv2013")
imf.update <- merge(imf.update, x, by="ccode", all.x=T)

x<-subset(pppconv, select=c("ccode","pppconv_GDP_2011"), pppconv$year==2014)
colnames(x) <- c("ccode","pppconv2014")
imf.update <- merge(imf.update, x, by="ccode", all.x=T)

x<-subset(pppconv, select=c("ccode","pppconv_GDP_2011"), pppconv$year==2015)
colnames(x) <- c("ccode","pppconv2015")
imf.update <- merge(imf.update, x, by="ccode", all.x=T)


imf.update$pppconv2012 <- as.numeric(imf.update$pppconv2012)
imf.update$pppconv2013 <- as.numeric(imf.update$pppconv2013)
imf.update$pppconv2014 <- as.numeric(imf.update$pppconv2014)
imf.update$pppconv2015 <- as.numeric(imf.update$pppconv2015)

imf.update$gdp.2012 <- imf.update$gdp.2012/imf.update$pppconv2012
imf.update$gdp.2013 <- imf.update$gdp.2013/imf.update$pppconv2013
imf.update$gdp.2014 <- imf.update$gdp.2014/imf.update$pppconv2014
imf.update$gdp.2015 <- imf.update$gdp.2015/imf.update$pppconv2015



colnames(imf.update)[1] <- "iso";imf.update <- imf.update[,-6]
imf <- merge(imf.update, imf[,-c(23)], by="iso")



# Obacht! NAs sind Syrien!!!
imf$gdp.2012<-as.numeric(imf$gdp.2012); imf$gdp.2012<-imf$gdp.2012*1000000
imf$gdp.2013<-as.numeric(imf$gdp.2013); imf$gdp.2013<-imf$gdp.2013*1000000
imf$gdp.2014<-as.numeric(imf$gdp.2014); imf$gdp.2014<-imf$gdp.2014*1000000
imf$gdp.2015<-as.numeric(imf$gdp.2015); imf$gdp.2015<-imf$gdp.2015*1000000
imf$gdp.2016<-as.numeric(imf$gdp.2016); imf$gdp.2016<-imf$gdp.2016*1000000
imf$gdp.2017<-as.numeric(imf$gdp.2017); imf$gdp.2017<-imf$gdp.2017*1000000
imf$gdp.2018<-as.numeric(imf$gdp.2018); imf$gdp.2018<-imf$gdp.2018*1000000
imf$gdp.2019<-as.numeric(imf$gdp.2019); imf$gdp.2019<-imf$gdp.2019*1000000
imf$gdp.2020<-as.numeric(imf$gdp.2020); imf$gdp.2020<-imf$gdp.2020*1000000
imf$gdp.2021<-as.numeric(imf$gdp.2021); imf$gdp.2021<-imf$gdp.2021*1000000
imf<-subset(imf, select=c("iso","gdp.2012","gdp.2013","gdp.2014","gdp.2015",
                          "gdp.2016","gdp.2017","gdp.2018","gdp.2019",
                          "gdp.2020","gdp.2021")  )


colnames(imf)[1]<-"country"
rm(imf.update, x)



# GDP Data File
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gdp.IMF$country <- substr(gdp.IMF$ID, 1,3)


# GDP Data File
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gdp.IMF$country <- substr(gdp.IMF$ID, 1,3)



gdp <- merge(gdp.IMF, imf, by="country", all.x=T, all.y=F)

rm(gdp.IMF,imf)

gdp <- subset(gdp, select = c(1:15))



save(gdp,file = "../Data/gdp.Rdata")
save(pppconv,file ="../Data/pppconv.Rdata")
