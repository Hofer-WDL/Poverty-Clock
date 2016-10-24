t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock")
rm(list = ls())


require(tidyr)
require(countrycode)
require(xlsx)
require(reshape)

load("ENVIRONMENT_Dataframes.RData")
rm(list=setdiff(ls(),"gdp"))
source("./R_functions/long_short_trans.R")
poverty.clock <- read.csv("Output/poverty.clock.csv")
names(poverty.clock)[c(2:4)] <- c("ccode","base.year.survey","source.survey")

start <- 2012
end <- 2030

load("./Data/population.io.RData")

## spread the dataset into long format
pop <- subset(population.io,subset = population.io$year %in% c(start:end),select=c("year","ccode","country","poptotal"))
rm(population.io)
pop$year <- paste0("pop.",pop$year)
pop <- spread(pop,value = "poptotal", key = "year")


gdp <-gdp[!duplicated(gdp$ccode), c("ccode",paste0("gdp.",start:end))  ]

x <- merge(poverty.clock,pop,by="ccode",all = T)
y <- merge(x,gdp,by="ccode",all.x=T)

co <- match("country",names(y))
y <- y[,c(1,co,     3:(co-1),    (co+1):length(names(y)))];rm(co)

rm(x,gdp,poverty.clock,pop)


#gdp/capita
a <- subset(y,select = paste0("gdp.",start:end))/subset(y,select = paste0("pop.",start:end))
names(a) <- paste0("gdp.cap.",start:end)
y <- cbind(y,a);rm(a)

#poverty share
a <- subset(y,select = paste0("hc.",start:end-2000))/subset(y,select = paste0("pop.",start:end))*100
names(a) <- paste0("hc.sh.",start:end)
y <- cbind(y,a);rm(a)


#add milennia and century to headcount
names(y)[names(y) %in% paste0("hc.",start:end - 2000)] <- gsub(".",".20",x = names(y)[names(y) %in% paste0("hc.",start:end - 2000)], fixed = TRUE)
#change the dot between year and variable with an underscore
names(y) <- gsub(".20","_20",x = names(y), fixed = TRUE)




missing.hc <- y[is.na(y$hc_2012)&!is.na(y$gdp.cap_2012),]
missing.hc.gdp <- y[is.na(y$gdp.cap_2012),]

full.data.list <- setdiff(as.character(y$ccode),c(as.character(missing.hc$ccode),as.character(missing.hc.gdp$ccode)))

#View(missing.hc.gdp)

# pppconv <- read.csv2("./Data/ppp_conversion_nice.csv")
# pppconv <- pppconv[-c(5426:5430),c(2:4)]
# pppconv$year <- paste0("ppp_",pppconv$year,"_2011")
# pppconv <- spread(pppconv,value = "pppconv_GDP_2011", key = "year")

emptycols <- sapply(missing.hc.gdp, function (k) all(is.na(k)))
# a <- merge(missing.hc.gdp[!emptycols],pppconv)

missing.hc.gdp <- missing.hc.gdp[!emptycols];rm(emptycols)

########CIA Data
library(readxl)
CIA  <- read_excel("./Data/Evaluate Missing Countries/CIA_Data.xlsx")

#For Somalia we could assume a stable growthrate of 2,6% from 2012 to 2013 and reverse the total gdp
a <- CIA[CIA$ccode=="SOM"&CIA$year=="2013",]
a[,c(3,5)]<- c(2012 , (1/1.026) * CIA$gdp_Billion_1e6[CIA$ccode=="SOM"&CIA$year=="2013"] )
CIA <- rbind(CIA,a);rm(a)

CIA$gdp <- CIA$gdp_Billion_1e6*1e9

#
# ######## US GDP Deflator
USA_deflator <- read.csv("./Data/Evaluate Missing Countries/USA_deflator_Data.csv")
USA_deflator <- USA_deflator[1,-c(1:4)]
USA_deflator <- data.frame(t(USA_deflator))
USA_deflator$year <- substr(rownames(USA_deflator),2,5)
names(USA_deflator)[1] <- "base2010"
USA_deflator$base2011 <- USA_deflator$base2010/ USA_deflator$base2010[USA_deflator$year==2011] *100

# Match the years the PPP USA GDP is based on in the CIA data with the Conversion Deflator to 2011
CIA <- data.frame(CIA,"2011_Deflator"=USA_deflator$base2011[match(CIA$baseyear, USA_deflator$year)])
CIA$gdp_2011PPP <- with(CIA, gdp/X2011_Deflator*100)
CIA <- subset(CIA,year>2011)



CIA <- subset(CIA,!is.na(gdp_2011PPP)& ccode %in% as.character( missing.hc.gdp$ccode) ,c("cname","ccode","year","gdp_2011PPP","growthrate"))

for (i in 1:length(unique(CIA$ccode))){
  CIA[CIA$ccode ==unique(CIA$ccode)[i],] <- growth(unique(CIA$ccode)[i],CIA)}

names(CIA)[names(CIA)=="gdp_2011PPP"] <- "gdp"
#CIA$year <- paste0("gdp_",CIA$year)

CIA <- melt(CIA,c("cname", "ccode","year"))

CIA <- CIA%>%  unite(ID, variable,year) %>%spread(ID, value)

missing.hc.gdp <- merge(missing.hc.gdp,subset(CIA,select = c("ccode", paste0("gdp_",2012:2014),paste0("growthrate_",2012:2014))),by="ccode",all.x = T)





#### recreate the same columns as missing.hc excluding gdp/capita
a <- setdiff(names(missing.hc),names(missing.hc.gdp))
a <- a[-grep("cap",a)]
b <- data.frame(matrix(NA,ncol = length(a),nrow = nrow(missing.hc.gdp )))
names(b) <- a; rm(a)
missing.hc.gdp  <- cbind(missing.hc.gdp,b);rm(b)

#### calculate gdp/capita
yearset <-  substr(names(missing.hc.gdp)[ grep("gdp.",names(missing.hc.gdp))] ,5,8)
a <- subset(missing.hc.gdp,select =  paste0("gdp_",yearset))/subset(missing.hc.gdp,select = paste0("pop_",yearset))
names(a) <- paste0("gdp.cap_",yearset)
missing.hc.gdp <- cbind(missing.hc.gdp,a);rm(a)

# get the same order of columns to rbind
missing.hc.gdp <- missing.hc.gdp[,match(names(missing.hc),names(missing.hc.gdp))]

all.equal(sort(names(y)),sort(names(missing.hc.gdp)))
rm(USA_deflator,yearset)
# ########################################################
# ########################################################
# Regression
# ########################################################
# ########################################################

varlist <- expand.grid(c("hc.sh_","hc_","pop_","gdp.cap_"),start:end)
varlist <- sort(paste0(varlist$Var1,varlist$Var2))


a  <- (subset(y,ccode %in% full.data.list ,  select = c("ccode","country",varlist)))



a <- transform(a)
missing.hc     <- transform(missing.hc)
missing.hc.gdp <- transform(missing.hc.gdp)

#require(plm)
#p <- pdata.frame(a, index = c("time","ccode"), drop.index = FALSE, row.names = TRUE)


require(stargazer)
require(glm2)

formula1 <- ~(log(pop)+ gdp.cap )* year
formula2 <- ~((log(pop)+ gdp.cap )^2)*year
formula3 <- ~log(pop) * gdp.cap * I(gdp.cap^2) * year
formula4 <- ~ log(pop)+ log(gdp.cap) + year
formula5 <- ~(log(pop)+ log(gdp.cap))*year
formula6 <- ~log(pop)* log(gdp.cap) *year

formula1.1 <- ~gdp.cap*year
formula6.1 <- ~log(gdp.cap)*year

# model1   <- lm(update(formula1,log(hc)~.), data = a)
# model2   <- lm(update(formula2,log(hc)~.), data = a)
# model3   <- lm(update(formula3,log(hc)~.), data = a)
model4   <- lm(update(formula4,log(hc)~.), data = a[a$hc>1,])
model4s   <- lm(update(formula4,log(hc)~.), data = a[a$hc>1,])
model5   <- lm(update(formula5,log(hc)~.), data = a[a$hc>1,])
model6   <- lm(update(formula6,log(hc)~.), data = a[a$hc>1,])

model1.1   <- lm(update(formula1.1,(hc.sh)~.), data = a[a$hc>1,])
#model1.1.p   <- glm(update(formula1.1,(hc.sh/100)~.), data = a[a$hc>1&a$hc.sh>5,],family=binomial(link="probit"))

# reduce model to the years 2012:2014 for our missing gdp countries
b <- subset(a,year %in% 2012:2014)
model1.2   <- lm(update(formula1,log(hc)~.), data = b[b$hc>1,])
model2.2   <- lm(update(formula2,log(hc)~.), data = b[b$hc>1,])
model3.2   <- lm(update(formula3,log(hc)~.), data = b[b$hc>1,])
model4.2   <- lm(update(formula4,log(hc)~.), data = b[b$hc>1,])
model5.2   <- lm(update(formula5,log(hc)~.), data = b[b$hc>1,])
model6.2   <- lm(update(formula6,log(hc)~.), data = b[b$hc>1,])

model6.2.1 <- lm(update(formula6.1,log(hc.sh)~.), data = b[b$hc>1,])
model1.2.1   <- lm(update(formula1.1,(hc.sh)~.), data = b[b$hc>1,])



# stargazer(model1,model2,model3,type = "text")
# stargazer(model6,type = "text")
# stargazer(model3,type = "latex")

# missing.hc$hc_estimate1 <- exp(model.matrix( formula1 , data=missing.hc)  %*% coef(model1))
# missing.hc$hc_estimate2 <- exp(model.matrix( formula2, data=missing.hc)  %*% coef(model2))
# missing.hc$hc_estimate3 <- exp(model.matrix( formula3 , data=missing.hc)  %*% coef(model3))
missing.hc$hc_estimate4 <- exp(model.matrix( formula4 , data=missing.hc)  %*% coef(model4))
# missing.hc$h_estimate5 <- exp(model.matrix( formula5 , data=missing.hc)  %*% coef(model5))
missing.hc$hc_estimate6 <- exp(model.matrix( formula6 , data=missing.hc)  %*% coef(model6))

missing.hc$hc.sh_estimate1.1 <- (model.matrix( formula1.1 , data=missing.hc)  %*% coef(model1.1))

missing.hc$hc.sh_estimate1.1.p <- pnorm(model.matrix( formula1.1 , data=missing.hc)  %*% coef(model1.1))
# plot(hc.sh~log(gdp.cap),a[a$hc>1&a$hc.sh>1,],add=T)
# plot(missing.hc$hc12_estimate1.1.p~log(gdp.cap),missing.hc)


missing.hc.gdp$hc_estimate4[!is.na(missing.hc.gdp$gdp.cap)] <- exp(model.matrix( formula4 , data=missing.hc.gdp)  %*% coef(model4.2))

missing.hc.gdp$hc_estimate6[!is.na(missing.hc.gdp$gdp.cap)] <- exp(model.matrix( formula6 , data=missing.hc.gdp)  %*% coef(model6.2))


#missing.hc.gdp$hc.sh_estimate6.1[!is.na(missing.hc.gdp$gdp.cap)] <- exp(model.matrix( formula6.1 , data=missing.hc.gdp)  %*% coef(model6.2.1))

missing.hc.gdp$hc.sh_estimate1.2.1[!is.na(missing.hc.gdp$gdp.cap)] <- (model.matrix( formula1.1 , data=missing.hc.gdp)  %*% coef(model1.2.1))



disect.interaction(model6)


a <- choose_model("6",missing.hc,type="hc")
#a <- rbind(a,subset(missing.hc.gdp,select = names(a))) 
a <- choose_model("6",missing.hc.gdp,type = "hc")


a$source <- "surveys"
a$source[a$ccode %in% (unique(missing.hc$ccode))] <- "regression.IMF"
a$source[a$ccode %in% unique(CIA$ccode)] <- "regression.CIA"
require(ggplot2)
ggplot(a, aes(x=log(gdp.cap), y=hc.sh,color=source)) +
  geom_point(shape=1)      # Use hollow circles

a <- a[,- match("source",names(a))]

t <- (melt(a,c("ccode", "country", "year")))
save(t,file = "./Data/imputed.RData")
t <- t%>%  unite(ID, variable,year) %>%spread(ID, value)


t$source <- "surveys"
t$source[t$ccode %in% (unique(missing.hc$ccode))] <- "regression.IMF"
t$source[t$ccode %in% unique(CIA$ccode)] <- "regression.CIA"
t$source[is.na(t$hc_2012)] <- "missing"




sum(t$hc_2016,na.rm = T)




#-----------------------------------------------------------------------------------------------------
# Evaluation and Results
#-----------------------------------------------------------------------------------------------------


t$change.16.17 <- t$hc_2017 - t$hc_2016    #absolute change reduction (2016-2017)
t$change.12.17 <- (t$hc_2017 - t$hc_2012)/5  #mean change reduction   (2012-2016)
t$change.12.14 <- (t$hc_2014 - t$hc_2012)/2  #mean change reduction   (2012-2014)
t$change.target <- t$hc_2016/15  * (-1)                        # 15 years 'till 2030
t$change.target[is.na(t$change.target)] <- t$hc_2014[is.na(t$change.target)]/17  * (-1)# 17 years 'till 2030


sec <- 366*24*60*60
t$tik.tak.16.17 <- t$change.16.17/sec                  #change per second
t$tik.tak.12.17 <- t$change.12.17/sec                  #change per second
t$tik.tak.12.14 <- t$change.12.14/sec                  #change per second
t$tik.tak.target <- t$change.target/sec

# Performance
t$performance.16.17 <- t$change.16.17/t$change.target
t$performance.12.17 <- t$change.12.17/t$change.target
t$performance.12.14 <- t$change.12.14/t$change.target


t$track.16.17 <- as.character(cut(t$performance.16.17,breaks = c(-Inf,0,1,Inf),labels = c("wrong","off","on")))
t$track.12.17 <- as.character(cut(t$performance.12.17,breaks = c(-Inf,0,1,Inf),labels = c("wrong","off","on")))
t$track.12.14 <- as.character(cut(t$performance.12.14,breaks = c(-Inf,0,1,Inf),labels = c("wrong","off","on")))

t$track.16.17[t$hc.sh_2016 < 1.5 & !is.na(t$track.16.17)] <- "no"
t$track.12.17[t$hc.sh_2016 < 1.5 & !is.na(t$track.12.17)] <- "no"
t$track.12.14[t$hc.sh_2014 < 1.5 & !is.na(t$track.12.14)] <- "no"

# Another Track measure. This time based on the poverty headcount forecast of 2030

SGD_met <- (t$hc_2030/t$pop_2030 )*100
SGD_tod <- (t$hc_2016/t$pop_2016 )*100

thresh <- 1.5

t$SGD_met <- NA
t$SGD_met[SGD_met<thresh & SGD_tod < thresh] <- "trivial"
t$SGD_met[SGD_met>thresh & SGD_tod>thresh & SGD_met>SGD_tod]  <- "wrong"
t$SGD_met[SGD_met>thresh & SGD_tod>thresh & SGD_met<SGD_tod]  <- "off"
t$SGD_met[SGD_met<thresh & SGD_tod>thresh & SGD_met<SGD_tod]  <- "on"
t$SGD_met[SGD_met>thresh & SGD_tod<thresh & SGD_met>SGD_tod]  <- "massive"




# global results
sum(t$hc_2016, na.rm=T)           #Number of poor people in 2016

sum(t$tik.tak.16.17, na.rm=T)   #Reduction per Second 2016-2017
sum(t$tik.tak.12.17, na.rm=T)   #avg Reduction per Second 2012-2017
sum(t$tik.tak.target, na.rm=T)  #Target Reduction

########Disect leaving and entering poverty

with(t,  data.frame(entering = sum(tik.tak.16.17[tik.tak.12.17>0],na.rm = T),
                                leaving = sum(tik.tak.16.17[tik.tak.12.17<0],na.rm = T))       )

with(t,  data.frame(entering = sum(tik.tak.12.17[tik.tak.12.17>0],na.rm = T),
                                leaving = sum(tik.tak.12.17[tik.tak.12.17<0],na.rm = T))       )






write.xlsx2(subset(t,source != "surveys",c("ccode","country","pop_2014","hc_2014","hc.sh_2014","tik.tak.target","tik.tak.12.14","track.12.14","track.12.17")),"Output/regression.xlsx",row.names = F)







####Write a nice output table to print

#output <- merge(subset(y,select = c("ccode","country","pop.2012","hc.12","gdp.cap.2012")),subset(missing.hc,select = c("ccode","hc12_estimate3","gdp.cap.2012")),by="ccode",all.x = T)

output <- subset(t,select = c("ccode","country","source","pop_2014","pop_2016","hc_2014","hc_2016","hc.sh_2014","hc.sh_2016","gdp.cap_2014","track.16.17","track.12.14","track.12.17","tik.tak.16.17","tik.tak.12.17","tik.tak.12.14","tik.tak.target"))


#output[,(sapply(output,is.numeric))] <- round(output[,(sapply(output,is.numeric))],2)


# get WB income classes
load("./Data/Evaluate Missing Countries/incomeclass.RData")
output$inc.class <- inc.groups.hist$inc.class_2015[match(output$ccode,inc.groups.hist$ccode)]


output.subset <- subset(output,inc.class!="H"& !is.na(inc.class) & source != "surveys" & pop_2014>200000,select = c( "ccode" , "country","pop_2014","hc_2014","hc.sh_2014", "source","gdp.cap_2014") )


write.xlsx2(output,file = "Output/poverty.hc.xlsx",row.names = F)


write.xlsx2(output.subset,file = "Output/large_missings.xlsx",row.names = F)
#missing.hc$hc12_estimate1.1 <- (model.matrix( formula1 , data=missing.hc)  %*% coef(model1.1))

