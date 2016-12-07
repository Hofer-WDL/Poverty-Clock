# Set working directory and choose between home and work
t <- try(setwd("E:/Drive/WDL_Data/Poverty Clock"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Google Drive/WDL_Data/Poverty Clock")
rm(list=ls())

load("./Data/imputed.RData")
load("./Data/popshares.RData")
load("./Data/WB_class.RData")
require(reshape2)
require(tidyr)
require(lubridate)

t$ccode <- as.character(t$ccode)

t <- subset(t,t$variable %in% c("hc","pop"))

#groups contains information on the relative share of men/women/children for various years. --> subset down to what we need
groups <- subset(groups,year %in% t$year & ccode %in% (t$ccode))
#melt the data this way we can just rowbind it to the HC dataset
groups <- melt(groups,c("ccode","country","year"))

t <- rbind(t,groups)

t$variable <- droplevels(t$variable)

#This is the Dataframe I've sent you
write.csv2(t,"Output/pc.csv",row.names = F)
read.csv("output/pc.csv",sep=";",header = T,na.strings = "NA")

######################################################
######################################################
#Spline
######################################################
######################################################

# store the data also in wide format in comparrison to t ... tall format
w <- t%>%  unite(ID, variable,year) %>%spread(ID, value)  

# empty list where we can write our spline functions into using a loop the elements will be in the same order as the countries in "w"
splines <- list()

for(i in 1: nrow(w))  {
    input <- subset(t,ccode == w$ccode[i] & year %in% 2012:2030 & variable =="hc")
    input <- input[order(input$year),]#just to be sure the years are in order
    #transform the year column into time and define each year value as it occured on july 2nd the midpoint of a year barring leapyears
    #in the northern hemisphere the midpoint is a 13:00 and in the southern it is at 11:00 since it won't matter in the long run we chose the midpoint 12:00. The math would change for leapyears, but again compared to other inaccuracies this one is only minor
    input$year <- as.POSIXct(paste(input$year , '07', '02' , '12:00:00'), format='%Y %m %d %H:%M:%S')
        #ERROR HANDLING: if the splinefunction would not work write a function that only returns NAs
    possibleError <- tryCatch(  splinefun(input$year,input$value, method="fmm",  ties = mean) ,
    error=function(e) e )
  if(inherits(possibleError, "error")) {splines[[i]] <- function(y){rep(NA,length(y))};next} # write a function that only returns NA
  if( any(is.na(input)) ) {splines[[i]] <- function(y){rep(NA,length(y))};next}              # only use countries where we have full info
    #REAL WORK
  splines[[i]] <- splinefun(input$year,input$value, method="fmm",  ties = mean)
}  #end for
rm(input)

#only for testing this can be dropped
splines[[34]](as.POSIXct("2012-07-02 12:00:00")) == subset(t,ccode=="CHN" & year == 2012 & variable == "hc",value)
splines[[34]](as.POSIXct("2012-07-01 12:00:00"))



clock <- function(day){
  today <- as.POSIXlt(day)
  yesterday   <- today - 31557600*1.5 #one and a halfe years ago
  tomorrow    <- today + 31557600*1.5 #one and a halfe years in the future
  # yesterday <- today  - 548 * 24 * 60^2 
  # tomorrow <- today   + 548 * 24 * 60^2
  diff <- as.numeric(tomorrow - yesterday) # how many days are in between 
  sgd.end   <- as.POSIXct("2030-12-31 23:59:59", format='%Y-%m-%d %H:%M:%S')  # start of the sgds
  sgd.start <- as.POSIXct("2015-01-01 00:00:00", format='%Y-%m-%d %H:%M:%S')  # end of the sgds
  sgd.duration <- as.numeric(sgd.end - sgd.start)  # days betwwen the start ant the end of the sgds
  year <- format(today,'%Y')
  
  time_since_SGD_start <- as.numeric(today - sgd.start) #how long are the sgds currently running
  
  hc.today      <- unlist(lapply(splines, do.call, list(as.POSIXct(today)))   )  
  hc.tomorrow   <- unlist(lapply(splines, do.call, list(as.POSIXct(tomorrow)) ))
  hc.yesterday  <- unlist(lapply(splines, do.call, list(as.POSIXct(yesterday))))
  hc.sgd.start  <- unlist(lapply(splines, do.call, list(as.POSIXct(sgd.start)))) #hc at the beginning of the sgds
 
  # daily poverty change; reductions are negative
  daily_change <- (hc.tomorrow - hc.yesterday) /(diff)
  daily_change_m.a <- daily_change * w[,paste0("male_Adult_",year)] # multiply the daily change with the demographic shares of that year
  daily_change_m.k <- daily_change * w[,paste0("male_Kid_",year)]
  daily_change_f.a <- daily_change * w[,paste0("female_Adult_",year)]
  daily_change_f.k <- daily_change * w[,paste0("female_Kid_",year)]
  
  daily_target <- (hc.today /as.numeric(today - sgd.end))
  performance <- daily_change/daily_target
  daily_track <- as.character(cut(performance,breaks = c(-Inf,0,1,Inf),labels = c("wrong","off","on")))
  #Countries with no extreme poverty
  no_pov <-  w$hc_2015/w$pop_2015 < 0.01& !is.na(w$hc_2015)
  
  daily_track[no_pov] <- "trivial"  # threshhold of pov/cap in 2015 to indicate trivial countries

  global_daily_target <- sum(daily_target,na.rm = T)
  global_daily_target2<- sum(daily_target[!no_pov],na.rm = T)  # remove the countries that had no poverty in the beginning
  
  #global poverty at the beginning of the sgds
  global_sgd_start    <- sum(hc.sgd.start,na.rm = T)
  #needed reduction per second at the beginning
  sgd_daily_change <-     hc.sgd.start/ (sgd.duration)
  global_sgd_daily_change <- sum(sgd_daily_change,na.rm = T)
  #today's povety if the needed reduction was exactly on point
  sgd_prediction <-  hc.sgd.start - sgd_daily_change*(time_since_SGD_start)
  global_sgd_prediction <- sum(sgd_prediction,na.rm = T)
  
  #Use the target rates for countries that aren't on track, the actual change for the ones that are and 0 for the trivials
  everyone_on_track <- NA
  everyone_on_track[daily_track=="on" & !is.na(daily_change)] <- daily_change[daily_track=="on" & !is.na(daily_change)]
  everyone_on_track[(daily_track=="off"| daily_track=="wrong")  & !is.na(daily_change)] <- 
    daily_target[(daily_track=="off"| daily_track=="wrong")  & !is.na(daily_change)]
  everyone_on_track[no_pov] <-0
  
  
  #View(cbind(w$country,everyone_on_track,daily_target,daily_change))
  everyone_on_track <- sum(everyone_on_track,na.rm=T)
  
  output <- data.frame(apply(data.frame(hc.yesterday,hc.today,hc.tomorrow,daily_change,daily_change_m.a ,daily_change_m.k, daily_change_f.a, daily_change_f.k,daily_target),2,round,0),  daily_track)
  
  output2 <-  data.frame(ccode = w$ccode, country = w$country,hc.today,sgd_prediction,daily_change,daily_change_m.a ,daily_change_m.k, daily_change_f.a, daily_change_f.k,daily_target, daily_track)
  #output[no_pov,c("hc.yesterday","hc.today","hc.tomorrow","daily_change","daily_target")] <- 0
  output2$ahead <- with(output2,-hc.today +  sgd_prediction)
  global <- apply(output2[,c("hc.today" ,"sgd_prediction", "daily_change","daily_target", "ahead" ) ]
                  ,2,sum,na.rm=T)
  list(countries = output2,global = global)}



ptm <- Sys.time()
clock(Sys.time())
Sys.time() -ptm 

n <- 20
output <- rep(NA,n)
for (i in 1:n){
  output[i] <- subset(clock(
    as.character(seq(as.Date("2016-07-02"),as.Date("2030-07-02"),length.out = n)[i])
  )$countries,ccode =="SSD","daily_track")
}
unlist(output)


 clock("2016-07-02 00:00")$global
 clock("2017-07-02 00:00")$global
 clock("2019-07-02 12:00")$global
# clock("2019-07-02 00:00")$global
clock("2028-07-02 00:00")$global

require(xlsx)

 round(as.numeric(clock("2016-07-01 12:00:00 CEST")$countries[,2])-w$hc_2016,1) # differences come from leap years and the fact that clock() does round 


a <- merge(class[,c("ccode","continent")],clock( "2016-11-30 12:00:00 CEST" )$countries,"ccode",all=T)

a <- subset(a,daily_track != "trivial",select = c("ccode","country","continent","hc.today","daily_track"))
a <- a[order(a$daily_track,a$continent,a$country),]


a <- data.frame(a,pop_2016 = w[match(a$ccode,w$ccode),c(grep("pop_2016",names(w)))])

write.csv2(a,file = "countries_shp/tracks.csv")

write.xlsx2(a,file = "Output/Wolfgang.xlsx",row.names = F)

a$count <- 1
b <- aggregate(hc.today~continent + daily_track,data = a, FUN = sum)
b[,3]<-round(b[,3],0)
b[order(b$continent),]


