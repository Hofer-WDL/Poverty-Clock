# Set working directory and choose between home and work
t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock")
rm(list=ls())

load("./Data/imputed.RData")
load("./Data/popshares.RData")
require(reshape2)
require(tidyr)
require(lubridate)

t$ccode <- as.character(t$ccode)


groups <- subset(groups,year %in% t$year & ccode %in% (t$ccode))
groups <- melt(groups,c("ccode","country","year"))

t <- rbind(t,groups)


###########Spline
w <- t%>%  unite(ID, variable,year) %>%spread(ID, value)  # wide format in comparrison to t ... tall format

splines <- list()



for(i in 1: nrow(w))  {
    input <- subset(t,ccode == w$ccode[i] & year %in% 2012:2030 & variable =="hc")
    input <- input[order(input$year),]

    input$year <- as.POSIXct(paste(input$year , '07', '01' , '12:00:00'), format='%Y %m %d %H:%M:%S')
    
        #ERROR HANDLING
    possibleError <- tryCatch(  splinefun(input$year,input$value, method="fmm",  ties = mean) ,
    error=function(e) e )
  if(inherits(possibleError, "error")) {splines[[i]] <- function(y){rep(NA,length(y))};next} # write a function that only returns NA
  if( any(is.na(input)) ) {splines[[i]] <- function(y){rep(NA,length(y))};next}              # only use countries where we have full info
    #REAL WORK
  splines[[i]] <- splinefun(input$year,input$value, method="fmm",  ties = mean)
}  #end for
rm(input)

splines[[34]](as.POSIXct("2012-07-01 12:00:00"))
splines[[34]](as.POSIXct("2012-07-02 12:00:00"))

splines[[2]](Sys.time())

a <- seq(as.POSIXct("2012-10-10 12:00:00") , as.POSIXct("2021-10-10 12:00:00"),length.out = 1000)

plot(splines[[34]](a))
plot(diff(splines[[34]](a)))
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################


clock <- function(day){
  today <- as.POSIXlt(day)
  yesterday   <- today;  yesterday$year <- yesterday$year - 1.5 #one and a halfe years ago
  tomorrow <- today   ;   tomorrow$year <- tomorrow$year + 1.5  #one and a halfe years in the future
  # yesterday <- today  - 548 * 24 * 60^2 
  # tomorrow <- today   + 548 * 24 * 60^2
  diff <- as.numeric(tomorrow - yesterday) # how many days are in between 
  sgd.end   <- as.POSIXct("2030-12-31 00:00:00", format='%Y-%m-%d %H:%M:%S')  # start of the sgds
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
  no_pov <-  w$hc_2015/w$pop_2015 < 0.015& !is.na(w$hc_2015)
  
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
  
  #Use the target rates for countries that arn't on track, the actual change for the ones that are and 0 for the trivials
  everyone_on_track <- NA
  everyone_on_track[daily_track=="on" & !is.na(daily_change)] <- daily_change[daily_track=="on" & !is.na(daily_change)]
  everyone_on_track[(daily_track=="off"| daily_track=="wrong")  & !is.na(daily_change)] <- 
    daily_target[(daily_track=="off"| daily_track=="wrong")  & !is.na(daily_change)]
  everyone_on_track[no_pov] <-0
  
  
  #View(cbind(w$country,everyone_on_track,daily_target,daily_change))
  everyone_on_track <- sum(everyone_on_track,na.rm=T)
  
  output <- data.frame(apply(data.frame(hc.yesterday,hc.today,hc.tomorrow,daily_change,daily_change_m.a ,daily_change_m.k, daily_change_f.a, daily_change_f.k,daily_target),2,round,0),  daily_track)
  
  output2 <-  data.frame(hc.today,sgd_prediction,daily_change,daily_change_m.a ,daily_change_m.k, daily_change_f.a, daily_change_f.k,daily_target, daily_track)
  #output[no_pov,c("hc.yesterday","hc.today","hc.tomorrow","daily_change","daily_target")] <- 0
  output2}



ptm <- Sys.time()
clock(Sys.time())
ptm - Sys.time()
View(cbind(w$country,clock(Sys.time())))

#total poverty at this second in millions
sum(clock(Sys.time())[,2],na.rm = T)/10e5

require(xlsx)

write.xlsx2(cbind(w$country,clock( "2016-10-13 10:00:00 CEST" )),file = "maria.xlsx")

 round(as.numeric(clock("2016-07-01 12:00:00 CEST")[,2])-w$hc_2016,1) # differences come from leap years and the fact that clock() does round 
