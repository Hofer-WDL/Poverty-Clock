#####################################################################
# Output tables/graphs etc
#####################################################################
# Set working directory and choose between home and work
t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock")
rm(list=ls())

load("pdf.RDATA")


index <- dimnames(b)


lapply(index,head)
dim(b)

any(is.na(b))

library(foreign)
library(plyr)
library(tidyr)
library(data.table)
require(ggplot2)
require(ggrepel)
library(scales)
require(xlsx)
require(xtable)
require(colorplaner)

# ################################################################
# Make PDF by quasi first differencing
# ################################################################

my.pdf <- function(steps,years=dimnames(b)$time,skip.regional = F){
  # remove doubles, convert inf to INF and order by size
  income <- toupper(as.character(sort(as.numeric( steps ))))
  income <- unique(c("0",income,"INF"))
  years.arg <- unique(as.character(sort(years)))
  
  if(any(!years.arg %in% dimnames(b)$time)) stop("At least one year is not contained in <b>")
  if(any(!income    %in% dimnames(b)$income_level)) stop("At least one income level from <steps> is not contained in <b>")
  
  d <- b[,,income,]
  
  #first difference the data, ie. subtract the lower income from the higher income level for each variable time and country
  x <- (d[,,   2:dim(d)["income_level"]  ,c("HC","Total_exp")]) - (d[,,   1:(dim(d)["income_level"] - 1)  ,c("HC","Total_exp")])
  
  #set names for the factor in the form of [low,high)
  dimnames(x)$income_level <- paste0("[", dimnames(d)$income_level[-length(dimnames(d)$income_level)], ",",
                                     dimnames(d)$income_level[-1] , ")"  )
  
  #convert from an array to a tall DF and subset the years. We went away from subsetting the years at the step "d <-" since if years is of lenght one we loose one dimension of the array and the first differencing command is incorrect. It would be more efficient to do above though
  middle.class <- subset(melt(x),time %in% years.arg)
  if(skip.regional){return(middle.class)
  }  else {
    # open a dataset with regional dummies
    load("./Data/WB_class.RData")
    class$region <- as.character(class$region2)
    #add the regional dummies to each country
    middle.class <- merge(middle.class,subset(class,select = c("ccode","region")),by.x = "country",by.y = "ccode",all.x = T)
    return(middle.class)}
}


#######################
# Table 1
######################
middle.class <- my.pdf(c("11","110"),c(2015,2020,2025,2030),F)
middle.class$value <- middle.class$value/10e5


# add up all HC and expenditures for each year and for each regional cluster
agg <- aggregate(formula =value~region+variable+income_level+time ,data = middle.class,FUN = sum)
agg <- rbind(agg,
             data.frame(region = "World",aggregate(value~variable+income_level + time ,middle.class,sum)))
agg1 <- subset(agg,time== "2015" & income_level == "[11,110)",c("region","variable","value"))
agg1 <-spread(agg1,key = variable,value = value) 
agg1 <- data.frame(agg1, SH= apply(agg1[,-1],2,function(x) paste0(round(100*x/x[length(x)],0),"%")   ))
#names(agg1) <- c("Region","Number of people in millions","Consumption in millions","Global share of People","Global share of Consumption")

addtorow <- list()
addtorow$pos <- list(0,0)
addtorow$command <- c(paste0(paste0('& \\multicolumn{2}{c}{', c("Number of People","Consumption"), '}', collapse=''), '\\\\'),paste0(paste0('& \\multicolumn{2}{c}{', c("(millions and global share)","(millions and global share)"), '}', collapse=''), '\\\\'))

agg1 <- agg1[,c(1,2,4,3,5)]
print(xtable(agg1 ,digits = c(0,0,0,0,0,0) ),include.rownames=FALSE,add.to.row = addtorow, include.colnames=F)


agg_23 <- subset(agg,income_level == "[11,110)")
agg_23 <- split(subset(agg_23,select = c("region","value","time")),agg_23$variable,drop = T)
agg_23 <- lapply(agg_23,FUN = spread,key= time,value = value)

my.fun <- function(x){print(xtable(x ,digits = rep(0,ncol(x)+1) ),include.rownames=FALSE, include.colnames=T)}

my.fun(agg_23[[1]])
my.fun(agg_23[[2]])

#######################
# BRICI Table
######################

middle.class <- my.pdf(steps = c(1.9,11,30,50,70,90,110),years = c("2012","2015","2020","2025","2030"),T)

BRICI <- c("BRA","RUS","IND","IDN","CHN")

middle.class <- subset(middle.class,country %in% BRICI)
middle.class$value <- round(middle.class$value/10e5)

require(xlsx)
temp <-  spread((middle.class), key =time,value = value)
temp <- temp[order(temp$country,temp$variable),]
write.xlsx( temp ,file="Output/BRICI.xlsx",sheetName = "all",row.names = F);rm(temp)

middle.class <- split(middle.class,paste0(middle.class$country,middle.class$variable))

middle.class <- lapply(middle.class, function(x) spread(subset(x,select = c("value","time","income_level")), key =time,value = value))

# for(i in length(middle.class)){
#   write.xlsx2(middle.class[[i]],file="Output/BRICI.xlsx",sheetName = "2",startColumn = i*8,col.names = F)}

#######################
# BRICI TS
######################

middle.class <- my.pdf(c(11,110),skip.regional =T)

ggplot(
  subset(middle.class,country %in% BRICI & income_level =="[11,110)" & variable =="HC") ,aes(x=time,y=value,group=country ,color = country)) +
  geom_line(size=1.3)+ ggtitle(  "[11,110)"  )+
  scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+ scale_x_continuous(name = "Time")+
  theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()

ggsave("Output/TS_HC_BRICI.png")

#######################
# BRICI Split TS
######################

middle.class <- my.pdf(c(11,50,110),skip.regional =T)

BRARU <- c("BRA","RUS")
INCHN <- c("CHN","IND")

# ggplot(
#   subset(middle.class,country %in% BRARU & variable =="HC") ,aes(x=time,y=value ,linetype = country, color = income_level)) +
#   geom_line(size=1.3)+ 
#   scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+ scale_x_continuous(name = "Time")+
#   theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()
# ggsave("Output/test_TS.png")
# 
# middle.class$time <- factor(middle.class$time)
# ggplot(
#   subset(middle.class,country %in% "BRA" & variable =="HC" & time %in% c(2015,2030) & income_level != "[0,11)") ,aes(x=income_level,y=value))  + geom_bar(aes(fill = time), position = "dodge", stat="identity")+
#   scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+
#   theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()+
#   ggtitle(  "fehlende dim: Countries"  )
# ggsave("Output/test1.png")
# 
# middle.class$time <- factor(middle.class$time)
# ggplot(
#   subset(middle.class,country %in% BRARU & variable =="HC" & time %in% c(2015,2030) & income_level != "[0,11)") ,aes(x=country,y=value ))  + geom_bar(aes(fill = income_level), position = "dodge", stat="identity")+
#   scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+
#   theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()+
#   ggtitle(  "fehlende dim: Time"  )
# ggsave("Output/test2.png")
# 
# middle.class$time <- factor(middle.class$time)
# ggplot(
#   subset(middle.class,country %in% BRARU & variable =="HC" & time %in% c(2015,2030) & income_level != "[0,11)") ,aes(x=income_level,y=value ))  + geom_bar(aes(fill = country), position = "dodge", stat="identity")+
#   scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+
#   theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()+
#   ggtitle(  "fehlende dim: time"  )
# ggsave("Output/test3.png")
# 
# middle.class$time <- factor(middle.class$time)
# ggplot(
#   subset(middle.class,country %in% "BRA" & variable =="HC" & time %in% c(2015,2030) & income_level != "[0,11)") ,aes(x=income_level,y=value ))  + geom_bar(aes(fill = time), position = "dodge", stat="identity")+
#   scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+
#   theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()+
#   ggtitle(  "fehlende dim: country"   )
# ggsave("Output/test4.png")
# 
# 






middle.class$time <- factor(middle.class$time)
ggplot(
  subset(middle.class,country %in% BRARU & variable =="HC" & time %in% c(2015,2030) & income_level != "[0,11)") ,aes(x=country,y=value ))  + geom_bar(aes(fill = time), position = "dodge", stat="identity")+
  scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+
  theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()+ facet_grid(. ~ income_level)
ggsave("Output/facets1.png")

middle.class$time <- as.numeric(as.character(middle.class$time))
ggplot(
  subset(middle.class,country %in% BRARU & variable =="HC" & income_level != "[0,11)") ,aes(x=time,y=value ))  + geom_line(aes(color = country))+
  scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+
  theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()+ facet_grid(. ~ income_level)
ggsave("Output/facets1_TS.png")


middle.class$time <- factor(middle.class$time)
ggplot(
  subset(middle.class,country %in% BRARU & variable =="HC" &  time %in% c(2015,2030) & income_level != "[0,11)") ,aes(x = time,y=value ))  + geom_bar(aes(fill = time), position = "dodge", stat="identity")+
  scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+
  theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()+ facet_grid(income_level ~ country)
ggsave("Output/facets2.png")



###########################################
# sub TS
############################################
middle.class <- my.pdf(steps = c(1.9,11,30,50,70,90,110),skip.regional =T)

ggplot(
  aggregate(value~income_level+time
            ,subset(middle.class, variable == "HC" & income_level %in%  c("[11,30)","[30,50)","[50,70)","[70,90)","[90,110)")),sum)
  ,aes(x=time,y=value,group=income_level ,color = income_level)) +
  geom_line(size=1.3)+ ggtitle("World")+
  scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+ scale_x_continuous(name = "Time")+
  theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()#+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("Output/TS_sub_income_levels.png")



###########################################
# main TS
############################################

middle.class <- my.pdf( c("0","1.9","11","110","INF"), skip.regional = T)

ggplot(
  aggregate(value~variable+time+income_level , subset(middle.class, variable == "HC"),sum)
  ,aes(x=time,y=value,group=income_level ,color = income_level)) +
  geom_line(size=1.3)+ ggtitle("")+
  scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+ scale_x_continuous(name = "Time")+
  theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()#+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("Output/TS_main_income_levels.png")



##########
middle.class <- my.pdf( c(1.9,11,30,50,70,90,110), skip.regional = T)
middle.class$value <- middle.class$value/10e5
middle.class <- spread(aggregate(value~variable+time+income_level , middle.class,sum),key=variable, value = value)

ggplot(middle.class,aes(x=HC,y=Total_exp,shape = income_level,color= time))+
  geom_point()# + scale_x_continuous(trans='log10') +   scale_y_continuous(trans='log10')


library(grid)

ggplot(middle.class,aes(x=HC,y=Total_exp,color= income_level,label=time))+
  geom_line()+ guides(label=FALSE)+
  geom_text_repel(aes(label=ifelse(time%in%c(2012,2030),as.character(time),'')),show.legend = FALSE)
ggsave("Output/Scatter_HC_exp.png")


ggplot(middle.class,aes(x=HC,y=Total_exp,group= income_level,color=income_level))+
  geom_path(aes(group=income_level),arrow = arrow(length=unit(0.30,"cm")))#+ scale_x_continuous(trans='log10') +   scale_y_continuous(trans='log10')
ggsave("Output/Scatter_HC_exp2.png")





middle.class$time <- as.numeric(middle.class$time )
levels(middle.class$income_level ) <- c(0,1.9,11,30,50,70,90,110)
middle.class$income_level <- as.numeric(middle.class$income_level)
require(colorplaner)
ggplot(middle.class,aes(x=HC,y=Total_exp,group = income_level,color= income_level,color2 = time))+
  geom_line()+   scale_color_colorplane()
