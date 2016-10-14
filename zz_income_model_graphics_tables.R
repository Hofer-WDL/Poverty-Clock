#####################################################################
# Output tables/graphs etc
#####################################################################
# Set working directory and choose between home and work
t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock")
rm(list=ls())

load("pdf.RDATA")


b <- pdf[[1]]
index <- dimnames(b)
pop <- pdf[[2]]
pop <- melt(pop,"country")
pop$variable <- paste0(20,substr(pop$variable,5,6))

rm(pdf)

lapply(index,head)

library(foreign)
library(plyr)
library(tidyr)
library(data.table)
require(ggplot2)
library(scales)
require(xlsx)

# View(b["SYC",,c("1.9","11"),c(2)])
# View(b["ZAF",,c("1.9","11"),c(2)])
# View(b["CHN",,c("1.9","11"),c(2)])
# View(b[c("MLT","SYC"),,"11",])

# View(round(b[,,"11",2][b[,"2015","11",2]<0,],2))


# ################################################################
# ################################################################
# Graphic that shows the aggregate world number of ppl in different income groups over time
# ################################################################
# ################################################################
incomesubset <- c("1.9","11","110")

a <- melt(b[,,incomesubset,"HC"])
a <- spread(a,key=income_level,value=value)

#b <- data.frame(matrix(NA,nrow=nrow(a),ncol=length(incomesubset)-1))
x <- a[,names(a) %in% incomesubset][,-1]-
  a[,names(a) %in% incomesubset][,-length(incomesubset)]

names(x) <- paste0("X",incomesubset[-length(incomesubset)],"_",incomesubset[-1])

x$X110_plus <-   pop$value[match(paste(a$country,a$time),paste(pop$country,pop$variable))] - a$`110`
#x$X1.9_minus<-   pop$value[match(paste(a$country,a$time),paste(pop$country,pop$variable))] - a$`110`

middle.inc <- data.frame(a[,c(1,2,3)],x)

#check whether we cover the whole pop
#round(with(middle.inc,X1.9+X1.9_11+X11_110+X110_plus - pop$value[match(paste(a$country,a$time),paste(pop$country,pop$variable))]),2)

middle.inc <- melt(    middle.inc    ,c("country","time"))
#middle.inc <- spread(middle.inc,key = "time",value = "value")
rm(a,x)
names(middle.inc) <- c("country","time","range","value")
#View(spread(middle.inc,key = "range",value = "value"))


middle.inc <- aggregate(value~range+time,middle.inc,sum)
levels(middle.inc$range) <- c("< 1.9","[1.9,11)","[11,110)","110 <=")


ggplot(middle.inc,aes(x=time,y=value,group=range ,color = range)) +
  geom_line(size=1.3)+ ggtitle("")+
 scale_y_continuous(labels = function(x){x/10e8},name = "Population in Billions")+ scale_x_continuous(name = "Time")+ guides(color=guide_legend(title="Daily Income"))+
   theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()#+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
 ggsave("check.png")

#  a <- middle.inc
# a$value <- a$value/10e8
#  write.xlsx( spread(a,key=range,value = value),file = "output.xlsx",row.names = F)


# ################################################################
# ################################################################
# homis table
# ################################################################
# ################################################################
h="110";l="11"
year = "2020"

# total number of ppl consuming between 11 an 110 USD
sum(b[,year,h,"HC"]-b[,year,l,"HC"],na.rm = T)


middle.class <- (data.frame(index$country,
                            hc =b[,year,h,"HC"]-
                              b[,year,l,"HC"],
                            total.exp = (b[,year,h,"Total_exp"]- # number of ppl below h* avg expend. -
                                           b[,year,l,"Total_exp"])  # number of ppl below l* avg expend. /
))

middle.class$avg.exp <- with(middle.class,total.exp/hc)

load("./Data/WB_class.RData")


middle.class <- merge(middle.class,subset(class,select = c("ccode","region"),by = "ccode"),all.x = T)

agg <- aggregate(formula =hc~region ,data = middle.class,FUN = sum)
agg$total.exp <- aggregate(formula =total.exp~region ,data = middle.class,FUN = sum)[,2]






# country assessment
coverage <- as.character(middle.class$ccode[!is.na(middle.class$hc)])

load("./Data/population.io.RData")
population <- subset(population.io,year==2015);rm(population.io)

#not covered
population[!population$ccode %in% coverage,"country"]
# share of pop not covered vs. world
not.covered <- population[!population$ccode %in% coverage,c("ccode","country","poptotal")] 
not.covered$popshare <- not.covered$poptotal / sum(population[,"poptotal"])

# between l and h$ for all years
h="110";l="11"
medium.inc <- melt(data.frame(ccode = index$country,
                              range = paste0("[",l, "," ,h, ")"),
                              b[,,h,3]-b[,,l,3]),
                   c("ccode","range"))

h="11";l="1.9"
medium.inc <- rbind(medium.inc,
                    melt(data.frame(ccode = index$country,
                                    range = paste0("[",l, "," ,h, ")"),
                                    b[,,h,3]-b[,,l,3]),
                         c("ccode","range")))

names(medium.inc)[3] <- "time"
#medium.inc$time <- index[[2]][as.numeric(substr(medium.inc$time,2,3))]
medium.inc$time <- as.numeric(substr(medium.inc$time,2,5))

medium.inc$id <- paste(medium.inc$ccode,medium.inc$range,sep = "_")




require(ggplot2)
library(scales)
countries <- c("BRA","NGA","IDN")


ggplot(medium.inc[medium.inc$ccode %in% countries & medium.inc$range == "[1.9,11)",],aes(x=time,y=value,group=id ,color = ccode )) +
  geom_line(size=1.3,linetype =2 )+ ggtitle("")+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ scale_y_continuous(labels = function(x){x/10e6},name = "Population in Millions")+ scale_x_continuous(name = "Time")+ guides(color=guide_legend(title="Country"))


ggplot(medium.inc[medium.inc$ccode %in% countries & medium.inc$range == "[11,110)",],aes(x=time,y=value,group=id ,color = cname )) +
  geom_line(size=1,linetype =1 )+ ggtitle("")+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ scale_y_continuous(labels = function(x){x/10e6},name = "Population in Millions")+ scale_x_continuous(name = "Time")+ guides(color=guide_legend(title="Country"))



ggplot(medium.inc[medium.inc$ccode %in% countries,],aes(x=time,y=value,group=id ,color = ccode,linetype = range )) +
  geom_line(size=1.3)+ ggtitle("") + 
  scale_y_continuous(labels = function(x){x/10e6},name = "Population in Millions")+ scale_x_continuous(name = "Time")+ guides(color=guide_legend(title="Country"),linetype = guide_legend(title="Range"))+
  theme(plot.title = element_text(lineheight=.8,face="bold"),text = element_text(size = 15))+theme_bw()#+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("check.png")
