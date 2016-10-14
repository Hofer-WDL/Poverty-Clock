t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock")
rm(list=ls())

load("./Data/merged_surveys.Rdata")
library(foreign)
library(plyr)
library(data.table)


#------------------------------------------------------#
# Variables for GQL-Estimation see Datt(1998) page 6   #
#------------------------------------------------------#
data$L<-ifelse(data$L>0.9, 1, data$L) 
data$dep.gql<-ifelse(data$p-data$L!=0, data$L*(1-data$L), NA)      # L(1-L)
data$exp.gql.1<-ifelse(data$p-data$L!=0, data$p^2-data$L, NA)      # (p^2-L) 
data$exp.gql.2<-ifelse(data$p-data$L!=0, data$L*(data$p-1), NA)    # L(p-1)
data$exp.gql.3<-ifelse(data$p-data$L!=0, data$p-data$L, NA)        # (p-L)



#-----------------------------------------
# Estimating Generalized Quadratic LC
#-----------------------------------------

#Regression
subset<-subset(data, !is.na(inc.share)  )
subset$TID<-as.factor(subset$TID)
subset$TID<-droplevels(subset$TID)

dat <- split(subset, subset$TID)[c(levels(subset$TID))]


ols.gql <- lapply(dat, function(subset)
  lm(dep.gql ~ 0 + exp.gql.1 + exp.gql.2 + exp.gql.3, data=subset))

# GQL Matrix for calculating the LC
GQL<-t(as.data.frame(lapply(ols.gql, coefficients))); GQL<-data.frame(GQL)
GQL$d<-rownames(GQL);
colnames(GQL)<-c("a","b","c","TID"); rownames(GQL)<-c(1:nrow(GQL))
GQL<-GQL[,c(ncol(GQL),1:(ncol(GQL)-1))]

GQL$e <- (-1)*(GQL$a + GQL$b + GQL$c + 1)
GQL$m <- GQL$b^2 - 4*GQL$a
GQL$n <- 2*GQL$b*GQL$e - 4*GQL$c
GQL$r <- (GQL$n^2 - 4*GQL$m*GQL$e^2)^(1/2)
GQL.coeff<-GQL

x<-subset(data, select=c("TID", "p","L"))
GQL<-merge(x, GQL, by="TID");

GQL$GQL<-(-1/2)*(                                      # (-1/2)*(
  GQL$b*GQL$p + GQL$e +                                # b*p + e +
    ( GQL$m*GQL$p^2 + GQL$n*GQL$p + GQL$e^2)^(1/2) )     # (m*p^2 + n*p +e^2)^(1/2)


GQL<-subset(GQL, select=c("TID","GQL"))
NAs<-subset(data, is.na(data$inc.share), select=c("TID","inc.share"))
colnames(NAs)<-c("TID", "GQL"); GQL<-rbind(GQL,NAs)
GQL<-GQL[order(GQL[,1], GQL[,2]),]; data<-data[order(data[,1], data[,6]),]


#---------------------------------------------------------------------------------------
# Checking for valTIDity of GQL-Lorenzcurve (Conditions see Gaurav Datt (1998), S. 12)  #
#---------------------------------------------------------------------------------------
GQL<-merge(GQL.coeff, GQL, by="TID")

#condition 1: e<0
GQL$GQL <- ifelse(GQL$e<0, GQL$GQL, NA)

#condition 2: a+c>=1
GQL$ac <- GQL$a + GQL$c
GQL$GQL <- ifelse(GQL$ac<1, NA, GQL$GQL)

#condition 3: c>=0
GQL$GQL <- ifelse(GQL$c<0, NA, GQL$GQL)

#condition 4: 
GQL$GQL <- ifelse(GQL$m < 0, GQL$GQL,
                  ifelse(GQL$m>0 & GQL$m < (GQL$n^2/(4*GQL$e^2)) & GQL$n>=0, GQL$GQL,
                         ifelse(GQL$m>0 & GQL$m<(-GQL$n/2) & GQL$m < (GQL$n^2/(4*GQL$e^2)),  
                                GQL$GQL, NA)            ))


#data$GQL<-GQL$GQL
GQL_coeff <- GQL.coeff
save(GQL_coeff,file = "./Data/GQL_coeff.RData")



