t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock")
rm(list=ls())

load("./Data/merged_surveys.Rdata")
library(foreign)
library(plyr)
library(data.table)


#---------------------------------------------------------------------------------------
#
#
# Coefficients for Beta-Lorenzcurve Estimation
#
#
#---------------------------------------------------------------------------------------
rm(list=setdiff(ls(), c("data")))



#------------------------------------------------------------------#
# Variables for beta-Estimation see Datt(1998) page 6              #
#                                                                  #
# note the log transformation:                                     #
# p-L(p)=t*p^g*(1-p)^d implies the following transformation        #
# log[p-L(p)]=log(t)+g*log(p)+d*log(1-p)                           #
# where t = Theta, g = Gamma and d = Delta                         #
#------------------------------------------------------------------#
data$dep.beta<-ifelse(data$p-data$L!=0, log(data$p-data$L), NA)  # log[p-L(p)]
data$exp.beta.1<-ifelse(data$p-data$L!=0, log(data$p), NA)         # log[p]
data$exp.beta.2<-ifelse(data$p-data$L!=0, log(1-data$p), NA)       # log[1-p]



#------------------------------------------------------------------#
#                                                                  #
#                                                                  #
# Estimating Lorenz Curves                                         #
#                                                                  #
#                                                                  #
#------------------------------------------------------------------#

# Regression
data$TID<-as.factor(data$TID)
data$TID<-droplevels(data$TID)

dat <- split(data, data$TID)[c(levels(data$TID))]

ols.beta <- lapply(dat, function(subset)
  lm(dep.beta ~ exp.beta.1 + exp.beta.2, data=subset))



# BETA Matrix for calculating the LC
BETA<-t(as.data.frame(lapply(ols.beta, coefficients))); BETA<-data.frame(BETA)
BETA$country<-rownames(BETA);
colnames(BETA)<-c("intercept","gamma","delta","TID"); rownames(BETA)<-c(1:nrow(BETA))
BETA<-BETA[,c(ncol(BETA),1:(ncol(BETA)-1))]

BETA$theta<-exp(BETA$intercept)

data <- merge(data,BETA, by="TID")


# Estimated Lorenzcurves
data$beta.lc<-data$p - data$theta * data$p^data$gamma*(1-data$p)^data$delta


rm(BETA, dat, ols.beta)

#------------------------------------------------------------------#
#                                                                  #
#                                                                  #
# Validity of the Lorenz Curves                                    #
#                                                                  #
#                                                                  #
#------------------------------------------------------------------#
# Conditions <-  L(0) = 0               # automatically with beta LC
#                L(1) = 1               # automatically with beta LC
#                L'(0+) >=0             
#                L''(p) >=0 for pE(0,1)

#LC
L = function(p,t,g,d){p - t*p^g*(1-p)^d}

#first derivative 
L_d = function(p,t,g,d){1 -( t*p^g * (1-p)^g - ((g/p)- (d/(1-p)) ) )  }

#sedond derivative 
L_dd = function(p,t,g,d){t*p^g * (1-p)^g *( ( (g*(1-g) ) / (p^2) )+( (2*g*d) / (p*(1-p)) )+( (d*(1-d)) / (1-p)^2 ) )   }

# #t = data$theta; g =  data$gamma; d = data$delta
# 
# L_dd(0.01,t = data$theta, g =  data$gamma, d = data$delta)

evaluate <- function(t,g,d){
  cond_3 <- (L_d(0.001,t,g,d)>=0)
  vec <- seq(0.01,0.99,by=0.01)
  output <- rep(NA,length(t))
  a <- rep(NA,length(vec))
  for(i in 1:length(output)){a <- L_dd( vec ,t[i],g[i],d[i] );  output[i] <- any(a<0)}
  cond_4<- (!output)
  return(data.frame(cond_3,cond_4))}



a <- data.frame(ccode = data$country,evaluate(t = data$theta, g =  data$gamma, d = data$delta))
a <- a[!duplicated(a$ccode),]
b <- (a[!a$cond_4,])
require(countrycode)
b$ccode <- countrycode(b$ccode,"iso3c","country.name")


L_dd(seq(0.01,0.99,by=0.01),t = data$theta[816], g =  data$gamma[816], d = data$delta[816])
plot(L(seq(0.01,0.99,by=0.01),t = data$theta[816], g =  data$gamma[816], d = data$delta[816]))

# SAVE------------------------------------------------------------------------------------
save.image("./ENVIRONMENT_Beta-LC.RData")
#-----------------------------------------------------------------------------------------
