t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock")
rm(list=ls())

load("./ENVIRONMENT_Dataframes.RDATA")
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


# SAVE------------------------------------------------------------------------------------
save.image("./ENVIRONMENT_Beta-LC.RData")
#-----------------------------------------------------------------------------------------
