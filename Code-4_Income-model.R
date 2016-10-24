
# Set working directory and choose between home and work
t <- try(setwd("E:/Dropbox/World_Data_Lab/Poverty Clock"))
if("try-error" %in% class(t)) setwd("C:/Users/hofer/Dropbox/World_Data_Lab/Poverty Clock")
rm(list=ls())



load("./ENVIRONMENT_Beta-LC.RDATA")
library(foreign)
library(plyr)
library(tidyr)
library(data.table)





#-------------------------------------------------------------------------------------
#
#
#
#  Poverty Clock (based on Beta Lorenzcurves)
#
#
#
#--------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------#
#                             Specifiy Key Variables                                 #
#------------------------------------------------------------------------------------#
# for time specific poverty outputs we assume GDP data to be launched by 31st of Dec. .
# Therefore the inter year period spans from 31st of Dec. to 31st of Dec of the following year.
# Intra-year time points we just project linearily to the respective date. (say 15.mio sec to June)








#########################################################
# Specify desired mean-anchor (GDP/Capita or Survey Mean)
######################################################### 

fun.set.anchor <- function(hierarchy,dataset = data){
  # get the first and the latest anchor year
  extended.anchor <- paste0(hierarchy[1],"20")
  substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
  assign("end", as.numeric( substrRight(names(dataset)[max(grep(extended.anchor,names(dataset)))],2)),envir=globalenv())
  assign("start",as.numeric( substrRight(names(dataset)[min(grep(extended.anchor,names(dataset)))],2)),envir=globalenv() )
  varlist <- expand.grid(hierarchy,start:end+2000)
  varlist <- paste0(varlist$Var1,varlist$Var2)
  if ( sum(varlist %in% names(dataset) ) != length(varlist)) warning("Not all Anchor Variables for all years in dataframe")
  output <- data.frame(matrix(NA,nrow=nrow(dataset),ncol=length(start:end)))
  names(output) <- paste0("anchor.",start:end+2000)
  for(i in 1:length(hierarchy)){
    output[is.na(output)] <- subset(dataset,select = paste0(hierarchy[i],start:end+2000))[is.na(output)] }
  return(cbind(dataset,output))}


# the function fun.set.anchor can receive a anchor hierarchy in the form of a vector like c("svy.mean.","cons.","gdp.capita.")
# Starting with the first anchor the function uses the given variable as anchor for the lorenz curves if this data is not available it uses the next anchor in the hieararchy
#note the function could use some work since as of now it could happen that we mix within a country which will produce spikes for all forecasts due to a rapid change of the anchor (we forecast wherever we have some data but if something slips this could happen)

#data <- fun.set.anchor(c("HH.ex.capita."),data)
data <- fun.set.anchor(c("HH.ex.capita.","cons."),data)
#data <- fun.set.anchor(c("HH.ex.capita.","cons.","svy.mean.","gdp.capita."),data)
#------------------------------------------------------------------------------------#
#                           Start of Analysis                                        #
#------------------------------------------------------------------------------------#



# Poverty Headcounts

x <- subset(data,select = c("TID",
                            paste0("anchor.",(start:end + 2000)),
                            paste0("pop.",start:end),
                            "gamma","delta","theta"))


x <- as.data.frame(x[!duplicated(x),])

x$country=(substr(x$TID, 1,3)); x$year=as.numeric(substr(x$TID,5,8))

x <- x[order(x$country, -abs(x$year)),]
x <- x[!duplicated(x$country),]
x <- x[complete.cases(x),]

require(countrycode)
countrycode(x[is.na(x$anchor.2012),"country"],"iso3c","country.name")
#--------------------------
# Headcount Estimation
#--------------------------

#Coefficients
gamma<-x$gamma; delta<-x$delta; theta<-x$theta #Beta

meanlist <- data.frame(matrix(NA,nrow=nrow(x)))
for(i in start:end){
  meanlist[,(i-11)] <-subset(x,select = paste0("anchor.",2000+i))}
names(meanlist) <- paste0("mean.",start:end)


#Prepare Beta Headcount estimation
beta<-as.data.frame(x$TID); names(beta)<-c("TID")
beta<-data.frame(country=(substr(beta$TID, 1,3)), year=(substr(beta$TID,5,8)), beta$TID)
names(beta)[3]<-c("TID")


a <- data.frame(matrix(NA,ncol = end-start+1))
names(a) <- paste0("hci.",start:end)
beta <- data.frame(beta,a);rm(a)



# Headcountindex----------------------------------------------------------------------

# ols root finding function removed the loop and moved it down to the others
# fun.fitbeta <- function(anchor,z){
#   output <- rep(NA,length(gamma))
#   for(i in 1:length(gamma)){
#     f<-function(H) 
#       (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/anchor[i])
#     try(temp<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
#     output[i]<-ifelse(temp<0,0,temp)}
#   return(ifelse(is.na(anchor)==TRUE, NA,output))}


fun.fitbeta <- function(anchor, z){
  # output <- "failed"  # using "failed instead of NA you can check wheter the root failed or there is just no data but the rest of the code is not built to handle this change
  output <- NA
  f<-function(H)
    (theta[i]*H^gamma[i]*(1-H)^delta[i]*(gamma[i]/H - delta[i]/(1-H))-1+ z/anchor)
  try(output<-uniroot(f, lower=0.1, upper=0.9, extendInt = "yes")$root, silent=T)
  return(ifelse(is.na(anchor)==TRUE, NA,output))}


# for(j in 1:ncol(meanlist)){
#   for(i in 1: length(gamma)){
#     beta[i,(3+j)] <- fun.fitbeta(meanlist[i,j])}}


# Poverty Gap index----------------------------------------------------------------------

fun.pov.gap.beta <- function(H,t,g,d,mean,z){H - (mean/z)* 
                                                           (H - t*H^g*(1-H)^d)}
fun.pov.gap.GQL  <- function(H,b,e,m,n,mean,z){H - (mean/z)*
                                                             (-0.5*(b*H+e+ (m*H^2+n*H+e^2)^0.5)) }

# Population Data

pop <- ( subset(x,select = c("country",paste0("pop.",start:end))))


income <- c(1.9,11,30,50,70,90,110)
#income <- (c(seq(1,200,by = 0.1 ),1.9))
require(countrycode)

income <- sort(unique(income))
index <- list(country = data.frame(ccode=substr(x$TID,1,3),
                                   cname=countrycode(substr(x$TID,1,3), "iso3c","country.name")),
              time=2000+start:end,
              income_level = income,
              variable = c("HC_index","Pov_gap_index","HC","Mean_exp","Total_exp"),
              LC_type = c("beta","GQL"))

ID <- index
ID[[1]] <- ID[[1]][,1]

ptm <- Sys.time()

b <- array(NA,dim = c(country = nrow(x),time = end-start+1, income_level = length(income),variable = length(index[[4]]),LC_type = length(index[[5]]) ),dimnames = ID)
rm(ID)

# for(k in 1:length(income)){
#   for(j in 1:ncol(meanlist)){
#       b[,j,k,1] <- fun.fitbeta(meanlist[,j],365*income[k]  )
#       for(i in 1:nrow(meanlist)){
#         b[i,j,k,"Pov_gap_index"]<-fun.pov.gap.beta(H = b[i,j,k,1],t = x$theta[i] ,
#                                  g = x$gamma[i] ,d = x$delta[i],mean = meanlist[i,j],z =365*income[k]    )    }
#   }}

for(k in 1:length(income)){
  for(j in 1:ncol(meanlist)){
    for(i in 1:nrow(meanlist)){
      b[i,j,k,"HC_index","beta"] <- fun.fitbeta(meanlist[i,j],365*income[k]  )
      b[i,j,k,"Pov_gap_index","beta"]<-fun.pov.gap.beta(H = b[i,j,k,"HC_index","beta"],
                                                        t = x$theta[i],
                                                        g = x$gamma[i],
                                                        d = x$delta[i],
                                                        mean = meanlist[i,j],z =365*income[k]    )    }
  }}

#countries that have no NA for 11 USD
# nona <- names(b[,"2012","11","HC_index",1])[!is.na(b[,"2012","11","HC_index",1])]
# b[nona,,"1.9","HC_index",1] [is.na(b[nona,,"1.9","HC_index",1])] <- 0



##########HCI after GQL
load("./Data/GQL_coeff.RData")
GQL_coeff <- GQL_coeff[match(x$TID,GQL_coeff$TID),]

gql.hci <- function(mean,z){with(GQL_coeff,
                                 (-1/(2*m)) * (n + r*(b + 2*z/mean)*
                                                 (((b + 2*z/mean)^2 - m)^(-1/2) ) ))}


for(k in 1:length(income)){
  for(j in 1:ncol(meanlist)){
          b[,j,k,1,"GQL"] <- gql.hci(meanlist[,j],365*income[k] )
          b[,j,k,1,"GQL"][b[,j,k,1,"GQL"]>1] <- 1
          b[,j,k,1,"GQL"][b[,j,k,1,"GQL"]<0] <- 0
      b[,j,k,"Pov_gap_index","GQL"]<-fun.pov.gap.GQL(H = b[,j,k,"HC_index","GQL"],
                                                     b = GQL_coeff$b,
                                                     e= GQL_coeff$e,
                                                     m= GQL_coeff$m,
                                                     n= GQL_coeff$n ,
                                                     mean = meanlist[,j],z =365*income[k]    )   
  }}




#multiply every entry of HC_index with every respective entry of the population matrix
#this probably works with only one apply as well if one replaces "beta" and "GDL" with empty
b[,,,"HC","beta"] <- apply(b[,,,"HC_index","beta"],3,function(x) x *as.matrix( pop[,-1]) )
b[,,,"HC","GQL"]  <- apply(b[,,,"HC_index","GQL"] ,3,function(x) x *as.matrix( pop[,-1]) )

# 1- poverty gap index*threshold is the avg income below the threshold
# mean expenditrue * HC = total expenditure of ppl below the threshold
for (l in 1: length(index[["LC_type"]])){
  for(k in 1: length(index[[3]])) {
  b[,,k,"Mean_exp",l]  <- (1-b[,,k,"Pov_gap_index",l]) *index[[3]][k]
  b[,,k,"Total_exp",l] <- b[,,k,"HC",l] * b[,,k,"Mean_exp",l]   }}


b["ZWE",,,"Mean_exp","GQL"]




##########Find NAs
miss <- is.na(b)

dim(miss)
apply(miss[,,,,],5,any)

apply(miss[,,,,"beta"],4,any)
apply(miss[,,,,"GQL"],4,any)

apply(miss[,,,"Total_exp","beta"],3,any)
#apply(miss[,,,"Total_exp","GQL"],3,any)

dimnames(miss)[[1]][apply(miss[,,"1.9","HC_index","beta"],1,any)]
dimnames(miss)[[1]][apply(miss[,,"1.9","Total_exp","beta"],1,any)]
dimnames(miss)[[1]][apply(miss[,,"11","Total_exp","beta"],1,any)]
dimnames(miss)[[1]][apply(miss[,,"11","Total_exp","beta"],1,any)]



####### Choose GQL or betaL

#The beta LC is the baseline
selected <- as.array(b[,,,,"beta"])
flag <- data.frame(ccode = dimnames(b)$country,lc_type = "beta",stringsAsFactors = F)

#From Code-2_Beta-LC-Coefficients we know of some beta LC that are invalid
inval <- dimnames(selected)[[1]][(dimnames(selected)[[1]] %in% invalid_beta$ccode)]
selected[inval,,,] <- b[inval,,,,"GQL"]
flag$lc_type[flag$ccode %in% inval] <- "GQ"

#There might be still countries for which we didn't find roots
miss <- is.na(selected)
dimnames(miss)[[1]][apply(miss[,,"1.9","HC_index"],1,any)]


# Every country where there are still NA's use the more robust GQL
for(i in 1:nrow(meanlist)){if (any(is.na(selected[i,,,]))){
  selected[i,,,] <- b[i,,,,"GQL"]
  flag$lc_type[i] <- "GQ"}  }

#are there still NAs?
any(is.na(selected))



###############################################################
# Add upper end of the PDF
###############################################################
require(abind)
upper <- array(NA,dim = dim( selected[,,1,]),dimnames = dimnames(selected[,,1,]))
lower <- array(0 ,dim = dim( selected[,,1,]),dimnames = dimnames(selected[,,1,]))
#prepare the population dataset to be binded to the array
sub_pop <- pop
names(sub_pop)[-1] <- paste0(20,substr(names(sub_pop)[-1],5,6))
#assure that the orders match
sub_pop <- sub_pop[ match(dimnames(upper)$country ,pop$country),  match(dimnames(upper)$time ,names(sub_pop)) ]
#name the dimensions
dimnames(sub_pop) <- dimnames(upper)[-3]

upper[,,"HC_index"] <- 1
upper[,,"HC"] <- as.matrix(sub_pop)
upper[,,"Mean_exp"] <- as.matrix(meanlist)/365
upper[,,"Total_exp"] <- upper[,,"Mean_exp"]*upper[,,"HC"]


#actually bind them together along the third dimension (income_level)
dim(upper)
dim(lower)
ID <- dimnames(selected)
ID$income_level <- c(0,ID$income_level,"INF")
selected <- abind("0" = lower,selected, INF=upper,along = match("income_level",names(dim(selected))),new.names =  ID)
names(dim(selected)) <- names(ID ) 
dimnames(selected) <- ID


b <- (selected)
dim(b)



save(b,file = "pdf.RData")
save(flag,file="flag_income_LC.RData")
Sys.time()-ptm





#solution for Z i.e incomethreshold given the headcount index ie percentage of pop
#z=
dimnames(selected)[[1]]
i=match("UGA",dimnames(selected)[[1]]) # change the 3 character to any country
fun.test <-  function(H) meanlist[i,1]*(1-t(1-H)^delta[i]*H^gamma[i] * (delta[i]/(H-1) + gamma[i]/H )  )
fun.test(0.697962)/(365)
curve(fun.test,0.05,0.95)


