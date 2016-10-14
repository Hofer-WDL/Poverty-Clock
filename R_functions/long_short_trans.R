transform <- function(a){
  a  <- subset(a             ,select = c("ccode","country",varlist))
  
  a <- melt(a,c("ccode","country"))
  a$variable <- as.character(a$variable)
  
  a$year    <- do.call("rbind",strsplit(a$variable,"_",fixed=T))[,2]
  a$variable<- do.call("rbind",strsplit(a$variable,"_",fixed=T))[,1]
  a<-spread(a,variable,value)
  a}


disect.interaction <- function(model){
  coef1 <- data.frame(coef(model))
  coef1$names <- row.names(coef1)
  coef1$names[-grep("year",coef1$names)] <- paste0(coef1$names[-grep("year",coef1$names)],":year2012")
  coef1$names[-grep(":",coef1$names)]    <- paste0("(Intercept):",coef1$names[-grep(":",coef1$names)])
  coef1$year    <-  do.call("rbind",strsplit(coef1$names,":year",fixed=T))[,2]
  coef1$names<- do.call("rbind",strsplit(coef1$names,":year",fixed=T))[,1]
  coef1 <- (spread(coef1,key = "year",value = "coef.model."))
  coef1[,-c(1,2)] <- coef1[,-c(1,2)] + coef1[,2]
  my.names <- coef1[,1]
  coef1 <- data.frame(year=names(coef1)[-1],t(coef1[,-1]))
  names(coef1)[-1] <- my.names
    coef1
  }


choose_model <-  function(modelcode,data,type){
  if(type=="hc"){
    m <- data
    m$hc <- m[,paste0("hc_estimate",modelcode)]
    m$hc.sh <- (m$hc/m$pop)*100
    rbind(a,subset(m,select = names(a)))}
 else if(type=="hc.sh"){
   m <- data
   m$hc.sh <- m[,paste0("hc.sh_estimate",modelcode)]
   m$hc <- m$hc.sh*m$pop
   m$hc.sh <- m$hc.sh*100
   rbind(a,subset(m,select = names(a)))}
  else{warning("wrong type specified")
    return(data)}
}


growth <- function(countryc,dataset){
  x <- subset(dataset,ccode==countryc)
  x <- x[order(x$year),]
  x$growthrate[-length(x$growthrate)] <- (tail(x$gdp_2011PPP,length(x$growthrate)-1)/head(x$gdp_2011PPP,length(x$growthrate)-1)) -1
  x}