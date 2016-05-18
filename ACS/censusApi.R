library(httr)
library(jsonlite)
library(RCurl)
library(dplyr)
library(tidyr)
library(reshape2)

key = '3a6a31e5e9739d354bc99907b46aac3274304332'

setwd("V:\\IPEDS\\Code\\ipeds\\ACS")
acs <- read.csv('acsFieldOfDegree.csv', stringsAsFactors = FALSE)
acs <- filter(acs, descType %in% c('stem','total'))
year <- c(2012,2013,2014)

acsSTEM <- data.frame()

for (i in 1:length(acs)){
  
  varName <- acs[i,1]
  
  url <- paste0('http://api.census.gov/data/',2014,'/acs1?get=NAME,',acs[i,1],'&for=state:*&key=',key)
  state <- as.data.frame(fromJSON(getURL(url)))
  state <- state[-1,]
  names(state) <- c('geoName','value','FIPS')
  state$year <- 2014
  state$varName <- varName
  
  url <- paste0('http://api.census.gov/data/',2014,'/acs1?get=NAME,',acs[i,1],'&for=metropolitan+statistical+area/micropolitan+statistical+area:*&key=',key)
  metro <- as.data.frame(fromJSON(getURL(url)))
  metro <- metro[-1,]
  names(metro) <- c('geoName','value','FIPS')
  metro$year <- 2014
  metro$varName <- varName
  
  
  acsSTEM <- rbind(acsSTEM,state,metro)  
  
}

#write.csv(acsSTEM,"C:\\Users\\SKulkarni\\Desktop\\acsSTEM.csv")

acs <- as.data.frame(cbind(c('B15010_001E','B15010_002E','B15010_003E','B15010_004E','B15010_005E','B15010_006E'),c(0,1,1,1,1,1)))
names(acs) <- c('varName','stem')

acsSTEM <- left_join(acsSTEM, acs, by="varName")
acsSTEM$value <- as.numeric(levels(acsSTEM$value))[acsSTEM$value]

attach(acsSTEM)
aggData <- aggregate(acsSTEM$value, by=list(descType,FIPS), FUN=sum, na.omit=TRUE)
names(aggData) <- c('type','FIPS','value2014')
acsSTEM2014 <- dcast(aggData, FIPS ~ type, value.var='value2014')
acsSTEM2014$stemShare <- acsSTEM2014$stem / acsSTEM2014$total

write.csv(acsSTEM2014, 'acsSTEM2014.csv')
