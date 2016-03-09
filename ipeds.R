setwd("V:\\IPEDS\\Code\\ipeds")
library('dplyr')

onet <- read.csv('Crosswalks\\ciponet.csv', stringsAsFactors=FALSE)
onet$CIPCODE <- as.numeric(substr(onet$cipcode,1,7))
onet <- mutate(onet, comp_high_any = as.numeric(comp_score > (mean(onet$comp_score, na.rm=TRUE) + sd(onet$comp_score, na.rm=TRUE))))
onet <- select(onet, CIPCODE, comp_high_any, stem_high_any)

awlevel <- read.csv('Crosswalks\\awlevel.csv', stringsAsFactors=FALSE)

c2010 <- read.csv('Completions\\c2010_a_rv.csv')
c2011 <- read.csv('Completions\\c2011_a_rv.csv')
c2012 <- read.csv('Completions\\c2012_a_rv.csv')
c2013 <- read.csv('Completions\\c2013_a_rv.csv')
c2014 <- read.csv('Completions\\c2014_a.csv')

hd2010 <- read.csv('Institution\\hd2010.csv')
hd2011 <- read.csv('Institution\\hd2011.csv')
hd2012 <- read.csv('Institution\\hd2012.csv')
hd2013 <- read.csv('Institution\\hd2013.csv')
hd2014 <- read.csv('Institution\\hd2014.csv')

c2010 <- merge(hd2010, c2010, by='UNITID')
c2011 <- merge(hd2011, c2011, by='UNITID')
c2012 <- merge(hd2012, c2012, by='UNITID')
c2013 <- merge(hd2013, c2013, by='UNITID')
c2014 <- merge(hd2014, c2014, by='UNITID')

c2010$year <- 2010
c2010 <- select(c2010,STABBR,CBSA,AWLEVEL,CTOTALT,year,CIPCODE)
c2011$year <- 2011
c2011 <- select(c2011,STABBR,CBSA,AWLEVEL,CTOTALT,year,CIPCODE)
c2012$year <- 2012
c2012 <- select(c2012,STABBR,CBSA,AWLEVEL,CTOTALT,year,CIPCODE)
c2013$year <- 2013
c2013 <- select(c2013,STABBR,CBSA,AWLEVEL,CTOTALT,year,CIPCODE)
c2014$year <- 2014
c2014 <- select(c2014,STABBR,CBSA,AWLEVEL,CTOTALT,year,CIPCODE)

total <- rbind(c2010,c2011,c2012,c2013,c2014)
total <- left_join(total, awlevel, by='AWLEVEL')

groupState <- group_by(total, STABBR, AWLEVEL, CIPCODE, year, degType)
byState <- as.data.frame(summarise(groupState, totalawards = sum(CTOTALT)))
stateTotal <- filter(byState, CIPCODE == 99)
stateTotal <- select(stateTotal, STABBR, AWLEVEL, totalawards,year)
names(stateTotal) <- c('STABBR','AWLEVEL','statetotal','year')
byState <- merge(byState, stateTotal, by=c('STABBR','AWLEVEL','year'))
byState <- left_join(byState, onet, by='CIPCODE')
byState$share <- byState$totalawards / byState$statetotal

attach(byState)
stemShareState <- aggregate(byState$share, by=list(stem_high_any,STABBR,AWLEVEL,year), FUN=sum, na.rm=TRUE)
names(stemShareState) <- c('stem','state','awlevel','year','share')
compShareState <- aggregate(byState$share, by=list(comp_high_any,STABBR,AWLEVEL,year), FUN=sum, na.rm=TRUE)
names(compShareState) <- c('comp','state','awlevel','year','share')

attach(byState)
degStateCip <- aggregate(totalawards, by=list(STABBR,degType,year,stem_high_any), FUN=sum, na.rm=TRUE)
names(degStateCip) <- c('STABBR','degType','year','stem_high_any','totalawards')
degState <- aggregate(totalawards, by=list(STABBR,degType,year), FUN=sum, na.rm=TRUE)
names(degState) <- c('STABBR','degType','year','statetotal')
degStateStem <- merge(degState,degStateCip,by=c('STABBR','degType','year'))
degStateStem$statetotal <- degStateStem$statetotal / 2
degStateStem$share <- degStateStem$totalawards / degStateStem$statetotal

attach(byState)
degStateCip <- aggregate(totalawards, by=list(STABBR,degType,year,comp_high_any), FUN=sum, na.rm=TRUE)
names(degStateCip) <- c('STABBR','degType','year','comp_high_any','totalawards')
degState <- aggregate(totalawards, by=list(STABBR,degType,year), FUN=sum, na.rm=TRUE)
names(degState) <- c('STABBR','degType','year','statetotal')
degStateComp <- merge(degState,degStateCip,by=c('STABBR','degType','year'))
degStateComp$statetotal <- degStateComp$statetotal / 2
degStateComp$share <- degStateComp$totalawards / degStateComp$statetotal

##CBSA
groupCBSA <- group_by(total, CBSA, AWLEVEL, CIPCODE, year, degType)
byCBSA <- as.data.frame(summarise(groupCBSA, totalawards = sum(CTOTALT)))
CBSATotal <- filter(byCBSA, CIPCODE == 99)
CBSATotal <- select(CBSATotal, CBSA, AWLEVEL, totalawards,year)
names(CBSATotal) <- c('CBSA','AWLEVEL','CBSAtotal','year')
byCBSA <- merge(byCBSA, CBSATotal, by=c('CBSA','AWLEVEL','year'))
byCBSA <- left_join(byCBSA, onet, by='CIPCODE')
byCBSA$share <- byCBSA$totalawards / byCBSA$CBSAtotal

attach(byCBSA)
stemShareCBSA <- aggregate(byCBSA$share, by=list(stem_high_any,CBSA,AWLEVEL,year), FUN=sum, na.rm=TRUE)
names(stemShareCBSA) <- c('stem','cbsa','awlevel','year','share')
compShareCBSA <- aggregate(byCBSA$share, by=list(comp_high_any,CBSA,AWLEVEL,year), FUN=sum, na.rm=TRUE)
names(compShareCBSA) <- c('comp','cbsa','awlevel','year','share')

attach(byCBSA)
degCbsaCip <- aggregate(totalawards, by=list(CBSA,degType,year,stem_high_any), FUN=sum, na.rm=TRUE)
names(degCbsaCip) <- c('CBSA','degType','year','stem_high_any','totalawards')
degCbsa <- aggregate(totalawards, by=list(CBSA,degType,year), FUN=sum, na.rm=TRUE)
names(degCbsa) <- c('CBSA','degType','year','Cbsatotal')
degCbsaStem <- merge(degCbsa,degCbsaCip,by=c('CBSA','degType','year'))
degCbsaStem$Cbsatotal <- degCbsaStem$Cbsatotal / 2
degCbsaStem$share <- degCbsaStem$totalawards / degCbsaStem$Cbsatotal

attach(byCBSA)
degCbsaCip <- aggregate(totalawards, by=list(CBSA,degType,year,comp_high_any), FUN=sum, na.rm=TRUE)
names(degCbsaCip) <- c('CBSA','degType','year','comp_high_any','totalawards')
degCbsa <- aggregate(totalawards, by=list(CBSA,degType,year), FUN=sum, na.rm=TRUE)
names(degCbsa) <- c('CBSA','degType','year','Cbsatotal')
degCbsaComp <- merge(degCbsa,degCbsaCip,by=c('CBSA','degType','year'))
degCbsaComp$Cbsatotal <- degCbsaComp$Cbsatotal / 2
degCbsaComp$share <- degCbsaComp$totalawards / degCbsaComp$Cbsatotal

write.csv(stemShareState,'stemByState.csv')
write.csv(compShareState,'compByState.csv')
write.csv(stemShareCBSA, 'stemByCBSA.csv')
write.csv(compShareCBSA, 'compByCBSA.csv')

write.csv(degCbsaComp, 'compByCBSAByDegree.csv')
write.csv(degCbsaStem, 'stemByCBSAByDegree.csv')
write.csv(degStateComp, 'compByStateByDegree.csv')
write.csv(degStateStem, 'stemByStateByDegree.csv')
