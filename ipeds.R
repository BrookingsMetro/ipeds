setwd("V:\\IPEDS\\Code\\ipeds")
library('dplyr')

csxwalk <- read.csv('Crosswalks\\compSci.csv')
stemxwalk <- read.csv('Crosswalks\\allSTEM.csv')

csxwalk$CIPCODE <- (substr(csxwalk$Code, 1,7))
csxwalk <- as.data.frame(sapply(csxwalk,gsub,pattern="-",replacement='.'))
csxwalk$CIPCODE <- as.numeric(csxwalk$CIPCODE)
csxwalk$compSci <- 1

stemxwalk$CIPCODE <- as.numeric(substr(stemxwalk$Code, 1,7))
stemxwalk <- as.data.frame(sapply(stemxwalk,gsub,pattern='-',replacement='.'))
stemxwalk$CIPCODE <- as.numeric(stemxwalk$CIPCODE)
stemxwalk$STEM <- 1

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

g2010 <- group_by(c2010, STABBR, AWLEVEL, CIPCODE)
byState2010 <- as.data.frame(summarise(g2010, totalawards = sum(CTOTALT)))
stateTotal2010 <- filter(byState2010, CIPCODE == 99)
stateTotal2010 <- select(stateTotal2010, STABBR, AWLEVEL, totalawards)
names(stateTotal2010) <- c('STABBR','AWLEVEL','statetotals')
byState2010 <- merge(byState2010, stateTotal2010, by=c('STABBR','AWLEVEL'))
byState2010 <- left_join(byState2010, stemxwalk)
byState2010 <- left_join(byState2010, csxwalk)

g2011 <- group_by(c2011, STABBR, AWLEVEL, CIPCODE)
byState2011 <- as.data.frame(summarise(g2011, totalawards = sum(CTOTALT)))
stateTotal2011 <- filter(byState2011, CIPCODE == 99)
stateTotal2011 <- select(stateTotal2011, STABBR, AWLEVEL, totalawards)
names(stateTotal2011) <- c('STABBR','AWLEVEL','statetotals')
byState2011 <- merge(byState2011, stateTotal2011, by=c('STABBR','AWLEVEL'))
byState2011 <- left_join(byState2011, stemxwalk)
byState2011 <- left_join(byState2011, csxwalk)



g2014 <- group_by(c2014, STABBR, AWLEVEL, CIPCODE)
byState2014 <- as.data.frame(summarise(g2014, totalawards = sum(CTOTALT)))
stateTotal2014 <- filter(byState2014, CIPCODE == 99)
stateTotal2014 <- select(stateTotal2014, STABBR, AWLEVEL, totalawards)
names(stateTotal2014) <- c('STABBR','AWLEVEL','statetotals')
byState2014 <- merge(byState2014, stateTotal2014, by=c('STABBR','AWLEVEL'))
byState2014 <- merge(byState2014, stemxwalk, by=c('CIPCODE'))
byState2014 <- merge(byState2014, csxwalk, by=c("CIPCODE"))

write.csv(byState2010,'awardsByStateByCIP2010.csv')
write.csv(byState2011,'awardsByStateByCIP2011.csv')
write.csv(byState2014,'awardsByStateByCIP2014.csv')

