setwd("/Users/okada/myWork/coursera/RepData_PeerAssessment2")
library(plyr);library(ggplot2)
#dat <- read.csv("repdata-data-StormData.csv")
dat <- read.csv(bzfile("repdata-data-StormData.csv.bz2", open="r"))
dim(dat);head(dat)
#save(dat, file="dat.RData")
#load("dat.RData")


# Across the United States, which types of events (as indicated in the EVTYPE variable) 
# are most harmful with respect to population health?

# Across the United States, which types of events have the greatest economic consequences?


## Data processing
# def can be found in http://forecast.weather.gov/glossary.php?word=wfo
# WFO: Weather Forecast Office
# PROPDMG : property damage
# CROPDMG : crop damage

## summarise fatalities and injuries by event type
dat.sum.evtype <- ddply(dat, .(EVTYPE), summarize, 
                        fatalities=sum(FATALITIES), injuries=sum(INJURIES), 
                        total_health_harm = sum(FATALITIES+INJURIES),
                        propdmg = sum(PROPDMG), cropdmg=sum(CROPDMG),
                        total_economic_dmg = sum(PROPDMG+CROPDMG)
                        )
dat.sum.evtype <- arrange(dat.sum.evtype, desc(total_health_harm), desc(fatalities), desc(injuries))
head(dat.sum.evtype)
## worst 5 event : Tornado, excessive heat, Thunderstorm(TSTM) wind, flood, lightning


## economic damge can be estimated by PROPDMG and CROPDMG
dat.sum.evtype <- arrange(dat.sum.evtype, desc(total_economic_dmg),
                          desc(propdmg), desc(cropdmg))
head(dat.sum.evtype, n=5)
## tornado, flash flood, tstm wind, hail, flood
arrange(dat.sum.evtype, desc(cropdmg), desc(total_economic_dmg),
        desc(propdmg))[1:5, ]
arrange(dat.sum.evtype, desc(propdmg), desc(total_economic_dmg),
        desc(cropdmg))[1:5, ]
library(reshape2)
dat.melt <- melt(subset(dat.sum.evtype,  
                        select=-c(total_health_harm,total_economic_dmg)),
                 id.vars=c("EVTYPE"), variable.name="dmg_type", na.rm=TRUE)
## health damage
m1 <- ggplot(subset(dat.melt, EVTYPE %in% c("EXCESSIVE HEAT","HEAT", "LIGHTNING","HAIL","TORNADO","FLASH FLOOD","TSTM WIND","FLOOD","THUNDERSTORM WIND") & 
                      dmg_type %in% c("injuries","fatalities")),
             aes(x=EVTYPE, y=value, fill=factor(dmg_type)))
m1 <- m1 + geom_bar(stat="identity" )+coord_flip()
m1 + xlab("the number of fatalities and injuries") + ylab("Types of Events") +
  ggtitle("Impact of events on the population health")


## economic damage
m2 <- ggplot(subset(dat.melt, EVTYPE %in% c("EXCESSIVE HEAT","HEAT", "LIGHTNING","HAIL","TORNADO","FLASH FLOOD","TSTM WIND","FLOOD","THUNDERSTORM WIND") & 
                      dmg_type %in% c("propdmg","cropdmg")),
             aes(x=EVTYPE, y=value, fill=factor(dmg_type)))
m2 <- m2 + geom_bar(stat="identity" )+coord_flip()
m2 + xlab("the number of fatalities and injuries") + ylab("Types of Events") + 
  ggtitle("Impact of events on the economic activities")


#### 
library(knitr)
knit2html(input = "PA2_analysis.Rmd", output = "PA2_analysis.Rmd") 
knit2html(input = "PA2_analysis.Rmd", output = "PA2_analysis.md") 


