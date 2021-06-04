library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial) # riverplot in ggplot
library(GGally) # coplot in ggplot

## to prepare data in H1 format you could do something like this:
# species <- 126437 #haddock
# areas <- c('27.7.b','27.7.c','27.7.d','27.7.e','27.7.f','27.7.g','27.7.h','27.7.j','27.7.k')
# SAinfo <- subset(H1$SA,SAspeciesCode==species & SAarea %in% areas & SAcatchCategory=='Dis') %>% transmute(SAid,SSid,SAsam=as.numeric(SAsampleWeightMeasured),SAtot=as.numeric(SAtotalWeightMeasured)) %>% distinct()
# SSinfo <- left_join(SAinfo,H1$SS) %>% transmute(SSid,FOid,SAsam,SAtot,FOsam=as.numeric(SSnumberSampled))
# FOinfo <- left_join(SSinfo,H1$FO) %>% transmute(SSid,FOid,FTid,metier=FOmetier6,area=FOarea,SAsam,SAtot,FOsam) %>% distinct
# FTinfo <- left_join(FOinfo,H1$FT) %>% transmute(SSid,FTid,VSid,metier,area,SAsam,SAtot,FOsam,FOtot=as.numeric(FTnumberOfHauls),FTsam=as.numeric(FTnumberSampled)) %>% distinct()
# VSinfo <- left_join(FTinfo,H1$VS) %>% transmute(SSid,VSid,SDid,metier,area,SAsam,SAtot,FOsam,FOtot,FTsam) %>% distinct()
# CEinfo <- CE$CE %>% group_by(metier=CEmetier6,area=CEarea) %>% summarise(FTtot=sum(CEnumberOfFractionTrips))
# data <- left_join(VSinfo,CEinfo)
## note that this may not be correct, it is for illustration only

## because we cannot share the data on github we will simulate some data
## that looks a bit like the Irish data for CS haddock
## say, 50 samples for in one stratum
n <- 50
set.seed(1)
data <- data.frame(SSid=1:n,SAsam=exp(runif(n,-1,3)))
data <- data %>% mutate(SAtot=SAsam*runif(n,1,20)
                        ,FOsam=abs(round(rnorm(n,10,5)))+1
                        ,FOtot=round(FOsam*runif(n,1,5))
                        ,FTsam=abs(FOtot/6+rnorm(n,0,1)+1)
                        ,FTtot=10000
                        )

# SSid is the sample ID
# SAsam is the sample weight of discarded haddock from the haul (in kg)
# SAtot is the total discard weight of haddock from the haul
# FOsam is the number of hauls sampled on the trip
# FOtot is the total number of hauls on the trip
# FTsam is the effort of the sampled trips
# FTtot is the effort of the population

# Under design-based estimation, the sample weight gets inflated through the hierarchy
# up to the trip level and then apply a ratio estimator for the last step
# because we are assuming simple random samples here the ratio est is the same as design0-based
data2 <- data %>% 
  transmute(SSid,SA=SAsam,FO=SAtot,FT=SAtot*FOtot/FOsam,POP=FT*FTtot/FTsam) %>%
  pivot_longer(-1,names_to='Hierarchy',values_to='Volume')
data2$Hierarchy <- ordered(data2$Hierarchy,levels=c('SA','FO','FT','POP'))
data2 <- data2 %>% group_by(Hierarchy) %>% mutate(RelativeVolume=Volume/sum(Volume))

# this plot shows the weight that was actually sampled (250kg)
# and how it gets inflated in each step (up to just over 1000 tonnes)
ggplot(data2 %>% group_by(Hierarchy) %>% summarise(Volume=sum(Volume)),aes(Hierarchy,Volume*1e-3,group=1)) +
  geom_point() + geom_line() + ylab('Catch weight (t)') + scale_y_continuous(trans = 'log10')
ggsave('FollowSampleThroughEstimation1.png',width=6,height=4,units='in')

# This shows the relative weight (but which is also the weighting) of each sample
# as it progresses though the hierarchy
# The biggest sample (green at the top) contributes most to the final estimate
# In general the ranking does not change too drastically; there are no samples that 
# disproportionately dominate the final estimate
ggplot(data2,aes(Hierarchy,RelativeVolume,alluvium=SSid)) + 
  geom_alluvium(aes(fill = factor(SSid)),alpha=.75,col=1,decreasing=F,show.legend=F) +
  ylab('Relative weight')
ggsave('FollowSampleThroughEstimation2.png',width=6,height=8,units='in')

# pivot the data for the final plot
data3 <- data2 %>% pivot_wider(id_cols=SSid,names_from=Hierarchy,values_from=RelativeVolume)

# this shows how the contribution of each sample (its weight) in each step of the
# hierarchy correlates with the other steps
# in particular the bottom-left plot shows 
ggpairs(data3,columns=2:5) 
ggsave('FollowSampleThroughEstimation3.png',width=6,height=6,units='in')
