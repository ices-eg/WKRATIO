library(tidyverse)
library(viridis)

load('IE_2019.RData')

species <- 126437 #haddock
areas <- c('27.7.b','27.7.c','27.7.d','27.7.e','27.7.f','27.7.g','27.7.h','27.7.j','27.7.k')


SAinfo <- subset(H1$SA,SAspeciesCode==species & SAarea %in% areas & SAcatchCategory=='Dis') %>% transmute(SAid,SSid,SAsam=as.numeric(SAsampleWeightMeasured),SAtot=as.numeric(SAtotalWeightMeasured)) %>% distinct()
SSinfo <- left_join(SAinfo,H1$SS) %>% transmute(SSid,FOid,SAsam,SAtot,FOsam=as.numeric(SSnumberSampled))
FOinfo <- left_join(SSinfo,H1$FO) %>% transmute(SSid,FOid,FTid,metier=FOmetier6,area=FOarea,SAsam,SAtot,FOsam) %>% distinct
FTinfo <- left_join(FOinfo,H1$FT) %>% transmute(SSid,FTid,VSid,metier,area,SAsam,SAtot,FOsam,FOtot=as.numeric(FTnumberOfHauls),FTsam=as.numeric(FTnumberSampled)) %>% distinct()
VSinfo <- left_join(FTinfo,H1$VS) %>% transmute(SSid,VSid,SDid,metier,area,SAsam,SAtot,FOsam,FOtot,FTsam) %>% distinct()

CEinfo <- CE$CE %>% group_by(metier=CEmetier6,area=CEarea) %>% summarise(FTtot=sum(CEnumberOfFractionTrips))
data1 <- left_join(VSinfo,CEinfo)

subset(data1,SSid==9158)
data1 <- data1[which(!duplicated(data1$SSid)),]

data2 <- data1 %>% 
  transmute(SSid,metier,area,SA=SAsam,FO=SAtot,FT=SAtot*FOtot/FOsam,POP=FT*FTtot/FTsam) %>%
  pivot_longer(-c(1:3),names_to='Hierarchy',values_to='Volume')
data2$Hierarchy <- ordered(data2$Hierarchy,levels=c('SA','FO','FT','POP'))

ggplot(data2,aes(factor(SSid),Volume,fill=Hierarchy)) + geom_col(position=position_fill())

ggplot(subset(data2,Hierarchy%in%c('SA','FO')),aes(factor(SSid),Volume,fill=Hierarchy)) + geom_col()


library(ggalluvial)
dat <- subset(data2,metier=='OTB_DEF_100-119_0_0') %>% group_by(Hierarchy) %>% mutate(RelativeVolume=Volume/sum(Volume))
dat$sampleid <- as.numeric(factor(dat$SSid))
set.seed(1)
dat$col <- factor(sample(1:20,max(dat$sampleid),T)[dat$sampleid])

ggplot(dat,aes(Hierarchy,Volume*1e-6,alluvium=SSid)) + 
  geom_alluvium(aes(fill = col),alpha=.75,col=1,decreasing=F,show.legend=F) +
  ylab('Catch weight (t)')
ggsave('FollowSampleThroughEstimation1.png',width=6,height=8,units='in')

ggplot(dat %>% group_by(Hierarchy) %>% summarise(Volume=sum(Volume)),aes(Hierarchy,Volume*1e-6,group=1)) +
  geom_point() + geom_line() + ylab('Catch weight (t)') + scale_y_continuous(trans = 'log10')
ggsave('FollowSampleThroughEstimation2.png',width=6,height=4,units='in')
  
ggplot(dat,aes(Hierarchy,RelativeVolume,alluvium=SSid)) + 
  geom_alluvium(aes(fill = col),alpha=.75,col=1,decreasing=F,show.legend=F) +
  ylab('Relative weight')
ggsave('FollowSampleThroughEstimation3.png',width=6,height=8,units='in')


dat2 <- dat %>% pivot_wider(id_cols=c(SSid),names_from=Hierarchy,values_from=RelativeVolume)
plot(dat2[,-1])

library(GGally)
ggpairs(dat2,columns=2:5)
ggsave('FollowSampleThroughEstimation4.png',width=6,height=6,units='in')


