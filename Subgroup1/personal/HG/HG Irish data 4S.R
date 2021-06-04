# quick/dirty ratio est code from Laurent, applied to IE 2019 data

library(dplyr)
library(ggplot2)

load('IE_2019.RData')

#naive (and probably wrong) ratio estimator
#pop data
wtot<- CL$CL %>%
  group_by(time=CLyear,metier=CLmetier6,space=CLarea,spp=CLspeciesCode) %>%
  summarise(w=sum(CLofficialWeight))%>%
  ungroup()


FOinfo <- H1$FO %>% transmute(FOid,time=substring(FOendDate,1,4),metier=FOmetier6,space=FOarea) %>% distinct()
SSinfo <- H1$SS %>% transmute(FOid,SSid) %>% distinct()
SAinfo <- H1$SA %>% transmute(SAid,SSid,spp=SAspeciesCode,CatchCat=SAcatchCategory,wsamp=as.numeric(SAsampleWeightMeasured),wsamptot=as.numeric(SAtotalWeightMeasured)) %>% distinct()
FMinfo <- H1$FM %>% transmute(FMid,SAid,len=as.numeric(FMclass),n=as.numeric(FMnumberAtUnit))

#agg length tot
wsamp <- SAinfo%>%left_join(SSinfo)%>%left_join(FOinfo)%>%
  group_by(time,metier,space,spp,CatchCat)%>%
  summarise(wsamp=sum(wsamp,na.rm=T),wsamptot=sum(wsamptot,na.rm=T))%>%
  ungroup()

nsamp<-FMinfo%>%left_join(SAinfo)%>%left_join(SSinfo)%>%left_join(FOinfo)%>%
  ungroup()%>%
  group_by(time,metier,space,spp,len,CatchCat)%>%
  summarise( n=sum(n,na.rm=T))%>%
  ungroup() %>%left_join(wsamp)

#merge nsamp and wtot
nsamp1 <-left_join(nsamp,wtot)%>%mutate(npop=n*w/wsamptot)
nsamp1 <- subset(nsamp1,CatchCat=='Lan')

topsp <- as.numeric(names(sort(-table(nsamp1$spp))))[1:5]

ggplot(subset(nsamp1,spp%in%topsp),aes(x=len,y=npop))+geom_path()+facet_grid(metier~spp,scale="free")

###

FMinfo1 <- H1$FM %>% group_by(SAid) %>% summarise(n=sum(as.numeric(FMnumberAtUnit)))
nsamp2 <- FMinfo1%>%left_join(SAinfo)%>%left_join(SSinfo)%>%left_join(FOinfo)

ggplot(subset(nsamp2,spp%in%topsp),aes(n,wsamp,col=metier)) + geom_point() +facet_wrap(~spp,scale="free")
       


######################
# now try something that looks like 4S

FMinfo <- H1$FM %>% transmute(FMid,SAid,len=as.numeric(FMclass),n=as.numeric(FMnumberAtUnit))
SAinfo <- subset(H1$SA) %>% transmute(SAid,SSid,spp=SAspeciesCode,CatchCat=SAcatchCategory,SAsam=as.numeric(SAsampleWeightMeasured),SAtot=as.numeric(SAtotalWeightMeasured)) %>% distinct()
SSinfo <- H1$SS %>% transmute(SSid,FOid,SSsam=as.numeric(SSnumberSampled),SStot=as.numeric(SSnumberTotal))
FOinfo <- H1$FO %>% transmute(FOid,FTid,time=substring(FOendDate,1,4),metier=FOmetier6,space=FOarea) %>% distinct()
FTinfo <- H1$FT %>% transmute(FTid,VSid,FOtot=as.numeric(FTnumberOfHauls),FTsam=as.numeric(FTnumberSampled)) %>% distinct()
VSinfo <- H1$VS %>% transmute(VSid,SDid) %>% distinct()
SDinfo <- H1$SD %>% transmute(SDid,DEid) %>% distinct()
DEinfo <- H1$DE %>% transmute(DEid,DEstratumName) %>% distinct()

CEinfo <- CE$CE %>% group_by(time=CEyear,metier=CEmetier6,space=CEarea) %>% summarise(FTtot=sum(CEnumberOfFractionTrips))
CLinfo <- CL$CL %>% group_by(time=CLyear,spp=CLspeciesCode,space=CLarea) %>% summarise(landings=sum(CLscientificWeight))

MonsterTable <- left_join(FMinfo,SAinfo); nrow(MonsterTable)
MonsterTable <- left_join(MonsterTable,SSinfo); nrow(MonsterTable)
MonsterTable <- left_join(MonsterTable,FOinfo); nrow(MonsterTable)
MonsterTable <- left_join(MonsterTable,FTinfo); nrow(MonsterTable)
MonsterTable <- left_join(MonsterTable,VSinfo); nrow(MonsterTable)
MonsterTable <- left_join(MonsterTable,SDinfo); nrow(MonsterTable)
MonsterTable <- left_join(MonsterTable,DEinfo); nrow(MonsterTable)
MonsterTable <- left_join(MonsterTable,CEinfo); nrow(MonsterTable)
MonsterTable <- left_join(MonsterTable,CLinfo); nrow(MonsterTable)

MonsterTable <- subset(MonsterTable,CatchCategory='Lan') %>% mutate(PopN=n*(SAtot/SAsam)*(SStot/SSsam)*(FTtot/FTsam))

a <- MonsterTable %>% group_by(time,spp,space,len,method='Design-based') %>% summarise(PopN=sum(PopN*1e-6,na.rm=T))
b <- nsamp1 %>% group_by(time,spp,len,space,method='Rough') %>% summarise(PopN=sum(npop,na.rm=T))
c <- rbind(a,b)
c1 <- subset(c,spp %in% '126437') #haddock

ggplot(c1,aes(len/10,PopN,col=method)) + geom_line() + facet_wrap(~space,scales='free')

subset(a,metier=='OTB_DEF_100-119_0_0' & spp==127146)
subset(b,metier=='OTB_DEF_100-119_0_0' & spp==127146)
subset(nsamp1,metier=='OTB_DEF_100-119_0_0' & spp==127146)

source('readintercatch.r')

IC <- ReadIntercatch('intercatch_had.27.7b-k_landings_length.csv')
ic <- IC$SD %>% group_by(time='2019',spp='127146',space=FishingArea,len=AgeLength*10,method='Submitted') %>% summarise(PopN=sum(NumberCaught))
c2 <- rbind(c1,ic)
ggplot(c2,aes(len/10,PopN,col=method)) + geom_line() + facet_wrap(~space)#,scales='free')




     