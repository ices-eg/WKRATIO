# quick/dirty ratio est code from Laurent, applied to IE 2019 data

library(dplyr)
library(ggplot2)

load('IE_2019.RData')

#naive (and probably wrong) ratio estimator
#pop data
wtot<- CL$CL %>%
  group_by(time=CLyear,metier=CLmetier6,space="all",spp=CLspeciesCode) %>%
  summarise(w=sum(CLofficialWeight))%>%
  ungroup()


FOinfo <- H1$FO %>% transmute(FOid,time=substring(FOendDate,1,4),metier=FOmetier6,space="all") %>% distinct()
SSinfo <- H1$SS %>% transmute(FOid,SSid) %>% distinct()
SAinfo <- H1$SA %>% transmute(SAid,SSid,spp=SAspeciesCode,wsamp=as.numeric(SAsampleWeightMeasured),wsamptot=as.numeric(SAtotalWeightMeasured)) %>% distinct()
FMinfo <- H1$FM %>% transmute(FMid,SAid,len=as.numeric(FMclass),n=as.numeric(FMnumberAtUnit))

#agg length tot
wsamp <- SAinfo%>%left_join(SSinfo)%>%left_join(FOinfo)%>%
  group_by(time,metier,space,spp)%>%
  summarise(wsamp=sum(wsamp),wsamptot=sum(wsamptot))%>%
  ungroup()

nsamp<-FMinfo%>%left_join(SAinfo)%>%left_join(SSinfo)%>%left_join(FOinfo)%>%
  ungroup()%>%
  group_by(time,metier,space,spp,len)%>%
  summarise( n=sum(n))%>%
  ungroup() %>%left_join(wsamp)

#merge nsamp and wtot
nsamp1 <-left_join(nsamp,wtot)%>%mutate(npop=n*w/wsamptot)

topsp <- as.numeric(names(sort(-table(nsamp$spp))))[1:5]

ggplot(subset(nsamp1,spp%in%topsp),aes(x=len,y=npop))+geom_path()+facet_grid(metier~spp,scale="free")

###

FMinfo1 <- H1$FM %>% group_by(SAid) %>% summarise(n=sum(as.numeric(FMnumberAtUnit)))
nsamp1 <- FMinfo1%>%left_join(SAinfo)%>%left_join(SSinfo)%>%left_join(FOinfo)

ggplot(subset(nsamp1,spp%in%topsp),aes(n,wsamp,col=metier)) + geom_point() +facet_wrap(~spp,scale="free")
       