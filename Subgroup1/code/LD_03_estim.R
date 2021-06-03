library(dplyr)
library(ggplot2)

#load the data
clrdb<-readRDS("../outputs/clrdb.rds")
datrdb<-readRDS("../outputs/datrdb.rds")

#naive (and probably wrong) ratio estimator
#pop data
wtot<-clrdb%>%
	group_by(time=CLyear,metier=CLmetier6,space="all",spp=CLspecFAO)%>%
	summarise(w=sum(CLoffWeight))%>%
	ungroup()
#samp data
FOinfo<-datrdb$FO%>%transmute(FOid,time=FOendDate,metier=FOgear,space="all")%>%distinct()
SSinfo<-datrdb$SS%>%transmute(FOid,SSid)%>%distinct()
SAinfo<-datrdb$SA%>%transmute(SAid,SSid,spp=SAspeciesCode,
			      wsamp=SAsampleWeightMeasured,wsamptot=SAtotalWeightMeasured)%>%distinct()
FMinfo<-datrdb$FM%>%filter(FMtype=="l")%>%
		transmute(FMid,SAid,len=FMclass,n=FMnumberAtUnit)
#agg length tot
wsamp<-SAinfo%>%left_join(SSinfo)%>%left_join(FOinfo)%>%
	group_by(time,metier,space,spp)%>%
	summarise(wsamp=sum(wsamp),wsamptot=sum(wsamptot))%>%
	ungroup()

nsamp<-FMinfo%>%left_join(SAinfo)%>%left_join(SSinfo)%>%left_join(FOinfo)%>%
	ungroup()%>%
	group_by(time,metier,space,spp,len)%>%
	summarise( n=sum(n))%>%
	ungroup() %>%left_join(wsamp)
#merge nsamp and wtot
nsamp<-left_join(nsamp,wtot)%>%mutate(npop=n*w/wsamptot)

ggplot(nsamp,aes(x=len,y=npop))+geom_path()+facet_grid(metier~spp,scale="free")
#load simulated data
uu<-readRDS("../outputs/datapop.rds")
npg<-readRDS("../outputs/datapopn.rds")%>%filter(year==1,value>0)%>%transmute(metier=gear,
						time=year,
						space="all",
						spp,len,
						n=value,type="ori")
npgestim<-nsamp%>%transmute(metier,time,space,spp,len,n=npop,type="estim")
pipo<-rbind(npg,npgestim)
ggplot(pipo,aes(x=len,y=n,color=type,group=type))+geom_path()+facet_wrap(metier~spp,scale="free")

#ggplot(pipo%>%filter(spp=="Dab"),aes(x=len,y=n,color=type,group=type))+geom_path()+facet_grid(metier~spp,scale="free")
ggplot(pipo%>%filter(spp=="Dab"),aes(x=len,y=n,color=type,group=type))+geom_path()+facet_wrap(~type,scale="free")





