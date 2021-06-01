# Laurent use FO, SS,SA table with RDBES format

library(dplyr)
library(lubridate)
library(ggplot2)

#load the data
# clrdb<-readRDS("./data/clrdb.rds")
# datrdb<-readRDS("./data/datrdb.rds")
clrdb<-CL
clrdb$CLyear<-as.integer(clrdb$CLyear)
clrdb$CLspecCode<-as.character(clrdb$CLspecCode)
clrdb$CLoffWeight <- as.numeric(clrdb$CLoffWeight)
#naive (and probably wrong) ratio estimator
#pop data
wtot<-clrdb%>%
  group_by(time=CLyear,metier=substr(CLmetier6,1,3),space="all",spp=CLspecCode)%>%
  summarise(w=sum(CLoffWeight, na.rm = TRUE))%>%
  ungroup()
#samp data
FOinfo<-FO%>%
  transmute(FOid,
            time=year(FOendDate),
            metier=FOgear,
            space="all")%>%
  distinct()
SSinfo<-SS%>%
  transmute(FOid,SSid)%>%
  distinct()
SAinfo<-SA%>%
  transmute(SAid,
            SSid,
            spp=SAspeCode,
            wsamp=SAsampWtMes,
            wsamptot=SAtotalWtMes)%>%
  distinct()
FMinfo<-FM%>%
  filter(FMtype=="Total Length")%>%
  transmute(FMid,SAid,len=FMclass,n=FMnumAtUnit)
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

#### KK I need to cut my data to ex.top 10 spp??
#merge nsamp and wtot
nsamp<-left_join(nsamp,wtot)%>%mutate(npop=n*w/wsamptot)
nsamp<-subset(nsamp,!is.na(w))

top10_spp <- nsamp %>%
  group_by(spp) %>%
  summarise(c=sum(n)) %>%
  arrange(desc(c)) %>%
  top_n(10)

top10_spp$top10_spp <- "X"
nsamp <- left_join(nsamp,top10_spp,by="spp")
nsamp$topspp <- ifelse(is.na(nsamp$top10_spp),"Not_top",nsamp$spp)
nsamp<-subset(nsamp,top10_spp=='X')

x11()
ggplot(nsamp,aes(x=len,y=npop))+geom_path()+facet_grid(metier~spp,scale="free")


#load simulated data
# uu<-readRDS("D:/WKRATIO/output/data/datapop.rds")
# npg<-readRDS("D:/WKRATIO/output/data/datapopn.rds")%>%filter(year==1,value>0)%>%transmute(metier=gear,
#                                                                           time=year,
#                                                                           space="all",
#                                                                           spp,len,
#                                                                           n=value,type="ori")
# npgestim<-nsamp%>%transmute(metier,time,space,spp,len,n=100*npop,type="estim")
# pipo<-rbind(npg,npgestim)
# ggplot(pipo,aes(x=len,y=n,color=type,group=type))+geom_path()+facet_wrap(metier~spp,scale="free")
# 
# ggplot(pipo%>%filter(spp=="Dab"),aes(x=len,y=n,color=type,group=type))+geom_path()+facet_grid(metier~spp,scale="free")
# ggplot(pipo%>%filter(spp=="Dab"),aes(x=len,y=n,color=type,group=type))+geom_path()+facet_wrap(~type,scale="free")
