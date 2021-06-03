#library
library(dplyr)
library(ggplot2)

#load the data
datsim<-readRDS("../outputs/datasimu2.rds")
clrdb<-readRDS("../outputs/datclrdbsimpop.rds")
datrdbpop<-readRDS("../outputs/datrdbsimpop.rds")
datrdbsamp<-readRDS("../outputs/datrdbsimsamp.rds")
fct1<-function(dat){
	#dat<-datrdbpop
	pipo<-left_join(dat$FM,dat$SA)%>%
		left_join(dat$SS)%>%
		left_join(dat$FO)%>%
		left_join(dat$FT)%>%
		left_join(dat$VS)
	return(pipo)
}

#check data
tmp1<-datsim%>%filter(year==1)%>%mutate(vname=paste0("v",VDid),tname=paste0("t",TRid),ori="sim")
tmp2<-fct1(datrdbpop)%>%filter(FOendDate==1)%>%
	mutate(vname=VSencryptedVesselCode,
	       tname=FTunitName,
	       spp=SAspeciesCodeFAO,
	       len=FMclass,
	       n=FMnumberAtUnit,
	       FOid=FOunitName,
	       ori="rdbes")

#some graph{{{
if(F){
ggplot(tmp1, aes(x=len,y=n,group=FOid,color=FOid))+
	geom_path()+
	facet_grid(spp~vname+tname,scale="free")
x11()
ggplot(tmp2, aes(x=len,y=n,group=FOid,color=FOid))+
	geom_path()+
	facet_grid(spp~vname+tname,scale="free")
#compute total by year gear spp
tmp1<-datsim%>%group_by(gear,spp,len)%>%summarise(n=sum(n),ori="ori")%>%ungroup()#filter(year==1)%>%mutate(vname=paste0("v",VDid),tname=paste0("t",TRid),ori="sim")
tmp2<-fct1(datrdbpop)%>%
	group_by(#year=FOendDate,
		 gear=FOgear,
		 spp=SAspeciesCodeFAO,
		 len=FMclass)%>%summarise(n=sum(FMnumberAtUnit),ori="rdbes")%>%
	ungroup()
ggplot(rbind(tmp1,tmp2), aes(x=len,y=n,group=ori,color=ori))+
	geom_path()+
	facet_grid(spp~gear,scale="free")
}
#end graph }}}
#seems fiiiiiiiiiiiine 
#test ratio estimators based on Liz code
#fct to calculte automatically incl prob whatever the hierarchy is
#input : rdbes data following david format
#hypothese of SRS
	#compute incp
	doincp<-function(b,a){ 
		#b<-datrdbpop$FO
		#a<-"FOinclusionProb"
		id<-(which(names(b)%in%a))
		if(length(id)==1){
			idtot<-which(substr(names(b),3,13)=="numberTotal"&nchar(names(b))<=13)
			idsamp<-which(substr(names(b),3,15)=="numberSampled"&nchar(names(b))<=15)
			b[,id]<-b[,idsamp]/b[,idtot]
			#print(id,idtot,idsamp)
		}
		return(b)
	}
	#some testing
	pipo<-doincp(datrdbpop$FT,"FTinclusionProb")
	pipo<-doincp(datrdbpop$FO,"FOinclusionProb")


	#datrdbpop<-mapply(doincp,datrdbpop,
	datrdbpop<-mapply(doincp,datrdbpop,
		     paste0(names(datrdbpop),"inclusionProb"),
		     SIMPLIFY=FALSE)
	#

# -----------------------------------------------------------------------
# a function to calculate inclusion probabilities for the units at the final stage
# of sampling given all the inclusion probabilities for the other stages
# -----------------------------------------------------------------------
getIncProb <- function(RDB,stages){
	RDB<-datrdbpop
	stages<-stages
  nStages <- length(stages)
  if (any(stages %in% c("FM"))) {
    RDB[["FM"]][["FMinclusionProb"]] <- 1
  }
  RDB[[stages[[1]]]][["inclusionProb"]] <- RDB[[stages[[1]]]][[paste(stages[[1]],"inclusionProb",sep="")]]
  for (i in 2:(nStages)) {
    indx <- RDB[[stages[[i]]]][[paste(stages[[i-1]],"id",sep="")]]
    indxPrev <- RDB[[stages[[i-1]]]][[paste(stages[[i-1]],"id",sep="")]]
    RDB[[stages[[i]]]][["inclusionProbPrev"]] <- RDB[[stages[[i-1]]]][[paste("inclusionProb",sep="")]][match(indx,indxPrev)]
    RDB[[stages[[i]]]][["inclusionProb"]] <- RDB[[stages[[i]]]][["inclusionProbPrev"]]*RDB[[stages[[i]]]][[paste(stages[[i]],"inclusionProb",sep="")]]
  }
  return(RDB)
}

#calcul incprob
stages<-list("VS","FT","FO","SS","SA","FM")
datrdbpop<-getIncProb(datrdbpop,stages)

#add domain to FM and SA 
#DOtime=temporal domain, DOtech=technical (metier), DOspp domain
#find the domain where they are: generic key
#DOtech and DOtime in FO, DOspp in SA
DO<-datrdbpop$FO%>%transmute(FOid,FTid,DOtech=FOgear,DOtime=FOendDate)%>%
	#add the key to SS
	left_join(datrdbpop$SS%>%transmute(SSid,FOid))%>%
	#add the key to SA
	left_join(datrdbpop$SA%>%transmute(SAid,SSid,DOspp=SAspeciesCodeFAO))%>%
	#add the key to FM
	left_join(datrdbpop$FM%>%transmute(FMid,SAid))

#add domain to SA and FM
datrdbpop$FM<-left_join(datrdbpop$FM,DO)
datrdbpop$SA<-left_join(datrdbpop$SA,DO%>%select(-FMid)%>%distinct())

#n@len

# calculate a Horvitz Thompson estimate for total numbers at length by domain
# assuming srs within the domain - this is valid for the RDBshare data
est4nlenpop <- datrdbpop$FM%>%group_by(len=FMclass,year=DOtime,gear=DOtech,spp=DOspp)%>%summarise(n=sum(FMnumberAtUnit/inclusionProb),type="est")%>%
	ungroup()#%>%transmute
#graph
tmp1<-datsim%>%group_by(year,gear,spp,len)%>%summarise(n=sum(n),type="pop")%>%ungroup()#filter(year==1)%>%mutate(vname=paste0("v",VDid),tname=paste0("t",TRid),ori="sim")
ggplot(rbind(tmp1,est4nlenpop), aes(x=len,y=n,color=year,group=year))+#group=year,color=type))+
	geom_path()+
	facet_grid(spp~gear+type,scale="free")+
	theme_bw()


# calculate a HT estimate for total landed weight using the sampled landed
# weights
# assuming srs etc as before
estX <- tapply(datrdbpop$SA$SAtotalWtLive/datrdbpop$SA$incProbPrev,list(datrdbpop$SA$domain),sum)/1e3

# get the relevant population totals
popX <- popDat[match(names(estX),rownames(popDat)),"liveWt"]

# calculate the ratio estimates for numbers at length
estLR <- estL*matrix(rep(popX/estX,dim(estL)[1]),byrow=T,ncol=dim(estL)[2])













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





