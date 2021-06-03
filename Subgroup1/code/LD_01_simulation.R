library(LeMaRns)
library(ggplot2)
library(dplyr)

#link https://github.com/CefasRepRes/LeMaRns 
#plosONE
#https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0227767

#a short test
NS_params <- LeMansParam(NS_par, tau=NS_tau, eta=NS_eta, L50=NS_L50,other=1e12)
#NS_par: species growth and co
#tau: interaction matrix (row prey columns predator if 1 ij means j eat i)
#eta: steepness of the catchability curve
#L50: length @50% of the max catchability of the catchability curve
#other: amount of other food available for prey not in the model (in g)
#mortality: background mortality M1, pred mortality M2, fishing mort F
effort <- matrix(0.25, 50, dim(NS_params @Qs)[3])
#4 gears
model_run <- run_LeMans(NS_params, years=50, effort=effort)
plot_SSB(NS_params,model_run)

#example with mixed fisheries

## mixed fishery
## setup the model
NS_params <- LeMansParam(df=NS_par, gdf = NS_mixed_fish, tau=NS_tau, eta=NS_eta, L50=NS_L50, other=NS_other)
### run the initial 50 years
effort <- matrix(0, 50, dim(NS_params@Qs)[3])
colnames(effort) <- c("Industrial", "Otter", "Beam", "Pelagic")
model_run <- run_LeMans(NS_params, years=50, effort=effort)
#plot SSB initial run
#another run with some effort
ef_lvl <- c(0, 0.5, 1, 1.5, 2)
efs <- expand.grid(Industrial=ef_lvl, Otter=ef_lvl, Beam=ef_lvl, Pelagic=ef_lvl)
ef<-as.numeric(rep(0.25,4))
effort_mat <- matrix(ef, 50, dim(NS_params@Qs)[3], byrow=T)
colnames(effort_mat) <- c("Industrial", "Otter", "Beam", "Pelagic")
model_run <- run_LeMans(params=NS_params, N0=model_run@N[,,501], years=50, effort=effort_mat)
plot_SSB(inputs=NS_params,outputs=model_run,full_plot_only=T)
plot_biomass(inputs=NS_params,outputs=model_run,full_plot_only=T)

#plot_indicators(inputs=NS_params,outputs=model_run)
#get catch per gear dim i species j gear k timestep
ycatch<-data.frame((get_annual_catch(NS_params,outputs=model_run)))
colnames(ycatch)<-NS_params@species_names
ycatch$year<-1:50
pipo<-tidyr::pivot_longer(ycatch,cols=1:length(NS_params@species_names))
ggplot(pipo,aes(x=year,y=value,color=name))+geom_line()

#catch per gear
cpg0<-get_CPG(inputs=NS_params,outputs=model_run,effort=effort_mat,years=50)
#add names to pivot_longer the array
attr(cpg0,"dimnames")[[1]]<-NS_params@species_names
attr(cpg0,"dimnames")[[2]]<-names(data.frame(effort_mat))
phi_min<-0.1
nbstep<-dim(cpg0)[3]
nbyear<-(nbstep)*phi_min
idyear<-ceiling(((1:(nbyear/phi_min))*phi_min))
attr(cpg0,"dimnames")[[3]]<-paste0("Y",idyear)
cpg0long<-data.frame(ftable(cpg0))%>%
	transmute(spp=as.character(Var1),
		  year=as.numeric(sub("Y","",as.character(Var3))),
		  gear=as.character(Var2),
		  value=Freq)%>%
	group_by(spp,year,gear)%>%summarise(value=sum(value,na.rm=T))%>%
	ungroup()
ggplot(cpg0long,aes(x=year,y=value,color=spp))+geom_line()+facet_wrap(~gear)

#get number at len by gear
get_NPG<-function(Catch,Qs,effort,years=nrow(effort),phi_min=0.1){
if(F){
Catch<-model_run@N
Qs<-NS_params@Qs
effort<-effort_mat
years<-nrow(effort)
ph_min<-0.1
}
	Catch<-Catch[,,-1]
        tot_time <- dim(Catch)[3]
        Fs <- array(0, dim=c(dim(Catch)[1], dim(Catch)[2], tot_time,dim(Qs)[3]))
        year <- ceiling(1:tot_time*phi_min)
	dimnames(Fs)[[4]] <- colnames(effort)
	for (j in colnames(effort)) {
	  for (ts in 1:tot_time) {
	    Fs[,,ts,j] <- effort[year[ts], j]*Qs[,,j]
	  }
	}
	#norm of fishing mort
 	tele0<-Fs
            for (ts in 1:tot_time) {
              tmp <- apply(Fs[,,ts,], c(1,2), sum)
              for(j in 1:dim(tele0)[4]) {
                tele <- Fs[,,ts,j]/tmp
                if (any(is.na(tele))) {
                  tele[is.na(tele)] <- 0
                }
		tele0[,,ts,j]<-tele
              }
	    }
	#get N fish by gear
	NPG<-tele0
        for(j in 1:dim(tele0)[4]) {
		NPG[,,,j]<-tele0[,,,j]*Catch
	}
	attr(NPG,"dimnames")[[1]]<-paste0("L",NS_params@u_bound)
	attr(NPG,"dimnames")[[2]]<-NS_params@species_names
	nbstep<-dim(NPG)[3]
	nbyear<-(nbstep)*phi_min
	idyear<-ceiling(((1:(nbyear/phi_min))*phi_min))
	attr(NPG,"dimnames")[[3]]<-paste0("Y",idyear)
	return(NPG)
}
npg0<-get_NPG(Catch=model_run@N,Qs=NS_params@Qs,effort=effort_mat,years=nrow(effort),phi_min=0.1)

npg0long<-data.frame(ftable(npg0))%>%
	transmute(len=as.numeric(sub("L","",as.character(Var1))),
		  spp=as.character(Var2),
		  year=as.numeric(sub("Y","",as.character(Var3))),
		  gear=as.character(Var4),
		  value=Freq)%>%
	group_by(len,spp,year,gear)%>%summarise(value=sum(value,na.rm=T))%>%
	ungroup()



ggplot(npg0long,aes(x=len,y=value,color=year,group=year))+geom_line()+facet_grid(spp~gear,scales="free")

#simulate a sampling plan stratified by the total catch of the gear over one
#year 
simstratsamp<-function(cpglong,npglong,param,nyear=1,ntot=100,ftot=1000){
	if(F){
	cpglong<-cpg0long
	npglong<-npg0long
	param<-NS_params
	nyear<-1
	ntot<-100
	ftot<-1000
	}
	#nb of samples by gear
	sgear<-cpglong%>%filter(year==nyear)%>%group_by(gear)%>%
		summarise(w=sum(value))%>%ungroup()%>%
		slice_sample(weight_by=w,n=ntot,replace=T)%>%
		group_by(gear)%>%summarise(n=n())%>%ungroup()
	#generate samp by gear
	datasamp<-data.frame()
	for(i in 1:nrow(sgear)){
		for(j in 1:sgear$n[i]){
			print(j)
			stmp<-npglong%>%
				filter(year==nyear,gear==sgear$gear[i],value>0)%>%
				slice_sample(weight_by=value,n=ftot,replace=T)%>%
				group_by(len,spp,year,gear)%>%summarise(n=n())%>%ungroup()%>%
				mutate(nsamp=j)
			datasamp<-rbind(datasamp,stmp)
		}
	}
	#add other info to datasamp
	lwa<-data.frame(spp=NS_params@species_names,a=NS_params@W_a,b=NS_params@W_b,
			Linf=NS_params@Linf,k=NS_params@k)
	datasamp<-datasamp%>%left_join(lwa)
	datasamp$wind<-datasamp$a*datasamp$len^datasamp$b
	#datasamp$lentmp<-datasamp$len
	#datasamp$lentmp[datasamp$len>datasamp$Linf]<-datasampe$Linf[datasamp$len>datasamp$Linf]
	fct1<-function(t0=0,K,Linf,L){ max(1, ((-log(1 - L/Linf))/K + t0))}
	datasampage<-NA
	for(i in 1:nrow(datasamp)){
		len<-datasamp$len[i];Linf<-datasamp$Linf[i];k<-datasamp$k[i]
		if(len>Linf){len<-Linf-1e-1}
		datasamp$age[i]<-fct1(0,k,Linf,len)
	}
	#add tot w
	datasamp<-datasamp%>%
		group_by(spp,year,gear,nsamp)%>%
		mutate(wspptot=sum(n*wind))%>%
		ungroup()%>%
		group_by(year,gear,nsamp)%>%
		mutate(wtriptot=sum(n*wind))%>%
		ungroup()
	return(datasamp)
}

datasamp<-simstratsamp(cpg0long,npg0long,NS_params,nyear=1,ntot=10,ftot=100)
ggplot(datasamp,aes(x=len,y=n,group=nsamp))+geom_point()+facet_grid(spp~gear,scales="free")

ggplot(datasamp,aes(x=len,y=wind,group=spp,color=spp))+geom_point()
ggplot(datasamp,aes(x=len,y=age,group=spp,color=spp))+geom_point()
saveRDS(datasamp,file="../outputs/datasimu.rds")
#truc for CL
saveRDS(cpg0long,file="../outputs/datapop.rds")
saveRDS(npg0long,file="../outputs/datapopn.rds")

#improvment: generating trrrrip for the whole pop and sampling theeeeeeem
if(F){
#npg0long,NS_params,n1=20,nbvess=data.frame(gear=c("Beam","Industrial","Otter","Pelagic"),nvess=c(4,2,4,3))
#ntrip is 1:n1

	#add vessel vessel trip proportionnal
	tr0<-npg0long%>%select(year,gear)%>%distinct()
	#generate a vessel population and related trip
	nbvess<-data.frame(gear=c("Beam","Industrial","Otter","Pelagic"),nvess=c(4,2,4,3))
	#generate up to 20 FO by gear and gear and up to max fo trip
	nfo<- tr0%>%rowwise()%>%mutate(nfo=sample(1:20,1))%>%
		mutate(ntrip=sample(1:nfo,1,replace=T))%>%
		ungroup()%>%
		left_join(nbvess)
	#trip id + FO id
	nfo<-nfo[rep(1:nrow(nfo),times=nfo$nfo),]%>%
		group_by(year,gear,nfo)%>%
		mutate(FOid=row_number())%>%ungroup()%>%
		rowwise()%>%
		mutate(TRid=sample(1:ntrip,1,replace=TRUE))%>%
		group_by(year,gear,TRid)%>%
		mutate(FOid=row_number())%>%ungroup()%>%
		arrange(year,gear,TRid,FOid)%>%
		ungroup()
	#separate elaboration of vess id (unique by trip: trick for that)
	nvesstmp<-nfo%>%select(year,gear,TRid,nvess)%>%distinct()%>%
		rowwise()%>%mutate(VDid=sample(1:nvess,1,replace=T))%>%
		ungroup()
	nall<-nfo%>%left_join(nvesstmp)
	#generate the prop of the catch taken by each FO
	nall<-nall%>%group_by(year,gear)%>%mutate(prop=runif(nfo))%>%
		mutate(prop=prop/sum(prop))%>%ungroup()
	#pipo%>%group_by(year,gear)%>%summarise(tot=sum(prop))%>%filter(tot!=1)
	npg0longsim<-left_join(nall,npg0long)%>%mutate(value=value*prop)%>%
		filter(value>0)%>%select(-prop)
	#check
	pipo1<-npg0longsim%>%group_by(year,gear,len,spp)%>%summarise(value0=sum(value))%>%ungroup()
	uu<-full_join(pipo1,npg0long)%>%filter(abs(value0-value)>1)
	if(nrow(uu)>0){strop("pb trip prop")}

	#npg0longsim%>%filter(year==1,gear=="Beam",FOid==1,TRid==1,VDid==1)
	#add biological information
	lwa<-data.frame(spp=NS_params@species_names,a=NS_params@W_a,b=NS_params@W_b,
			Linf=NS_params@Linf,k=NS_params@k)
	npg0longsim<-npg0longsim%>%left_join(lwa)
	npg0longsim$wind<-npg0longsim$a*npg0longsim$len^npg0longsim$b
	#add age
	fct1<-function(t0=0,K,Linf,L){ max(1, ((-log(1 - L/Linf))/K + t0))}
	npg0longsim<-npg0longsim%>%mutate(lentmp=ifelse(len>Linf,Linf-1e-1,len))%>%
		rowwise()%>%
		mutate(age=fct1(0,k,Linf,lentmp))%>%
		ungroup()
	#ggplot(npg0longsim,aes(x=len,y=age,group=spp,color=spp))+geom_point()
	#add to w by spp
	npg0longsim<-npg0longsim%>%mutate(wspp=value*wind)
	#clean the data
	npg0longsim<-npg0longsim%>%transmute(year,gear,nfo,ntrip,nvess,FOid,TRid,VDid,spp,len,age,
					  n=value,wind,wspp)


	#sampling scheme
	#nb vessel, nbtrip ,n FO and prop sampled
	nplanVD<-2;nplanTR<-1;nplanFO<-seq(1,max(npg0longsim$nfo),2);
	nplanSPP<-15;planprop<-0.1
	#sample space 
	sampspace<-npg0longsim%>%select(year,gear,FOid,TRid,VDid)%>%
		distinct()
	#random sampling of 2 vessels by strata
	sampVD<-sampspace%>%select(year,gear,VDid)%>%
		distinct()%>%
		group_by(year,gear)%>%
		slice_sample(n=nplanVD,replace=F)%>%
		ungroup()%>%
		mutate(VDsamp=TRUE)
	sampspace<-left_join(sampspace,sampVD)%>%mutate(VDsamp=ifelse(is.na(VDsamp),F,VDsamp))
	#weighted random sampling of 1 trip by vessels by strata (aka the vessel
	#with the more trip are highest proba of selection)
	sampTR<-sampspace%>%filter(VDsamp==T)%>%select(year,gear,VDid,TRid)%>%
		distinct()%>%
		group_by(year,gear,VDid)%>%mutate(nbtrip=n_distinct(TRid))%>%
		group_by(year,gear,VDid)%>%
		slice_sample(n=nplanTR,weight_by=nbtrip,replace=F)%>%
		ungroup()%>%
		mutate(TRsamp=TRUE)%>%
		select(-nbtrip)
	sampspace<-left_join(sampspace,sampTR)%>%mutate(TRsamp=ifelse(is.na(TRsamp),F,TRsamp))
	#take one FO on 3
	sampspace<-sampspace%>%
		mutate(FOsamp=VDsamp&TRsamp&FOid%in%nplanFO)
	#plan to sample 10% of the catch
	sampspace<-sampspace%>%
		mutate(propsamp=ifelse(FOsamp,planprop,0))
	#add the sampling plan to npglon
	npg0longsim<-left_join(npg0longsim,sampspace)

	#https://humanitarian-user-group.github.io/post/tidysampling/
	saveRDS(npg0longsim,file="../outputs/datasimu2.rds")


}

