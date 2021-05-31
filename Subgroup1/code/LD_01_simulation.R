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

if(F){
#add vessel and trip and fo artificially
tr0<-npg0long%>%select(year,gear)%>%distinct()
#n trip by gear
ntrip<-5
tr0<-tr0[rep(1:nrow(tr0),each=ntrip),]%>%mutate(idtrip=rep(1:nrow(tr0),each=ntrip))%>%
	group_by(year,gear,idtrip)%>%mutate(prop=runif(ntrip))%>%mutate(prop=prop/sum(prop))%>%ungroup()
pipo<-left_join(tr0,npg0long)%>%mutate(value2=value*prop)
pipo1<-pipo1%>%group_by(

sum(pipo$value2)
sum(npg0long$value)
table(npg0long$gear,npg0long$year)
}


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



#a simple random samples of 





#all.equal(apply((pipo[,10,1,1:4]),1,sum),model_run@N[,10,2])
#all.equal(apply((pipo[,10,100,1:4]),1,sum),model_run@N[,10,101])
#translate npg0 in yearly data
