library(dplyr)
library(ggplot2)
#dave fct
source("RDBES_Functions.R")


# This file shows how to generate test data for the RDBES

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

## STEP 1) LOAD OUR DATA

# Load the validation data
validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')
# Load the reference data
allowedValues <- loadReferenceData(downloadFromICES = FALSE)

# Load the lists of tables required for each hierarchy: either refresh from ICES
# or just use a local copy
allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
#allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')


## STEP 2: generate a hierarchy

  # Define some parameters for our test data
  myHierarchyToGenerate <- 'H1'
  print(myHierarchyToGenerate)
  myLowerHierarchyToGenerate <- 'A'
  myYear <- 1965 
  myCountry <- 'ZZ'
  # Generate some random data
  myTestData <- createTestData(HierarchyToGenerate = "H1", 
			       LowerHierarchyToGenerate = "A", 
			       RDBESvalidationdata = validationData, 
			       RDBEScodeLists = allowedValues, 
			       RequiredTables = allRequiredTables, 
			       NumberOfStrata = list(DE=1,VS=1), 
			       NumberSampled = list(VS=1,FT=1,FO=1,SL=21,SA=1,FM=1),
			       NumberTotal = list(VS=1,FO=1,SS=1,FM=1,BV=1), 
			       SelectionMethods = list())
  myNewTestData <- makeTestDataMoreRealistic(DataToUse = myTestData,CountryToUse="ZZ",YearToUse="1965",MetierList= NULL,SpeciesList= NULL,RDBEScodeLists=allowedValues)
  #load simudat and add it to the myTestData

  # The data we just geenerated is too random and won't pass validation or upload check - lets fix that now
  datrdb<- makeTestDataMoreRealistic(DataToUse = myTestData,CountryToUse=myCountry,YearToUse=myYear,MetierList= NULL,SpeciesList= NULL,RDBEScodeLists=allowedValues)
  #use simudat to populate datrdb for all the year ?
  datsim<-readRDS("../outputs/datasimu2.rds")
  #DE
  tmp<-datsim%>%transmute(year)%>%distinct()
  datrdb$DE<-datrdb$DE[rep(1,nrow(tmp)),]%>%
	  mutate(DEid=1:nrow(tmp),
		 DEsamplingScheme="SIM stratified sampling",
		 DEsamplingSchemeType="National Routine",
		 DEyear=tmp$year,
		 DEstratumName="Fleet")
  #SD
  datrdb$SD<-datrdb$SD[rep(1,nrow(datrdb$DE)),]%>%
	  mutate(SDid=1:nrow(datrdb$DE),
		 SDinstitution="in silico",
		 DEid=datrdb$DE$DEid)
  #VS
  tmp<-datsim%>%group_by(year,gear)%>%
	  mutate(nbvess=n_distinct(VDid))%>%
	  ungroup()%>%
	  select(year,gear,nbvess,VDid)%>%
	  distinct()%>%
	  group_by(year,gear)%>%
	  mutate(VDidseq=row_number())%>%
	  ungroup()%>%
	  mutate(namevess=paste0(gear,VDid))%>%
	  mutate(namestrat=paste0("year",year,gear))
  #prepare info VD: based add the VSid in tmp
  tmp2<-tmp%>%select(year,namevess)%>%distinct()%>%
	  mutate(newVDid=row_number())
  tmp<-left_join(tmp,tmp2)
  #add info to the table
  datrdb$VS<-datrdb$VS[rep(1,nrow(tmp)),]%>%
	  mutate(VSid=1:nrow(tmp),
		 VSsequenceNumber=tmp$VDidseq,
		 VSencryptedVesselCode=tmp$namevess,
		 VSstratification="Y",
		 VSstratumName=tmp$namestrat,
		 VSsampler="Observer",
		 VSnumberTotal=tmp$nbvess,
		 VSnumberSampled=tmp$nbvess,
		 VSselectionProb=1,
		 VSinclusionProb=NA,
		 VSselectionMethod="Census",
		 VSunitName=tmp$VDid,
		 VDid=tmp$newVDid
		 )
  #VD
  tmp<-tmp%>%select(newVDid,year,namevess)%>%distinct() 
  datrdb$VD<-datrdb$VD[rep(1,nrow(tmp)),]%>%
	  mutate(VDid=tmp$newVDid,
		 VDencryptedVesselCode=tmp$namevess,
		 VDyear=tmp$year,
		 VDlengthCategory=NA)
  #FT
  tmp<-datsim%>%mutate(namevess=paste0(gear,VDid),
		       namestrat=paste0("year",year,gear))%>%
	 select(year,gear,namestrat,namevess,TRid,FOid)%>%distinct()%>%
	 group_by(year,gear,namestrat,namevess,TRid)%>%summarise(nfo=n())%>%
	 group_by(year,gear,namestrat,namevess)%>%mutate(ntr=n_distinct(TRid))%>%
	 ungroup()%>%
	 group_by(year,gear,namestrat,namevess)%>%
	 mutate(TRidseq=row_number())%>%
	 ungroup()
  tmp2<-datrdb$VD%>%transmute(year=VDyear,namevess=VDencryptedVesselCode,VDid)%>%distinct()
  tmp<-left_join(tmp,tmp2)
  tmp2<-datrdb$VS%>%transmute(namevess=VSencryptedVesselCode,
			      namestrat=VSstratumName,
			      VSid)%>%distinct()
  tmp<-left_join(tmp,tmp2)%>%mutate(namestrattr=paste0(namevess,"_",namestrat),
				    nametrip=paste0(namevess,"trip",TRid))
  datrdb$FT<-datrdb$FT[rep(1,nrow(tmp)),]%>%
	  mutate(FTid=1:nrow(tmp),
		 FTencryptedVesselCode=tmp$namevess,
		 FTsequencenumber=tmp$TRidseq,
		 FTstratification="Y",
		 FTstratumName=tmp$namestrattr,
		 FTsample="Observer",
		 FTnumberOfHauls=tmp$nfo,
		 FTdepartureDate=NA,
		 FTarrivalDate=tmp$year,
		 FTnumbertotal=tmp$ntr,
		 FTnumberSampled=tmp$ntr,
		 FTselectionMethod="Census",
		 FTunitName=tmp$nametrip,
		 VSid=tmp$VSid,
		 VDid=tmp$VDid)
  #FO
  tmp<-datsim%>%mutate(namevess=paste0(gear,VDid),
		       namestrat=paste0("year",year,gear),
		       namestrattr=paste0(namevess,"_",namestrat),
		       nametrip=paste0(namevess,"trip",TRid))%>%
	 select(year,gear,namevess,namestrattr,nametrip,FOid)%>%distinct()%>%
	 group_by(year,gear,namevess,namestrattr,nametrip)%>%
	 mutate(nfo=n_distinct(FOid))%>%
	 ungroup()%>%
	 group_by(year,gear,namevess,namestrattr,nametrip)%>%
	 mutate(FOidseq=row_number())%>%
	 ungroup()
  tmp2<-datrdb$FT%>%transmute(namevess=FTencryptedVesselCode,
			      namestrattr=FTstratumName,
			      nametrip=FTunitName,
			      FTid)%>%distinct()
  tmp<-left_join(tmp,tmp2)%>%mutate(namestratfo=paste0(namestrattr,"_",nametrip),
				    namefo=paste0(nametrip,"fo",FOid))
  datrdb$FO<-datrdb$FO[rep(1,nrow(tmp)),]%>%
	  mutate(FOid=1:nrow(tmp),
		 FOsequenceNumber=tmp$FOidseq,
		 FOsampler="Observer",
		 FOvalidity="V",
		 FOendDate=tmp$year,
		 FOarea=NA,
		 FOgsaSubarea=NA,
		 FOmetier6=tmp$gear,
		 FOgear=tmp$gear,
		 FOobservationCode=NA,
		 FOnumberTotal=tmp$nfo,
		 FOnumberSampled=tmp$nfo,
		 FOselectionMethod="SS",
		 FOunitName=tmp$namefo,
		 FTid=tmp$FTid)
  #SL
  tmp<-datsim%>%select(year,spp)%>%distinct()%>%
	  group_by(year)%>%
	  mutate(namespplist=paste0("ZZ_",year,"_SpeciesList"))%>%
	  ungroup()
  datrdb$SL<-datrdb$SL[rep(1,nrow(tmp)),]%>%
	  mutate(SLid=1:nrow(tmp),
		 SLinstitute="in silico",
		 SLspeciesListName=tmp$namespplist,
		 SLyear=tmp$year,
		 SLcatchFraction="all",
		 SLcommercialTaxon=tmp$spp,
		 SLspeciesCode=NA)

  #############again

  #prep pk and parameters inside the datsim object
  tmp<-datsim%>%
	  #FM
	  group_by(year,gear,FOid,TRid,VDid,spp,len)%>%
	  mutate(newFMid=cur_group_id())%>%ungroup()%>%
  	  #SA
	  group_by(year,gear,FOid,TRid,VDid,spp)%>%
	  mutate(newSAid=cur_group_id(),
		 SAwtot=sum(wspp),SAwsamp=sum(wspp),
		 SAntot=n_distinct(len),SAnsamp=n_distinct(len),
		 SAsel="Census",SAname=paste(year,gear,VDid,TRid,FOid,spp,sep="_"))%>%#ungroup()
	  #SS
	  group_by(year,gear,FOid,TRid,VDid)%>%
	  mutate(newSSid=cur_group_id(),
		 SSntot=n_distinct(spp),SSnsamp=n_distinct(spp),
		 SSsel="Census",SSname=paste(year,gear,VDid,TRid,FOid,spp,sep="_"))%>%#ungroup()
	  #FO
	  group_by(year,gear,FOid,TRid,VDid)%>%
	  mutate(newFOid=cur_group_id())%>%
	  group_by(year,gear,TRid,VDid)%>%
	  mutate(FOntot=n_distinct(FOid),FOnsamp=n_distinct(FOid))%>%ungroup()%>%
	  mutate(FOsel="Census",FOname=paste(year,gear,VDid,TRid,FOid,sep="_"),
		 FOstratname=paste(year,gear,VDid,TRid,sep="_"))%>%#ungroup()
	  #FT
	  group_by(year,gear,TRid,VDid)%>%
	  mutate(newFTid=cur_group_id())%>%
	  group_by(year,gear,VDid)%>%
	  mutate(FTntot=n_distinct(TRid),FTnsamp=n_distinct(TRid))%>%ungroup()%>%
	  mutate(FTsel="Census",FTname=paste(year,gear,VDid,TRid,sep="_"),
		 FTstratname=paste(year,gear,VDid,sep="_"))%>%#ungroup()
	  #VS
	  group_by(year,gear,VDid)%>%
	  mutate(newVSid=cur_group_id())%>%
	  group_by(year,gear)%>%
	  mutate(VSntot=n_distinct(VDid),VSnsamp=n_distinct(VDid))%>%ungroup()%>%
	  mutate(VSsel="Census",VSname=paste(year,gear,VDid,sep="_"),
		 VSstratname=paste(year,gear,sep="_"))%>%#ungroup()
	  #SD & DE
	  group_by(year)%>%
	  mutate(newSDid=cur_group_id(),newDEid=cur_group_id())%>%
	  ungroup()
  	
  
	  tmp%>%select(year,gear,FOid,TRid,VDid,spp,#len,
		       #newFMid,
		       #newSAid,SAntot,SAnsamp,SAname,
		       newSSid,SSntot,SSnsamp,SSname,
		       newFOid,FOntot,FOnsamp,FOname,FOstratname,
		       newFTid,FTntot,FTnsamp,FTname,FTstratname,
		       newVSid,VSntot,VSnsamp,VSname,VSstratname
		       )%>%distinct()%>%
		  head(1000)%>%View
  
  #FM
  pipo<-tmp%>%transmute(newFMid,len,n,newSAid)%>%distinct()
  datrdb$FM[rep(1,nrow(pipo),]








  #FM
  datsimFMl<-datsimSAFM%>%select(SAid,FMid,len,n)%>%distinct()%>%mutate(type="l")
  datsimFMw<-datsimSAFM%>%select(SAid,FMid,n,wind)%>%distinct()%>%mutate(type="w")
  FMl<-datrdb$FM[rep(1,nrow(datsimFMl)),]%>%
	  mutate(FMid=datsimFMl$FMid,
		 FMclass=datsimFMl$len,
		 FMnumberAtUnit=datsimFMl$n,
		 FMtype=datsimFMl$type,
		 SAid=datsimFMl$SAid)
  FMw<-datrdb$FM[rep(1,nrow(datsimFMw)),]%>%
	  mutate(FMid=datsimFMw$FMid,
		 FMclass=datsimFMw$wind,
		 FMnumberAtUnit=datsimFMw$n,
		 FMtype=datsimFMw$type,
		 SAid=datsimFMw$SAid)
  datrdb$FM<-rbind(FMl,FMw)

  #CL data
  cl<-readRDS("../outputs/datapop.rds")%>%filter(year==1)%>%filter(value>0)
  clrdb<-cl%>%transmute(Clid=1:nrow(cl),
	  CLrecType="CL", CLdTypSciWeig="Official",
	  CLdSouSciWeig="Combination of official data",
	  CLsampScheme="census", CLdSouLanVal="Other",
	  CLlanCou="ZZ", CLvesFlagCou="ZZ",
	  CLyear=year, CLquar=NA, CLmonth=NA,
	  CLarea=NA, CLstatRect=NA,CLgsaSubarea=NA,
	  CLjurisdArea=NA, CLeconZoneIndi=NA,
	  CLspecCode=NA, CLspecFAO=cl$spp,
	  CLlandCat="HuC", CLcatchCat="Lan",
	  CLsizeCatScale=NA, CLsizeCat=NA,
	  CLnatFishAct=NA, CLmetier6=cl$gear,
	  CLIBmitiDev=NA, CLloc=NA,
	  CLvesLenCat=NA, CLfishTech=NA,
	  CLdeepSeaReg=NA, CLoffWeight=cl$value,
	  CLsciWeight=NA, CLexpDiff=NA,
	  CLtotOffLanVal=NA, CLnumUniqVes=NA,
	  CLsciLanRSE=NA, CLvalRSE=NA,
	  CLsciLanQualBias=NA)

  #save
  saveRDS(datrdb,file="../outputs/datrdb.rds")
  saveRDS(clrdb,file="../outputs/clrdb.rds")







			   


