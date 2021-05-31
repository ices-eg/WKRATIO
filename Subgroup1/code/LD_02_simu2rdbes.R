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
  # Number of strata in different tables - if no value if given for a table then it is assumed to be unstratified
  myStrata <- list(DE = 2, VS = 2)
  # Number of things sampled in different tables - if no value is given for a table then it is assumed to be 1
  mySampled <- list(VS=5,FO=3,SS=1,SA=3, FM=20,BV=2, VD=10, SL=20)
  # Total number of things in different tables - if no value is given for a table then it is assumed to be equal to the number sampled + 1
  myTotal <- list(VS=30,FO=10,SS=4, FM=20, BV=2)
  # Select methods used in different tables - if no value is given for a table then it is assumed to be simple random sampling SRSWR
  myMethods <- list()
  # Generate some random data
  myTestData <- createTestData(HierarchyToGenerate = "H1", 
			       LowerHierarchyToGenerate = "A", 
			       RDBESvalidationdata = validationData, 
			       RDBEScodeLists = allowedValues, 
			       RequiredTables = allRequiredTables, 
			       NumberOfStrata = list(DE=1,VS=1), 
			       NumberSampled = list(VS=1,FT=1,FO=1,SL=21,SA=1,FM=1),
			       NumberTotal = list(VS=1,FO=1,SS=1,FM=1,BV=1), 
			       SelectionMethods = myMethods)
  myNewTestData <- makeTestDataMoreRealistic(DataToUse = myTestData,CountryToUse="ZZ",YearToUse="1965",MetierList= NULL,SpeciesList= NULL,RDBEScodeLists=allowedValues)
  #load simudat and add it to the myTestData
  # The data we just geenerated is too random and won't pass validation or upload check - lets fix that now
  datrdb<- makeTestDataMoreRealistic(DataToUse = myTestData,CountryToUse=myCountry,YearToUse=myYear,MetierList= NULL,SpeciesList= NULL,RDBEScodeLists=allowedValues)
  #use simudat and add it to myTest
  datsim<-readRDS("../outputs/datasimu.rds")%>%mutate(FOid=paste0(nsamp,gear))
  #FO
  datsimFO<-datsim%>%select(nsamp,gear,year,FOid)%>%distinct()
  datrdb$FO<-datrdb$FO[rep(1,nrow(datsimFO)),]%>%
	  mutate(FOid=datsimFO$FOid,
		 FOendDate=datsimFO$year,
		 FOgear=datsimFO$gear)
  #SL
  datsimSL<-datsim%>%select(spp,year)%>%distinct()
  datrdb$SL<-datrdb$SL[rep(1,nrow(datsimSL)),]%>%
	  mutate(SLid=1:nrow(datsimSL),
		 SLspeciesListName=paste0("ZZ_",datsimSL$year,"_SpeciesList"),
		 SLyear=datsimSL$year,
		 SLcommercialTaxon=datsimSL$spp)
  #SS
  datsimSS<-datsim%>%filter(n>0)%>%select(nsamp,gear,spp,year,FOid)%>%distinct()%>%
	  group_by(nsamp,gear,year,FOid)%>%summarise(n=n_distinct(spp))%>%
	  ungroup()
  datrdb$SS<-datrdb$SS[rep(1,nrow(datsimSS)),]%>%
	  mutate(SSid=datsimSS$FOid,
		 SSspeciesListName=paste0("ZZ_",datsimSS$year,"_SpeciesList"),
		 SSnumberTotal=21,
		 SSnumberSampled=datsimSS$n,
		 FOid=datsimSS$FOid)

  #SA
  datsimSAFM<-datsim%>%
	  group_by(spp,FOid)%>%mutate(SAid=cur_group_id())%>%ungroup()%>%
	  group_by(spp,FOid,len)%>%mutate(FMid=cur_group_id())%>%ungroup()
  datsimSA<-datsimSAFM%>%select(spp,FOid,gear,SAid,wspptot)%>%distinct()
  datrdb$SA<-datrdb$SA[rep(1,nrow(datsimSA)),]%>%
	  mutate(SAid=datsimSA$SAid,
		 SAspeciesCode=datsimSA$spp,
		 SAgear=datsimSA$gear,
		 SAtotalWeightMeasured=datsimSA$wspptot,
		 SAsampleWeightMeasured=datsimSA$wspptot,
		 SSid=datsimSA$FOid)
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







			   


