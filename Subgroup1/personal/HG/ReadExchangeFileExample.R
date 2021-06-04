## Example of how to import exchange files - assumes exchange files are saved in
## the folder ./output/uploaded


setwd('H:\\My Documents\\Work\\Meetings\\WKRATIO21\\R\\WKRATIO\\Subgroup1\\code\\')

# Load our functions
source("RDBES_Functions.R")

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

## STEP 1) LOAD OUR DATA

# Load the validation data
validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')

# 11/9/2020 Temp fix because the validation fields aren't up to date :-(
validationData[validationData$type == 'tRS_Stratification','type'] <- 'tYesNoFields'

# Load the reference data: either refresh from ICES or just use a local copy
allowedValues <- loadReferenceData(downloadFromICES = FALSE)
#allowedValues <- loadReferenceData(downloadFromICES = TRUE, validationData=validationData)

# Load the lists of tables required for each hierarchy: either refresh from ICES or just use a local copy
allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
#allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

#myExchangeFileVD <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = '../inputs/DK_1966_HVD.csv' )
#myExchangeFileSL <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = '../inputs/DK_1966_HSL.csv' )
#myExchangeFileH1 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = '../inputs/DK_1966_H1.csv',RequiredTables = allRequiredTables )

H1 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = '\\\\galwayfs03\\fishdata\\Data Collection Regulation\\DATA CALL REGISTER\\2020 Data Call Files\\RDBES\\submitted\\20200923\\IE_2019_H1.csv',RequiredTables = allRequiredTables)
CL <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = '\\\\galwayfs03\\fishdata\\Data Collection Regulation\\DATA CALL REGISTER\\2020 Data Call Files\\RDBES\\submitted\\20200923\\IE_2019_HCL.csv')
CE <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = '\\\\galwayfs03\\fishdata\\Data Collection Regulation\\DATA CALL REGISTER\\2020 Data Call Files\\RDBES\\submitted\\20200923\\IE_2019_HCE.csv')

save(H1,CL,CE,file='H:\\My Documents\\Work\\Meetings\\WKRATIO21\\R\\WKRATIO\\Subgroup1\\personal\\HG\\IE_2019.RData')

