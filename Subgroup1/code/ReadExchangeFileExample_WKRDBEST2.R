## Example of how to import exchange files - assumes exchange files are saved in
## the folder ./output/uploaded

# Load our functions
source("RDBES_Functions.R")

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

## STEP 1) LOAD OUR DATA

# Load the validation data
#validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')
validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')

# Load the reference data: either refresh from ICES or just use a local copy
#allowedValues <- loadReferenceData(downloadFromICES = FALSE)
allowedValues <- loadReferenceData(downloadFromICES = FALSE, validationData=validationData)

# Load the lists of tables required for each hierarchy: either refresh from ICES or just use a local copy
#allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')

myExchangeFileVD <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = '../inputs/DK_1966_HVD.csv' )
myExchangeFileSL <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'output/uploaded/DK_1966_HSL.csv' )
myExchangeFileH1 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = '../inputs/DK_1966_H1.csv',RequiredTables = allRequiredTables )
