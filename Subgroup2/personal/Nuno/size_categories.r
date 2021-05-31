testData <- data.frame(Strata = paste("size ",1:5), numberTotalBoxes = c(1:3,4,4), numberSampledBoxes = 1)

# kg/fish
testData$totalWeightBoxes <- testData$numberTotalBoxes*10 

testData$sampledWeightBoxes <- c(10, 10, 10, 10, 10)

testData$fishSampled<-testData$sampledWeight*1/c(7, 5.5, 3, 1.5, 1)