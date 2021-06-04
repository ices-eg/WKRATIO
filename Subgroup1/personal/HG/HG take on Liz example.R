# copy liz' example from introductory presentation

# y correlates with x, which is known for the pop
# e.g. discards correlate with effort

effort <- runif(1000,0,100)
discards <- effort+rnorm(length(effort),0,5)
pop <- data.frame(effort,discards); rm(effort,discards)
plot(pop)

out <- NULL
for(i in 1:500){
  s <- sample(1:nrow(pop),20,FALSE)
  sam <- pop[s,]
  points(sam,col=2,pch=16)
  
  Rest <- sum(pop$effort)*mean(sam$discards)/mean(sam$effort) # lohr p121
  HTest <- sum(sam$discards/(nrow(sam)/nrow(pop)))            # Horwitz Thompson
  MeanDis <- mean(sam$discards)
  out <- rbind(out,data.frame(Rest,HTest,MeanDis))
}

boxplot(out[,1:2])

plot(out$MeanDis,out$HTest)
points(out$MeanDis,out$Rest,col=2)
