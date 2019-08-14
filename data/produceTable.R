mydata <- read.table("results.txt", header=TRUE, sep="",fill=TRUE)
# , row.names="model","mutOperator"
# remove row with FID < 0
#mydata = mydata[mydata$FID > 0,]
#remove NA
mydata <- na.omit(mydata) 
# order
mydata$model = factor(mydata$model,c("WashingMachine","Concurrency","Aircraft","LibSSH","Telecom"))
mydata$testPolicy = factor(mydata$testPolicy,c("UC","CC","CV", "CuCV","ValC"))
attach(mydata)

mydata$repaired <- ifelse(mydata$mutationTRepaired=="true",1,0)
mydata$sec <- rtime/1000

means <- aggregate(mydata, by=list(testPolicy,model),FUN=mean, na.rm=TRUE)

write.table(means, "meansTable.txt", sep="\t") 

mean(mydata$FID, na.rm=TRUE)

mean(means$FID)

aggregate(mydata, by=list(testPolicy),FUN=mean, na.rm=TRUE)

means

