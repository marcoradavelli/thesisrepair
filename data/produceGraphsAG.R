mydata <- read.table("results.txt", header=TRUE, sep="",fill=TRUE)
# , row.names="model","mutOperator"
# remove row with FID < 0
mydata = mydata[mydata$FID > 0,]
#remove NA
mydata <- na.omit(mydata)
# order
mydata$model = factor(mydata$model,c("WashingMachine","Concurrency","Aircraft","LibSSH","Telecom"))
mydata$testPolicy = factor(mydata$testPolicy,c("UC","CC","CuCV","ValC"))


attach(mydata)

# effort
boxplot((rtime/1000)~model,log="y") #main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon") 
boxplot(nInitTests~model,log="y") #main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon") 

# 
n_policies = 4


output = TRUE

plot_graph <- function(y, logy = FALSE, fileName = ""){
  #if (output) png(height=50, width=90, units = "mm", file='oracleEvaluation.png', res= 600)
  if (output) pdf(width=9, height=4,file=fileName)
  par(mar=c(4, 3, 0, 0))

  if (logy)
    myplot=boxplot(y~testPolicy*model,xaxt="n",log="y",na.rm=TRUE) #main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon") 
  else
    myplot=boxplot(y~testPolicy*model,xaxt="n",na.rm=TRUE) #main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon") 
  #spec names
  my_names=sapply(strsplit(myplot$names , '\\.') , function(x) x[[2]] )
  my_names=my_names[seq(1 , length(my_names) , n_policies)]
  #tp_names
  tp_names=sapply(strsplit(myplot$names , '\\.') , function(x) x[[1]] )

  axis(1, mgp=c(3, 2.5, 0), at = seq(2.5, 5 * n_policies , n_policies), labels = my_names , tick=FALSE , cex=0.3 , par(las=1))
  axis(1, mgp=c(3, .3, 0), at = seq(1 , 5 * n_policies , 1), labels = tp_names , tick=TRUE , cex=0.2, par(las=2))
  for(i in seq(0.5 , 30 , n_policies)){ abline(v=i,lty=1, col="grey")}
  if (output) dev.off()
}

plot_graph(rtime/1000,logy = TRUE,fileName =  "time.pdf")

#plot_graph(sqrt(oracleEvaluation2),fileName =  "oraclecalls.pdf")

#plot_graph(oracleEvaluation2,fileName =  "oraclecalls.pdf")

plot_graph(oracleEvaluation,fileName =  "oraclecalls.pdf")

plot_graph(nInitTests,logy = TRUE,fileName =  "initTests.pdf")

plot_graph(nBENTests,fileName =  "BENTests.pdf")


plot_graph(FID,fileName =  "FID.pdf")


library(plyr)

TRMA <-count(mutationTRepaired, c('model', 'testPolicy'))
# those true
mydataTRMT = mydata[mutationTRepaired =="true",]
attach(mydataTRMT)
TRMT <-count(mutationTRepaired, c('model', 'testPolicy'))
# merge tables
total <- merge(TRMA,TRMT,by=c("model", 'testPolicy'))
total$ratio <- total$freq.y/total$freq.x
attach(total)
# order
total$model = factor(total$model,c("WashingMachine","Concurrency","Aircraft","LibSSH","Telecom"))
total$testPolicy = factor(total$testPolicy,c("UC","CC","CuCV","ValC"))

plot_graph(ratio,fileName =  "TRM.pdf")


#boxplot(FID~testPolicy*model) #main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon") 

