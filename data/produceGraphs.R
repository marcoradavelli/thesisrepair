# Obs: It works only if the script is "Sourced" before the run.
# The suggestion is to use Rstudio as IDE and check the flag "Source on Save"
# ( see: http://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location )

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

mydata <- read.table("results.txt", header=TRUE, sep="",fill=TRUE)
# , row.names="model","mutOperator"
# remove row with FID < 0 - assuming something went wrong with BEN (execption)
#mydata = mydata[mydata$FID >= 0,]
#remove NA
mydata <- na.omit(mydata) 
# order
mydata$model = factor(mydata$model,c("WashingMachine","Concurrency","Aircraft","LibSSH","Telecom"))
mydata$testPolicy = factor(mydata$testPolicy,c("UC","CC","CV","CuCV","ValC"))

withGGPLOT <- function() {
  library(ggplot2)
  library(gridExtra)
  library(gtable)
  library(grid)
  library(scales)
  
  OUTPUT = TRUE
 # p1 <- ggplot(mydata, aes(model,time/1000,color=model)) + geom_boxplot()
#  print(p1)
  optLegend <- theme(legend.direction = "horizontal", legend.justification = c(0, 1), legend.position = c(0.03,0.99), axis.title.x=element_blank(), axis.title.y=element_blank())
  noLegend <- theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank())
  
  # oracleEvaluation
  p1 <- ggplot(mydata, aes(model,oracleEvaluation,fill=testPolicy)) + geom_boxplot() + scale_fill_grey(start = 0.5, end = 1) + theme_bw() + optLegend
  print(p1)
  if (OUTPUT) ggsave(p1, file="oraclecalls.pdf", width=5, height=2.3)
 
  # initTests
  p2 <- ggplot(mydata, aes(model,nInitTests,fill=testPolicy)) + geom_boxplot() + scale_fill_grey(start = 0.5, end = 1 ) + theme_bw() + 
        scale_y_continuous(trans='log10', breaks=c(2,5,10,20,50,100,200,500,1000,2000,5000)) + optLegend
  print(p2)
  if (OUTPUT) ggsave(p2, file="initTests.pdf", width=5, height=2.3)
  
  # time
  p3 <- ggplot(mydata, aes(model,rtime/1000,fill=testPolicy)) + geom_boxplot() + scale_fill_grey(start = 0.5, end =1  ) + theme_bw() + scale_y_continuous(trans='log10', breaks=c(0.1,0.2,0.5,1,2,5,10,20,50,100,200,1000), labels = scales::comma) + ylab("repairing time") + optLegend
  print(p3)
  if (OUTPUT) ggsave(p3, file="time.pdf", width=5, height=2.3)
  
  # FID
  p4 <- ggplot(mydata, aes(model,FID,fill=testPolicy)) + geom_boxplot() + scale_fill_grey(start = 0.5, end = 1) + theme_bw() + ylim(0,1) + noLegend
  print(p4)
  #if (OUTPUT) ggsave(p4, file="FID.pdf", width=5, height=2.3)
  
  # BENTests
  p5 <- ggplot(mydata, aes(model,nBENTests,fill=testPolicy)) + geom_boxplot() + scale_fill_grey(start = 0.5, end =1 ) + theme_bw() + optLegend
  print(p5)
  if (OUTPUT) ggsave(p5, file="BENTests.pdf", width=5, height=2.3)
  
  options(gsubfn.engine = "R")  # http://stackoverflow.com/questions/17128260/r-stuck-in-loading-sqldf-package
  library(sqldf)
  
  mydata$m2 <- 0
  
  sqldf() # start a sequence of SQL statements
  fn$sqldf("update mydata set m2=1 where mutationTRepaired = 'true' ")
  mydata <- sqldf("select * from main.mydata")
  sqldf() # SQL statements finished
  trm <- sqldf("select model,testPolicy,sum(m2)/count(*) as ratio from mydata group by model,testPolicy")
  #print(trm)
  trm$testPolicy = factor(trm$testPolicy,c("UC","CC","CV","CuCV","ValC"))
  trm$model = factor(trm$model,c("WashingMachine","Concurrency","Aircraft","LibSSH","Telecom"))
  p6 <- ggplot(trm, aes(model,fill=testPolicy,y=ratio)) + geom_bar(stat="identity", position="dodge",colour="black") + 
    scale_fill_grey(start = 0.5, end =1 ) + theme_bw() + optLegend + ylim(0,1)
  print(p6)
  if (OUTPUT) ggsave(p6, file="TRM.pdf",width=5, height=2.3)

}

withGGPLOT()

normalPlots <- function() {
  # effort
  boxplot((time/1000)~model,log="y") #main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon") 
  boxplot(nInitTests~model,log="y") #main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon") 
  
  # 
  n_policies = 5
  
  output = FALSE
  
  #if (output) png(height=50, width=90, units = "mm", file='oracleEvaluation.png', res= 600)
  if (output) pdf(width=9, height=5,file='oracleEvaluation.pdf')
  par(mar=c(3, 2, 0, 0))
  
  myplot=boxplot(oracleEvaluation~testPolicy*model,xaxt="n") #main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon") 
  #spec names
  my_names=sapply(strsplit(myplot$names , '\\.') , function(x) x[[2]] )
  my_names=my_names[seq(1 , length(my_names) , n_policies)]
  #tp_names
  tp_names=sapply(strsplit(myplot$names , '\\.') , function(x) x[[1]] )
  
  axis(1, at = seq(1.5 , 10 , n_policies), labels = my_names , tick=FALSE , cex=0.3)
  axis(1, mgp=c(3, .3, 0), at = seq(1 , 6 , 1), labels = tp_names , tick=TRUE , cex=0.7)
  for(i in seq(0.5 , 20 , n_policies)){ abline(v=i,lty=1, col="grey")}
  if (output) dev.off()
  
  boxplot(oracleEvaluation~model,data=mydata) #main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon") 
  
  
  
  
  boxplot(FID~time,data=mydata) #main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon")
}

#normalPlots()
