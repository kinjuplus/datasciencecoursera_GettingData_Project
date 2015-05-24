run_analysis<-function(){
  

  testLabelPath<-"./test/y_test.txt"
  testSetPath<-"./test/X_test.txt"
  tidyTest<-tidyData(testLabelPath,testSetPath) 
  
  trainLabelPath<-"./train/y_train.txt"
  trainSetPath<-"./test/X_train.txt"
  tidyTrain<-tidyData(testLabelPath,testSetPath) 
  finalTidy<-rbind(tidyTest,tidyTrain)
  
  final<-finalTidy%>%group_by(activity) %>% summarize(mean=mean(measurement),sd=sd(measurement))
  write.table(final,file="result.txt",row.names = FALSE)
  final
  
}

tidyData<-function(labelPath,setPath){
  
  initialValue<-read.table(setPath)
  initialCate<-read.table(labelPath,col.names=c("activity"))
  
  newLabel<-lapply(initialCate$activity,changeLabel)
  initialCate$activity<-newLabel
  initialCate$activity<-as.character(initialCate$activity)
  
  firstCombined<-cbind(initialCate,initialValue)
  library(tidyr)
  
  melt<-firstCombined %>% gather(key,measurement,V1:V561) %>% 
    select(activity,measurement)
  melt
}

changeLabel <-function(type){
  if(type==1)
    "WALKING"
  else if(type==2)
    "WALKING_UPSTAIRS"
  else if(type==3)
    "WALKING_DOWNSTAIRS"
  else if(type==4)
    "SITTING"
  else if(type==5)
    "STANDING"
  else if(type==6)
    "LAYING"
  else
    "unKnown"
}