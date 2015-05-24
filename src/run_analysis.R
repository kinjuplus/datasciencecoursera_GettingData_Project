run_analysis<-function(){
  
  ##tidy test data set
  testLabelPath<-"./test/y_test.txt"
  testSetPath<-"./test/X_test.txt"
  tidyTest<-tidyData(testLabelPath,testSetPath) 
  
  ##tidy training data set
  trainLabelPath<-"./train/y_train.txt"
  trainSetPath<-"./test/X_train.txt"
  tidyTrain<-tidyData(testLabelPath,testSetPath) 
  
  ##combine test data set and training data set
  finalTidy<-rbind(tidyTest,tidyTrain)
  
  ##summarize calculation on each activity group
  final<-finalTidy%>%group_by(activity) %>% summarize(mean=mean(measurement),sd=sd(measurement))
  ##write final txt file
  write.table(final,file="result.txt",row.names = FALSE)
  final
  
}

##tidy data function which:
## 1.read category and data set value by given path argument
## 2.give a descriptive, informative variable name on each activity category
## 3.combine catgegory and data set
## 4.melt combined data set to tidy data and return it
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

##change category vaule baed on its input
## 1==>WALKING
## 2==>WALKING_UPSTAIRS
## 3==>WALKING_DOWNSTAIRS
## 4 ==>SITTING
## 5 ==>STANDING
## 6 ==>LAYING
## other==>unKnown
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