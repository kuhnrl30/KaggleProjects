
rm(list=ls())
library(e1071)

Train.raw <- read.csv('analytics/RProjects/africa_soil/training.csv', header = TRUE)
Test.raw <- read.csv('analytics/RProjects/africa_soil/sorted_test.csv',header = TRUE)
Train.raw$Depth <-  with ( Train.raw, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) )
Test.raw$Depth <-  with ( Test.raw, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) )

Train.Spec         <- Train.raw[,c(2:2655,2671:3579)]
Train.CO2          <- Train.raw[,c(2656:2670)]
Train.Other        <- Train.raw[,3580:3595]
Train.Properties   <- Train.raw[,3596:3600]

Test.Spec          <- Test.raw[,c(2:2655,2671:3579)]
Test.CO2           <- Test.raw[,c(2656:2670)]
Test.Other         <- Test.raw[,3580:3595]

#scores.= 0.677 in eval below
SVM.P   <-svm(y=Train.Properties$P,   x=Train.raw[,2:3595],cross=12,type="eps-regression",scale=FALSE)
SVM.Ca  <-svm(y=Train.Properties$Ca,  x=Train.raw[,2:3595],cross=12,type="eps-regression",scale=FALSE)
SVM.pH  <-svm(y=Train.Properties$pH,  x=Train.raw[,2:3595],cross=12,type="eps-regression",scale=FALSE)
SVM.SOC <-svm(y=Train.Properties$SOC, x=Train.raw[,2:3595],cross=12,type="eps-regression",scale=FALSE)
SVM.Sand<-svm(y=Train.Properties$Sand,x=Train.raw[,2:3595],cross=12,type="eps-regression",scale=FALSE)

###########
#Evaluation
Avg.Score<-data.frame()  
for (i in 1:500){
  Eval.index<-sample(1:1157,115)

  Eval.P   <-predict(SVM.P,   newdata=Train.raw[Eval.index,2:3595])
  Eval.Ca  <-predict(SVM.Ca,  newdata=Train.raw[Eval.index,2:3595])
  Eval.pH  <-predict(SVM.pH,  newdata=Train.raw[Eval.index,2:3595])
  Eval.SOC <-predict(SVM.SOC, newdata=Train.raw[Eval.index,2:3595]) 
  Eval.Sand<-predict(SVM.Sand,newdata=Train.raw[Eval.index,2:3595])
  Train.Pred<- data.frame(Ca=Eval.Ca,
                          P=Eval.P,
                          pH=Eval.pH,
                          SOC=Eval.SOC,
                          Sand=Eval.Sand)
  Train.Eval<- Train.Properties[Eval.index,]

  Scores<-sqrt(colMeans((Train.Eval-Train.Pred)^2))
  Summary.Scores<- c(Scores,mean(Scores))
  Avg.Score<-rbind(Avg.Score,Summary.Scores)
}
names(Avg.Score)<-c("Ca","P","pH","SOC","Sand","MCRMSE")
Eval.SVM<-round(colMeans(Avg.Score),4)
Eval.SVM
write.csv(Eval.SVM,"analytics/kaggle_challenges/africa_soil/eval-svm.csv",row.names=F)

#########
##Make predictions
SVM.Out.P    <- predict(SVM.P,   newdata=Test.raw[,2:3595])
SVM.Out.Ca   <- predict(SVM.Ca,  newdata=Test.raw[,2:3595])
SVM.Out.pH   <- predict(SVM.pH,  newdata=Test.raw[,2:3595])
SVM.Out.SOC  <- predict(SVM.SOC, newdata=Test.raw[,2:3595])
SVM.Out.Sand <- predict(SVM.Sand,newdata=Test.raw[,2:3595])

#########
##Create submission set

Submit<-data.frame(PIDN=Test.raw[,1],
                   Ca=SVM.Out.Ca,
                   P=SVM.Out.P,
                   pH=SVM.Out.pH,
                   SOC=SVM.Out.SOC,
                   Sand=SVM.Out.Sand)

write.csv(Submit,"analytics/kaggle_challenges/Africa_Soil/SVM.csv",row.names=F)
