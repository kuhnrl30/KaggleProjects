library(caret)
library(stats)

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

PCA<- prcomp(Train.Spec,scale=T)
plot(PCA$x,type="p")
screeplot(PCA,type="lines",col=3)

myData<- data.frame(Ca=Train.Properties[,"Ca"],
                      P=Train.Properties[,"P"],
                      pH=Train.Properties[,"pH"],
                      SOC=Train.Properties[,"SOC"],
                      Sand=Train.Properties[,"Sand"],
                      PCA$x[,1:2])

PrinComp<-PCA$x[,1]

indx <- createFolds(Train.Properties[,1], returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)


######
##Scores 1.029 in Evaluation below 
glmTuneCa  <-train(Ca~PC1+PC2, data=myData, method="glm", trControl=ctrl)
glmTuneP   <-train(P~PC1+PC2,  data=myData, method="glm", trControl=ctrl)
glmTunepH  <-train(Ca~PC1+PC2, data=myData, method="glm", trControl=ctrl)
glmTuneSOC <-train(SOC~PC1+PC2,data=myData, method="glm", trControl=ctrl)
glmTuneSand<-train(Ca~PC1+PC2, data=myData, method="glm", trControl=ctrl)



##########
##Evaluation
Avg.Score<-data.frame()  
for (i in 1:500){
  Eval.index<-sample(1:1157,115)

  Eval.PCA <-predict(PCA,newdata=Train.Spec[Eval.index,])
  Eval.P   <-predict(glmTuneP,   newdata=data.frame(Eval.PCA),type="raw")
  Eval.Ca  <-predict(glmTuneCa,  newdata=data.frame(Eval.PCA),type="raw")
  Eval.pH  <-predict(glmTunepH,  newdata=data.frame(Eval.PCA),type="raw")
  Eval.SOC <-predict(glmTuneSOC, newdata=data.frame(Eval.PCA),type="raw")
  Eval.Sand<-predict(glmTuneSand,newdata=data.frame(Eval.PCA),type="raw")
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
Eval.PCA2<-round(colMeans(Avg.Score),4)
Eval.PCA2
write.csv(Eval.PCA2,"analytics/kaggle_challenges/africa_soil/eval-pca2.csv",row.names=F)


##########
##Create submission set
Predict.PCA<-predict(PCA,newdata=Test.Spec)
Predict.Ca<-predict(glmTuneCa,newdata=data.frame(Predict.PCA),type="raw")
Predict.P<-predict(glmTuneP,newdata=data.frame(Predict.PCA),type="raw")
Predict.pH<-predict(glmTunepH,newdata=data.frame(Predict.PCA),type="raw")
Predict.SOC<-predict(glmTuneSOC,newdata=data.frame(Predict.PCA),type="raw")
Predict.Sand<-predict(glmTuneSand,newdata=data.frame(Predict.PCA),type="raw")

testResults <- data.frame(PIDN = Test.raw[,1],
                          Ca=Predict.Ca,
                          P=Predict.P,
                          pH=Predict.pH,
                          SOC=Predict.SOC,
                          Sand=Predict.Sand)

write.csv(testResults,file = "analytics/kaggle_challenges/africa_soil/PCA2.csv",row.names = FALSE)
