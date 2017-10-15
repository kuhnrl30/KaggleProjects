rm(list=ls())
library(caret)

Train.raw <- read.csv('analytics/RProjects/africa_soil/training.csv', header = TRUE)
Test.raw <- read.csv('analytics/RProjects/africa_soil/sorted_test.csv',header = TRUE)
Train.raw$Depth <-  with ( Train.raw, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) )
Test.raw$Depth <-  with ( Test.raw, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) ) 

Xtrain <- Train.raw[,2:3595]
Ytrain <- Train.raw[,3596:3600]
Xtest <- Test.raw[,2:3595]
IDtest <- Test.raw[,1]


#delete highly correlated (>0.95) features.
tooHigh <- findCorrelation(cor(rbind(Xtrain,Xtest)), .95)

Xtrainfiltered <- Xtrain[, -tooHigh]
Xtestfiltered  <-  Xtest[, -tooHigh]

# 10 fold cv
indx <- createFolds(Ytrain[,1], returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)

## Scores 0.555 in eval below
lmTuneCa   <- train(x = Xtrainfiltered, y = Ytrain$Ca,  method = "lm", trControl = ctrl)
lmTuneP    <- train(x = Xtrainfiltered, y = Ytrain$P,   method = "lm", trControl = ctrl)
lmTunepH   <- train(x = Xtrainfiltered, y = Ytrain$pH,  method = "lm", trControl = ctrl)
lmTuneSOC  <- train(x = Xtrainfiltered, y = Ytrain$SOC, method = "lm", trControl = ctrl)
lmTuneSand <- train(x = Xtrainfiltered, y = Ytrain$Sand,method = "lm", trControl = ctrl)


##########
## Evaluation
Avg.Score<-data.frame()  
for (i in 1:500){
  Eval.index<-sample(1:1157,115)
  
  Eval.P   <-predict(lmTuneP,   Xtrainfiltered[Eval.index,])
  Eval.Ca  <-predict(lmTuneCa,  Xtrainfiltered[Eval.index,])
  Eval.pH  <-predict(lmTunepH,  Xtrainfiltered[Eval.index,])
  Eval.SOC <-predict(lmTuneSOC, Xtrainfiltered[Eval.index,]) 
  Eval.Sand<-predict(lmTuneSand,Xtrainfiltered[Eval.index,])
  
  Train.Pred<- data.frame(Ca=Eval.Ca,
                          P=Eval.P,
                          pH=Eval.pH,
                          SOC=Eval.SOC,
                          Sand=Eval.Sand)
  Train.Eval<- Train.raw[Eval.index,3596:3600]
  
  Scores<-sqrt(colMeans((Train.Eval-Train.Pred)^2))
  Summary.Scores<- c(Scores,mean(Scores))
  Avg.Score<-rbind(Avg.Score,Summary.Scores)
}
names(Avg.Score)<-c("Ca","P","pH","SOC","Sand","MCRMSE")
Eval.SLR<-round(colMeans(Avg.Score),4)
Eval.SLR
write.csv(Eval.SLR,"analytics/kaggle_challenges/africa_soil/eval-slr.csv",row.names=F)


##########
## Create Submission set
predict.Ca   <- predict(lmTuneCa, Xtestfiltered)
predict.p    <- predict(lmTuneP,Xtestfiltered)
predict.pH   <- predict(lmTunepH,Xtestfiltered)
predict.SOC  <- predict(lmTuneSOC,Xtestfiltered)
predict.Sand <- predict(lmTuneSand,Xtestfiltered)

testResults<- data.frame(PIDN =IDtest,
                         Ca   =predict.Ca,
                         P    =predict.p,
                         pH   =predict.pH,
                         SOC  =predict.SOC,
                         Sand =predict.Sand)
                         
  
write.csv(testResults,file = "analytics/kaggle_challenges/africa_soil/slr.csv",row.names = FALSE)
