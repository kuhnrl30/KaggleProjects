Train.raw<- read.csv("analytics/RProjects/africa_soil/training.csv")
Test.raw<- read.csv("analytics/RProjects/africa_soil/sorted_test.csv",stringsAsFactors=F)
Train.raw$Depth <-  with ( Train.raw, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) )
Test.raw$Depth <-  with ( Test.raw, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) )

Train.Spec         <- Train.raw[,c(1:2655,2671:3579)]
Train.CO2          <- Train.raw[,c(1,2656:2670)]
Train.Other        <- Train.raw[c(1,3580:3599)]
Train.Properties   <- Train.raw[,c(1,3596:3600)]
PIDN               <- Train.raw[,1]
soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")

Test.Spec         <- Test.raw[,c(1:2655,2671:3579)]
Test.Co2          <- Test.raw[,c(1,2656:2670)]
Test.Other        <- Test.raw[c(1,3580:3595)]
Test.PIDN         <- Test.raw[,1]


cor(Train.Properties[,2:5],Train.Other[sapply(Train.Other,is.numeric)])

##########
## Scores 0.936 in evaluation below
train.glm.P   <- glm(P   ~BSAV+TMAP+TMFI+LSTD,data=Train.Other)
train.glm.Ca  <- glm(Ca  ~BSAN+BSAS+BSAV+ELEV+REF7,data=Train.Other)
train.glm.pH  <- glm(pH  ~EVI+LSTD+TMAP,data=Train.Other)
train.glm.SOC <- glm(pH  ~TMAP+ELEV,data=Train.Other)
train.glm.Sand<- glm(Sand~ELEV+LSTN+REF7+TMAP,data=Train.Other)

summary(train.glm.pH)
summary(train.glm.SOC)
summary(train.glm.P)
summary(train.glm.Ca)
summary(train.glm.Sand)

##########
## Evaluation
Avg.Score<-data.frame()  
for (i in 1:500){
  Eval.index<-sample(1:1157,115)
  
  Eval.P   <-predict(train.glm.P,   newdata=Train.Other[Eval.index,],type="response")
  Eval.Ca  <-predict(train.glm.Ca,  newdata=Train.Other[Eval.index,],type="response")
  Eval.pH  <-predict(train.glm.pH,  newdata=Train.Other[Eval.index,],type="response")
  Eval.SOC <-predict(train.glm.SOC, newdata=Train.Other[Eval.index,],type="response")
  Eval.Sand<-predict(train.glm.Sand,newdata=Train.Other[Eval.index,],type="response")
  
  Train.Pred<- data.frame(Ca  =Eval.Ca,
                          P   =Eval.P,
                          pH  =Eval.pH,
                          SOC =Eval.SOC,
                          Sand=Eval.Sand)
  Train.Eval<- Train.Properties[Eval.index,]
  
  Scores<-sqrt(colMeans((Train.Eval-Train.Pred)^2))
  Summary.Scores<- c(Scores,mean(Scores))
  Avg.Score<-rbind(Avg.Score,Summary.Scores)
}
names(Avg.Score)<-c("Ca","P","pH","SOC","Sand","MCRMSE")
Eval.GLM<-round(colMeans(Avg.Score),4)
Eval.GLM
write.csv(Eval.GAM,"analytics/kaggle_challenges/africa_soil/eval-glm.csv",row.names=F)

##########
## Create Submission set

predict.P   <- predict.glm(train.glm.P,   newdata=Test.Other,type="response")
predict.Ca  <- predict.glm(train.glm.Ca,  newdata=Test.Other,type="response")
predict.pH  <- predict.glm(train.glm.pH,  newdata=Test.Other,type="response")
predict.SOC <- predict.glm(train.glm.SOC, newdata=Test.Other,type="response")
predict.Sand<- predict.glm(train.glm.Sand,newdata=Test.Other,type="response")

submit<- as.data.frame(cbind(Test.PIDN,
                             Ca=predict.Ca,
                             P=predict.P,
                             pH=predict.pH,
                             SOC=predict.SOC,
                             Sand=predict.Sand))
names(submit)<-c("PIDN","Ca","P","pH","SOC","Sand")

write.csv(submit,"analytics/kaggle_challenges/africa_soil/submit-glm.csv",row.names=F)
