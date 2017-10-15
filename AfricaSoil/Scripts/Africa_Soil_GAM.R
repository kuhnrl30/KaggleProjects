require(mgcv)

Train.raw<- read.csv("analytics/RProjects/africa_soil/training.csv",stringsAsFactors=F)
Test.raw<- read.csv("analytics/RProjects/africa_soil/sorted_test.csv",stringsAsFactors=F)

Train.Spec         <- Train.raw[,c(2:2655,2671:3579)]
Train.CO2          <- Train.raw[,c(2656:2670)]
Train.Other        <- Train.raw[,3580:3600]
Train.Properties   <- Train.raw[,3596:3600]
PIDN               <- Train.raw[,1]
soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")


Test.Spec         <- Test.raw[,c(1:2655,2671:3579)]
Test.Co2          <- Test.raw[,2656:2670]
Test.Other        <- Test.raw[,3580:3595]
Test.PIDN         <- Test.raw[,1]

##########
#Scores 0.9106 in eval below
train.gam.pH  <- gam(pH~  s(EVI) +s(LSTD)+s(TMAP),data=Train.Other)
train.gam.SOC <- gam(pH~  s(TMAP)+s(ELEV),data=Train.Other)
train.gam.Sand<- gam(Sand~s(TMAP)+s(BSAN),data=Train.Other)
train.gam.P   <- gam(P~   s(TMAP)+s(BSAN),data=Train.Other)
train.gam.Ca  <- gam(Ca~  s(TMAP)+s(BSAN),data=Train.Other)


summary(train.gam.pH)
summary(train.gam.SOC)
summary(train.gam.Sand)
summary(train.gam.P)
summary(train.gam.Ca)

##########
##EVALUATION
Avg.Score<-data.frame()  
for (i in 1:500){
    Eval.index<-sample(1:1157,115)
    
    Eval.P   <-predict.gam(train.gam.P,   newdata=Train.Other[Eval.index,],type="response")
    Eval.Ca  <-predict.gam(train.gam.Ca,  newdata=Train.Other[Eval.index,],type="response")
    Eval.pH  <-predict.gam(train.gam.pH,  newdata=Train.Other[Eval.index,],type="response")
    Eval.SOC <-predict.gam(train.gam.SOC, newdata=Train.Other[Eval.index,],type="response")
    Eval.Sand<-predict.gam(train.gam.Sand,newdata=Train.Other[Eval.index,],type="response")

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
Eval.GAM<-round(colMeans(Avg.Score),4)
Eval.GAM
write.csv(Eval.GAM,"analytics/kaggle_challenges/africa_soil/eval-gam.csv",row.names=F)


##########
##Create Submission Set

predict.Ca  <- predict.gam(train.gam.Ca,  newdata=Test.Other,type="response")
predict.P   <- predict.gam(train.gam.P,   newdata=Test.Other,type="response")
predict.SOC <- predict.gam(train.gam.SOC, newdata=Test.Other,type="response")
predict.pH  <- predict.gam(train.gam.pH,  newdata=Test.Other,type="response")
predict.Sand<- predict.gam(train.gam.Sand,newdata=Test.Other,type="response")

submit<- data.frame(PIDN=Test.PIDN,
                    Ca=predict.Ca,
                    P=predict.P,
                    pH=predict.pH,
                    SOC=predict.SOC,
                    Sand=predict.Sand)
names(submit)<-c("PIDN","Ca","P","pH","SOC","Sand")

write.csv(submit,"analytics/kaggle_challenges/africa_soil/submit-gam.csv",row.names=F)

