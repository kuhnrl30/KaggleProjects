#Summarize Evals
Eval.Table<-data.frame()
GAM.Eval <-read.csv("analytics/RProjects/africa_soil/eval-gam.csv")
GLM.Eval <-read.csv("analytics/RProjects/africa_soil/eval-glm.csv")
PCA2.Eval<-read.csv("analytics/RProjects/africa_soil/eval-pca2.csv")
SLR.Eval <-read.csv("analytics/RProjects/africa_soil/eval-slr.csv") 
SVM.Eval <-read.csv("analytics/RProjects/africa_soil/eval-svm.csv")

Eval.Table<-matrix(,nrow=5,ncol=5)


Eval.Table<-cbind(GAM.Eval,GLM.Eval,PCA2.Eval,SLR.Eval,SVM.Eval)
names(Eval.Table)<-c("GAM","GLM","PCA2","SLR","SVM")
row.names(Eval.Table)<- c("Ca","P","pH","SOC","Sand","mean")
write.csv(Eval.Table,"analytics/kaggle_challenges/africa_soil/eval-table.csv",row.names=T)




##########
## Create Submission Set
GAM <-read.csv("analytics/kaggle_challenges/Africa_Soil/submit-gam.csv",header=T)
GLM <-read.csv("analytics/kaggle_challenges/Africa_Soil/submit-glm.csv",header=T)
PCA2<-read.csv("analytics/kaggle_challenges/Africa_Soil/PCA2.csv",header=T)
SLR <-read.csv("analytics/kaggle_challenges/Africa_Soil/slr.csv",header=T)
SVM <-read.csv("analytics/kaggle_challenges/Africa_Soil/SVM.csv",header=T)

names(GAM)
names(GLM)
names(PCA2)
names(SLR)
names(SVM)

Fin.Sub.P<- rowMeans(cbind(GAM[,"P"],GLM[,"P"],PCA2[,"P"],SLR[,"P"],SVM[,"P"]))
Fin.Sub.pH<- rowMeans(cbind(SLR[,"P"],SVM[,"P"]))
Fin.Sub.Sand<- rowMeans(cbind(SLR[,"Sand"],SVM[,"Sand"]))
Final.Submit<-data.frame(PIDN=Test.raw[,1],
                         Ca=SLR[,"Ca"],
                         P=Fin.Sub.P,
                         pH=Fin.Sub.pH,
                         SOC=SLR[,"SOC"],
                         Sand=Fin.Sub.Sand)

write.csv(Final.Submit,"analytics/kaggle_challenges/Africa_Soil/Final-Submit.csv",row.names=F)
