## ----setoptions, echo=F--------------------------------------------------
knitr::opts_chunk$set(cache=FALSE, fig.height=3, fig.width = 7, comment=NULL, eval=T, tiy=T, message = F, warning = F, width=80)

## ----vignette, eval=F----------------------------------------------------
#  vignette("ExploratoryAnalysis")

## ----environment---------------------------------------------------------
library(RandomActsofPizza)
library(dplyr)
library(caret)
library(rpart.plot)
data(train)
data(test)

## ----baseline------------------------------------------------------------
train %>%
    summarise(N=length(received_pizza),
              Success=sum(received_pizza)) %>%
    mutate(Percent=paste(round(Success/N,3)*100,"%",sep=""))

## ------------------------------------------------------------------------
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)

## ----trainlogmodel, results='hide'---------------------------------------
train <- train %>%
    mutate(received_pizza= factor(received_pizza, labels=c("N","Y")))

glm_ctrl<- trainControl(method="repeatedCV",
                        number=10,
                        repeats=10,
                        classProbs=TRUE,
                        summaryFunction = twoClassSummary,
                        allowParallel = TRUE)

LogMdl <- train(y=train$received_pizza,
                x=subset(train, select=-received_pizza),
                method="glm",
                metric="ROC",
                trControl=glm_ctrl,
                family= "binomial") 


stopCluster(cl)

## ----logmdlsummary-------------------------------------------------------
summary(LogMdl)

## ----traincart, warning=FALSE--------------------------------------------
cl <- makeCluster(3)
registerDoParallel(cl)

Cart_ctrl<- trainControl(method="cv",
                        number=10,
                        classProbs=TRUE,
                        summaryFunction = twoClassSummary,
                        allowParallel = TRUE)

CartMdl<- train(y=train$received_pizza,
                x=subset(train, select=-received_pizza),
                metric="ROC",
                method="rpart",
                trControl= Cart_ctrl,
                cp=.05)



stopCluster(cl)


prp(CartMdl$finalModel,
    main= "RAOP Classification Tree",
    extra=1,
    box.col=c("pink","palegreen")[CartMdl$frame$yval],
    leaf.round=2)


## ----scoring-------------------------------------------------------------
LogScore<- predict(LogMdl, data=train, type="prob")
confusionMatrix(LogScore[,2]>.5, train$received_pizza=="Y", positive="TRUE")

CartScore<- predict(CartMdl, data=train, type="prob")
confusionMatrix(CartScore[,2]>.5, train$received_pizza=="Y", positive="TRUE")

MergedScore<- MergeModels(cbind(LogScore[,2],CartScore[,2]),c(.6,.4))
confusionMatrix(MergedScore>.5, train$received_pizza=="Y", positive="TRUE")

## ----makepredictions-----------------------------------------------------
LogPred  <- predict(LogMdl,newdata=test, type="prob")
CARTPred <- predict(CartMdl, newdata=test, type="prob")
Merged<-MergeModels(cbind(LogPred[,2],CARTPred[,2]), c(.6,.4))


Submit<- data.frame(request_id=test$request_id,
                    received_pizza=Merged)

