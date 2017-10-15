# For the Home Depot competition on Kaggle.com
# https://www.kaggle.com/c/home-depot-product-search-relevance

# Environment
library(dplyr)
library(caret)

#setwd("C:/users/ryan/dropbox/rprojects/homedepot")
#load("data/datasets.Rdata")


#Train.y<- Train$relevance
#Train.x<- Train %>% select(nmatch_title, nwords, nmatch_desc, nmatch_brand)


# Train the models ----
print("Train the model")
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)

glm_ctrl<- trainControl(method="repeatedCV",
                        number=10,
                        repeats=10,
                        predictionBounds = c(1,3),
                        allowParallel = TRUE)

glm_mdl<- train(relevance~nmatch_title+nmatch_desc+nwords+nmatch_brand,
                data=Train,
                method="glm")


svm_ctrl<- trainControl(classProbs=FALSE,
                        method="repeatedcv",
                        repeats=5,
                        allowParallel = TRUE)

svm_mdl<- train(relevance~nmatch_title+nmatch_desc+nwords+nmatch_brand, 
                data=Train,
                model="svmLinear",
                tuneLength=3,
                metric="RMSE",
                trControl=svm_ctrl)


xgb_ctrl<- trainControl(allowParallel = TRUE,
                        number= 5,
                        repeats=2,
                        method="repeatedcv")

xgbGrid <- expand.grid(nrounds = 20,
                       lambda = 0,
                       alpha = 0)

xgb_mdl<- train(relevance~nmatch_title+nmatch_desc+nwords+nmatch_brand,
                data = Train,
                method = "xgbLinear",
                tuneLength = 3,
                trControl = xgb_ctrl,
                tuneGrid = xgbGrid)


stopCluster(cl)






Pred_trn<- data.frame(XGB= predict(xgb_mdl, newdata=Train),
                      SVM= predict(svm_mdl, newdata=Train),
                      GLM= predict(glm_mdl, newdata=Train))

# Get RMSE of average of 3 models
RMSE <- sqrt(mean((Train$relevance-rowMeans(Pred_trn))^2))
RMSE


# Stacked model: train simple linear model on top of Train and output of 3 models
NewTrain<- cbind(Train, Pred_trn)
stk_mdl <- lm(relevance~nmatch_title+nmatch_desc+nwords+nmatch_brand + XGB +SVM + GLM,
             data=NewTrain)

stk_prd  <- predict(stk_mdl, newdata=NewTrain)
RMSE_stk <- sqrt(mean((Train$relevance-stk_prd)^2))
RMSE_stk





Pred_glm<- predict(glm_mdl, newdata=Test)
Pred_svm<- predict(svm_mdl, newdata=Test)
Pred_xgb<- predict(xgb_mdl, newdata=Test)



Pred_test<- data.frame(XGB = predict(xgb_mdl, newdata=Test),
                       SVM = predict(svm_mdl, newdata=Test),
                       GLM = predict(glm_mdl, newdata=Test))


NewTest<- data.frame(Test, Pred_test)
Predict<- predict(stk_mdl, newdata=NewTest)


Predict<- ifelse(Predict>3, 3, Predict)
Predict<- ifelse(Predict<1, 1, Predict)


submit<- data.frame(id= Test$id, relevance= Predict )
write.csv(submit, file="Submit.csv", row.names=F)