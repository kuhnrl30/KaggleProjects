# For the Home Depot competition on Kaggle.com
# https://www.kaggle.com/c/home-depot-product-search-relevance

# Environment
library(ggplot2)
library(dplyr, quietly = T)
library(caret)
library(tm)

setwd("C:/users/ryan/dropbox/rprojects/homedepot")
Train<- read.csv("Data/train.csv", header=T, stringsAsFactors = F)
Test<- read.csv("Data/test.csv", header=T, stringsAsFactors = F)
Train$OldRel<- round(Train$relevance,0)
Mapping<- data.frame(OldRel=c(1,2,3),
                     NewRel= c("A","B","C"),
                     stringsAsFactors = F)
Train<- left_join(Train, Mapping)
Train$NewRel<- factor(Train$NewRel)


Attributes<-read.csv("Data/attributes.csv", header=T, stringsAsFactors = F)
Descriptions<- read.csv("Data/product_descriptions.csv", header=T, stringsAsFactors = F)

TextProcess<- function(x){
  z<-Corpus(VectorSource(x))
  z<-tm_map(z, content_transformer(tolower))
  z<-tm_map(z, PlainTextDocument)
  z<-tm_map(z, removePunctuation)
  z<-tm_map(z, removeWords,stopwords("english"))
  z<-tm_map(z, stemDocument)
  sapply(1:length(x), function(y) as.character(z[[y]][[1]]))
}

print("Processing text")
Descriptions$NewDesc<- TextProcess(Descriptions$product_description)
Train$NewTitle      <- TextProcess(Train$product_title)
Train$NewSearch     <- TextProcess(Train$search_term)
Test$NewSearch      <- TextProcess(Test$search_term)
Test$NewTitle      <- TextProcess(Test$product_title)



Train<- left_join(Train, Descriptions)
Test<- left_join(Test, Descriptions)


word_match <- function(words,title,desc){
  n_title <- 0
  n_desc <- 0
  words <- unlist(strsplit(words," "))
  nwords <- length(words)
  for(i in 1:length(words)){
    pattern <- paste("(^| )",words[i],"($| )",sep="")
    n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
    n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
  }
  return(c(n_title,nwords,n_desc))
}

print("Calc the summary stats")
train_words <- as.data.frame(t(mapply(word_match, Train$NewSearch, Train$NewTitle, Train$NewDesc)))
Train$nmatch_title <- train_words[,1]
Train$nwords <- train_words[,2]
Train$nmatch_desc <- train_words[,3]

test_words <- as.data.frame(t(mapply(word_match, Test$NewSearch, Test$NewTitle, Test$NewDesc)))
Test$nmatch_title <- test_words[,1]
Test$nwords <- test_words[,2]
Test$nmatch_desc <- test_words[,3]

rm(train_words, test_words, Descriptions, DescCorp, SerCorp, TSerCorp, Attributes)

print("Train the model")
fitControl<- trainControl(method="repeatedCV",
                          number=10,
                          repeats=10,
                          classProbs = T)


fit<- train(NewRel~nmatch_title+nmatch_desc+nwords, data=Train, method="rf", vebose=F)
fit

Pred<- predict(fit, newdata=Test)

Pred<- ifelse(Pred>3, 3, Pred)
Pred<- ifelse(Pred<1, 1, Pred)

submit<- data.frame(id= Test$id, relevance= Pred )
write.csv(submit, file="Submit.csv", row.names=F)
