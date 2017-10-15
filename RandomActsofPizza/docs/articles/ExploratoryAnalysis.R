## ---- echo=F-------------------------------------------------------------
knitr::opts_chunk$set(cache=FALSE, fig.height=3, fig.width = 7, comment=NULL, eval=T, tidy=F, width=80)

## ----environment, warning=F, message=F-----------------------------------
library(RandomActsofPizza)
library(plyr)
library(dplyr)      # data manipulation
library(ggplot2)    # plotting
library(httr)
library(tm)

## ----loaddata------------------------------------------------------------
trainURL  <- "https://onedrive.live.com/download?cid=CE1B639CAE3DDA03&resid=CE1B639CAE3DDA03%2128205&authkey=AMx1Yp1M2GIga6o"
Train.raw <- httr::content(GET(trainURL),"parsed")
train     <- PrepareData(Train.raw)

testURL   <- "https://onedrive.live.com/download?cid=CE1B639CAE3DDA03&resid=CE1B639CAE3DDA03%2128034&authkey=AJSyWkddIKdJaCg"
Test.raw  <- httr::content(GET(testURL),"parsed")
test      <- PrepareData(Test.raw)

## ----cleanup, echo=F-----------------------------------------------------
rm(trainURL, Train.raw, testURL, Test.raw)

## ------------------------------------------------------------------------
#remove columns not in test set and put in the same order
train<- train %>%
    select(one_of(names(test)), requester_received_pizza)

# Convert all N/A to NA
train<-as.data.frame(lapply(train,function(x) gsub("N/A",NA,x)),stringsAsFactors=F)
test <-as.data.frame(lapply(test, function(x) gsub("N/A",NA,x)),stringsAsFactors=F)

# convert character columns to numeric
train[,c(5:11,13:14,16:17)]<-apply(train[,c(5:11,13:14,16:17)], 2,as.numeric)
test[,c(5:11,13:14,16:17)] <-apply(test[,c(5:11,13:14,16:17)], 2,as.numeric)
train$requester_received_pizza<-ifelse(train$requester_received_pizza=="TRUE",1,0)

## ------------------------------------------------------------------------
NA.counts<-plyr::ldply(lapply(train,function(x) sum(is.na(x))))
NA.counts %>%
    filter(!V1==0) %>%
    simpleprint()

## ----convertTime---------------------------------------------------------
# Convert date field to POSIXct format and create field for the year
train<- train %>% 
    mutate(unix_timestamp_of_request_utc= as.POSIXct(unix_timestamp_of_request_utc, 
                                                     origin= "1970-01-01", 
                                                     tz="UTC"),
           Year= factor(format(unix_timestamp_of_request_utc,format='%Y')))

test<- test %>%
    mutate(unix_timestamp_of_request_utc= as.POSIXct(unix_timestamp_of_request_utc, 
                                                     origin= "1970-01-01", 
                                                     tz="UTC"),
           Year= factor(format(unix_timestamp_of_request_utc,format='%Y')))

train %>%
    group_by(Year) %>%
    summarise(Count=length(Year),
              Success= sum(requester_received_pizza)) %>%
    mutate(Percent=paste(round(Success/Count,3)*100,"%", sep=""))

## ----weekdays------------------------------------------------------------
train<- train %>%
    mutate(weekday= factor(weekdays(unix_timestamp_of_request_utc,F))) 

test<- test %>%
    mutate(weekday= factor(weekdays(unix_timestamp_of_request_utc,F))) 

train %>%
    group_by(weekday) %>%
    summarise(Count=length(weekday),
              Success=sum(requester_received_pizza)) %>%
    mutate(Percent=paste(round(Success/Count,3)*100,"%",sep="")) 

## ----giverusername-------------------------------------------------------
train<- train %>%
    mutate(giver_username_if_known= ifelse(is.na(giver_username_if_known),0,1))

test<- test %>%
    mutate(giver_username_if_known= ifelse(is.na(giver_username_if_known),0,1))

train %>%
    group_by(giver_username_if_known) %>%
    summarise(Count=length(giver_username_if_known),
              Success= sum(requester_received_pizza)) %>%
    mutate(Percent=paste(round(Success/Count,3)*100,"%",sep=""))

## ----image---------------------------------------------------------------
train<- train %>%
    mutate(Image = ifelse(grepl("i.imgur",train$request_text),1,0))

test<- test %>%
    mutate(Image = ifelse(grepl("i.imgur",test$request_text),1,0))

train %>%
    group_by(Image) %>%
    summarise(Count=length(Image),
              Success= sum(requester_received_pizza==1)) %>%
    mutate(percent=paste(round(Success/Count,3)*100,"%",sep=""))

## ----accountAge----------------------------------------------------------
p<- ggplot()
p<- p + aes(x= 1:2000,
            y= FindCutoff(train$requester_received_pizza,
                   train$requester_account_age_in_days_at_request, 
                   1:2000))
p<- p + geom_line()
p<- p + labs(x="Cutoff level",
             y="Success Rate")
p<- p + theme_minimal()
p


train <- train %>%
    mutate(Acct.Age=ifelse(requester_account_age_in_days_at_request>500,1,0))

test <- test %>%
    mutate(Acct.Age=ifelse(requester_account_age_in_days_at_request>500,1,0))

## ----numComments---------------------------------------------------------
p<- ggplot()
p<- p + aes(x= 1:88,
            y= FindCutoff(train$requester_received_pizza,
                          train$requester_number_of_comments_in_raop_at_request, 
                          1:88))
p<- p + geom_line()
p<- p + labs(x="Cutoff level",
             y="Success Rate")
p<- p + theme_minimal()
p

train <- train %>%
    mutate(BnRAOP = ifelse(requester_number_of_comments_in_raop_at_request>9,1,0))

test <- test %>%
    mutate(BnRAOP = ifelse(requester_number_of_comments_in_raop_at_request>9,1,0))

## ----words---------------------------------------------------------------
train$Words<- sapply(strsplit(train$request_text," ",fixed=T),length)
test$Words <- sapply(strsplit(test$request_text," ",fixed=T),length)

train<- within(train, Word.bin<- as.integer(cut(Words,quantile(Words, probs=0:2/2), 
                                                include.lowest=T)))
test <- within(test, Word.bin<- as.integer(cut(Words, quantile(Words, probs=0:2/2), 
                                                include.lowest=T)))
train %>%
    group_by(Word.bin) %>%
    summarise(Count=length(Word.bin),
              Success= sum(requester_received_pizza==1)) %>%
    mutate(Percent=paste(round(Success/Count,3)*100,"%",sep=""))

## ----trainTextAnalytics,results='hide', warning=FALSE--------------------
removewords<-c("pizza","anyon","anyth","appreci","back")
dtmTrain<- ProcessText(train$request_text_edit_aware, remove=removewords)
dtmTest <- ProcessText(test$request_text_edit_aware, remove=removewords)

sparseTrain<-removeSparseTerms(dtmTrain,0.90)

TextTrain<- as.data.frame(as.matrix(sparseTrain), row.names=F)

TextTest <- as.data.frame(as.matrix(dtmTest), row.names=F)
TextTest <- TextTest[,colnames(TextTest) %in% names(TextTrain)]

## ----plottraintext-------------------------------------------------------
names(TextTrain)
barplot(colSums(TextTrain))

## ------------------------------------------------------------------------
names(train)<-gsub("requester_","",names(train))
names(train)<-gsub("_at_request","",names(train))

names(test) <-gsub("requester_","",names(test))
names(test) <-gsub("_at_request","",names(test))

justInCase<- as.data.frame(rbind(train[, c(2:4,12,15:17)],
                                  test[, c(2:4,12,15:17)]))

train <- as.data.frame(cbind(train[,-c(2:4,12,15:17)], TextTrain),
                       stringsAsFactors=F)

test <- as.data.frame(cbind(test[,-c(3:4,12,15:17)],  TextTest),
                      stringsAsFactors=F)

