# For the Home Depot competition on Kaggle.com
# https://www.kaggle.com/c/home-depot-product-search-relevance

# Environment
library(dplyr, quietly = T)
library(tm)
setwd("C:/users/ryan/dropbox/rprojects/homedepot")
source("scripts/0_functions.R")
Train       <- read.csv("Data/train.csv", 
                        header=T, 
                        stringsAsFactors = F)

Test        <- read.csv("Data/test.csv", 
                        header=T, 
                        stringsAsFactors = F)

Attributes  <- read.csv("Data/attributes.csv", 
                        header=T, 
                        stringsAsFactors = F)

Descriptions<- read.csv("Data/product_descriptions.csv", 
                        header=T, 
                        stringsAsFactors = F)


# spell check ----
Dictionary<- read.csv("data/clean-dictionary.csv", 
                      stringsAsFactors=F,
                      header=T)

Train<- left_join(Train,Dictionary, by=c("search_term"="Orig"))
Train$New<- ifelse(is.na(Train$New), Train$search_term, Train$New)

Test<- left_join(Test, Dictionary, by=c("search_term"="Orig"))
Test$New <- ifelse(is.na(Test$New), Test$search_term, Test$New)


# Brand names ----
df_brand<- Attributes %>% filter(name=="MFG Brand Name") %>%dplyr::rename(brand= value)

Train<- left_join(Train, df_band)
Test<- left_join(Test, df_brand)

# Text Processing ----
print("Processing text")
Descriptions$NewDesc<- TextProcess(Descriptions$product_description)
Train$NewTitle      <- TextProcess(Train$product_title)
Train$NewSearch     <- TextProcess(Train$New)
Train$brand         <- TextProcess(Train$brand)
Test$NewSearch      <- TextProcess(Test$New)
Test$NewTitle       <- TextProcess(Test$product_title)
Test$brand         <- TextProcess(Test$brand)

Train<- left_join(Train, Descriptions)
Test<- left_join(Test, Descriptions)



# Sumary stats ----
print("Calc the summary stats")
train_words <- as.data.frame(t(mapply(word_match, 
                                      Train$NewSearch, 
                                      Train$NewTitle, 
                                      Train$NewDesc,
                                      Train$brand)))

Train$nmatch_title <- train_words[,1]
Train$nwords <- train_words[,2]
Train$nmatch_desc <- train_words[,3]
Train$nmatch_brand<- train_words[,4]



test_words <- as.data.frame(t(mapply(word_match, 
                                     Test$NewSearch, 
                                     Test$NewTitle, 
                                     Test$NewDesc,
                                     Test$brand)))
Test$nmatch_title <- test_words[,1]
Test$nwords <- test_words[,2]
Test$nmatch_desc <- test_words[,3]
Test$nmatch_brand <- test_words[,4]


save(Train, Test, file="data/Datasets.Rdata")
rm(train_words, test_words, Descriptions, Attributes)