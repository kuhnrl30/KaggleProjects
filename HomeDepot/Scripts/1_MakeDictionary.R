setwd("C:/users/ryan/dropbox/rprojects/homedepot")
rawdict<- readLines("data/dictionary")
rawdict<- strsplit(rawdict, split=":", fixed=T)

rawdict<-data.frame(matrix(unlist(rawdict), ncol=2, byrow=T), stringsAsFactors=F)

dict<- rawdict
dict[,1]<- gsub("(^\')|(\')$","", dict[,1])
dict[,2]<- gsub("(,)$","", dict[,2])
dict[,2]<- gsub("(^ +\')|(\')$","", dict[,2])
names(dict)<- c("Orig","New")

write.csv(dict,"data/clean-dictionary.csv", row.names=F)
