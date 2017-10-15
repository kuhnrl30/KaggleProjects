word_match <- function(words,title,desc, brand){
  n_title <- 0
  n_desc <- 0
  n_brand <- 0
  words <- unlist(strsplit(words," "))
  nwords <- length(words)
  for(i in 1:length(words)){
    pattern <- paste("(^| )",words[i],"($| )",sep="")
    n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
    n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    n_brand <- n_brand + grepl(pattern,brand,perl=TRUE,ignore.case=TRUE)
  }
  return(c(n_title,nwords,n_desc, n_brand))
}




TextProcess<- function(x){
  require(tm)
  z<- tolower(x)
  z<- gsub("(  )|\\$|\\?|-", " ", z)
  z<- gsub("( x )|\\*|(by)", "xbi", z)
  z<- gsub("(inches)|(in\\.)", "in", z)
  z<- gsub("((pounds)|(lbs)|(lbs\\.))", "lb", z)
  z<- gsub("((gallon)|(gal\\.)|(gallons))", "gal", z)
  z<- gsub("((ounce)|(oz\\.)|(ounces))", "oz", z )
  z<- gsub("(centimeters)|(cm\\.)", "cm", z )
  z<- gsub("(amperes)|(ampere)|(amps)|( amp )", "amp", z )
  z<- gsub("(volts)", "volt", z )
  z<- gsub("((square)|(sq))((\\s)|(\\.))((foot)|(ft))", " SqFt", z )
  z<- gsub("((cubic)|(cu))((\\s)|(\\.))((foot)|(ft))", " CuFt", z )
  z<- gsub("([0-9])x([0-9])", "\1 xbi \2", z )
  z<- gsub(" \\ ", " ", z)
  z<-Corpus(VectorSource(z))
  z<-tm_map(z, PlainTextDocument)
  z<-tm_map(z, removePunctuation)
  z<-tm_map(z, removeWords,stopwords("english"))
  z<-tm_map(z, stemDocument)
  sapply(1:length(x), function(y) as.character(z[[y]][[1]]))
}