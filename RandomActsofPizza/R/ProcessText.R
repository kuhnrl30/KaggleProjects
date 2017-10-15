#' Preprocess text fields
#'
#' To preprocess text fields so they can be used for further analysis. The
#' preprocessing follows these steps:
#' \itemize{
#' \item Convert to corpus
#' \item Remove capitalized letters
#' \item Convert to plain text
#' \item Remove punctuation
#' \item Remove stopwords
#' \item Stem words
#' \item Convert to document term matrix
#' }
#'
#' @param x character for preprocessing
#' @param remove a vector of additional words to remove
#' @return Document term matrix
#' @export
#'
ProcessText<- function(x,remove=NULL){

    Corp<-Corpus(VectorSource(x))
    Corp<-tm_map(Corp, content_transformer(tolower))
    Corp<-tm_map(Corp, PlainTextDocument)
    Corp<-tm_map(Corp, removePunctuation)
    Corp<-tm_map(Corp, removeWords, c(remove, stopwords("english")))
    Corp<-tm_map(Corp, stemDocument)
    DocumentTermMatrix(Corp)
    }
