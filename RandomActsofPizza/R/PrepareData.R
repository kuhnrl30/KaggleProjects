#' Convert to tabular format
#'
#' Prepare a tabular dataset from json
#'
#' @param y either training or test datasets in json format
#' @return dataframe
#' @export
#'
PrepareData<-function(y){
  y<- FillNulls(y)
  x<- as.data.frame(matrix(unlist(y),
                           byrow=T,
                           nrow=length(y)),
                    stringsAsFactors=F)
  names(x)<-names(y[[1]])
  return(x)
  }
