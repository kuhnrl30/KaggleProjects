#'Fills Null values with NA
#'
#'Fill the null values in the training and test datasets with NA
#' @param y json object with null values
#' @return json object
#' @export

FillNulls<-function(y){
  y<- lapply(y, lapply, function(x) ifelse(is.null(x),NA,x))
  lapply(y, lapply, lapply, function(x) ifelse(is.null(x),NA,x))
  }
