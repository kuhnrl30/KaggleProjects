#' Find the cuttoff for binary variables
#'
#' Find the cutoff point when converting a numeric vector to binary. The
#' function calculates the share of a instances in a class given a
#' cutoff point.
#' @param objective vector of classes, should be binary 0/1 or T/F
#' @param lever numeric vector to
#' @param range range of values used as cutoffs
#' @return vector of percentages by class
#' @export

FindCutoff<- function(objective, lever, range){
    sapply(range, function(x) sum(subset(objective,lever<x))/sum(lever<x))
}
