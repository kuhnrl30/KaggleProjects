#' To merge the models into singe predictions
#'
#' Takes a weighted average of the prediction scores to create and ensembled model.
#'
#' @param x matrix or dataframe of predictions from singe models
#' @param weights vector of weights to apply to the dataframe
#' @return single prediction for each observation
#' @export
#' @examples
#' Preds<- cbind(1:5,6:10)
#' MergeModels(Preds, c(.4,.6)) # different weights
#' MergeModels(Preds) # Equal weights
#'
MergeModels<- function(x, weights=NULL){
    if(hasArg(weights)){
        if(!length(weights)==ncol(x)){
            stop("Length of weights does not match the number of prediction columns")
        }
    }

    if (is.null(weights)){
        weights<- 1/ncol(x)
    }

    if(!sum(weights)==1){
        warning("Weights do not sum to 1")
    }
    weightedPred<-x*weights
    rowSums(weightedPred,na.rm=T)
    }
