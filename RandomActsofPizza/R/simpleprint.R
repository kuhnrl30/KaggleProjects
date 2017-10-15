#' Simple print function
#'
#' Prints the contents of a dataframe without column or row names
#' @param df dataframe
#' @export
simpleprint <- function(df){
  write.table(format(df, justify="right"),
              row.names=F,
              col.names=F,
              quote=F)
  }
