#' Training dataset
#'
#' This dataset is the product of Exploratory Analysis where
#' he raw training dataset was downloaded and processed. Call
#' \code{vignette("ExploratoryAnalysis")} to see the documentation.
#' @name train
#' @docType data
#' @format dataframe with 4040 rows.  There are 17 primary
#' variables plus additional variables to cound the number of
#' times a word occurs in a request.
#' \itemize{
#' \item giver_username_if_known
#' \item account_age_in_days
#' \item days_since_first_post_on_raop
#' \item number_of_comments
#' \item number_of_comments_in_raop
#' \item number_of_posts
#' \item number_of_posts_on_raop
#' \item number_of_subreddits
#' \item upvotes_plus_downvotes
#' \item received_pizza
#' \item Year
#' \item weekday
#' \item Image
#' \item Acct.Age
#' \item BnRAOP
#' \item Words
#' \item Word.bin
#' }
NULL

#' Test dataset
#'
#' This dataset is the product of Exploratory Analysis where
#' the raw training dataset was downloaded and processed. Call
#' \code{vignette("ExploratoryAnalysis")} to see the documentation.
#' @docType data
#' @name test
#' @format dataframe with 4040 rows.  There are 17 primary
#' variables plus additional variables to cound the number of
#' times a word occurs in a request.
#' \itemize{
#' \item giver_username_if_known
#' \item request_id
#' \item account_age_in_days
#' \item days_since_first_post_on_raop
#' \item number_of_comments
#' \item number_of_comments_in_raop
#' \item number_of_posts
#' \item number_of_posts_on_raop
#' \item number_of_subreddits
#' \item upvotes_plus_downvotes
#' \item Year
#' \item weekday
#' \item Image
#' \item Acct.Age
#' \item BnRAOP
#' \item Words
#' \item Word.bin
#' }
NULL


#' Random Acts of Pizza.
#'
#' To document the process used to compete in Kaggle's Random Acts of Pizza
#' competition. The raw data is downloaded from Dropbox and the analysis can be
#' seen though vignettes. The process uses a mix of time and text analyses.
#' Read the vingettes to follow the workflow.
#' @name RandomActsofPizza
#' @docType package
#' @author Ryan Kuhn, CPA
#' @import tm
#' @import utils
#' @importFrom methods "hasArg"
#' @examples
#' vignette("ExploratoryAnalysis")
#' vignette("ModelBuilding")
#'
NULL

#' Extraneous data points
#'
#' This dataset is the product of Exploratory Analysis where
#' the raw training dataset was downloaded and processed. Call
#' \code{vignette("ExploratoryAnalysis")} to see the documentation.
#' @docType data
#' @name justInCase
NULL
