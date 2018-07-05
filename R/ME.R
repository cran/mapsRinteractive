#' @title Mean Error (ME)
#' @description Calculates the mean error (ME) from observed and predicted values.
#' @param observed a numeric vector of observed values
#' @param predicted a numeric vector of predicted values. The length shall be the same as for observed.
#' @return the mean  error (ME) calculated from the observed and the predicted values.
#' @details me = bias = mean(observed - predicted)
#' @examples
#' o<-1:5
#' p<-c(2,2,4,3,5)
#' me(observed=o, predicted=p)
#' @export
me<-function(observed, predicted) return(mean(observed-predicted))
