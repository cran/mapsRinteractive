#' @title Mean Absolute Error (MAE)
#' @description Calculates the mean absolute error (MAE) from observed and predicted values.
#' @param observed a numeric vector of observed values
#' @param predicted a numeric vector of predicted values. The length shall be the same as for observed.
#' @return the mean absolute error (MAE) calculated from the observed and the predicted values.
#' @details mae = mean(abs(observed - predicted))
#' @examples
#' o<-1:5
#' p<-c(2,2,4,3,5)
#' mae(observed=o, predicted=p)
#' @export
mae<-function(observed, predicted) return(mean(abs(observed-predicted)))












