#' @title Nash-Sutcliffe Modelling Efficiency (E)
#' @description Calculates the Nash-Sutcliffe modelling efficiency (E) from observed and predicted values.
#' @param observed a numeric vector of observed values
#' @param predicted a numeric vector of predicted values. The length shall be the same as for observed.
#' @return Nash-Sutcliffe modelling efficiency (E) calculated from observed and predicted values.
#' @details E = 1 - sum(observed - predicted)/sum(observed - mean (observed))
#' @references Nash, J. E., & Sutcliffe, J. V. (1970). River flow forecasting through conceptual models part I—A discussion of principles. Journal of hydrology, 10(3), 282-290.
#' @examples
#' o<-1:5
#' p<-c(2,2,4,3,5)
#' E(observed=o, predicted=p)
#' @export
E<-function(observed, predicted) return(1-(sum((observed-predicted)^2)/sum((observed-mean(observed))^2)))
