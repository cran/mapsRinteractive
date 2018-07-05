#' @title Evaluation

#' @description Evaluation for MRI

#' @param m a list of all objects to be used.

#' @keywords internal

#' #fix no visible binding for global variables
if(getRversion() >= "2.15.1") globalVariables('pts.sp')

##define function
evaluate<-function(m){

  #unlist objects
  list.a<-m
  for (h in 1:length(list.a)){
    n<-names(list.a)[h]
    g<-unlist(list.a[h], use.names=F, recursive = T)
    if(is.list(g))g<-g[[1]]
    if(!is.null(n)|length(n)==0)assign(x=n, value= g)
  }

  #create data.frame
  evaluation<-matrix(nrow=4, ncol=6, data=NA)
  evaluation<-as.data.frame(evaluation)
  names(evaluation)<-c('Measure', 'MAE', 'RMSE', 'E', 'r2', 'ME')
  evaluation$Measure<-c('map', 'ordkrig_cv', 'reskrig_cv', 'regkrig_cv')

  #compute evalauation measures
  pts.df<-as.data.frame(pts.sp)
  for (j in evaluation$Measure){
    o<-pts.df[,'obs']
    p<-pts.df[,j]
    evaluation[evaluation$Measure==j,'MAE']<-mae(o, p)
    evaluation[evaluation$Measure==j,'RMSE']<-rmse(o, p)
    evaluation[evaluation$Measure==j,'E']<-E(o, p)
    evaluation[evaluation$Measure==j,'r2']<-r2(o, p)
    evaluation[evaluation$Measure==j,'ME']<-me(o, p)
  }

  #give feedback
  print('Evaluation measures have been computed')

  #return objects
  return(list(mget(c('evaluation', names(list.a))))[[1]])
}
