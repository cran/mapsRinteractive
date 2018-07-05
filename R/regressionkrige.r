#' @title Regression kriging

#' @description Regression kriging for MRI

#' @param m a list of all objects to be used.

#' @inheritParams mri

#' @keywords internal

#' #fix no visible binding for global variables
if(getRversion() >= "2.15.1") globalVariables(c('adaptation.area.sp',  'prj'))

##define function
regkrig<-function(m,md, rg, ng){

  #unlist objects
  list.a<-m
  for (h in 1:length(list.a)){
    n<-names(list.a)[h]
    g<-unlist(list.a[h], use.names=F, recursive = T)
    if(is.list(g))g<-g[[1]]
    if(!is.null(n)|length(n)==0)assign(x=n, value= g)
  }

  #cross validate
  for (i in 1:nrow(pts.sp)){

    ##split dataset
    cal<-pts.sp[-i,]
    val<-pts.sp[i,]

    ##parameterize and use linear regression model
    lin.mod<-lm(obs~map, data= cal)
    cal$reg_pred<-predict(object=lin.mod, newdata=cal)

    ##compute residuals
    cal$regres<-cal$reg_pred-cal$obs

    ##parameterize standardized semivariogram model
    if(is.null(rg)) rg<-0.5*sqrt(area(adaptation.area.sp))
    sill<-var(cal$regres, na.rm=T)
    mod<-    vgm(  psill = (1-ng)*sill,
                   model= md,
                   range = rg,
                   nugget= ng*sill
    )

    ##predict for validation point location
    reg_pred<-predict(object=lin.mod, newdata=val)
    regres_pred<-krige(res~1, locations=cal, newdata=val, model = mod)
    regres_pred<-as.data.frame(regres_pred)['var1.pred']
    pts.sp[i,'regkrig_cv']<-reg_pred-regres_pred
  }

  #regression kriging to raster
  ##parameterize and use linear regression model
  lin.mod<-lm(obs~map, data= pts.sp)
  pts.sp$reg_pred<-predict(object=lin.mod, newdata=pts.sp)

  ##compute residuals
  pts.sp$regres<-pts.sp$reg_pred-pts.sp$obs

  ##parameterize standardized semivariogram model
  sill<-var(pts.sp$regres, na.rm=T)
  mod<-    vgm(  psill = (1-ng)*sill,
                 model= md,
                 range = rg,
                 nugget= ng*sill
  )

  ##predict for validation point location
  proj4string(map.r) = CRS(paste0(prj))
  reg_pred.r<-raster::predict( object=map.r, model=lin.mod)
  krige.mod<-gstat(formula=res~1, locations=cal, model=mod)
  regres_pred.r<-interpolate(map.r, krige.mod,  fun=predict)
  regkrig.r=reg_pred.r-regres_pred.r

  #give feedback
  print('Regression kriging ready')

  #clean up
  pts.sp<-pts.sp[,c('obs', 'map', 'ordkrig_cv', 'reskrig_cv', 'regkrig_cv')]

  #return objects
  return(list(mget(c('regkrig.r', names(list.a))))[[1]])
}
