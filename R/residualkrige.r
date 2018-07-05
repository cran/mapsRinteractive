#' @title Residualkriging

#' @description Residual kriging for MRI

#' @param m a list of all objects to be used.

#' @inheritParams mri

#' @keywords internal

#' #fix no visible binding for global variables
if(getRversion() >= "2.15.1") globalVariables(c('adaptation.area.sp',  'prj'))

##define function
reskrig<-function(m, md, rg, ng){

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

    ##compute residuals
    cal$res<-cal$map-cal$obs

    ##parameterize standardized semivariogram model
    if(is.null(rg)) rg<-0.5*sqrt(area(adaptation.area.sp))
    sill<-var(cal$res, na.rm=T)
    mod<-    vgm(  psill = (1-ng)*sill,
                   model= md,
                   range = rg,
                   nugget= ng*sill
    )

    ##predict for validation point location
    res_pred<-krige(res~1, locations=cal, newdata=val, model = mod)
    res_pred<-as.data.frame(res_pred)['var1.pred']
    pts.sp[i,'reskrig_cv']<-val$map-res_pred
  }

  #residual kriging to raster
  ##compute residuals
  pts.sp$res<-pts.sp$map-pts.sp$obs

  ##parameterize standardized semivariogram model
  sill<-var(pts.sp$res, na.rm=T)
  mod<-    vgm(  psill = (1-ng)*sill,
                 model= md,
                 range = rg,
                 nugget= ng*sill
  )

  ##predict for validation point location
  proj4string(map.r) = CRS(paste0(prj))
  krige.mod<-gstat(formula=res~1, locations=cal, model=mod)
  res_pred.r<-interpolate(map.r, krige.mod,  fun=predict)
  reskrig.r=map.r-res_pred.r

  #give feedback
  print('Residual kriging ready')

  #return objects
  return(list(mget(c('reskrig.r', names(list.a))))[[1]])
}
