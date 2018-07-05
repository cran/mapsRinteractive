#' @title Ordinary kriging

#' @description Ordinary kriging for MRI

#' @param m a list of all objects to be used.

#' @inheritParams mri

#' @keywords internal

#' #fix no visible binding for global variables
if(getRversion() >= "2.15.1") globalVariables(c('adaptation.area.sp',  'prj'))

##define function
ordkrig<-function(m,md, rg, ng){

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

    ##parameterize standardized semivariogram model
    if(is.null(rg)) rg<-0.5*sqrt(raster::area(adaptation.area.sp))
    sill<-var(cal$obs, na.rm=T)
    mod<-    vgm(  psill = (1-ng)*sill,
                   model= md,
                   range = rg,
                   nugget= ng*sill
    )

    ##predict for validation point location
    obs_pred<-krige(obs~1, locations=cal, newdata=val, model = mod)
    obs_pred<-as.data.frame(obs_pred)['var1.pred']
    pts.sp[i,'ordkrig_cv']<-obs_pred
  }

  #ordinary kriging to raster
  ##parameterize standardized semivariogram model
  sill<-var(pts.sp$obs, na.rm=T)
  mod<-    vgm(  psill = (1-ng)*sill,
                 model= md,
                 range = rg,
                 nugget= ng*sill
  )

  ##predict
  proj4string(map.r) = CRS(paste0(prj))
  krige.mod<-gstat(formula=obs~1, locations=cal, model=mod)
  ordkrig.r<-interpolate(map.r, krige.mod,  fun=predict)

  #give feedback
  print('Kriging ready')

  #return objects
  return(list(mget(c('ordkrig.r', names(list.a))))[[1]])
}
