#' @title Import

#' @description Imports data for MRI

#' @inheritParams mri

#' @keywords internal

#' #fix no visible binding for global variables
if(getRversion() >= "2.15.1") globalVariables('area.sp')

#define function
import<-function(rst.r, rst.file, pts.shp, pts.spdf, pts.df, pts.txt, pts.attr,
                 pts.x, pts.y, pts.dec,pts.sep, area.spdf, area.shp,
                 epsg, rst.res, b, rg){

  #create text string for projection definition
  prj<-paste0('+init=epsg:',epsg)

  #import raster data
  if(!is.null(rst.file)) map.r<-raster(rst.file)
  if(!is.null(rst.r)) map.r<-rst.r
  proj4string(map.r) <- sp::CRS(prj)
  names(map.r)<-'map'

  #import area shape file
  if(!is.null(area.shp)){
    poly.sp<-shapefile(area.shp) #ignore waring
    poly.sp<-spTransform(x= poly.sp)
    proj4string(poly.sp) <- sp::CRS(prj)
  }

  if(!is.null(area.spdf)) poly.sp<-area.sp

  #clip raster to the rectangular extent of the area polygon
  if(exists('poly.sp')) map.r<-crop (x=map.r, y= extent(poly.sp))

  #resample raster
  if(!is.null(rst.res)){
  template.r<-raster(ext = extent(map.r), resolution = rst.res)
  map.r<-resample(x=map.r, y=template.r, method='bilinear')
  }

  #import point location data
  if(!is.null(pts.txt))pts<-read.csv(pts.txt, sep=pts.sep, stringsAsFactors=F, dec=pts.dec)
  if(!is.null(pts.df)) pts<-pts.df
  if(!is.null(pts.shp)) pts.sp<-shapefile(pts.shp)
  if(!is.null(pts.spdf)) pts<-data.frame(pts.spdf@coords, pts.spdf@data)

  names(pts)[names(pts)==pts.attr]<-'obs'
  names(pts)[names(pts)==pts.x]<-'x'
  names(pts)[names(pts)==pts.y]<-'y'
  pts$obs[pts$obs<=0]<-NA
  pts<-pts[complete.cases(c('x','y','obs')),]
  pts<-as.data.frame(pts)
  xy<-as.data.frame(pts[,c('x', 'y')])
  pts.sp<-SpatialPointsDataFrame(coords=xy,data=pts)
  proj4string(pts.sp) <- sp::CRS(prj)

  #create feedback
  feedback<-data.frame(matrix(nrow=4, ncol=2))
  feedback[1,1]<-'No of point observation data'
  feedback[1,2]<-nrow(pts)
  feedback[2,1]<-'No of NA point observation data'
  feedback[2,2]<-length(pts$obs[!is.na(pts$obs)])

  #set 0 values to NA
  map.r[map.r==0]<-NA
  pts.sp@data[pts.sp@data==0]<-NA

  #clip spatial data to the intersect of intersect of the area and the buffered
  #point dataset the buffer used = max distanse to nearest point * 1.5
  if(is.null(b)) b<-1000000

  pts.area.sp<-gBuffer(spgeom = pts.sp, byid=F, width = b)
  if(exists('poly.sp')) adaptation.area.sp<-intersect(pts.area.sp,poly.sp)
  if(!exists('poly.sp')) adaptation.area.sp<-pts.area.sp
  map.r<- mask(crop(map.r, extent(adaptation.area.sp)), adaptation.area.sp)
  pts.sp<-pts.sp[adaptation.area.sp,]
  #create feedback
  feedback[3,1]<-'Number of point observation data within the mapping area'
  feedback[3,2]<- nrow(pts.sp)

  #omitpoint locations where map appribute is NA or where uploaded point
  #observation data is NA or 0
  pts.sp<-extract(map.r, y=pts.sp, method= 'simple', sp=T)

  #create feedback
  feedback[4,1]<-'Number of point locations without map data'
  feedback[4,2]<-sum(is.na(pts.sp$map))

  #omit samples
  cc<-complete.cases(as.data.frame(pts.sp[,c('obs','map')]));
  pts.sp<-pts.sp[cc,c('obs','map')]

  #tests for user feedback
  feedback[5,1]<-'Number of used point observation data'
  feedback[5,2]<-nrow(pts.sp) ## no of used samples

  #give feedback
  print('Your data have been imported')

  #compute variogram range if not specified
  if(is.null(rg)) rg<-0.5*sqrt(area(adaptation.area.sp))

  #return objects
  return(list(mget(c('map.r', 'pts.sp', 'feedback','adaptation.area.sp', 'prj', 'rg')))[[1]])
}

