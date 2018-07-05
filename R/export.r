#' @title Export

#' @description Export data for MRI

#' @param m a list of all objects to be used.

#' @inheritParams mri

#' @keywords internal

#' #fix no visible binding for global variables
if(getRversion() >= "2.15.1") globalVariables(names=c('map.r', 'ordkrig.r',
  'reskrig.r','regkrig.r', 'adaptation.area.sp',  'pts.sp', 'evaluation',
  'out.dec', 'out.sep','feedback'), add=T)

##define function
export<-function(m,out.folder, out.prefix, rst.data.type, out.dec, out.sep){

  #unlist objects
  list.a<-m
  for (h in 1:length(list.a)){
    n<-names(list.a)[h]
    g<-unlist(list.a[h], use.names=F, recursive = T)
    if(is.list(g))g<-g[[1]]
    if(!is.null(n)|length(n)==0)assign(x=n, value= g)
  }

  #prepare data to export/return
  all_maps.r<-stack(map.r, ordkrig.r, reskrig.r, regkrig.r)
  all_maps.r[is.na(map.r)]<-NA
  names(all_maps.r)<-c('map', 'ordkrig', 'reskrig', 'regkrig')
  mapped.area.sp<-adaptation.area.sp
  used.pts.sp<-pts.sp

  feedback<-data.frame(feedback[1:5], feedback[6:10])
  names(feedback)<-c('Test', 'Value')

  evaluation<-data.frame(evaluation[1:4],
                       evaluation[5:8],
                       evaluation[9:12],
                       evaluation[13:16],
                       evaluation[17:20],
                       evaluation[21:24]
                       )
 names(evaluation)<-c('Measure', 'MAE', 'RMSE', 'E', 'r2', 'ME')

  #create output folder if it is specidfied but does not exist
  if(!is.null(out.folder)) if(!dir.exists(out.folder)) dir.create(out.folder)

  #export data
  if (!is.null(out.folder)){

    ##create path for exporting including filename prefix
    out.path<-paste0(out.folder,'/',out.prefix)

    ##shapefiles
    shapefile(mapped.area.sp, filename = paste0(out.path,'mapped_area.shp'),
              overwrite=T)
    shapefile(used.pts.sp, filename = paste0(out.path,'used_samples.shp'),
              overwrite=T)

    ##text files
    write.table(evaluation, file=paste0(out.path,'evaluation.txt'),
                dec= out.dec, sep = out.sep,
                row.names = F, col.names=T)
    write.table(feedback, file=paste0(out.path,'feedback.txt'),
                dec= out.dec, sep = out.sep,
                row.names = F, col.names=T)

    ##raster files
    writeRaster(x=all_maps.r, filename= out.path, format ='GTiff',
                bylayer=T, suffix='names', datatype=rst.data.type, overwrite=TRUE)

    #give feedback
    print('Your output data have been exported. Please
          check the specified output folder')
    }


  print('MRI is ready')

  #return objects
  return(list(mget(c( 'all_maps.r', 'mapped.area.sp', 'used.pts.sp', 'evaluation', 'feedback' )))[[1]])
  }
