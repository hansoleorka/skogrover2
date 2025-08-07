#' Clip lidar point clouds according to PointSourceID
#' 
#' Clip lidar point clouds according to polygons with PointSourceID 
#' 
#' @param cluster las-object
#' @param flightlines sf polygon object with attribute names PointSourceID identical to the las-object.
#' @references Geovekstforum, n.d. Spesifikasjon Laserdata til Skogbruksformål, (Laser-Skog) (No. Versjon 1.0 (2011-03-01)).
#' @note Date: Mar 21, 2022
#' @name 
#' @export
#' @examples 
#' ctg <- catalog("/dz/")
#' opt_output_files(ctg) <- "/dz_clip/{ORIGINALFILENAME}_filtered"
#' opt_filter(ctg) <- "-drop_class 7 17 24"
#' ctg_filtered <- catalog_apply(ctg,clipPointSource,flightlines)
clipPointSourceID <- function(cluster,flighlines){
  require(lidR)
  require(sf)
  require(dplyr)
  fsin <- unique(flightlines$PointSourceID)
  las <- readLAS(cluster)
  if(nrow(las)>0){
    ps <- unique(las$PointSourceID)
    print(ps)
    ps <- ps[ps %in% fsin]
    message(ps)
    output <- c()
    if(length(ps) > 0){
      k <- 0
      for(i in c(1:length(ps))){
        fl <- flightlines %>% filter(PointSourceID == ps[i]) 
        las2 <- clip_roi(las,fl)
        if(is.null(las2) != TRUE){
          las2 <- filter_poi(las2,PointSourceID == ps[i])
          k <- k + 1
          
          if(k == 1){output <- las2}
          if(k >1){output <- rbind(output,las2)
          }
        }
      }
    }  
  }
  return(output)
}