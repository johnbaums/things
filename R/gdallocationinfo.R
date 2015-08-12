#' Query raster files at points
#'
#' Return cell values for one or more raster files, at a given point or set of 
#' points. Requires that GDAL is installed and avilable to \code{system} (i.e.
#' listed in the PATH environmental variable).
#'
#' @param srcfile A character vector of file paths to the raster files to be 
#'   queried.
#' @param pts A \code{matrix}, \code{data.frame}, or \code{SpatialPoints*} 
#'   object giving the x and y coordinates of a point (or points), given in the 
#'   coordinate reference system of the rasters indicated at \code{srcfile}.
#' @param sp Logical. If \code{TRUE}, a \code{SpatialPointsDataFrame} will be
#'   returned, otherwise a \code{data.frame} will be returned. If \code{pts} is
#'   a \code{SpatialPoints*} object and has a non-\code{NA} CRS, then if 
#'   \code{sp} is \code{TRUE}, the returned object will have that same CRS 
#'   assigned.
#' @return A \code{data.frame} with one column per element of \code{srcfile}, 
#'   with rows giving the values of cells corresponding to the coordinates given
#'   by \code{x} and \code{y}.
#' @keywords spatial, raster, points, query, extract, sample
#' @importFrom sp proj4string coordinates SpatialPointsDataFrame CRS
#' @export
#' @examples
#' library(raster)
#' r <- raster(matrix(runif(1.6e7), ncol=4000))
#' r[sample(ncell(r), 10000000)] <- NA
#' writeRaster(r, f <- tempfile(fileext='.tif'))
#' file.info(f)
#' xy <- data.frame(x=runif(10000), y=runif(10000))
#' 
#' # Querying a single file at xy
#' vals <- gdallocationinfo(f, pts=xy)
#' 
#' # Querying multiple files (here identical) at xy:
#' vals2 <- gdallocationinfo(c(f, f, f), pts=xy)
#' 
#' # Returning as a SpatialPointsDataFrame
#' vals2 <- gdallocationinfo(c(f, f, f), pts=xy, sp=TRUE)
gdallocationinfo <- function(srcfile, pts, sp=FALSE) {
  tryCatch(suppressWarnings(system('gdallocationinfo', intern=TRUE)), 
           error=function(e) {
             stop('GDAL is not installed, or gdallocationinfo is not available on PATH.')
           })
  stopifnot(all(file.exists(srcfile)))
  if(is(pts, 'SpatialPoints')) {
    p4s <- proj4string(pts)
    pts <- coordinates(pts)
  } else {
    p4s <- NA_character_
  }
  xy <- do.call(paste, as.data.frame(pts))
  nms <- names(stack(srcfile))
  vals <- setNames(data.frame(lapply(srcfile, function(f) {
    nodata <- gsub('^.*=', '', 
                   grep('NoData', system(sprintf('gdalinfo "%s"', f), 
                                         intern=TRUE), val=TRUE))
    nodata <- as.numeric(nodata)
    if(is.na(nodata)) {
      warning('NoData value not identified. Interpret extracted values accordingly.')
    }
    message('Querying layer: ', f)
    message('NoData value identified as: ', 
            ifelse(is.na(nodata), 'unknown', nodata))
    v <- system(sprintf('gdallocationinfo -valonly "%s" -geoloc', f), 
                input=xy, intern=TRUE)
    v <- as.numeric(v)
    if(!is.na(nodata)) v[v==nodata] <- NA
    v
  })), nms)
  
  if(isTRUE(sp)) {
    SpatialPointsDataFrame(pts, vals, proj4string=CRS(p4s))
  } else {
    vals
  }
}