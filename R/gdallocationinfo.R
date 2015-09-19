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
#' @param simplify Logical. If \code{TRUE}, then if all files referred to in 
#'   \code{srcfile} are single band rasters, the sampled values will be
#'   simplified to a matrix.
#' @param sp Logical. If \code{TRUE}, a \code{SpatialPointsDataFrame} will be
#'   returned, otherwise a \code{data.frame} will be returned. If \code{pts} is
#'   a \code{SpatialPoints*} object and has a non-\code{NA} CRS, then if 
#'   \code{sp} is \code{TRUE}, the returned object will have that same CRS 
#'   assigned.
#' @return If only one file is queried, then a matrix is returned, with columns
#'   corresponding to bands of the file. If \code{simplify} is \code{TRUE} and
#'   all files are single band, the returned value will be a \code{matrix} with
#'   one column for each file, and rows giving the values of cells corresponding
#'   to the coordinates given by \code{x} and \code{y}. If \code{simplify} is
#'   \code{FALSE}, or if one or more files is multiband, the returned value is a
#'   list of matrices, with the columns of each matrix corresponding to the
#'   bands of the file. Further, if \code{sp} is \code{TRUE}, data will be
#'   returned as \code{SpatialPointsDataFrames}.
#' @keywords spatial, raster, points, query, extract, sample
#' @importFrom sp proj4string coordinates SpatialPointsDataFrame CRS
#' @importFrom rgdal GDALinfo
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
gdallocationinfo <- function(srcfile, pts, simplify=TRUE, sp=FALSE) {
  tryCatch(suppressWarnings(system('gdallocationinfo', intern=TRUE)), 
           error=function(e) {
             stop('GDAL is not installed, ', 
                  'or gdallocationinfo is not available on PATH.')
           })
  stopifnot(all(file.exists(srcfile)))
  if(is(pts, 'SpatialPoints')) {
    p4s <- proj4string(pts)
    pts <- coordinates(pts)
  } else {
    p4s <- NA_character_
  }
  xy <- do.call(paste, as.data.frame(pts))
  #nms <- sapply(srcfile, function(x) names(raster(x)))
  # nms <- names(stack(srcfile))
  vals <- setNames(lapply(srcfile, function(f) {
    meta <- attr(GDALinfo(f, returnStats=FALSE, returnRAT=FALSE, 
                          returnColorTable=FALSE), 'df')
    nbands <- nrow(meta)
    nodata <- ifelse(meta$hasNoDataValue, meta$NoDataValue, NA)
    if(any(is.na(nodata))) {
      warning('NoData value not identified for one or more bands of ', f,  
              '. Interpret values accordingly. See ?nodata.', call. = FALSE)
    }
    message(sprintf('Querying %sraster data: %s', 
                    ifelse(nbands > 1, 'multiband ', ''), f))
    v <- system(sprintf('gdallocationinfo -valonly "%s" -geoloc', f), 
                input=xy, intern=TRUE)
    v <- as.numeric(v)
    v <- ifelse(!is.na(nodata) & v==nodata, NA, v)
    v <- t(matrix(v, nrow=nbands))
  }), srcfile)
  
  if(isTRUE(simplify) && all(sapply(vals, ncol)==1)) {
    vals <- do.call(cbind, vals)
    colnames(vals) <- srcfile
    vals
  }
  if(is.list(vals) && length(vals)==1) {
    vals <- vals[[1]]
  }
  if(isTRUE(sp)) {
    if(!is.list(vals)) vals <- list(vals)
    spdf <- lapply(vals, function(x) {
      SpatialPointsDataFrame(pts, as.data.frame(vals), proj4string=CRS(p4s))  
    })
    if(length(spdf)==1) spdf[[1]] else spdf
  } else {
    vals
  }
}