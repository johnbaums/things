#' Query raster files for their NoData values
#'
#' Return the NoData value associated with one or more raster files.
#'
#' @param srcfile A character vector of file paths to the raster files to be 
#'   queried.
#' @param simplify Logical. If \code{TRUE}, then when all files referred to in
#'   \code{srcfile} have the same number of bands, the returned value will be
#'   simplified to an array (if \code{srcfile} refers to more than one file), or
#'   an atomic vector (if \code{srcfile} refers to a single file).
#' @return A list with as many elements as the number of files specified in 
#'   \code{srcfile}. Each list element contains a single vector whose values are
#'   the NoData values for each band of the corresponding raster file. If GDAL
#'   is unable to identify the NoData value, \code{NA} will be returned.
#' @keywords spatial, raster, query
#' @importFrom rgdal GDALinfo
#' @export
#' @examples
#' library(raster)
#' r <- raster(matrix(runif(100), ncol=10))
#' NAvalue(r) <- -9999
#' writeRaster(r, f <- tempfile(fileext='.tif'))
#' nodata(f)
#' nodata(c(f, f, f))
nodata <- function(srcfile, simplify=TRUE) {
  v <- setNames(lapply(srcfile, function(f) {
    meta <- attr(GDALinfo(f, returnStats=FALSE, returnRAT=FALSE, 
                          returnColorTable=FALSE), 'df')
    ifelse(meta$hasNoDataValue, meta$NoDataValue, NA)   
  }), srcfile)
  if(isTRUE(simplify)) drop(simplify2array(v)) else v
}
