#' Cell-wise raster standard deviation
#'
#' Calculate a raster layer whose cells' values are the standard deviations
#' across the bands of a single raster, or across multiple rasters.
#'
#' @param infile A file path to a single, multiband raster file, or a vector of
#'   paths to multiple raster files. If a vector of paths is provided, then if
#'   any of the rasters have multiple bands, the first band will be used. A
#'   maximum of 26 bands or files can be provided to \code{infile}.
#' @param outfile Optional. A path to the desired output raster file, which will
#'   be created if it doesn't already exist (though the containing directory
#'   must exist). If not provided and \code{return_raster} is \code{TRUE}, a
#'   temporary output file will be created.
#' @param return_raster Logical. Should the output raster be read back in to R
#'   as a \code{Raster} object?
#' @param quiet Logical. Should GDAL output messages be suppressed?
#' @return Unless \code{return_raster} is \code{TRUE}, \code{gdal_sd} creates
#'   the file specified at \code{outfile} but returns \code{NULL}, invisibly, to
#'   R. If \code{return_raster} is \code{TRUE}, the standard deviation
#'   \code{Raster} object is returned to R.
#' @details The quantity calculated is the corrected sample standard deviation,
#'   calculated as \eqn{sqrt(sum((x - mean(x))^2)/(length(x)-1))}.
#' @importFrom raster raster
#' @importFrom rgdal GDALinfo
#' @export
#' @examples
#' library(raster)
#' s <- stack(replicate(5, raster(matrix(rnorm(100), ncol=10))))
#' 
#' # Standard deviation across multiple files
#' writeRaster(s, ff <- paste0(tempfile(), 1:5, '.tif'), bylayer=TRUE)
#' gdal_sd(ff)
#' 
#' # Standard deviation across multiple bands 
#' writeRaster(s, f <- tempfile(fileext='.tif'))
#' gdal_sd(f)
gdal_sd <- function(infile, outfile, return_raster=TRUE, quiet=TRUE) {
  gdal_calc <- Sys.which('gdal_calc.py')
  if(gdal_calc=='') stop('gdal_calc.py not found on system.')
  if(missing(outfile) && return_raster) outfile <- tempfile(fileext='.tif')
  if(file.exists(outfile)) stop('outfile already exists.')
  nbands <- sapply(infile, function(x) nrow(attr(GDALinfo(x), 'df')))
  if(length(infile) > 26 || nbands > 26) stop('Maximum number of inputs is 26.')
  if(length(nbands) > 1 & any(nbands > 1)) 
    warning('One or more rasters have multiple bands. First band used.')
  if(length(infile)==1) {
    inputs <- paste0('-', LETTERS[seq_len(nbands)], ' ', infile, ' --', 
                     LETTERS[seq_len(nbands)], '_band ', seq_len(nbands), 
                     collapse=' ')
    n <- nbands
  } else {
    inputs <- paste0('-', LETTERS[seq_along(nbands)], ' ', infile, ' --', 
                     LETTERS[seq_along(nbands)], '_band 1', collapse=' ')
    n <- length(infile)
  }
  message('Calculating standard deviation and writing to ', basename(outfile))
  cmd <- 'python %s %s --outfile=%s --calc="std([%s], 0, ddof=1)"'
  out <- system(
    sprintf(cmd, gdal_calc, inputs, outfile, 
            paste0(LETTERS[seq_len(n)], collapse=',')),
    show.output.on.console=!quiet, intern=TRUE
  )
  if(grepl('Error', out)) stop(out, call.=FALSE)
  if(return_raster) {
    raster::raster(outfile)
  } else {
    invisible(NULL)
  }
}