#' Linear interpolation between pairs of rasters
#'
#' Linearly interpolate cell values between pairs of raster grids, at specified
#' locations (e.g. "times"). Extrapolation to times beyond the last raster in
#' the series is also possible.
#'
#' @param s A \code{RasterStack} or \code{RasterBrick} containing the rasters
#'   between which interpolation will be performed.
#' @param z_in A numeric vector indicating the positions (e.g., times) 
#'   associated with layers in \code{s} (in the same order as the layers of
#'   \code{s} - see \code{names(s)}).
#' @param z_out  a numeric vector that indicates the times to be interpolated to
#'   (if \code{z_out} extends beyond the latest time slice in \code{z_in}, it
#'   will be extrapolated using the rate of change from the period between the
#'   last and second-to-last time in \code{z_in}).
#' @param outdir The directory to which files will be written (recursively
#'   created if not already in existence) (character string).
#' @param prefix The output files will have pattern \code{prefix_x.tif}, where
#'   \code{x} is the position (timestep) (potentially multiple digits), and
#'   \code{prefix} is a string that you specify here (character string).
#' @param progress Logical. Show progress bar?
#' @param write_change Logical. Should the change grids (which define the change
#'   in cells' values per timestep between each pair of time slices) be written
#'   to \code{outdir}.
#' @param return_raster Logical. Should the interpolated grids (at timesteps
#'   \code{z_out}) be returned as a \code{RasterStack}?
#' @param ... Additional arguments passed to writeRaster.
#' @return Unless \code{return_raster} is \code{TRUE}, \code{interpolate}
#'   creates files in \code{outdir}, but returns \code{NULL}, invisibly, to R.
#'   If \code{return_raster} is \code{TRUE}, a \code{RasterStack} of
#'   interpolated \code{Raster} objects is returned to R.
#' @details Extrapolation below the earliest grid in \code{s} is currently not 
#'   implemented. Interpolation is linear between pairs of time slices existing 
#'   in \code{z_in}, and extrapolation beyond the last slice in \code{s}, if
#'   requested, follows the trend observed between the last and second-to-last
#'   slice in \code{z_in}.
#' @importFrom raster writeRaster nlayers stack overlay
#' @export
#' @examples
#' library(raster)
#' r1 <- raster(matrix(rnorm(100), ncol=10))
#' r2 <- raster(matrix(rnorm(100, mean=11), ncol=10))
#' s <- stack(r1, r2)
#' s_interp <- interpolateTemporal(s, c(2001, 2011), 2001:2020, tempdir(), 
#'                                 'example', returnstack=TRUE)
#' plot(s_interp, zlim=range(values(s_interp)))
interpolate <- 
  function(s, z_in, z_out, outdir, prefix, progress=TRUE, write_change=TRUE, 
           return_stack=FALSE, ...) {
    if(raster::nlayers(s) != length(xin)) 
      stop('Length of xin must equal the number of layers in s.')
    if(raster::nlayers(s) < 2) 
      stop('stack s must have at least 2 layers.')
    if(!all(findInterval(xout, range(xin), rightmost.closed=TRUE) == 1)) {
      if(any(xout < min(xin))) {
        stop('This function does not extrapolate backwards (i.e. below the ',
             'earliest element in xin). All elements of xout must be greater ',
             'than min(xin).')
      } else {      
        warning('Some values of xout require extrapolation beyond the range of ',
                'xin.\nThe rate of change for extrapolation is assumed to be ',
                'equal to that for the period between the last and ',
                'second-to-last elements of xin (after temporal ordering).')
      }
    }
    outdir <- normalizePath(sub('/$|\\\\$', '', outdir), winslash='/', 
                            mustWork=FALSE)
    if(!file.exists(outdir)) dir.create(outdir, recursive=TRUE)
    xout <- unique(xout)
    if(is.unsorted(xin)) {
      s <- s[[order(xin)]]
      xin <- sort(xin)
    }
    len <- diff(xin)
    base <- findInterval(xout, xin)
    lower <- unique(base[base < nlayers(s)])
    s.change <- raster::stack(
      sapply(if(length(lower) > 0) lower else raster::nlayers(s) - 1, 
             function(x) {
               message(
                 sprintf('Calculating change grid for %s to %s.', 
                         xin[x], xin[x+1])
               )
               raster::overlay(
                 s[[x]], s[[x+1]], fun=function(x1, x2) (x2-x1)/len[x],
                 filename=ifelse(
                   writechange, 
                   file.path(outdir, sprintf('changegrid_%s_%s', 
                                             xin[x], xin[x+1])), 
                   ''), recycle=FALSE, format='GTiff', ...)
             })
    )
    multi <- xout - xin[base]
    chg.ind <- ifelse(base > raster::nlayers(s.change), 
                      raster::nlayers(s.change), base)
    message('Calculating grids for years specified in xout...')
    if(progress) pb <- txtProgressBar(0, length(xout), style=3)
    invisible(sapply(seq_along(xout), function(x) {
      out.rast <- if(xout[x] %in% xin) {
        s[[base[x]]]
      } else {
        raster::overlay(s[[base[x]]], s.change[[chg.ind[x]]],
                        fun=function(x1, x2) x1 + (x2*multi[x]))
      }
      raster::writeRaster(
        out.rast, 
        filename=file.path(outdir, sprintf('%s_%s', prefix, xout[x])), 
        format='GTiff', ...)
      if(progress) setTxtProgressBar(pb, x)
    }))
    if(isTRUE(returnstack)) {
      f <- file.path(outdir, paste0(prefix, '_', xout, '.tif'))
      return(raster::stack(
        f[order(as.numeric(sub('.*_(\\d+)\\.tif$', '\\1', f)))]
      ))
    }
  }