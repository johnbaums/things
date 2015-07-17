#' Get proj4 strings and EPSG codes by CRS keywords.
#'
#' @param x Character. Key words to search for.
#' @param type Either \code{'epsg'} or \code{'proj4'}.
#' @param crs Logical. Should a CRS object be returned?
#' @return If \code{crs} is \code{TRUE}, a \code{CRS} object. Otherwise, if
#'   \code{type} is \code{'epsg'}, a vector of EPSG codes, or if \code{type} is
#'   \code{'proj4'}, a vector of proj4 strings.
#' @keywords spatial, projection, proj4, gis
#' @importFrom rgdal make_EPSG
#' @importFrom sp CRS
#' @export
#' @examples
#' proj_by_name('gda94', type='proj4')
#' proj_by_name('gda94 albers', crs=TRUE)
proj_by_name <- function(x, type='proj4', crs=FALSE) {
  if (!type %in% c('epsg', 'proj4'))
    stop('Type must be either "epsg" or "proj4".', call.=FALSE)
  projs <- make_EPSG()
  x <- strsplit(x, ' ')[[1]]
  type <- switch(type, epsg='code', proj4='prj4')
  i <- Reduce(intersect, lapply(x, grep, projs$note, ignore.case=TRUE))
  m <- projs[i, ]
  out <- setNames(m[, type], m[, 'note'])
  if (isTRUE(crs)) {
    if(length(out) > 1) {
      message("More than one match. Can't return as CRS object.")
    } else if(length(out) == 1) {
      return(CRS(out))
    }
  }
  if (length(out) == 0) {
    message('No matches found. Please adjust your search string.')
    return(invisible(NULL))
  } else {
    return(out)
  }
}
