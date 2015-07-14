#' Identify IBRA 7 regions
#'
#' Return the names of the set of IBRA regions within which a set of points
#' falls.
#'
#' @param pts A SpatialPolygons* object.
#' @param tabulate Logical. If \code{TRUE}, return a named vector indicating the
#'   number of points in each occupied region.
#' @param min_n The minimum number of points a region must contain in order for 
#'   the region name to be returned when \code{tabulate} is \code{FALSE}.
#' @return If \code{tabulate} is \code{FALSE}, a vector of IBRA 7 region names 
#'   corresponding to the IBRA 7 regions that at least \code{min_n} \code{pts} 
#'   fall within. If \code{tabulate} is \code{TRUE}, a named vector showing the 
#'   number of points in each occupied IBRA 7 region. Points that do not overlie
#'   an IBRA 7 region will be ignored with a warning.
#' @keywords spatial, ibra
#' @export
#' @examples
#' xy <- SpatialPoints(data.frame(x=runif(1000, 140, 145), y=runif(1000, -38, -35)))
#' proj4string(xy) <- '+init=epsg:4283'
#' reg <- ibra_region(xy)
#' reg
ibra_region <- function(pts, tabulate=TRUE, min_n=1) {
  if(min_n != 1 & !tabulate) warning('If tabulate is TRUE, min_n is ignored.')
  if(is.na(proj4string(pts))) stop('pts has no CRS', call.=FALSE)
  if(proj4string(pts) != proj4string(ibra7_albers))
    pts_albers <- spTransform(pts, '+init=epsg:3577')
  o <- over(pts_albers, ibra7_albers)
  if (any(is.na(o$REG_NAME_7)))
    warning(sum(is.na(o$REG_NAME_7)), ' points did not coincide with IBRA 7 regions.')
  tab <- table(as.character(o$REG_NAME_7))
  if(isTRUE(tabulate)) {
    return(c(tab))
  } else {
    names(tab[tab >= min_n])
  }
}
