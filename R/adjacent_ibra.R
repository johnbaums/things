#' Subset IBRA 7 by points
#'
#' Subset IBRA 7 to those polygons that contain, or are adjacent to polygons
#' that contain, one or more given points.
#'
#' @param pts A SpatialPoints* object.
#' @param min_n The minimum number of points a subregion must contain in order for 
#'   it to be included in the returned subset.
#' @param type Character. Either \code{'region'} or \code{'subregion'}. This 
#'   determines how the IBRA polygons will be subset: \code{'region'} results in
#'   subregion polygons bing returned even if they contain no points, as long as
#'   other subregions of that region contain a total of at least \code{min_n}
#'   points; \code{subregion} returns only the subregion polygons that contain
#'   at least \code{min_n} points.
#' @param plot_output Logical. Should the resulting polygon subset be plotted? 
#'   If \code{TRUE}, the polygons and points will be plotted. Points that don't
#'   overlie an IBRA 7 subregion will be plotted in red.
#' @return A \code{SpatialPolygonsDataFrame} representing the subset of regions
#'   (or subregions, see \code{type}) of \code{ibra7_albers} within which
#'   \code{min_n} or more points (pts) fall, and those regions (or subregions)
#'   adjacent to these.
#' @seealso \code{\link{ibra_region}}
#' @importFrom sp over spTransform proj4string CRS 
#' @keywords spatial, ibra
#' @export
#' @examples
#' library(sp)
#' xy <- SpatialPoints(data.frame(x=runif(1000, 140, 145), y=runif(1000, -38, -35)))
#' proj4string(xy) <- '+init=epsg:4283'
#' # Sub-regions with at least 1 point
#' adj <- adjacent_ibra(xy)
#' plot(adj, col='gray80', border='transparent')
#' # Sub-regions with at least 20 points
#' adj2 <- adjacent_ibra(xy, min_n=20)
#' plot(adj2, col='gray60', border='transparent', add=TRUE)
#' # Regions with at least 1 point
#' adj3 <- adjacent_ibra(xy, type='region')
#' plot(adj3, col='gray80', border='transparent')
#' # Regions with at least 20 points
#' adj4 <- adjacent_ibra(xy, min_n=20, type='region')
#' plot(adj4, col='gray60', border='transparent', add=TRUE)
#' 
#' # With plot_output=TRUE
#' adj <- adjacent_ibra(xy, plot_output=TRUE)
adjacent_ibra <- function(pts, min_n=1, type='subregion', plot_output=FALSE) {
  if(!type %in% c('region', 'subregion'))
    stop('type must be either "region" or "subregion".', call.=FALSE)
  if(is.na(proj4string(pts))) stop('pts has no CRS', call.=FALSE)
  if(proj4string(pts) != proj4string(ibra7_albers))
    pts <- spTransform(pts, CRS(proj4string(ibra7_albers)))
  o <- over(pts, ibra7_albers)
  idx_contains <- switch(type,
         'region'={
           tab <- table(o$REG_CODE_7)
           row.names(ibra7_albers)[match(names(tab[tab >= min_n]), ibra7_albers$REG_CODE_7)]
         },
         'subregion'={
           tab <- table(o$SUB_CODE_7)
           row.names(ibra7_albers)[match(names(tab[tab >= min_n]), ibra7_albers$SUB_CODE_7)]
           })
  out <- ibra7_albers[
    union(names(which(rowSums(ibra7_nbs[, idx_contains, drop=FALSE]) > 0)), 
          idx_contains), ]
  if(isTRUE(plot_output)) {
    plot(out, col=ifelse(row.names(out) %in% idx_contains, 'steelblue', 'gray85'), 
         border='gray25')
    points(pts, pch=20, 
           col=ifelse(is.na(o$SUB_CODE_7), 'tomato2', 'gray10'))
    plot(things::aus_albers, add=TRUE, lwd=2)
    box()
  }
  out
}
utils::globalVariables(c('ibra7_albers', 'ibra7_nbs', 'aus_albers'))