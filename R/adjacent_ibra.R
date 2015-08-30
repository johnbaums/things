#' Subset IBRA 7 by points
#'
#' Subset IBRA 7 to those polygons that contain, or are adjacent to polygons
#' that contain, one or more given points.
#'
#' @param pts A SpatialPoints* object.
#' @param min_n The minimum number of points a subregion must contain in order
#'   for it to be included in the returned subset.
#' @param include_type Character. Either \code{'polygon'}, \code{'region'} or 
#'   \code{'subregion'}. This determines the level of aggregation at which
#'   occupied IBRA polygons will be considered. For \code{'polygon'}, adjacency
#'   will be calculated with respect to the polygons that actually contain
#'   \code{min_n} or more points, whereas for \code{'subregion'} (and
#'   \code{'region'}), adjacency will be calculated with respect to all polygons 
#'   belonging to the IBRA subregions (or regions) that have polygons containing
#'   at least \code{min_n} points. Default is \code{'polygon'}.
#' @param expand_type Character. Either \code{'polygon'}, \code{'region'} or 
#'   \code{'subregion'}. This determines the level of aggregation at which
#'   neighbouring IBRA polygons will be aggregated prior to calculating
#'   adjacency. For \code{'polygon'}, only the polygons adjacent to those
#'   polygons with \code{min_n} or more points will be considered neighbours,
#'   whereas for whereas for \code{'subregion'} (and \code{'region'}),
#'   neighbours will comprise all polygons belonging to the IBRA subregions (or
#'   regions) that have polygons adjacent to polygons containing at least
#'   \code{min_n} points. Default behaviour is for \code{expand_type} to be the
#'   same as \code{include_type}.
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
#' 
#' #' # Polygons with at least 1 point
#' adj <- adjacent_ibra(xy, include_type='polygon')
#' plot(adj, col='gray80', border='transparent')
#' 
#' # Polygons with at least 20 points
#' adj2 <- adjacent_ibra(xy, include_type='polygon', min_n=20)
#' plot(adj2, col='gray60', border='transparent', add=TRUE)
#' 
#' # Sub-regions with at least 1 point
#' adj3 <- adjacent_ibra(xy, include_type='subregion')
#' plot(adj3, col='gray80', border='transparent')
#' 
#' # Sub-regions with at least 20 points
#' adj4 <- adjacent_ibra(xy, include_type='subregion', min_n=20)
#' plot(adj4, col='gray60', border='transparent', add=TRUE)
#' 
#' # Regions with at least 1 point
#' adj5 <- adjacent_ibra(xy, include_type='region')
#' plot(adj5, col='gray80', border='transparent')
#' 
#' # Regions with at least 20 points
#' adj6 <- adjacent_ibra(xy, min_n=20, include_type='region')
#' plot(adj6, col='gray60', border='transparent', add=TRUE)
#' 
#' # With plot_output=TRUE
#' adj7 <- adjacent_ibra(xy, plot_output=TRUE)
#' 
#' # Compare include_type and expand_type
#' apply(expand.grid(c('polygon', 'subregion', 'region'), 
#'                   c('polygon', 'subregion', 'region')), 1, 
#'       function(x) {
#'         adjacent_ibra(xy, include_type=x[1], expand_type=x[2], plot=TRUE)
#'         title(main=sprintf('include type: %s; expand type: %s', x[1], x[2]))
#'       })
adjacent_ibra <- function(pts, min_n=1, include_type='polygon', 
                          expand_type=include_type, plot_output=FALSE) {
  if(!include_type %in% c('polygon', 'region', 'subregion'))
    stop('include_type must be either "region" or "subregion".', call.=FALSE)
  if(!expand_type %in% c('polygon', 'region', 'subregion'))
    stop('expand_type must be either "region" or "subregion".', call.=FALSE)
  if(is.na(proj4string(pts))) stop('pts has no CRS', call.=FALSE)
  if(proj4string(pts) != proj4string(ibra7_albers))
    pts <- spTransform(pts, CRS(proj4string(ibra7_albers)))
  o <- over(pts, ibra7_albers)
  idx_contains <- switch(
    include_type,
    'polygon'={
      tab <- table(o$POLY_ID)
      row.names(ibra7_albers)[ibra7_albers$POLY_ID %in% names(tab[tab >= min_n])]
    },
    'region'={
      tab <- table(o$REG_CODE_7)
      row.names(ibra7_albers)[ibra7_albers$REG_CODE_7 %in% names(tab[tab >= min_n])]
    },
    'subregion'={
      tab <- table(o$SUB_CODE_7)
      row.names(ibra7_albers)[ibra7_albers$SUB_CODE_7 %in% names(tab[tab >= min_n])]
    })
  
  polys <- union(names(which(rowSums(ibra7_nbs[, idx_contains, drop=FALSE]) > 0)), 
                 idx_contains)
  out <- switch(
    expand_type,
    'polygon'={
      ibra7_albers[polys, ]
    },
    'region'={
      subset(ibra7_albers, REG_CODE_7 %in% ibra7_albers[polys, ]$REG_CODE_7)
    },
    'subregion'={
      subset(ibra7_albers, SUB_CODE_7 %in% ibra7_albers[polys, ]$SUB_CODE_7)
    })
  if(isTRUE(plot_output)) {
    plot(out, border='gray25',
         col=ifelse(row.names(out) %in% idx_contains, 'steelblue', 'gray85'))
    points(pts, pch=20, col=ifelse(is.na(o$SUB_CODE_7), 'tomato2', 'gray10'))
    plot(things::aus_albers, add=TRUE, lwd=2)
    box()
  }
  out
}
utils::globalVariables(c('ibra7_albers', 'ibra7_nbs', 'aus_albers'))