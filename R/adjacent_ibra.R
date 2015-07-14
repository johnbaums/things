#' Subset IBRA 7 by points
#'
#' Subset IBRA 7 to those polygons that contain, or are adjacent to polygons
#' that contain, one or more given points.
#'
#' @param pts A SpatialPolygons* object.
#' @return A SpatialPolygonsDataFrame representing the subset of polygons of
#'   ibra7_albers within which one or more points (pts) fall, and those polygons
#'   adjacent to these.
#' @keywords spatial, ibra
#' @export
#' @examples
#' xy <- SpatialPoints(data.frame(x=runif(1000, 140, 145), y=runif(1000, -38, -35)))
#' proj4string(xy) <- '+init=epsg:4283'
#' adj <- adjacent_ibra(xy)
#' plot(adj, col='gray80', border='transparent')
adjacent_ibra <- function(pts, min_n) {
  if(is.na(proj4string(pts))) stop('pts has no CRS', call.=FALSE)
  if(proj4string(pts) != proj4string(ibra7_albers))
    pts_albers <- spTransform(pts, '+init=epsg:3577')
  o <- over(pts_albers, ibra7_albers)
  tab <- table(o$REC_ID)
  ibra7_albers[ibra7_albers$REC_ID %in% names(tab[tab >= min_n]), 'REC_ID']
#   i <- row.names(ibra7_albers)[
#     match(as.character(na.omit(unique(o$REG_NAME_7))), ibra7_albers$REG_NAME_7)]
#   adj <- ibra7_albers[union(which(rowSums(ibra7_nbs[, i]) > 0),
#                             match(i, rownames(ibra7_nbs))),]
}
