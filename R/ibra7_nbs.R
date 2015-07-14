#' IBRA 7 region adjacency matrix
#'
#' A 419 x 419 logical adjacency matrix indicating whether the polygon with ID
#' given by the row name is adjacent to ("touches", sensu GEOS) the polygon with
#' ID given by the column name. Row sums (or, equivalently, column sums) give
#' the total number of polygons that a given polygon touches. Note that the
#' diagonal is \code{FALSE}, i.e. polygons are not considered to touch
#' themselves.
#'
#' This dataset was derived from the IBRA 7 regions polygon shapefile provided
#' by the Australian Government Department of the Environment. The
#' \code{rgeos:gTouches} function was used to produce the adjacency matrix,
#' for the original (non-projected, i.e. GDA94) shapefile.
#'
#' @source Department of the Environment (2012), Interim Biogeographic
#'   Regionalisation for Australia (Regions - States and Territories) v. 7
#'   (IBRA) [ESRI shapefile] Available from
#'   \url{http://www.environment.gov.au/fed/catalog/search/resource/details.page?uuid=\%7BFB89EEC9-5ABE-4CCD-B50E-7D485A3BAA4C\%7D}
#' @docType data
#' @format A \code{matrix} with 419 rows and 419 columns.
"ibra7_nbs"
