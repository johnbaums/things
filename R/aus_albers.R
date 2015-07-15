#' Natural Earth - Australia SPDF
#'
#' A SpatialPolygonsDataFrame of the Australian subset of
#' \href{http://www.naturalearthdata.com/}{Natural Earth's} 1:10 million scale
#' polygon shapefile of States and Territories. 
#'
#' This dataset has been projected from WGS84 (EPSG:4326) to Australian Albers
#' (EPSG:3577) using \code{sp::spTransform}.
#'
#' @source \href{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-1-states-provinces/}{Natural Earth}
#' @docType data
#' @format A \code{SpatialPolygonsDataFrame} with 9 polygons and 59 fields:
#'   \itemize{
#'   \item{\code{adm1_code}}{: } 
#'   \item{\code{OBJECTID_1}}{: } 
#'   \item{\code{diss_me}}{: } 
#'   \item{\code{adm1_cod_1}}{: } 
#'   \item{\code{iso_3166_2}}{: } 
#'   \item{\code{wikipedia}}{: } 
#'   \item{\code{iso_a2}}{: } 
#'   \item{\code{adm0_sr}}{: } 
#'   \item{\code{name}}{: } 
#'   \item{\code{name_alt}}{: } 
#'   \item{\code{name_local}}{: } 
#'   \item{\code{type}}{: } 
#'   \item{\code{type_en}}{: } 
#'   \item{\code{code_local}}{: } 
#'   \item{\code{code_hasc}}{: } 
#'   \item{\code{note}}{: } 
#'   \item{\code{hasc_maybe}}{: } 
#'   \item{\code{region}}{: } 
#'   \item{\code{region_cod}}{: } 
#'   \item{\code{provnum_ne}}{: } 
#'   \item{\code{gadm_level}}{: } 
#'   \item{\code{check_me}}{: } 
#'   \item{\code{scalerank}}{: } 
#'   \item{\code{datarank}}{: } 
#'   \item{\code{abbrev}}{: } 
#'   \item{\code{postal}}{: } 
#'   \item{\code{area_sqkm}}{: } 
#'   \item{\code{sameascity}}{: } 
#'   \item{\code{labelrank}}{: } 
#'   \item{\code{featurecla}}{: } 
#'   \item{\code{name_len}}{: } 
#'   \item{\code{mapcolor9}}{: } 
#'   \item{\code{mapcolor13}}{: } 
#'   \item{\code{fips}}{: } 
#'   \item{\code{fips_alt}}{: } 
#'   \item{\code{woe_id}}{: } 
#'   \item{\code{woe_label}}{: } 
#'   \item{\code{woe_name}}{: } 
#'   \item{\code{latitude}}{: } 
#'   \item{\code{longitude}}{: } 
#'   \item{\code{sov_a3}}{: } 
#'   \item{\code{adm0_a3}}{: } 
#'   \item{\code{adm0_label}}{: } 
#'   \item{\code{admin}}{: } 
#'   \item{\code{geonunit}}{: } 
#'   \item{\code{gu_a3}}{: } 
#'   \item{\code{gn_id}}{: } 
#'   \item{\code{gn_name}}{: } 
#'   \item{\code{gns_id}}{: } 
#'   \item{\code{gns_name}}{: } 
#'   \item{\code{gn_level}}{: } 
#'   \item{\code{gn_region}}{: } 
#'   \item{\code{gn_a1_code}}{: } 
#'   \item{\code{region_sub}}{: } 
#'   \item{\code{sub_code}}{: } 
#'   \item{\code{gns_level}}{: } 
#'   \item{\code{gns_lang}}{: } 
#'   \item{\code{gns_adm1}}{: } 
#'   \item{\code{gns_region}}{: } 
#' }
"aus_albers"
