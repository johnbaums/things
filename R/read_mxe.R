#' Read raster data stored in Maxent's mxe format
#'
#' Read (and optionally clip) binary mxe files created by the Maxent habitat 
#' suitability modelling software package.
#'
#' @param file The path to the mxe file to be read.
#' @param ext (Optional) An extent to clip the grid to. This must be either an
#'   \code{Extent} object, a \code{SpatialPolygons*}, or a numeric vector with
#'   four elements in the order xmin xmax, ymin, ymax.
#' @param snap Character. One of \code{'near'}, \code{'out'}, or \code{'in'}.
#'   This will adjust the extent given in \code{ext} such that it aligns with
#'   the raster data being read. Note that currently, supplying \code{'out'} or
#'   \code{'in'} will result in the extent being expanded/contracted even if 
#'   the provided extent already aligns with the grid. \code{snap} is ignored if
#'   \code{ext} is not provided.
#' @param chunk_size A numeric value specifying the size of the chunk of binary 
#'   data to be read at a time (size is usually in units of KB of data).
#'   \code{chunk_size} is ignored if \code{ext} is not provided. If clipping the
#'   imported data (i.e. if \code{ext} is provided), the raster data are read in
#'   chunks, with \code{chunk_size} values being read, and subsequently filtered
#'   to those values within the desired extent, at a time. Decreasing
#'   \code{chunk_size} leads to lower system memory demand, but also to longer
#'   processing time.
#' @param return_raster Logical. If \code{FALSE}, then the cell values data will
#'   returned as a vector; if \code{TRUE}, a \code{raster} object will be
#'   returned.
#' @return If \code{return_raster} is \code{TRUE}, a \code{raster} object. If 
#' \code{return_raster} is \code{FALSE}, a list is returned, with the following
#' elements:
#'   \itemize{
#'   \item{\code{xll}}{: The x coordinate of the lower left corner of the extent.}
#'   \item{\code{yll}}{: The y coordinate of the lower left corner of the extent.}
#'   \item{\code{cellsize}}{: The grid resolution. Note that horizontal and
#'   vertical resolution are assumed equal.}
#'   \item{\code{nrow}}{: The number of rows of data.}
#'   \item{\code{ncol}}{: The number of columns of data.}
#'   \item{\code{nodata}}{: The nodata value.}
#'   \item{\code{datatype}}{: A character string indicating the data type.}
#'   \item{\code{data}}{: A vector whose elements are the raster cell values.}
#' }
#' @author John B. Baumgartner, \email{johnbaums@@gmail.com}
#' @author Peter D. Wilson
#' @references Based on \href{http://rpubs.com/puddleduck/91946}{"Reading mxe
#'   files with R - revisited"} by Peter D. Wilson.
#' @keywords maxent, read
#' @seealso \code{\link{project_maxent}}
#' @importFrom raster raster extent res<-
#' @export
read_mxe <- function(file, ext, snap='near', chunk_size=100000, return_raster=TRUE) {
  if (missing(file)) stop("No file name supplied.", call.=FALSE)
  if (!file.exists(file)) 
    stop("Supplied file name (", file, ") not found.", call.=FALSE)
  mxe.gz <- gzfile(file, "rb")
  on.exit(close(mxe.gz))
  
  java_header <- readBin(mxe.gz, "raw", 4, endian="big")
  blocktype <- readBin(mxe.gz, "raw", 1, endian="big")
  blocksize <- 
    switch(as.character(blocktype), 
           '77'=readBin(mxe.gz, "integer", size=1, endian="big", signed=FALSE),
           '7a'=readBin(mxe.gz, "integer", endian="big"),
           stop('This is not a recognized mxe format.', call.=FALSE))
  
  xll <- readBin(mxe.gz, "numeric", endian="big")
  yll <- readBin(mxe.gz, "numeric", endian="big")     
  cellsize <- readBin(mxe.gz, "numeric", endian="big")
  nrow <- readBin(mxe.gz, "integer", endian="big")    
  ncol <- readBin(mxe.gz, "integer", endian="big")    
  nodata <- readBin(mxe.gz, "integer", endian="big")  
  datatype <- readBin(mxe.gz, "integer", endian="big")
  
  if (!missing(ext)) {
    if(inherits(ext, 'SpatialPolygons')) {
      ext <- extent(ext)
    } else if (!(is.numeric(ext) & length(ext) == 4) & !is(ext, 'Extent') ) {
      stop('If provided, extent must be an Extent object, a SpatialPolygons* ',
           'object from which an extent can be extracted, or a numeric vector ',
           'giving the clipping extent as xmin, xmax, ymin, ymax.')
    } 
  } else {
    ext <- c(xll, xll + ncol*cellsize, yll, yll + nrow*cellsize)
    chunk_size <- ncol*nrow
  }
  
  if(ext[1] > ext[2] | ext[3] > ext[4]) 
    stop('Invalid extent provided. If ext is a vector, it must have order: ',
         'xmin, xmax, ymin, ymax.')
  
  x_seq <- seq(xll, xll + ncol*cellsize, by=cellsize)
  y_seq <- seq(yll, yll + nrow*cellsize, by=cellsize)
  
  if(!missing(ext)) {
    ext <- switch(
      snap,
      near=c(x_seq[apply(abs(sapply(ext[1:2], '-', x_seq)), 2, which.min)],
             y_seq[apply(abs(sapply(ext[3:4], '-', y_seq)), 2, which.min)]),
      out=c(max(x_seq[which((ext[1] - x_seq) > 0 & 
                              (ext[1] - x_seq) < cellsize)], xll),
            min(x_seq[which((ext[2] - x_seq) < 0 & 
                              abs(ext[2] - x_seq) < cellsize)], xll+ncol*cellsize),
            max(y_seq[which((ext[3] - y_seq) > 0 & 
                              (ext[3] - y_seq) < cellsize)], yll),
            min(y_seq[which((ext[4] - y_seq) < 0 & 
                              abs(ext[4] - y_seq) < cellsize)], yll+nrow*cellsize)),
      `in`=c(max(x_seq[which((ext[1] - x_seq) < 0 & 
                               abs(ext[1] - x_seq) < cellsize)], xll),
             min(x_seq[which((ext[2] - x_seq) > 0 & 
                               (ext[2] - x_seq) < cellsize)], xll+ncol*cellsize),
             max(y_seq[which((ext[3] - y_seq) < 0 & 
                               abs(ext[3] - y_seq) < cellsize)], yll),
             min(y_seq[which((ext[4] - y_seq) > 0 & 
                               (ext[4] - y_seq) < cellsize)], yll+nrow*cellsize)),
      stop("snap must be one of: 'near', 'in', or 'out'."))
  }
  
  ext_rc <- round(c((ext[1:2] - xll)/cellsize, nrow - (ext[3:4] - yll)/cellsize))
  
  rows <- (ext_rc[4]+1):ext_rc[3]
  cols <- (ext_rc[1]+1):ext_rc[2]
  rc <- expand.grid(cols, rows)
  cells <- (rc[, 2] - 1) * ncol + rc[, 1]
  
  dat <- switch(
    as.character(datatype), 
    `0` = list(datatype='2-byte integer', what='integer', size=2, signed=TRUE), 
    `1` = list(datatype='32-bit float', what='numeric', size=4, signed=TRUE),
    `2` = list(datatype='signed byte', what='integer', size=1, signed=TRUE), 
    `3` = list(datatype='4-byte integer', what='integer', size=4, signed=TRUE),  
    `5` = list(datatype='unsigned byte', what='integer', size=1, signed=FALSE), 
    list(datatype='Unknown', what=NA, size=NA)
  )
  
  dat$data <- switch(
    as.character(blocktype),
    '77'=readBin(mxe.gz, dat$what, size=dat$size, n=blocksize/dat$size, 
                 endian="big", signed=dat$signed),
    '7a'= {
      out <- rep(NA_real_, length(cells))
      n <- ceiling(nrow * ncol * dat$size / blocksize) - 1
      
      tmp <- readBin(mxe.gz, dat$what, size=dat$size, n=(blocksize-40)/dat$size, 
                     endian="big", signed=dat$signed)
      
      pre <- tmp[seq_len((blocksize-40)/dat$size) %in% cells]
      
      len <- blocksize/dat$size
      split_max_groupsize <- function(x, n) 
        split(x, gl(ceiling(length(x)/n), n, length(x)))
      grp <- split_max_groupsize(seq_len(n), chunk_size)
      grp_lengths <- cumsum(c(0, sapply(grp, length)*len))
      
      if(length(grp) > 1) {
        pb <- txtProgressBar(1, length(grp), 1, style=3) 
      }
      d <- unlist(c(list(pre), lapply(seq_along(grp), function(i) {
        cells_g <- (blocksize-40)/dat$size + 
          grp_lengths[i] + 1:(len*length(grp[[i]]))
        d <- unlist(lapply(grp[[i]], function(ii) {
          blocktype <- readBin(mxe.gz, 'raw', n=1, endian="big")
          readBin(mxe.gz, 'integer', size=ifelse(blocktype=='7a', 4, 1), 
                  endian="big")  
          readBin(mxe.gz, dat$what, size=dat$size, n=len, endian="big", 
                  signed=dat$signed)  
        }))
        if (length(grp) > 1) setTxtProgressBar(pb, i)
        d[cells_g %in% cells]
      })))
      if(length(grp) > 1) close(pb)
      d
    })
  
  dat$data[dat$data == nodata] <- NA
  
  if(isTRUE(return_raster)) {
    r <- raster(xmn=ext[1], ymn=ext[3], xmx=ext[2], ymx=ext[4])
    res(r) <- cellsize
    r[] <- dat$data
    r
  } else {
    list(xll=ext[1], yll=ext[3], cellsize=cellsize, nrow=diff(ext_rc[4:3]), 
         ncol=diff(ext_rc[1:2]), nodata=nodata, datatype=dat$datatype, 
         data=dat$data)  
  }
}