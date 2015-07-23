#' Read raster data stored in Maxent's mxe format
#'
#' Read binary mxe files created by the Maxent habitat suitability modelling 
#' software package.
#'
#' @param file The path to the mxe file to be read.
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
#' @importFrom raster raster
#' @export
read_mxe <- function(file, return_raster=TRUE) {
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
  
  dat <- switch(
    as.character(datatype), 
    `0` = list(datatype='2-byte integer', what='integer', size=2, signed=TRUE), 
    `1` = list(datatype='32-bit float', what='numeric', size=4, signed=TRUE),
    `2` = list(datatype='signed byte', what='integer', size=1, signed=TRUE), 
    `3` = list(datatype='4-byte integer', what='integer', size=4, signed=TRUE),  
    `5` = list(datatype='unsigned byte', what='integer', size=1, signed=FALSE), 
    list(datatype='Unknown', what=NA, size=NA)
  )
  
  if (dat$datatype == 'Unknown') {
    dat$data <- NA
  } else {
    dat$data <- switch(
      as.character(blocktype),
      '77'=readBin(mxe.gz, dat$what, size=dat$size, n=blocksize/dat$size, 
                   endian="big", signed=dat$signed),
      '7a'= {
        n <- ceiling(nrow * ncol * dat$size / blocksize) - 1
        c(readBin(mxe.gz, dat$what, size=dat$size, n=(blocksize-40)/dat$size, 
                  endian="big", signed=dat$signed), 
          
          
          unlist(lapply(seq_len(n), 
                        function(i) {
                          blocktype <- readBin(mxe.gz, 'raw', n=1, endian="big")
                          readBin(mxe.gz, 'integer', size=ifelse(blocktype=='7a', 4, 1), 
                                  endian="big")  
                          readBin(mxe.gz, dat$what, size=dat$size, 
                                  n=blocksize/dat$size, endian="big", 
                                  signed=dat$signed)
                        })))
      })
  }
  
  dat$data[dat$data == nodata] <- NA
  
  if(isTRUE(return_raster)) {
    raster(matrix(dat$data, ncol=ncol, byrow=TRUE), xmn=xll, ym=yll, 
                   xmx=xll + (ncol * cellsize), ymx=yll + (nrow*cellsize))
  } else {
    list(xll=xll, yll=yll, cellsize=cellsize, nrow=nrow, ncol=ncol, 
         nodata=nodata, datatype=dat$datatype, data=dat$data)  
  }
}