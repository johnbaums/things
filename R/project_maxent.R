#' Project a fitted Maxent model
#'
#' Project a fitted Maxent model by predicting to new environmental data.
#'
#' @param lambdas Either a \code{MaxEnt} fitted model object (fitted with the 
#'   \code{maxent} function in the \code{dismo} package), or a file path to a 
#'   Maxent .lambdas file.
#' @param newdata A \code{RasterStack}, \code{RasterBrick}, \code{list},
#'   \code{data.frame}, \code{data.table}, or \code{matrix} that has
#'   layers/elements/columns whose names correspond to the names of predictors
#'   used to fit the model. These layers/elements/columns must all have the same
#'   length.
#' @param mask (Optional; requires that \code{newdata} is a \code{Raster*} 
#'   object.) A \code{Raster} object with \code{NA} values in cells for which 
#'   the model should \emph{not} be projected. These cells will be assigned
#'   \code{NA} in the returned output.
#' @return If \code{newdata} is a \code{RasterStack} or \code{RasterBrick}, a 
#'   list with two elements: 
#'  \itemize{
#'   \item{\code{prediction_raw}}{: a \code{Raster} layer giving the raw Maxent
#'   prediction; and}
#'   \item{\code{prediction_logistic}}{: a \code{Raster} layer giving the
#'   logistic Maxent prediction.}
#' }
#' If \code{newdata} is \emph{not} a \code{RasterStack} or \code{RasterBrick},
#' the raster layers will be replaced with \code{data.table}s in the returned
#' list.
#' @author John B. Baumgartner, \email{johnbaums@@gmail.com}
#' @details \code{project_maxent} uses feature weights described in a .lambas
#'   file or \code{MaxEnt} object to predict a Maxent model to environmental
#'   data. This function performs the projection entirely in R, without the need
#'   for the Maxent Java software. For tested datasets, it performs the 
#'   projection in roughly one third of the time taken for the same projection
#'   by maxent.jar.
#' @section Warning:
#' This function is still in development, and no guarantee is made for the
#' accuracy of its projections.
#' @keywords maxent, predict, project
#' @seealso \code{\link{read_mxe}}
#' @importFrom raster raster mask compareRaster 
#' @importFrom data.table data.table as.data.table is.data.table :=
#' @export
project_maxent <- function(lambdas, newdata, mask) {
  if(!missing(mask)) {
    if(!is(mask, 'RasterLayer')) {
      stop('mask should be a RasterLayer object')
    } else {
      if(!is(newdata, 'Raster')) {
        stop('If mask is provided, newdata should be a Raster object with the same dimensions, extent, and CRS.')
      }
      if(!compareRaster(mask, newdata, stopiffalse=FALSE))
        stop('If mask is provided, newdata should be a Raster object with the same dimensions, extent, and CRS.')
    }
  }
  if(is(lambdas, 'MaxEnt')) {
    lambdas <- lambdas@lambdas
  } else {
    lambdas <- readLines(lambdas)
  }
  n <- count.fields(textConnection(lambdas), ',', quote='')
  meta <- setNames(lapply(strsplit(lambdas[n==2], ', '), 
                          function(x) as.numeric(x[2])),
                   sapply(strsplit(lambdas[n==2], ', '), '[[', 1))
  lambdas <- setNames(data.frame(do.call(rbind, strsplit(lambdas[n==4], ', ')), 
                                 stringsAsFactors=FALSE),
                      c('var', 'lambda', 'min', 'max'))
  lambdas[, -1] <- lapply(lambdas[, -1], as.numeric)
  lambdas$var <- sub('<', '<=', lambdas$var)
  lambdas$type <- sapply(lambdas$var, function(x) {
    switch(gsub("\\w|\\.|-|\\(|\\)", "", x),
           "<=" = "threshold",
           "^" = "quadratic",
           "*" = "product", 
           "`" = "reverse_hinge",
           "'" = 'forward_hinge',
           'linear')
  })
  nms <- gsub("\\^2|\\(.*<=|`|\\'|\\)", "", lambdas$var)
  nms <- unique(unlist(strsplit(nms, '\\*')))
  clamp_limits <- data.table(lambdas[lambdas$type=='linear', ])
  lambdas <- split(lambdas, c('other', 'hinge')[grepl('hinge', lambdas$type) + 1])
  if (is(newdata, 'RasterStack') | is(newdata, 'RasterBrick')) {
    pred_raw <- pred_logistic <- raster(newdata)
    if(!missing(mask)) {
      newdata <- mask(newdata, mask)
    }
    newdata <- as.data.table(as.data.frame(newdata))
  }
  if (is.matrix(newdata)) newdata <- as.data.table(newdata)
  if (is.list(newdata) & !is.data.frame(newdata)) {
    if(length(unique(sapply(newdata, length))) != 1) 
      stop('newdata was provided as a list, but its elements vary in length.')
    newdata <- as.data.table(newdata)
  }
  if (!is.data.frame(newdata))
    stop('newdata must be a list, data.table, data.frame, matrix, RasterStack, or RasterBrick.')
  if (!is.data.table(newdata)) 
    newdata <- as.data.table(newdata)
  if(!all(nms %in% names(newdata))) {
    stop(sprintf('Variables missing in newdata: %s', 
                 paste(setdiff(nms, colnames(newdata)), collapse=', ')))
  }
  newdata <- newdata[, setdiff(names(newdata), nms) := NULL]
  na <- !complete.cases(newdata)
  newdata <- newdata[!na]
  
  # Clamp newdata
  invisible(lapply(names(newdata), function(x) {
    newdata[, c(x) := pmax(pmin(get(x), clamp_limits[var==x, max]), 
                        clamp_limits[var==x, min])]
  }))
  
  
  lfx <- numeric(nrow(newdata))
  
  if('other' %in% names(lambdas)) {
    for (i in seq_len(nrow(lambdas$other))) {
      cat('\r', 'Calculating contribution of feature', i, 
          'of', sum(sapply(lambdas, nrow)))
      x <- with(newdata, eval(parse(text=lambdas$other$var[i])))
      # clamp feature
      x <- pmin(pmax(x, lambdas$other$min[i]), lambdas$other$max[i])
      lfx <- lfx + (lambdas$other$lambda[i] * 
                      with(newdata, (x - lambdas$other$min[i]) /
                             (lambdas$other$max[i] - lambdas$other$min[i])))
    }
    rm(x)
  }
  
  if('hinge' %in% names(lambdas)) {
    hinge <- split(lambdas$hinge, lambdas$hinge$type)
    if('forward_hinge' %in% names(hinge)) {
      for (i in seq_len(nrow(hinge$forward_hinge))) {
        cat('\r', 'Calculating contribution of feature', nrow(lambdas$other) + i, 
            'of', sum(sapply(lambdas, nrow)))
        x <- with(newdata, get(sub("'", "", hinge$forward_hinge$var[i])))
        # no clamping for hinge features (raw features have already been clamped)
        lfx <- lfx +
          with(newdata, (x >= hinge$forward_hinge$min[i]) * 
                 (hinge$forward_hinge$lambda[i] * 
                    (x - hinge$forward_hinge$min[i]) / 
                    (hinge$forward_hinge$max[i] - 
                       hinge$forward_hinge$min[i])))
      }
      rm(x)
    }
    if('reverse_hinge' %in% names(hinge)) {
      for (i in seq_len(nrow(hinge$reverse_hinge))) {
        cat('\r', 'Calculating contribution of feature', nrow(lambdas$other) + 
              nrow(hinge$forward_hinge) + i, 'of', sum(sapply(lambdas, nrow)))
        x <- with(newdata, get(sub("`", "", hinge$reverse_hinge$var[i])))
        # no clamping for hinge features (raw features have already been clamped)
        lfx <- lfx +
          with(newdata, (x < hinge$reverse_hinge$max[i]) * 
                 (hinge$reverse_hinge$lambda[i] * 
                    (hinge$reverse_hinge$max[i] - x) / 
                    (hinge$reverse_hinge$max[i] - 
                       hinge$reverse_hinge$min[i])))
      }
      rm(x)
    }
  }
  raw <- exp(lfx - meta$linearPredictorNormalizer) / meta$densityNormalizer
  logistic <- raw * exp(meta$entropy) / (1 + raw * exp(meta$entropy))
  if(exists('pred_raw', inherits=FALSE)) {
    pred_raw[which(!na)] <- raw
    pred_logistic[which(!na)] <- logistic
    return(list(prediction_raw=pred_raw,
                prediction_logistic=pred_logistic))
  } else {
    return(list(prediction_raw=raw,
                prediction_logistic=logistic))
  } 
}