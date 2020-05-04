#'
#' Some tools for convert the date into c ++ files.
#'
#'
#' A function to check the length vector
#'
#' @param x a vector
#'
#' @param y a vector
#'
#' @rdname gbm-internals
#' @export
checkMissing <- function(x, y){
  nms <- getVarNames(x)
  #### Check for NaNs in x and NAs in response
  j <- apply(x, 2, function(z) any(is.nan(z)))
  if(any(j)) {
    stop("Use NA for missing values. NaN found in predictor variables:",
         paste(nms[j],collapse=","))
  }
  if(any(is.na(y))) stop("Missing values are not allowed in the response")
  invisible(NULL)
}



#' A function to make a vector of weight
#'
#' @param w a vector of weight
#'
#' @param n a size
#'
#' @return a vector of weight with length n
#'
#' @rdname gbm-internals
#' @export
checkWeights <- function(w, n){
  # Logical checks on weights
  if(length(w)==0) { w <- rep(1, n) }
  else if(any(w < 0)) stop("negative weights not allowed")
  w
}




#' A function to to check is the interaction.depth  is eable
#'
#' @param id  interaction.depth  which must less then 50
#'
#' @rdname gbm-internals
#' @export
checkID <- function(id){
  # Check for disallowed interaction.depth
  if(id < 1) {
    stop("interaction.depth must be at least 1.")
  }
  else if(id > 49) {
    stop("interaction.depth must be less than 50. You should also ask yourself why you want such large interaction terms. A value between 1 and 5 should be sufficient for most applications.")
  }
  invisible(id)
}



#' A function to measure the offset
#'
#' @param o the offset
#'
#' @param y a vector
#'
#' @return a vector of offset with y's length
#'
#' @rdname gbm-internals
#' @export
checkOffset <- function(o, y){
  # Check offset
  if(is.null(o) | all(o==0)) { o <- NA  }
  else if(length(o) != length(y))   {
    stop("The length of offset does not equal the length of y.")
  }
  o
}

#' A function to get the bariable name
#'
#' @param x a dateset
#'
#' @return a vector of names
#'
#' @rdname gbm-internals
#' @export
getVarNames <- function(x){
  if(is.matrix(x)) { var.names <- colnames(x) }
  else if(is.data.frame(x)) { var.names <- names(x) }
  else { var.names <- paste("X",1:ncol(x),sep="") }
  var.names
}


