#'
#' Adds additional trees with settled shrinkage and interaction.depth to a gbm object, .
#'
#' @param object A object created from an initial call
#' .
#'
#' @param n.new.trees Integer specifying the number of additional trees to add
#' to \code{object}. Default is 100.
#'
#' @param data An optional data frame containing the variables in the model. By
#' default the variables are taken from \code{environment(formula)}, typically
#' the environment from which \code{gbm} is called. If \code{keep.data=TRUE} in
#' the initial call to \code{gbm} then \code{gbm} stores a copy with the
#' object. If \code{keep.data=FALSE} then subsequent calls
#' must resupply the same dataset. It becomes the user's
#' responsibility to resupply the same data at this point.
#'
#' @param interaction.depth The maximum depth of variable interactions. A value
#' of 1 implies an additive model, a value of 2 implies a model with up to 2-way
#' interactions, etc. Default is objects's interaction.depth.
#'
#' @param shrinkage The shrinkage parameter applied to each tree in the
#' expansion. Also known as the learning rate or step-size reduction; 0.001 to
#' 0.1 usually work, but a smaller learning rate typically requires more trees.
#' Default is objects shrinkage
#'
#'
#' @param weights An optional vector of weights to be used in the fitting
#' process. Must be positive but do not need to be normalized. If
#' \code{keep.data=FALSE} in the initial call to \code{gbm} then it is the
#' user's responsibility to resupply the weights.
#'
#' @param offset A vector of offset values.
#'
#' @param verbose Logical indicating whether or not to print out progress and
#' performance indicators (\code{TRUE}). If this option is left unspecified for
#' \code{gbm.more}, then it uses \code{verbose} from \code{object}. Default is
#' \code{FALSE}.
#'
#' @return A object.
#'
#' @export
#'
gbm.more <- function(object,
                     n.new.trees = 100,
                     interaction.depth = NULL,
                     shrinkage = NULL,
                     data = NULL,
                     weights = NULL,
                     offset = NULL,
                     verbose = NULL) {
  theCall <- match.call()
  nTrain  <- object$nTrain

  distribution.call.name <- object$distribution$name


  if(is.null(object$Terms) && is.null(object$data))
  {
    stop("The gbm model was fit using gbm.fit (rather than gbm) and keep.data was set to FALSE. gbm.more cannot locate the dataset.")
  }
  else if(is.null(object$data) && is.null(data))
  {
    stop("keep.data was set to FALSE on original gbm call and argument 'data' is NULL")
  }
  else if(is.null(object$data))
  {
    m <- eval(object$m, parent.frame())

    Terms <- attr(m, "terms")
    a <- attributes(Terms)

    y <- as.vector(model.extract(m, "response"))
    offset <- model.extract(m,offset)
    x <- model.frame(delete.response(Terms),
                     data,
                     na.action=na.pass)

    w <- weights
    if(length(w)==0) w <- rep(1, nrow(x))

    if (object$distribution$name != "pairwise")
    {
      w <- w*length(w)/sum(w) # normalize to N
    }

    if(is.null(offset) || (offset==0))
    {
      offset <- NA
    }
    Misc <- NA

    # create index upfront... subtract one for 0 based order
    x.order <- apply(x[1:nTrain,,drop=FALSE],2,order,na.last=FALSE)-1
    x <- data.matrix(x)
    cRows <- nrow(x)
    cCols <- ncol(x)
  }
  else
  {
    y       <- object$data$y
    x       <- object$data$x
    x.order <- object$data$x.order
    offset  <- object$data$offset
    Misc    <- object$data$Misc
    w       <- object$data$w
    nTrain  <- object$nTrain
    cRows   <- length(y)
    cCols   <- length(x)/cRows
  }

  if(is.null(verbose))
  {
    verbose <- object$verbose
  }
  if(is.null(interaction.depth))
  {
    interaction.depth <- object$interaction.depth
  }
  if(is.null(shrinkage))
  {
    shrinkage <- object$shrinkage
  }
  x <- as.vector(x)

  gbm.obj <- .Call("gbm_fit",
                   Y = as.double(y),
                   Offset = as.double(offset),
                   X = as.double(x),
                   X.order = as.integer(x.order),
                   weights = as.double(w),
                   Misc = as.double(Misc),
                   cRows = as.integer(cRows),
                   cCols = as.integer(cCols),
                   var.type = as.integer(object$var.type),
                   var.monotone = as.integer(object$var.monotone),
                   distribution = as.character(distribution.call.name),
                   n.trees = as.integer(n.new.trees),
                   interaction.depth = as.integer(object$interaction.depth),
                   n.minobsinnode = as.integer(object$n.minobsinnode),
                   n.classes = as.integer(object$num.classes),
                   shrinkage = as.double(object$shrinkage),
                   bag.fraction = as.double(object$bag.fraction),
                   train.fraction = as.integer(nTrain),
                   fit.old = as.double(object$fit),
                   n.cat.splits.old = as.integer(length(object$c.splits)),
                   n.trees.old = as.integer(object$n.trees),
                   verbose = as.integer(verbose),
                   PACKAGE = "mypkg")
  names(gbm.obj) <- c("initF","fit","train.error","valid.error",
                      "oobag.improve","trees","c.splits")

  gbm.obj$initF         <- object$initF
  gbm.obj$train.error   <- c(object$train.error, gbm.obj$train.error)
  gbm.obj$valid.error   <- c(object$valid.error, gbm.obj$valid.error)
  gbm.obj$oobag.improve <- c(object$oobag.improve, gbm.obj$oobag.improve)
  gbm.obj$trees         <- c(object$trees, gbm.obj$trees)
  gbm.obj$c.splits      <- c(object$c.splits, gbm.obj$c.splits)

  # cv.error not updated when using gbm.more
  gbm.obj$cv.error      <- object$cv.error
  gbm.obj$cv.folds      <- object$cv.folds

  gbm.obj$n.trees        <- length(gbm.obj$trees)
  gbm.obj$distribution   <- object$distribution
  gbm.obj$train.fraction <- object$train.fraction
  gbm.obj$shrinkage      <- object$shrinkage
  gbm.obj$bag.fraction   <- object$bag.fraction
  gbm.obj$var.type       <- object$var.type
  gbm.obj$var.monotone   <- object$var.monotone
  gbm.obj$var.names      <- object$var.names
  gbm.obj$interaction.depth <- object$interaction.depth
  gbm.obj$n.minobsinnode    <- object$n.minobsinnode
  gbm.obj$num.classes       <- object$num.classes
  gbm.obj$nTrain            <- object$nTrain
  gbm.obj$response.name     <- object$response.name
  gbm.obj$Terms             <- object$Terms
  gbm.obj$var.levels        <- object$var.levels
  gbm.obj$verbose           <- verbose


  if(!is.null(object$data))
  {
    gbm.obj$data <- object$data
  }
  else
  {
    gbm.obj$data <- NULL
  }
  gbm.obj$m <- object$m
  gbm.obj$call <- theCall

  class(gbm.obj) <- "gbm"
  return(gbm.obj)
}
