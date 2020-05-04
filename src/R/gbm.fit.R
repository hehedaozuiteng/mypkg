#' Generalized Boosted Regression Modeling (GBM)
#'
#' Workhorse function providing the link between R and the C++ gbm engine.
#' GMB is a front-end to \code{gbm.fit} that uses the familiar R
#' modeling formulas. For power-users with many variables use
#' gbm.fit. For general practice \code{gbm} is preferable.
#'
#' @param x A data frame or matrix containing the predictor variables. The
#' number of rows in \code{x} must be the same as the length of \code{y}.
#'
#' @param y A vector of outcomes. The number of rows in \code{x} must be the
#' same as the length of \code{y}.
#'
#' @param offset A vector of offset values.
#'
#'
#' @param w A vector of weights of the same length as the \code{y}.
#'
#' @param var.monotone an optional vector, the same length as the number of
#' predictors, indicating which variables have a monotone increasing (+1),
#' decreasing (-1), or arbitrary (0) relationship with the outcome.
#'
#' @param n.trees the total number of trees to fit. This is equivalent to the
#' number of iterations and the number of basis functions in the additive
#' expansion.
#'
#' @param interaction.depth The maximum depth of variable interactions. A value
#' of 1 implies an additive model, a value of 2 implies a model with up to 2-way
#' interactions, etc. Default is \code{1}.
#'
#' @param n.minobsinnode Integer specifying the minimum number of observations
#' in the trees terminal nodes. Note that this is the actual number of
#' observations not the total weight.
#'
#' @param shrinkage The shrinkage parameter applied to each tree in the
#' expansion. Also known as the learning rate or step-size reduction; 0.001 to
#' 0.1 usually work, but a smaller learning rate typically requires more trees.
#' Default is \code{0.1}.
#'
#' @param bag.fraction The fraction of the training set observations randomly
#' selected to propose the next tree in the expansion. This introduces
#' randomnesses into the model fit. If \code{bag.fraction} < 1 then running the
#' same model twice will result in similar but different fits. \code{gbm} uses
#' the R random number generator so \code{set.seed} can ensure that the model
#' can be reconstructed. Default is \code{0.5}.
#'
#' @param nTrain An integer representing the number of cases on which to train.
#' This is the preferred way of specification for \code{gbm.fit}; The option
#' \code{train.fraction} in \code{gbm.fit} is deprecated and only maintained
#' for backward compatibility. These two parameters are mutually exclusive. If
#' both are unspecified, all data is used for training.
#'
#'
#' @param keep.data Logical indicating whether or not to keep the data and an
#' index of the data stored with the object.
#'
#' @param verbose Logical indicating whether or not to print out progress and
#' performance indicators (\code{TRUE}). If this option is left unspecified for
#' \code{gbm.more}, then it uses \code{verbose} from \code{object}. Default is
#' \code{FALSE}.
#'
#' @param response.name Character string label for the response variable.
#'
#' @return A gbm object.
#'
#' @details
#'
#' \code{gbm} implement from the Generalized Boosted Regression Models
#'
#' This package implements the generalized boosted modeling framework. Boosting
#' is the process of iteratively adding basis functions in a greedy fashion so
#' that each additional basis function further reduces the selected loss
#' function. This implementation closely follows Friedman's Gradient Boosting
#' Machine (Friedman, 2001).
#'
#' In addition to many of the features documented in the Gradient Boosting
#' Machine, \code{gbm} offers additional features including the out-of-bag
#' estimator for the optimal number of iterations, the ability to store and
#' manipulate the resulting \code{gbm} object, and a variety of other loss
#' functions that had not previously had associated boosting algorithms,
#' including the Cox partial likelihood for censored data, the poisson
#' likelihood for count outcomes, and a gradient boosting implementation to
#' minimize the AdaBoost exponential loss function.
#'
#' @author
#' Henglin Huang
#'
#' @references
#'
#' Greg Ridgeway \email{gregridgeway@gmail.com}
#' Gbm-Developers. “Gbm-Developers/Gbm.” GitHub, 12 Feb. 2019, github.com/gbm-developers/gbm.
#'
#' Y. Freund and R.E. Schapire (1997) \dQuote{A decision-theoretic
#' generalization of on-line learning and an application to boosting,}
#' \emph{Journal of Computer and System Sciences,} 55(1):119-139.
#'
#' G. Ridgeway (1999). \dQuote{The state of boosting,} \emph{Computing Science
#' and Statistics} 31:172-181.
#'
#' J.H. Friedman, T. Hastie, R. Tibshirani (2000). \dQuote{Additive Logistic
#' Regression: a Statistical View of Boosting,} \emph{Annals of Statistics}
#' 28(2):337-374.
#'
#' J.H. Friedman (2001). \dQuote{Greedy Function Approximation: A Gradient
#' Boosting Machine,} \emph{Annals of Statistics} 29(5):1189-1232.
#'
#' J.H. Friedman (2002). \dQuote{Stochastic Gradient Boosting,}
#' \emph{Computational Statistics and Data Analysis} 38(4):367-378.
#'
#' B. Kriegler (2007). Cost-Sensitive Stochastic Gradient Boosting Within a
#' Quantitative Regression Framework. Ph.D. Dissertation. University of
#' California at Los Angeles, Los Angeles, CA, USA. Advisor(s) Richard A. Berk.
#' url{https://dl.acm.org/citation.cfm?id=1354603}.
#'
#' C. Burges (2010). \dQuote{From RankNet to LambdaRank to LambdaMART: An
#' Overview,} Microsoft Research Technical Report MSR-TR-2010-82.
#'
#' @export
mygbm.fit <- function(x, y, offset = NULL,
                    w = NULL, var.monotone = NULL, n.trees = 100,
                    interaction.depth = 1, n.minobsinnode = 10,
                    shrinkage = 0.001, bag.fraction = 0.5, nTrain = NULL,
                    keep.data = TRUE, verbose = TRUE,
                    response.name = "y") {

  # Reformat distribution into a named list
  distribution = "bernoulli"
  misc = NULL
  train.fraction = NULL

  distribution <- list(name = distribution)


  # Dimensions of predictor data
  cRows <- nrow(x)
  cCols <- ncol(x)

  if(nrow(x) != ifelse(class(y) == "Surv", nrow(y), length(y))) {
    stop("The number of rows in x does not equal the length of y.")
  }

  # The preferred way to specify the number of training instances is via the
  # parameter `nTrain`. The parameter `train.fraction` is only maintained for
  # back compatibility.
  if(!is.null(nTrain) && !is.null(train.fraction)) {
    stop("Parameters `nTrain` and `train.fraction` cannot both be specified.")
  } else if(!is.null(train.fraction)) {
    warning("Parameter `train.fraction` is deprecated, please specify ",
            "`nTrain` instead.")
    nTrain <- floor(train.fraction*cRows)
  } else if(is.null(nTrain)) {
    nTrain <- cRows  # both undefined, use all training data
  }
  if (is.null(train.fraction)){
    train.fraction <- nTrain / cRows
  }

  var.names <- getVarNames(x)


  # Check size of data
  if(nTrain * bag.fraction <= 2 * n.minobsinnode + 1) {
    stop("The data set is too small or the subsampling rate is too large: ",
         "`nTrain * bag.fraction <= n.minobsinnode`")
  }

  # Sanity checks
  ch <- checkMissing(x, y)
  interaction.depth <- checkID(interaction.depth)
  w <- checkWeights(w, length(y))
  offset <- checkOffset(offset, y)

  Misc <- NA

  # setup variable types
  var.type <- rep(0,cCols)
  var.levels <- vector("list",cCols)
  for(i in 1:length(var.type))
  {
    if(all(is.na(x[,i])))
    {
      stop("variable ",i,": ",var.names[i]," has only missing values.")
    }
    if(is.ordered(x[,i]))
    {
      var.levels[[i]] <- levels(factor(x[,i]))
      x[,i] <- as.numeric(factor(x[,i]))-1
      var.type[i] <- 0
    }
    else if(is.factor(x[,i]))
    {
      if(length(levels(x[,i]))>1024)
        stop("gbm does not currently handle categorical variables with more than 1024 levels. Variable ",i,": ",var.names[i]," has ",length(levels(x[,i]))," levels.")
      var.levels[[i]] <- levels(factor(x[,i]))
      x[,i] <- as.numeric(factor(x[,i]))-1
      var.type[i] <- max(x[,i],na.rm=TRUE)+1
    }
    else if(is.numeric(x[,i]))
    {
      var.levels[[i]] <- quantile(x[,i],prob=(0:10)/10,na.rm=TRUE)
    }
    else
    {
      stop("variable ",i,": ",var.names[i]," is not of type numeric, ordered, or factor.")
    }

    # check for some variation in each variable
    if(length(unique(var.levels[[i]])) == 1)
    {
      warning("variable ",i,": ",var.names[i]," has no variation.")
    }
  }

  nClass <- 1

  if(!("name" %in% names(distribution))) {
    stop("The distribution is missing a `name` component; for example, ",
         "distribution = list(name = \"gaussian\").")
  }

  distribution.call.name <- distribution$name

  # Check for potential problems with the distribution

  if((distribution$name == "bernoulli") && !all(is.element(y,0:1))) {
    stop("Bernoulli requires the response to be in {0,1}")
    if (is.factor(y)) {
      y <- as.integer(y) - 1
    }
  }
  # close if (dist... == "multinomial"



  # create index upfront... subtract one for 0 based order
  x.order <- apply(x[1:nTrain,,drop=FALSE],2,order,na.last=FALSE)-1

  x <- as.vector(data.matrix(x))
  predF <- rep(0,length(y))
  train.error <- rep(0,n.trees)
  valid.error <- rep(0,n.trees)
  oobag.improve <- rep(0,n.trees)

  if(is.null(var.monotone)) {
    var.monotone <- rep(0,cCols)
  } else if(length(var.monotone)!=cCols) {
    stop("Length of var.monotone != number of predictors")
  } else if(!all(is.element(var.monotone,-1:1))) {
    stop("var.monotone must be -1, 0, or 1")
  }
  fError <- FALSE

  gbm.obj <- .Call("gbm_fit",
                   Y=as.double(y),
                   Offset=as.double(offset),
                   X=as.double(x),
                   X.order=as.integer(x.order),
                   weights=as.double(w),
                   Misc=as.double(Misc),
                   cRows=as.integer(cRows),
                   cCols=as.integer(cCols),
                   var.type=as.integer(var.type),
                   var.monotone=as.integer(var.monotone),
                   distribution=as.character(distribution.call.name),
                   n.trees=as.integer(n.trees),
                   interaction.depth=as.integer(interaction.depth),
                   n.minobsinnode=as.integer(n.minobsinnode),
                   n.classes = as.integer(nClass),
                   shrinkage=as.double(shrinkage),
                   bag.fraction=as.double(bag.fraction),
                   nTrain=as.integer(nTrain),
                   fit.old=as.double(NA),
                   n.cat.splits.old=as.integer(0),
                   n.trees.old=as.integer(0),
                   verbose=as.integer(verbose),
                   PACKAGE = "mypkg")

  names(gbm.obj) <- c("initF","fit","train.error","valid.error",
                      "oobag.improve","trees","c.splits")

  gbm.obj$bag.fraction <- bag.fraction
  gbm.obj$distribution <- distribution
  gbm.obj$interaction.depth <- interaction.depth
  gbm.obj$n.minobsinnode <- n.minobsinnode
  gbm.obj$num.classes <- nClass
  gbm.obj$n.trees <- length(gbm.obj$trees) / nClass
  gbm.obj$nTrain <- nTrain
  gbm.obj$train.fraction <- train.fraction
  gbm.obj$response.name <- response.name
  gbm.obj$shrinkage <- shrinkage
  gbm.obj$var.levels <- var.levels
  gbm.obj$var.monotone <- var.monotone
  gbm.obj$var.names <- var.names
  gbm.obj$var.type <- var.type
  gbm.obj$verbose <- verbose
  gbm.obj$Terms <- NULL


  if(keep.data) {
    gbm.obj$data <- list(
        y = y,
        x = x,
        x.order = x.order,
        offset = offset,
        Misc = Misc,
        w = w
      )

  }
  else {
    gbm.obj$data <- NULL
  }

  # Reuturn object of class "gbm"
  class(gbm.obj) <- "gbm"
  gbm.obj

}

