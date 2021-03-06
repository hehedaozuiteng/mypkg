#' Generalized Boosted Regression Modeling (GBM)
#'
#' Fits generalized boosted regression models.
#'
#'
#' @param formula A symbolic description of the model to be fit. The formula
#' may include an offset term (e.g. y~offset(n)+x). If
#' \code{keep.data = FALSE} in the initial call to \code{gbm} then it is the
#' user's responsibility to resupply the offset to gbm.more.
#'
#'
#' @param data an optional data frame containing the variables in the model. By
#' default the variables are taken from \code{environment(formula)}, typically
#' the environment from which \code{gbm} is called. If \code{keep.data=TRUE} in
#' the initial call to \code{gbm} then \code{gbm} stores a copy with the
#' object. If \code{keep.data=FALSE} then subsequent calls to
#' gbm.more must resupply the same dataset. It becomes the user's
#' responsibility to resupply the same data at this point.
#'
#'
#' @param weights A vector of weights of the same length as the \code{y}.
#'
#' @param n.trees Integer specifying the total number of trees to fit. This is
#' equivalent to the number of iterations and the number of basis functions in
#' the additive expansion. Default is 100.
#'
#' @param interaction.depth Integer specifying the maximum depth of each tree
#' (i.e., the highest level of variable interactions allowed). A value of 1
#' implies an additive model, a value of 2 implies a model with up to 2-way
#' interactions, etc. Default is 1.
#'
#' @param n.minobsinnode Integer specifying the minimum number of observations
#' in the terminal nodes of the trees. Note that this is the actual number of
#' observations, not the total weight.
#'
#' @param shrinkage a shrinkage parameter applied to each tree in the
#' expansion. Also known as the learning rate or step-size reduction; 0.001 to
#' 0.1 usually work, but a smaller learning rate typically requires more trees.
#' Default is 0.1.
#'
#' @param bag.fraction the fraction of the training set observations randomly
#' selected to propose the next tree in the expansion. This introduces
#' randomnesses into the model fit. If \code{bag.fraction} < 1 then running the
#' same model twice will result in similar but different fits. \code{gbm} uses
#' the R random number generator so \code{set.seed} can ensure that the model
#' can be reconstructed.  Default is 0.5.
#'
#'
#' @param keep.data a logical variable indicating whether to keep the data and
#' an index of the data stored with the object.
#'
#'
#' @param n.cores The number of CPU cores to use. The cross-validation loop
#' will attempt to send different CV folds off to different cores. If
#' \code{n.cores} is not specified by the user, it is guessed using the
#' \code{detectCores} function in the \code{parallel} package. Note that the
#' documentation for \code{detectCores} makes clear that it is not failsafe and
#' could return a spurious number of available cores.
#'
#' @return A gbm object.
#'
#' @details
#'
#' \code{gbm} implement from the Generalized Boosted Regression Models
#'
#' \code{gbm.fit} provides the link between R and the C++ gbm engine.
#' \code{gbm} is a front-end to \code{gbm.fit} that uses the familiar R
#' modeling formulas. For power-users with many variables use
#' \code{gbm.fit}. For general practice \code{gbm} is preferable.
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
#'
#' Henglin Huang
#'
#'
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
mygbm <- function(formula = formula(data),
                data = list(), weights=NULL, n.trees = 100,
                interaction.depth = 1, n.minobsinnode = 10, shrinkage = 0.1,
                bag.fraction = 0.5,
                keep.data = TRUE,
                n.cores = NULL) {
  train.fraction = 1.0
  cv.folds = 0
  var.monotone = NULL
  verbose = FALSE
  class.stratify.cv = NULL

  # Match the call to gbm
  mcall <- match.call()

  # Construct model frame, terms object, weights, and offset
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "weights", "offset"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.pass
  mf[[1]] <- as.name("model.frame")
  m <- mf
  mf <- eval(mf, parent.frame())
  Terms <- attr(mf, "terms")
  w <- model.weights(mf)
  offset <- model.offset(mf)


  # Extract and check response values
  y <- model.response(mf)

  # Construct data frame of predictor values
  var.names <- attributes(Terms)$term.labels
  x <- model.frame(terms(reformulate(var.names)), data = data,
                   na.action = na.pass)


  # Extract response name as a character string
  response.name <- as.character(formula[[2L]])

  nTrain <- floor(train.fraction * nrow(x))

  # Set up for k-fold cross-validation
  cv.error <- NULL


  # called the gbm fit
  gbm.obj <- mygbm.fit(x = x, y = y, offset = offset,
                     w = weights, var.monotone = var.monotone, n.trees = n.trees,
                     interaction.depth = interaction.depth,
                     n.minobsinnode = n.minobsinnode, shrinkage = shrinkage,
                     bag.fraction = bag.fraction, nTrain = nTrain,
                     keep.data = keep.data, verbose = verbose, response.name = response.name)

  # Attach further components
  gbm.obj$train.fraction <- train.fraction
  gbm.obj$Terms <- Terms
  gbm.obj$cv.error <- cv.error
  gbm.obj$cv.folds <- cv.folds
  gbm.obj$call <- mcall
  gbm.obj$m <- m
  gbm.obj$tree <-  n.trees


  # Return "gbm" object
  gbm.obj

}
