#' Quick Nodes Morph Boosted Regression Modeling (QNMBRM)
#'
#' adpet the weight to reduce the FLOPS of algorithm
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
#' @param weights A vector of weights of the same length as the \code{y}.
#'
#' @param  stage.level integer the levels of stages groups with different weight and
#' study rate. Default is 3.
#'
#' @param  stage.rate integer the rate of difference betweent differents levels.
#' it should be a number larger then 1. this effect the size and study rate of
#' most of stages. Default is 2
#'
#' @param n.trees Integer specifying the total number of trees to fit. This is
#' equivalent to the number of iterations and the number of basis functions in
#' the additive expansion. Default is 100.
#'
#' @param interaction.depth Integer specifying the maximum depth of each tree
#' (i.e., the highest level of variable interactions allowed). A value of 1
#' implies an additive model, a value of 2 implies a model with up to 2-way
#' interactions, etc. Default is automatic fill a considered number.
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
#' @param shrinkage.top a shrinkage parameter start to working with.
#'
#' @return A gbm object.
#'
#' @author
#' Henglin Huang
#'
#'
#' @export
#'
Qnmbrm <- function(formula = formula(data),
                  data = list(),weights=NULL,stage.level=3,stage.rate=2, n.trees = 100,
                  interaction.depth = NULL, n.minobsinnode = 10, shrinkage = 0.1,shrinkage.top=0.1) {


  mcall <- match.call()
  # Construct model frame, terms object, weights, and offset
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "weights"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.pass
  mf[[1]] <- as.name("model.frame")
  m <- mf
  mf <- eval(mf, parent.frame())
  Terms <- attr(mf, "terms")
  var.names <- attributes(Terms)$term.labels



  depth = length(var.names)

  #

  total = 0
  if(is.null(interaction.depth))
  {
    interaction.depth <- floor(ceiling(depth*(2**0.5))*1.1)
  }
  for (i in 1:stage.level){
    total = total+stage.rate^i
  }

  ## list the tree number
  treenum =c()
  treeleft=n.trees
  if(stage.level > 1){
    for (i in 1:(stage.level-1)) {
      treenum[i] = floor(n.trees*stage.rate^i/total)
      treeleft = treeleft -  treenum[i]
    }
  }
  treenum[stage.level] = treeleft


  ##kernel of accelerate
  if(is.null(weights)){
    gbm0 = mygbm(formula = formula,
                 data = data, n.trees = treenum[stage.level],
                 interaction.depth = interaction.depth, n.minobsinnode = n.minobsinnode, shrinkage = shrinkage.top)
  }else{
    gbm0 = mygbm(formula = formula,
                 data = data, n.trees = treenum[stage.level],
                 interaction.depth = interaction.depth, n.minobsinnode = n.minobsinnode, shrinkage = shrinkage.top)
  }


  if(stage.level > 1){
    current.shrinkage = shrinkage.top
    for (i in 2:stage.level) {
      current.shrinkage = (current.shrinkage - shrinkage)/2 + shrinkage
       if (i == stage.level){
         gbm0 <- gbm.more(gbm0, n.new.trees = treenum[i], shrinkage=current.shrinkage)

      }else {
         gbm0 <- gbm.more(gbm0, n.new.trees = treenum[i], shrinkage=shrinkage)
      }
    }
  }

  gbm0

}




