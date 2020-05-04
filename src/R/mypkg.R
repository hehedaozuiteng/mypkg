#' Mypkg lead file
#'
#' This package contains 3 training model: GBM, logistic, and SVM:1.Gradient Boosting Regression(GMB) is a learning method that combines many decision trees as the weak predictors. into an ensemble, forming a strong predictor. In this package, we are focus on the Bernoulli distribution, and write a series of tools to improve its accuracy and reduce its running time. Otherwise, we are trying to use control weight of each observations to anti the unbalance dataset. 2.logistic resgression takes both training and validation dataset as parameter and recursively finding best fit penalty term to reuce the influence of extreme values.3.A SVM package implemted by Python with SMO algorithm  method.
#'
#' @importFrom grDevices adjustcolor
#' @importFrom graphics plot
#' @importFrom stats delete.response dnorm
#' @importFrom stats model.extract model.frame model.offset model.response
#' @importFrom stats model.weights na.pass quantile reformulate terms var
#'
#' @useDynLib mypkg, .registration = TRUE
#'
#' @name mypkg
#'
#' @docType package
#'
#' @author Henglin Huang, Jing Xie , Hujie Han, Fang yi Wang
#'
#' @keywords package
NULL
