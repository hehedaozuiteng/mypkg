context("gbm")

##devtools::test()

test_that("gbm", {
  for (i in c(1000,2000,4000)){

  set.seed(1)
  N <- i
  X1 <- runif(N)
  X2 <- runif(N)
  X3 <- factor(sample(letters[1:4],N,replace=T))
  X4 <- runif(N)
  mu <- c(-1,0,1,2)[as.numeric(X3)]

  p <- 1/(1+exp(-(cos(2*X1) - 4*X2+X4**2 + mu)))
  Y <- rbinom(N,1,p)

  data <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4)

  # fit initial model
  gbm1 <- mygbm(Y~.,                    # formula
              data=data,                 # dataset
              n.trees=2000,              # number of trees
              shrinkage=0.04,           # shrinkage or learning rate, 0.001 to 0.1 usually work
              interaction.depth=9,       # 1: additive model, 2: two-way interactions, etc
              n.minobsinnode = 10)       # minimum total weight needed in each node

  gbm2 = Qnmbrm(Y~.,                    # formula
                data=data,                 # dataset
                n.trees=2000,              # number of trees
                shrinkage=0.04,
                shrinkage.top=0.1,# shrinkage or learning rate, 0.001 to 0.1 usually work                                          # 1: additive model, 2: two-way interactions, etc
                n.minobsinnode = 10)
  best_tree = which.min(gbm1[["train.error"]])
  best_tree2 = which.min(gbm2[["train.error"]])
  # make some new data
  set.seed(2)
  N <- i
  X1 <- runif(N)
  X2 <- runif(N)
  X3 <- factor(sample(letters[1:4],N,replace=T))
  X4 <- runif(N)
  mu <- c(-1,0,1,2)[as.numeric(X3)]

  p <- 1/(1+exp(-(cos(2*X1) - 4*X2+X4**2 + mu)))
  Y <- rbinom(N,1,p)

  data2 <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4)


  #predict the value with model
  predict_odds <- predict(gbm1,data2, n.trees= gbm1$n.trees)
  predict_odds2 <- predict(gbm2,data2, n.trees= gbm2$n.trees)


  predict_probility = 1/(1+exp(-(predict_odds)))
  predict_y = rbinom(N,1,predict_probility)
  etheta = sum(predict_y)/N

  predict_probility2 = 1/(1+exp(-(predict_odds2)))
  predict_y2 = rbinom(N,1,predict_probility2)
  etheta2 = sum(predict_y2)/N

  # calculate the likelihood of Bernoulli distribution

  theta = sum(Y)/N
  loglik <- function(prob, theta) {
    theta*log(prob)+(length(theta)-theta)*log(1-prob)
  }

  x = seq(from = 0, to = 1, by = 0.0005)
  y = vector(length = length(x))
  for (i in 1:2001){
    y[i] = loglik(x[i],theta)

  }
  plot(x = x,y = y,type="l",ylab = "Bernoulli likelihood",xlab = "theta")
  abline(v=etheta, col="blue")
  sprintf("the true theta is %f ,the estimated theta is %f",theta,etheta)


  plot(x = x,y = y,type="l",ylab = "Bernoulli likelihood",xlab = "theta")
  abline(v=etheta2, col="blue")
  sprintf("the true theta is %f ,the estimated theta is %f",theta,etheta2)

  # compare the standard deviation between the true probability with the predict probability

  p.new =cos(2*X1) - 4*X2+X4**2 + mu
  sd(p.new - predict_probility)
  # RUnit::checkTrue(sd(p.new - predict_probility) < 1.5 )
  expect_equal(abs(etheta-theta)< 0.1*theta ,TRUE)
  expect_equal(sd(p.new - predict_probility)<1.5 ,TRUE)
  expect_equal(abs(etheta2-theta)< 0.1*theta ,TRUE)
  expect_equal(sd(p.new - predict_probility2)<1.5 ,TRUE)
  }
})
