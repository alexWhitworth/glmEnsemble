
library(glmEnsemble)
library(testthat)

context("regression accuracy metrics work") 

test_that("can produce length errors", {
  expect_error(rmse(rnorm(10), rnorm(9)))
  expect_error(rmse(rnorm(9), rnorm(10)))
  expect_error(mnAD(rnorm(10), rnorm(9)))
  expect_error(mnAD(rnorm(10), rnorm(9)))
  expect_error(mxAD(rnorm(10), rnorm(9)))
  expect_error(mxAD(rnorm(10), rnorm(9)))
  expect_error(wmnAD(rnorm(10), rnorm(9)))
  expect_error(wmnAD(rnorm(10), rnorm(9)))
})

test_that("get correct accuracy metrics", {
  set.seed(234L)
  x <- rnorm(50)
  y <- rnorm(50)
  
  expect_equal(rmse(x,y), sqrt(sum((x - y)^2, na.rm=T) / length(x)))
  expect_equal(rmse(x,y), 1.165133, tol= 0.0001)
  
  expect_equal(mnAD(x,y), mean(abs(x - y)))
  expect_equal(mnAD(x,y), 0.8854501, tol= 0.0001)
  
  expect_equal(wmnAD(x,y), {
    wts <- log(abs(y) + 2)
    1/ sum(wts, na.rm=T) * sum(wts * abs(x - y))})
  expect_equal(wmnAD(x,y), 0.9414487, tol= 0.0001)
  
  expect_equal(mxAD(x,y), max(abs(x - y)))
  expect_equal(mxAD(x,y), 3.127143, tol= 0.0001)
})


context("class accuracy metrics work") 

test_that("two class accuracy: errors, return-type, and bounds", {
  set.seed(1234L)
  p_vec <- runif(100)
  p_vec2 <- runif(75)
  c_vec <- sample(c(0,1), size= 100, replace= T, prob= c(.9,.1))
  w_vec <- runif(100)
  w_vec2 <- runif(75)
  
  # length errors
  expect_error(class_logloss(p_vec2, c_vec))
  expect_error(class_logloss(p_vec, c_vec, w_vec2))
  
  # return type
  expect_equal(length(class_logloss(p_vec, c_vec)), 1)
  expect_true(is.numeric(class_logloss(p_vec, c_vec)))
  
  # bounds
  expect_lt(class_logloss(p_vec, c_vec), 0)
  expect_lt(class_logloss(p_vec, c_vec, w_vec), 0)
  
  expect_gt(class_logloss(p_vec, c_vec), -Inf)
  expect_gt(class_logloss(p_vec, c_vec, w_vec), -Inf)
  
})

test_that("multiclass accuracy: errors, return-type, and bounds", {
  set.seed(97L)
  p_mat <- matrix(runif(90), ncol=3); p_mat <- t(apply(p_mat, 1, function(i) i / sum(i)))
  p_mat2 <- matrix(runif(81), ncol=3); p_mat2 <- t(apply(p_mat2, 1, function(i) i / sum(i)))
  c_mat <- t(rmultinom(30, size=1, prob= c(.6,.3,.1)))
  c_mat2 <- matrix(sample(letters, 90, replace= T), 30, 3)
  w_vec <- runif(30); w_vec2 <- runif(27)
  
  # length errors
  expect_error(multiclass_logloss(p_mat2, c_mat))
  expect_error(multiclass_logloss(p_mat, c_mat2))
  expect_error(multiclass_logloss(p_mat, c_mat, w_vec2))
  
  # return type
  expect_equal(length(multiclass_logloss(p_mat, c_mat)), 1)
  expect_true(is.numeric(multiclass_logloss(p_mat, c_mat)))
  
  # bounds
  expect_lt(multiclass_logloss(p_mat, c_mat), 0)
  expect_lt(multiclass_logloss(p_mat, c_mat, w_vec), 0)
  expect_gt(multiclass_logloss(p_mat, c_mat), -Inf)
  expect_gt(multiclass_logloss(p_mat, c_mat, w_vec), -Inf)
  
})


context("roc calcs")

test_that("ROC curves rock!", {
  set.seed(21345L)
  p <- runif(100)
  cv <- factor(ifelse(runif(100) > .75, TRUE, FALSE))
  
  expect_equal(dim(calc_roc(p, cv)), c(100,4))
  expect_true(all(calc_roc(p, cv) <= 1))
  expect_true(all(calc_roc(p, cv) >= 0))
  expect_equal(rownames(calc_roc(p, cv, thresholds = seq(0,1,.01))), as.character(seq(0,1,.01)))
})