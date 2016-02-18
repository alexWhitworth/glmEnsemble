
library(glmEnsemble)
library(testthat)

# ************************
context("predict.glmEnsemble - response")
# ************************

test_that("predictions valid, binomial-logit", {
  # setup
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$high_price <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))
  g1 <- glm_ensemble(hprice, dep_var= "high_price", n= 10L, family= binomial(link= "logit"))
  pred <- predict(g1, newdata= hprice, type= "response")
  
  # test results:
  expect_equal(length(pred), nrow(hprice))
  expect_true(is.numeric(pred))
  expect_equal(sum(is.na(pred)), 0)
  expect_true(all(is.finite(pred)))
  expect_true(all(pred <= 1))
  expect_true(all(pred >= 0))
})

test_that("predictions valid, binomial-probit", {
  # setup
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$high_price <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))
  g1 <- glm_ensemble(hprice, dep_var= "high_price", n= 10L, family= binomial(link= "probit"))
  pred <- predict(g1, newdata= hprice, type= "response")
  
  # test results:
  expect_equal(length(pred), nrow(hprice))
  expect_true(is.numeric(pred))
  expect_equal(sum(is.na(pred)), 0)
  expect_true(all(is.finite(pred)))
  expect_true(all(pred <= 1))
  expect_true(all(pred >= 0))
})

test_that("predictions valid, poisson-log", {
  # setup
  data(gala, package= "faraway")
  g1 <- glm_ensemble(gala, dep_var= "Species", n= 10L, family= poisson(link= "log"))
  pred <- predict(g1, newdata= gala, type= "response")
  
  # test results:
  expect_equal(length(pred), nrow(gala))
  expect_true(is.numeric(pred))
  expect_equal(sum(is.na(pred)), 0)
  expect_true(all(is.finite(pred)))
  expect_true(all(pred >= 0))
  expect_true(all(pred %% 1 == 0))
})

test_that("predictions valid, gaussian-identity", {
  # setup
  data(prostate, package= "faraway")
  g1 <- glm_ensemble(prostate, dep_var= "lcavol", n= 10L, family= gaussian(link= "identity"))
  pred <- predict(g1, newdata= prostate, type= "response")
  
  # test results
  expect_equal(length(pred), nrow(prostate))
  expect_true(is.numeric(pred))
  expect_equal(sum(is.na(pred)), 0)
  expect_true(all(is.finite(pred)))
  
})

# ************************
context("predict.glmEnsemble - response (link)")
# ************************

test_that("predictions valid, binomial-logit", {
  # setup
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$high_price <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))
  g1 <- glm_ensemble(hprice, dep_var= "high_price", n= 10L, family= binomial(link= "logit"))
  pred <- predict(g1, newdata= hprice, type= "link")
  
  # test results:
  expect_equal(length(pred), nrow(hprice))
  expect_true(is.numeric(pred))
  expect_equal(sum(is.na(pred)), 0)
  expect_true(all(is.finite(pred)))
  
})

test_that("predictions valid, binomial-probit", {
  # setup
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$high_price <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))
  g1 <- glm_ensemble(hprice, dep_var= "high_price", n= 10L, family= binomial(link= "probit"))
  pred <- predict(g1, newdata= hprice, type= "link")
  
  # test results:
  expect_equal(length(pred), nrow(hprice))
  expect_true(is.numeric(pred))
  expect_equal(sum(is.na(pred)), 0)
  expect_true(all(is.finite(pred)))
  
})

test_that("predictions valid, poisson-log", {
  # setup
  data(gala, package= "faraway")
  g1 <- glm_ensemble(gala, dep_var= "Species", n= 10L, family= poisson(link= "log"))
  pred <- predict(g1, newdata= gala, type= "link")
  
  # test results:
  expect_equal(length(pred), nrow(gala))
  expect_true(is.numeric(pred))
  expect_equal(sum(is.na(pred)), 0)
  expect_true(all(is.finite(pred)))
})

test_that("predictions valid, gaussian-identity", {
  # setup
  data(prostate, package= "faraway")
  g1 <- glm_ensemble(prostate, dep_var= "lcavol", n= 10L, family= gaussian(link= "identity"))
  pred <- predict(g1, newdata= prostate, type= "link")
  
  # test results
  expect_equal(length(pred), nrow(prostate))
  expect_true(is.numeric(pred))
  expect_equal(sum(is.na(pred)), 0)
  expect_true(all(is.finite(pred)))
  
})