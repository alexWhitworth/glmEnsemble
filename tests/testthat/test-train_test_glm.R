
library(glmEnsemble)
library(testthat)

context("train_test_glm")

test_that("binomial logit returns appropriately", {
  # setup
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$dep_var <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))
  p1 <- create_partitions(hprice, dep_var= "dep_var", n= 1L)
  
  glm_def <- glm(dep_var ~ ., data= hprice, family= binomial(link= "logit"))
  coef_nm <- colnames(model.matrix(glm_def))
  coef_vec <- rep(0, length(coef_nm)); names(coef_vec) <- coef_nm
  
  lg <- glmEnsemble:::train_test_glm(p1[[2]][[1]], p1[[1]],
                       out_vec= coef_vec, direction= "backward",
                       family= binomial(link="logit"))
  
  # expected return
  expect_true(is.list(lg))
  expect_equal(length(lg), 3)
  expect_equal(names(lg), c("coef", "weight", "family"))
  expect_equal(length(lg$coef), length(coef_vec))
  expect_true(is.numeric(lg$coef))
  expect_equal(length(lg$weight), 1)
  expect_true(is.numeric(lg$weight))
  expect_equal(lg$family, binomial(link="logit"))
})

test_that("binomial probit models work", {
  # setup
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$dep_var <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))
  p1 <- create_partitions(hprice, dep_var= "dep_var", n= 1L)
  
  glm_def <- glm(dep_var ~ ., data= hprice, family= binomial(link= "logit"))
  coef_nm <- colnames(model.matrix(glm_def))
  coef_vec <- rep(0, length(coef_nm)); names(coef_vec) <- coef_nm
  
  lg <- glmEnsemble:::train_test_glm(p1[[2]][[1]], p1[[1]],
                                     out_vec= coef_vec, direction= "backward",
                                     family= binomial(link="probit"))
  
  # expected return
  expect_true(is.list(lg))
  expect_equal(length(lg), 3)
  expect_equal(names(lg), c("coef", "weight", "family"))
  expect_equal(length(lg$coef), length(coef_vec))
  expect_true(is.numeric(lg$coef))
  expect_equal(length(lg$weight), 1)
  expect_true(is.numeric(lg$weight))
  expect_equal(lg$family, binomial(link="probit"))
})

test_that("poisson log models work", {
  # setup
  data(gala, package= "faraway")
  names(gala)[1] <- "dep_var"
  glm_def <- glm(dep_var ~ ., data= gala, family= poisson(link= "log"))
  coef_nm <- colnames(model.matrix(glm_def))
  coef_vec <- rep(0, length(coef_nm)); names(coef_vec) <- coef_nm
  
  gala_t <- gala[sample(1:nrow(gala), 10, replace=T),]
  
  lg <- glmEnsemble:::train_test_glm(gala, gala_t, 
                                     out_vec= coef_vec, direction= "backward",
                                     family= poisson(link= "log"))
  
  # expected return
  expect_true(is.list(lg))
  expect_equal(length(lg), 3)
  expect_equal(names(lg), c("coef", "weight", "family"))
  expect_equal(length(lg$coef), length(coef_vec))
  expect_true(is.numeric(lg$coef))
  expect_equal(length(lg$weight), 1)
  expect_true(is.numeric(lg$weight))
  expect_equal(lg$family, poisson(link= "log"))
})

test_that("gaussian identity models work", {
  # setup
  data(prostate, package= "faraway")
  names(prostate)[1] <- "dep_var"
  glm_def <- glm(dep_var ~ ., data= prostate, family= gaussian(link= "identity"))
  coef_nm <- colnames(model.matrix(glm_def))
  coef_vec <- rep(0, length(coef_nm)); names(coef_vec) <- coef_nm
  
  pros_t <- prostate[sample(1:nrow(prostate), 10, replace=T),]
  
  lg <- glmEnsemble:::train_test_glm(prostate, pros_t, 
                                     out_vec= coef_vec, direction= "backward",
                                     family= gaussian(link= "identity"))
  
  # expected return
  expect_true(is.list(lg))
  expect_equal(length(lg), 3)
  expect_equal(names(lg), c("coef", "weight", "family"))
  expect_equal(length(lg$coef), length(coef_vec))
  expect_true(is.numeric(lg$coef))
  expect_equal(length(lg$weight), 1)
  expect_true(is.numeric(lg$weight))
  expect_equal(lg$family, gaussian(link= "identity"))
})

