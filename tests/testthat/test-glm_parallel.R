
library(glmEnsemble)
library(testthat)

context("parallel glm")

test_that("binomial logit returns appropriately", {
  # setup
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$dep_var <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))
  p1 <- create_partitions(hprice, dep_var= "dep_var", n= 10L)
  
  glm_def <- glm(dep_var ~ ., data= hprice, family= binomial(link= "logit"))
  coef_nm <- colnames(model.matrix(glm_def))
  coef_vec <- rep(0, length(coef_nm)); names(coef_vec) <- coef_nm
  
  lg <- tt_glm_par(p1[[2]], p1[[1]],
                   out_vec= coef_vec, direction= "backward",
                   family= binomial(link="logit"))
  
  # expected return
  expect_true(is.list(lg))
  expect_equal(length(lg), 3)
  expect_equal(names(lg), c("coef_mat", "w_vec", "family"))
  expect_equal(ncol(lg$coef_mat), length(coef_vec))
  expect_equal(nrow(lg$coef_mat), length(p1[[2]]))
  expect_true(is.numeric(lg$coef_mat))
  expect_equal(length(lg$w_vec), length(p1[[2]]))
  expect_true(is.vector(lg$w_vec))
  expect_true(is.numeric(lg$w_vec))
  expect_equal(sum(lg$w_vec), 1)
  expect_equal(lg$family, binomial(link="logit"))
})


test_that("binomial probit returns appropriately", {
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$dep_var <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))
  p1 <- create_partitions(hprice, dep_var= "dep_var", n= 10L)
  
  glm_def <- glm(dep_var ~ ., data= hprice, family= binomial(link= "logit"))
  coef_nm <- colnames(model.matrix(glm_def))
  coef_vec <- rep(0, length(coef_nm)); names(coef_vec) <- coef_nm
  
  lg <- tt_glm_par(p1[[2]], p1[[1]],
                   out_vec= coef_vec, direction= "backward",
                   family= binomial(link="probit"))
  
  # expected return
  expect_true(is.list(lg))
  expect_equal(length(lg), 3)
  expect_equal(names(lg), c("coef_mat", "w_vec", "family"))
  expect_equal(ncol(lg$coef_mat), length(coef_vec))
  expect_equal(nrow(lg$coef_mat), length(p1[[2]]))
  expect_true(is.numeric(lg$coef_mat))
  expect_equal(length(lg$w_vec), length(p1[[2]]))
  expect_true(is.vector(lg$w_vec))
  expect_true(is.numeric(lg$w_vec))
  expect_equal(sum(lg$w_vec), 1)
  expect_equal(lg$family, binomial(link="probit"))
})

test_that("poisson log returns appropriately", {
  # setup
  data(gala, package= "faraway")
  names(gala)[1] <- "dep_var"
  p1 <- create_partitions(gala, dep_var= "dep_var", n= 10L)
  
  glm_def <- glm(dep_var ~ ., data= gala, family= poisson(link= "log"))
  coef_nm <- colnames(model.matrix(glm_def))
  coef_vec <- rep(0, length(coef_nm)); names(coef_vec) <- coef_nm
  
  lg <- tt_glm_par(p1[[2]], p1[[1]],
                   out_vec= coef_vec, direction= "backward",
                   family= poisson(link= "log"))
  
  # expected return
  expect_true(is.list(lg))
  expect_equal(length(lg), 3)
  expect_equal(names(lg), c("coef_mat", "w_vec", "family"))
  expect_equal(ncol(lg$coef_mat), length(coef_vec))
  expect_equal(nrow(lg$coef_mat), length(p1[[2]]))
  expect_true(is.numeric(lg$coef_mat))
  expect_equal(length(lg$w_vec), length(p1[[2]]))
  expect_true(is.vector(lg$w_vec))
  expect_true(is.numeric(lg$w_vec))
  expect_equal(sum(lg$w_vec), 1)
  expect_equal(lg$family, poisson(link= "log"))
})

test_that("gaussian identity returns appropriately", {
  # setup
  data(prostate, package= "faraway")
  names(prostate)[1] <- "dep_var"
  p1 <- create_partitions(prostate, dep_var= "dep_var", n= 10L, gaussian= TRUE)
  
  glm_def <- glm(dep_var ~ ., data= prostate, family= gaussian(link= "identity"))
  coef_nm <- colnames(model.matrix(glm_def))
  coef_vec <- rep(0, length(coef_nm)); names(coef_vec) <- coef_nm
  
  lg <- tt_glm_par(p1[[2]], p1[[1]],
                   out_vec= coef_vec, direction= "backward",
                   family= gaussian(link= "identity"))
  
  # expected return
  expect_true(is.list(lg))
  expect_equal(length(lg), 3)
  expect_equal(names(lg), c("coef_mat", "w_vec", "family"))
  expect_equal(ncol(lg$coef_mat), length(coef_vec))
  expect_equal(nrow(lg$coef_mat), length(p1[[2]]))
  expect_true(is.numeric(lg$coef_mat))
  expect_equal(length(lg$w_vec), length(p1[[2]]))
  expect_true(is.vector(lg$w_vec))
  expect_true(is.numeric(lg$w_vec))
  expect_equal(sum(lg$w_vec), 1)
  expect_equal(lg$family, gaussian(link= "identity"))
})