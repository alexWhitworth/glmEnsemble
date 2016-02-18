
library(glmEnsemble)
library(testthat)

context("glm_ensemble")

test_that("errors work appropriately", {
  data(prostate, package= "faraway")
  
  expect_error({g1 <- glm_ensemble(prostate, "lcavol", test_pct = 0)})
  expect_error({g1 <- glm_ensemble(prostate, "lcavol", test_pct = -.5)})
  expect_error({g1 <- glm_ensemble(prostate, "lcavol", test_pct = 1.25)})
  expect_error({g1 <- glm_ensemble(prostate, "lcavol", cols= c(2:5,10))})
  expect_error({g1 <- glm_ensemble(prostate, "lcavol", cols= c(-1,2:8))})
  expect_error({g1 <- glm_ensemble(prostate, "lcavol", cols= c(0,2:8))})
})

test_that("ensembles return correctly, binomial-logit (defaults)", {
  # setup
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$high_price <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))
  
  # otherwise defaults
  g1 <- glm_ensemble(hprice, dep_var= "high_price", n= 10L, family= binomial(link= "logit"))
  
  # expected returns:
  expect_equal(class(g1), "glmEnsemble")
  expect_true(is.list(g1))
  expect_equal(length(g1), 2)
  expect_equal(names(g1), c("wt_coef", ""))
  expect_true(is.numeric(g1[[1]]))
  expect_true(!is.null(names(g1[[1]])))
  
  expect_equal(length(g1[[2]]), 8)
  expect_true(is.matrix(g1[[2]]$ensemble_coef))
  expect_equal(nrow(g1[[2]]$ensemble_coef), g1[[2]]$n)
  expect_equal(ncol(g1[[2]]$ensemble_coef), length(g1[[1]]))
  expect_equal(length(g1[[2]]$ensemble_wts), nrow(g1[[2]]$ensemble_coef))
  expect_equal(length(g1[[2]]$ensemble_wts), g1[[2]]$n)
  expect_equal(g1[[2]]$family, binomial(link= "logit"))
  expect_equal(class(g1[[2]]$terms), c("terms", "formula"))
  expect_equal(nrow(hprice), nrow(g1[[2]]$data))
  expect_equal(ncol(hprice) - 1, ncol(g1[[2]]$data))
  expect_equal(length(g1[[2]]$cols), ncol(g1[[2]]$data))
  expect_true(is.numeric(g1[[2]]$cols))
})
 
test_that("ensembles return correctly, binomial-logit (forward)", {
  # setup
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$high_price <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0)) 
  
  # forward selection
  g1 <- glm_ensemble(hprice, dep_var= "high_price", n= 10L, direction= "forward",
                     family= binomial(link= "logit"))
  
  # expected returns:
  expect_equal(class(g1), "glmEnsemble")
  expect_true(is.list(g1))
  expect_equal(length(g1), 2)
  expect_equal(names(g1), c("wt_coef", ""))
  expect_true(is.numeric(g1[[1]]))
  expect_true(!is.null(names(g1[[1]])))
  
  expect_equal(length(g1[[2]]), 8)
  expect_true(is.matrix(g1[[2]]$ensemble_coef))
  expect_equal(nrow(g1[[2]]$ensemble_coef), g1[[2]]$n)
  expect_equal(ncol(g1[[2]]$ensemble_coef), length(g1[[1]]))
  expect_equal(length(g1[[2]]$ensemble_wts), nrow(g1[[2]]$ensemble_coef))
  expect_equal(length(g1[[2]]$ensemble_wts), g1[[2]]$n)
  expect_equal(g1[[2]]$family, binomial(link= "logit"))
  expect_equal(class(g1[[2]]$terms), c("terms", "formula"))
  expect_equal(nrow(hprice), nrow(g1[[2]]$data))
  expect_equal(ncol(hprice) - 1, ncol(g1[[2]]$data))
  expect_equal(length(g1[[2]]$cols), ncol(g1[[2]]$data))
  expect_true(is.numeric(g1[[2]]$cols))
})

test_that("ensembles return correctly, binomial-logit (forward)", {
  # setup
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$high_price <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0)) 
  
  # stepwise selection
  g1 <- glm_ensemble(hprice, dep_var= "high_price", n= 10L, direction= "both",
                     family= binomial(link= "logit"))
  
  # expected returns:
  expect_equal(class(g1), "glmEnsemble")
  expect_true(is.list(g1))
  expect_equal(length(g1), 2)
  expect_equal(names(g1), c("wt_coef", ""))
  expect_true(is.numeric(g1[[1]]))
  expect_true(!is.null(names(g1[[1]])))
  
  expect_equal(length(g1[[2]]), 8)
  expect_true(is.matrix(g1[[2]]$ensemble_coef))
  expect_equal(nrow(g1[[2]]$ensemble_coef), g1[[2]]$n)
  expect_equal(ncol(g1[[2]]$ensemble_coef), length(g1[[1]]))
  expect_equal(length(g1[[2]]$ensemble_wts), nrow(g1[[2]]$ensemble_coef))
  expect_equal(length(g1[[2]]$ensemble_wts), g1[[2]]$n)
  expect_equal(g1[[2]]$family, binomial(link= "logit"))
  expect_equal(class(g1[[2]]$terms), c("terms", "formula"))
  expect_equal(nrow(hprice), nrow(g1[[2]]$data))
  expect_equal(ncol(hprice) - 1, ncol(g1[[2]]$data))
  expect_equal(length(g1[[2]]$cols), ncol(g1[[2]]$data))
  expect_true(is.numeric(g1[[2]]$cols))
})


test_that("ensembles return correctly, binomial-probit (defaults)", {
  # setup
  data(hprice, package= "faraway"); hprice$msa <- NULL
  hprice$high_price <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))
  
  # otherwise defaults
  g1 <- glm_ensemble(hprice, dep_var= "high_price", n= 10L, family= binomial(link= "probit"))
  
  # expected returns:
  expect_equal(class(g1), "glmEnsemble")
  expect_true(is.list(g1))
  expect_equal(length(g1), 2)
  expect_equal(names(g1), c("wt_coef", ""))
  expect_true(is.numeric(g1[[1]]))
  expect_true(!is.null(names(g1[[1]])))
  
  expect_equal(length(g1[[2]]), 8)
  expect_true(is.matrix(g1[[2]]$ensemble_coef))
  expect_equal(nrow(g1[[2]]$ensemble_coef), g1[[2]]$n)
  expect_equal(ncol(g1[[2]]$ensemble_coef), length(g1[[1]]))
  expect_equal(length(g1[[2]]$ensemble_wts), nrow(g1[[2]]$ensemble_coef))
  expect_equal(length(g1[[2]]$ensemble_wts), g1[[2]]$n)
  expect_equal(g1[[2]]$family, binomial(link= "probit"))
  expect_equal(class(g1[[2]]$terms), c("terms", "formula"))
  expect_equal(nrow(hprice), nrow(g1[[2]]$data))
  expect_equal(ncol(hprice) - 1, ncol(g1[[2]]$data))
  expect_equal(length(g1[[2]]$cols), ncol(g1[[2]]$data))
  expect_true(is.numeric(g1[[2]]$cols))
})

test_that("ensembles return correctly, poisson-log (defaults)", {
  # setup
  data(gala, package= "faraway")
  
  # otherwise defaults
  g1 <- glm_ensemble(gala, dep_var= "Species", n= 10L, family= poisson(link= "log"))
  
  # expected returns:
  expect_equal(class(g1), "glmEnsemble")
  expect_true(is.list(g1))
  expect_equal(length(g1), 2)
  expect_equal(names(g1), c("wt_coef", ""))
  expect_true(is.numeric(g1[[1]]))
  expect_true(!is.null(names(g1[[1]])))
  
  expect_equal(length(g1[[2]]), 8)
  expect_true(is.matrix(g1[[2]]$ensemble_coef))
  expect_equal(nrow(g1[[2]]$ensemble_coef), g1[[2]]$n)
  expect_equal(ncol(g1[[2]]$ensemble_coef), length(g1[[1]]))
  expect_equal(length(g1[[2]]$ensemble_wts), nrow(g1[[2]]$ensemble_coef))
  expect_equal(length(g1[[2]]$ensemble_wts), g1[[2]]$n)
  expect_equal(g1[[2]]$family, poisson(link= "log"))
  expect_equal(class(g1[[2]]$terms), c("terms", "formula"))
  expect_equal(nrow(gala), nrow(g1[[2]]$data))
  expect_equal(ncol(gala) - 1, ncol(g1[[2]]$data))
  expect_equal(length(g1[[2]]$cols), ncol(g1[[2]]$data))
  expect_true(is.numeric(g1[[2]]$cols))
})

test_that("ensembles return correctly, gaussian-identity (defaults)", {
  # setup
  data(prostate, package= "faraway")
  
  # otherwise defaults
  g1 <- glm_ensemble(prostate, dep_var= "lcavol", n= 10L, family= gaussian(link= "identity"))
  
  # expected returns:
  expect_equal(class(g1), "glmEnsemble")
  expect_true(is.list(g1))
  expect_equal(length(g1), 2)
  expect_equal(names(g1), c("wt_coef", ""))
  expect_true(is.numeric(g1[[1]]))
  expect_true(!is.null(names(g1[[1]])))
  
  expect_equal(length(g1[[2]]), 8)
  expect_true(is.matrix(g1[[2]]$ensemble_coef))
  expect_equal(nrow(g1[[2]]$ensemble_coef), g1[[2]]$n)
  expect_equal(ncol(g1[[2]]$ensemble_coef), length(g1[[1]]))
  expect_equal(length(g1[[2]]$ensemble_wts), nrow(g1[[2]]$ensemble_coef))
  expect_equal(length(g1[[2]]$ensemble_wts), g1[[2]]$n)
  expect_equal(g1[[2]]$family, gaussian(link= "identity"))
  expect_equal(class(g1[[2]]$terms), c("terms", "formula"))
  expect_equal(nrow(prostate), nrow(g1[[2]]$data))
  expect_equal(ncol(prostate) - 1, ncol(g1[[2]]$data))
  expect_equal(length(g1[[2]]$cols), ncol(g1[[2]]$data))
  expect_true(is.numeric(g1[[2]]$cols))
})
