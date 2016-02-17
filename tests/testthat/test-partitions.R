

library(glmEnsemble)
library(testthat)


context("data partitioning")

test_that("error checking", {
  # setup 
  set.seed(14L)
  dtf <- data.frame(x1= rnorm(100),
                    x2= rnorm(100, mean=2),
                    x3= runif(100, 2,5))
  dtf$eta <- 2 * dtf$x1 + 4 * dtf$x2 + rnorm(100) * dtf$x3 + rnorm(100, sd= 0.5)
  dtf$p <- exp(dtf$eta) / (1+exp(dtf$eta))
  dtf$y <- apply(dtf, 1, function(i) sample(c(0,1), size= 1, replace=TRUE, prob = c(i[5], 1- i[5])))
  a <- runif(10)
  b <- letters[1:10]
  
  expect_error(create_partitions(a))
  expect_error(create_partitions(data.frame(b)))
  expect_error(create_partitions(dtf, "y", n= -1))
  expect_error(create_partitions(dtf, "y", n= 2.5))
  expect_error(create_partitions(dtf, "y", n= 10, major_class_wt = -1))
  expect_error(create_partitions(dtf, "y", n= 10, major_class_wt = 0.5))
  expect_error(create_partitions(dtf, "y", n= 10, major_class_wt = 1, test_pct = 0))
  expect_error(create_partitions(dtf, "y", n= 10, major_class_wt = 1, test_pct = 1))
  expect_error(create_partitions(dtf, "y", n= 10, major_class_wt = 1, test_pct = -0.5))
})

test_that("partitioning works", {
  # setup
  data(hprice, package= "faraway")
  hprice$high_price <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))
  
  p1 <- create_partitions(hprice, dep_var= "high_price", n= 10L)
  p2 <- create_partitions(hprice, dep_var= "high_price", n= 10L, major_class_wt = 2)
  
  expect_true(is.list(p1))
  expect_equal(length(p1), 2)
  expect_true(is.data.frame(p1[[1]]))
  expect_true(is.list(p1[[2]]))
  expect_equal(length(p1[[2]], 10L))
  expect_true(all(unlist(lapply(p1[[2]], class)) == "data.frame"))
  
  expect_true(all(unlist(lapply(p1[[2]], nrow)) <
                  unlist(lapply(p2[[2]], nrow))))
  expect_true(all(unlist(lapply(p1[[2]], ncol)) ==
                    unlist(lapply(p2[[2]], ncol))))
  
})
