

#' @title Create data partitions
#' @description Create n data partitions for training with equal sized classes via resampling.
#' @param df A \code{data.frame} for analysis
#' @param dep_var A character string denoting the dependent variable in \code{df}.
#' @param level level of interest. If \code{NULL} takes the 2nd level of a factor variable
#' or the 2nd unique value from a non-factor variable.
#' @param n An integer denoting the number of ensembles to build. Defaults to \code{100L}.
#' @param major_class_wt Controls the number of major class cases selected in each
#' partition as a multiple of the number of minority class observations. Defaults to \code{1}, 
#' which will produce equal sized sets of minority and non-minority class in each partition.
#' Must be greater than or equal to 1.
#' Poisson and Gaussian distributions use bagging on the non-test dataset.
#' @param seed An integer. Seed for reproducibility. Defaults to \code{379L}.
#' @param test_pct A number in (0,1) specifying the size of the test dataset as a percentage.
#' Defaults to \code{0.33}
#' @param binomial Logical. Does the response variable follow a Binomial distribution? Defaults 
#' to \code{TRUE}.
#' @export
create_partitions <- function(df, dep_var, level= NULL, n= 100L, major_class_wt= 1,
                              seed= 379L, test_pct= 0.33, binomial= TRUE) {
  # error checking
  if (!is.data.frame(df)) stop("df must be a data.frame. Coercion is intentionally not supported.")
  if (!is.character(dep_var)) stop("dep_var must be a character.")
  if (n < 1L | (n %% 1 != 0)) stop("n must be a positive integer.")
  if (major_class_wt < 1) stop("major_class_wt must be >= 1.")
  if (!is.numeric(test_pct) | test_pct <= 0 | test_pct >= 1) 
    stop("test_pct must be a number in (0,1).")
  
  if (binomial) {
    if (missing(level) | is.null(level)) {
      if (is.factor(get(dep_var, envir= as.environment(df)))) {
        level <- levels(get(dep_var, envir= as.environment(df)))[2]
      } else {
        conv <- get(paste("as", typeof(get(dep_var, envir= as.environment(df))), sep= "."))
        level <- names(table(get(dep_var, envir= as.environment(df))))[2]
        level <- conv(level)
      }
    }
    # 01. create training / test partitions
    set.seed(seed)
    test_ind <- createDataPartition(y= get(dep_var, envir= as.environment(df)),
                                    p= test_pct, times= 1,
                                    list= TRUE)
  } else {
    set.seed(seed)
    test_ind <- list(sample(1:nrow(df), size= floor(nrow(df) * test_pct)))
  }
  
  test_dat <- df[test_ind[[1]],]
  train_temp <- df[-test_ind[[1]],]
  train_dat <- vector("list", length= n)
  
  if (binomial) {
    p_ind <- which(get(dep_var, envir= as.environment(train_temp)) == level)
    for (i in 1:n) {
      n_ind <- which(get(dep_var, envir= as.environment(train_temp)) != level)
      n_ind <- sample(n_ind, size= floor(length(p_ind) * major_class_wt), replace= TRUE)
      train_dat[[i]] <- train_temp[c(p_ind, n_ind),]
    }
  } else {
    for (i in 1:n) {
      train_dat[[i]] <- train_temp[sample(1:nrow(train_temp),
                            size= nrow(train_temp), replace=TRUE),]
    }
  }
  
  return(list(test=test_dat, train=train_dat))
}