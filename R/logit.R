
# @description Takes a single training dataset and test dataset and builds 
# and a logit-model. 
# @param tr_dat A \code{data.frame} for training
# @param test_dat A \code{data.frame} for calculating error rate.
# @param out_vec A named \code{vector} "backbone" on which to place ensemble coefficients.
# @param direction A character string for the direction used in \code{step}
# @param family Used to specify the details of the glm methods. See \code{\link{[stats]family}}
train_test_glm <- function(tr_dat, test_dat, out_vec, direction, family= binomial(link="logit")) {
  
  temp_logit <- step(glm(dep_var ~., data= tr_dat, family= binomial(link="logit")),
                     trace= 0, direction= direction)
  # save coef
  ind_m <- unlist(sapply(names(coef(temp_logit)), function(i) {
    which(i == names(out_vec))}))
  out_vec[ind_m] <- coef(temp_logit)
  
  # calc accuracy / weight
  p <- predict(temp_logit, newdata= test_dat, type= "response")
  if (all.equal(family, binomial(link="logit"))) {
    w <- class_logloss(p, ifelse(test_dat$dep_var == 1, 1, 0))
  } else if (class(family) == "family") {
    w <- rmse(obs= test_dat$dep_var, pred= p)
  }
  
  
  # return
  return(list(coef= out_vec, weight= w, family= family))
}


#' @title Train-Test a GLM ensemble in parallel
#' @description Takes a list of training datasets and a single test dataset and builds 
#' an ensemble of GLMs in parallel. Additionally calculates ensemble weights based
#' on element accuracy.
#' @param train_list A \code{list} of training \code{data.frames} for ensemble training
#' @param test_dat A \code{data.frame} for calculating error rates from ensemble elements.
#' @param out_vec A named \code{vector} "backbone" on which to place ensemble coefficients.
#' @param direction A character string for the direction used in \code{\link{[stats]step}}
#' @param family Used to specify the details of the glm methods. See \code{\link{[stats]family}}
#' @param leave_cores An integer for number of cores to leave unused. Defaults to \code{1L}.
#' @export
tt_glm_par <- function(train_list, test_dat, out_vec, direction,
                         family= binomial(link= "logit"), leave_cores= 1) {
  nnodes <- min(length(train_list), detectCores() - leave_cores)
  
  if (grepl("Windows", sessionInfo()$running)) {cl <- makeCluster(nnodes, type= "PSOCK")}
  else {cl <- makeCluster(nnodes, type= "FORK")}
  
  ens_list <- do.call("c", clusterApplyLB(cl= cl, x= parallel:::splitList(train_list, length(cl)), 
              fun= lapply, FUN= train_test_glm,
              test_dat= test_dat,
              out_vec= out_vec,
              direction= direction,
              family= family))
  stopCluster(cl)
  
  coef_mat <- do.call("rbind", lapply(ens_list, "[[", 1))
  w_vec <- do.call("c", lapply(ens_list, "[[", 2))
  
  return(list(coef_mat= coef_mat, w_vec= w_vec / sum(w_vec),
              family= family))
}


#' @title Fit a GLM Ensemble model
#' @description Fit a GLM Ensemble model via \code{\link{[stats]glm}} in parallel. Training datasets
#' are chosen to have equal number of each class; and, a single dataset is used to determine
#' prediction error (and ensemble weight) for each element in the ensemble. Each GLM does variable
#' selection via the \code{\link{[stats]step}} function with non-verbose output.
#' @param df A \code{data.frame} for analysis
#' @param dep_var A character string denoting the dependent variable in \code{df}.
#' @param cols A vector of column indices corresponding the the variables you wish to regress on.
#' @param n An integer denoting the number of ensembles to build; defaults to \code{100L}.
#' @param level level of interest. If \code{NULL} takes the 2nd level of a factor variable
#' or the 2nd unique value from a non-factor variable.
#' @param major_class_wt Controls the number of major class cases selected in each
#' partition as a multiple of the number of minority class observations. Defaults to \code{1}, 
#' which will produce equal sized sets of minority and non-minority class in each partition.
#' Must be greater than or equal to 1.
#' @param seed An integer. Seed for reproducibility; defaults to \code{379L}.
#' @param test_pct A number in (0,1) specifying the size of the test dataset as a percentage.
#' @param direction A character vector for the step process.
#' @param family Used to specify the details of the glm methods. See \code{\link{[stats]family}}
#' @param leave_cores An integer for number of cores to leave unused. 
#' @return A list of with a matrix of coefficients from each ensemble element, the 
#' element weights, and the weighted coefficient estimates.
#' @export
glm_ensemble <- function(df, dep_var, cols= which(names(df) != dep_var), n= 100L, 
                         level= NULL,
                         major_class_wt= 1,
                         seed= 379L, test_pct= 0.33, 
                         direction= "backward", 
                         family= binomial(link= "logit"),
                         leave_cores= NULL) {
  if (test_pct <= 0 | test_pct >= 1) stop("test_pct must be in (0,1).")
  if (!is.numeric(cols) | length(cols) >= ncol(df)) 
    stop("cols must be a numeric vector with length < ncol(df).")
  
  if (is.null(leave_cores)) {leave_cores <- 1}
  
  df <- data.frame(df)
  df <- df[, c(dep_var, names(df)[cols])]
  names(df)[1] <- "dep_var"
  # 01. Create data partitions
  dat <- create_partitions(df= df, dep_var= "dep_var", n=n, 
                           level= level, major_class_wt = major_class_wt,
                           seed= seed, test_pct= test_pct)
  
  # build coef backbone
  glm_def <- glm(dep_var ~ ., data= dat$train[[1]], family= family)
  coef_nm <- colnames(model.matrix(glm_def))
  coef_vec <- rep(0, length(coef_nm)); names(coef_vec) <- coef_nm
  terms <- terms(glm_def)
  
  # 02. Loop through ensembles
  ll <- tt_glm_par(train_list= dat$train, test= dat$test, out_vec= coef_vec, 
                     direction= direction, 
                     family= family,
                     leave_cores= leave_cores)
  
  # 03. return
  ret <- list(wt_coef= apply(ll$coef_mat, 2, weighted.mean, w= ll$w_vec),
         list(ensemble_coef= ll$coef_mat, ensemble_wts= ll$w_vec,
              family= family, direction= direction, n= n,
              terms= terms, data= df[, cols], cols= cols))
  class(ret) <- "glmEnsemble"
  return(ret)
}

