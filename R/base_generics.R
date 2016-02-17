
#' @title Summarizing GLM-Ensemble fits
#' @description Generic methods for class "glmEnsemble"
#' @param object An object of class "glmEnsemble".
#' @param ... further arguments passed to or from other methods
#' @aliases print.glmEnsemble
#' @export
summary.glmEnsemble <- function(object, ...) {
  m <- cbind(object[[1]], 
          t(apply(object[[2]]$ensemble_coef, 2, quantile, probs= c(0.05, 0.95))))
  colnames(m) <- c("Estimate", "Q-5%", "Q-95%")
  
  cat("\n Family: ", object[[2]]$family$family,
      "\n Link function: ", object[[2]]$family$link, "\n",
      "\n Direction: ", object[[2]]$direction,
      "\n Num. Ensembles: ", object[[2]]$n, "\n \n")
  
  cat("Coefficients: \n "); m
}

#' @title Printing GLM-Ensemble fits
#' @description Generic methods for class "glmEnsemble"
#' @param object An object of class "glmEnsemble".
#' @param ... further arguments passed to or from other methods
#' @export
print.glmEnsemble <- function(object, ...) {
  cat("\n Family: ", object[[2]]$family$family,
      "\n Link function: ", object[[2]]$family$link, "\n",
      "\n Direction: ", object[[2]]$direction, "\n \n")
  
  cat("Coefficients: \n "); object[[1]]
}