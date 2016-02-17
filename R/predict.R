
#' @title Predict Method for GLM Ensembles
#' @description Obtains prediction from a fitted glmEnsemble model object.
#' @param object	a fitted object of class "glmEnsemble".
#' @param newdata	A data frame in which to look for variables with which 
#' to predict. 
#' @param type the type of prediction required. The default, "link", is on the scale of the 
#' linear predictors; the alternative "response" is on the scale of the response variable. 
predict.glmEnsemble <- function(object, newdata= NULL, 
                                type= c("link", "response")) {
  
  type <- match.arg(type, several.ok= FALSE)
  
  if (missing(newdata)) {
    stop("newdata must be supplied.")
  } else {
    mm <- model.matrix(~ ., newdata[, object$cols])
    beta <- object[[1]]
    if (ncol(mm) > length(beta)) stop("non-conformable arguments. New levels detected.")
    if (ncol(mm) < length(beta)) {
      beta <- beta[which(names(beta) %in% colnames(mm))]
      warning("missing levels detected... effects dropped from predictions")
    }
    lp <- drop(mm %*% beta)
  }
  
  if (type == "link") {
    return(lp)
  } else {
    if (all.equal(object[[2]]$family, binomial(link= "logit"))) {
      pred <- exp(lp) / (1 + exp(lp))
    } else if (all.equal(object[[2]]$family, binomial(link= "probit"))) {
      pred <- pnorm(lp)
    } else if (all.equal(object[[2]]$family, poisson(link= "log"))) {
      pred <- round(exp(lp),0)
    } else if (all.equal(object[[2]]$family, gaussian(link= "identity"))) {
      pred <- lp
    }
    
    return(pred)
  }
}