
#' @title Predict Method for GLM Ensembles
#' @description Obtains prediction from a fitted glmEnsemble model object.
#' @param object	a fitted object of class "glmEnsemble".
#' @param newdata	A data frame in which to look for variables with which 
#' to predict. 
#' @param type the type of prediction required. The default, "link", is on the scale of the 
#' linear predictors; the alternative "response" is on the scale of the response variable. 
#' @export
predict.glmEnsemble <- function(object, newdata= NULL, 
                                type= c("link", "response")) {
  
  type <- match.arg(type, several.ok= FALSE)
  
  if (missing(newdata)) {
    stop("newdata must be supplied.")
  } else {
    mm <- model.matrix(~ ., newdata[, object[[2]]$cols])
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
    if (object[[2]]$family$family == "binomial" & 
        object[[2]]$family$link == "logit") {
      pred <- ifelse(lp > 100, 1, exp(lp) / (1 + exp(lp)))
    } else if (object[[2]]$family$family == "binomial" & 
               object[[2]]$family$link == "probit") {
      pred <- pnorm(lp)
    } else if (object[[2]]$family$family == "poisson" & 
               object[[2]]$family$link == "log") {
      pred <- round(exp(lp),0)
    } else if (object[[2]]$family$family == "gaussian" & 
               object[[2]]$family$link == "identity") {
      pred <- lp
    }
    
    return(pred)
  }
}