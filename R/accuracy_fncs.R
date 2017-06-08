
# @description calculates root mean squared error
rmse <- function(obs, pred) {
  if (length(obs) != length(pred)) {stop("obs and pred must be eqi-length")}
  sqrt(sum((obs - pred)^2, na.rm=T) / length(obs))
}

# @description calculates mean absolute deviation
mnAD <- function(obs, pred) {
  if (length(obs) != length(pred)) {stop("obs and pred must be eqi-length")}
  mean(abs(obs - pred), na.rm=T)
}

# @description calculates weighted-mean absolute deviation
wmnAD <- function(obs, pred) {
  if (length(obs) != length(pred)) {stop("obs and pred must be eqi-length")}
  wts <- log(abs(pred) + 2)
  return(1/ sum(wts, na.rm=T) * sum(wts * abs(obs - pred), na.rm=T))
}

# @description calculates max absolute deviation
mxAD <- function(obs, pred) {
  if (length(obs) != length(pred)) {stop("obs and pred must be eqi-length")}
  max(abs(obs - pred), na.rm=T)
}

#' @title Calculate the two-class logloss
#' @description Calculates the logloss defined:
#' \eqn{\frac{1}{N}\sum_i^N y_{i} log(p_i)} similar to \code{\link{multiclass_logloss}} 
#' but for a two-class problem
#' @param p_vec a vector of predicted probabilities for the positive class
#' @param class_vec A vector of class labels
#' @param w_vec An optional vector of observation weights
#' @export
class_logloss <- function(p_vec, class_vec, w_vec= NULL) {
  if (length(p_vec) != length(class_vec)) stop("p_vec and class_vec must be eqi-length")
  if (!is.null(w_vec) & (length(w_vec) != length(p_vec)))
    stop("w_vec must have the same length as both p_vec and class_vec")
  
  # cap/floor predicted probabilities
  p_vec <- ifelse(p_vec < 10e-15, 10e-15, ifelse(p_vec > (1-10e-15), (1-10e-15), p_vec))
  
  # calculate logloss w/o weights
  if (is.null(w_vec) | length(w_vec) != length(p_vec)) {
    ll <- class_vec * log(p_vec)
  }
  else { # calculate logloss w/ weights
    ll <- w_vec * class_vec * log(p_vec)
  }
  return(mean(ll))
}

#' @title Calculate the multiclass logloss
#' @description Calculates the multiclass logloss defined:
#' \eqn{\frac{1}{N}\sum_i^N\sum_j^M y_{ij} log(p_ij)}
#' @param p_mat An N x m matrix of predicted probabilities for a multiclass
#' problem. N == number of observations. m = number of classes. 
#' It is assumed that the row-sums all == 1
#' @param class_mat An N x m {0,1} integer matrix giving the true classes
#' of the observations. It is assumed that the row-sums all == 1.
#' @param w_vec An optional vector of observation weights. Defaults to NULL 
#' ie - equal weights
#' @return A numeric scalar
multiclass_logloss <- function(p_mat, class_mat, w_vec= NULL) {
  if ((nrow(p_mat) != nrow(class_mat)) | (ncol(p_mat) != ncol(class_mat)))
    stop("p_mat and class_mat must have the same dimensions.")
  if (!any(class_mat %in% c(0,1))) 
    stop("class_mat must be integer {0,1}.")
  if (!is.null(w_vec) & (nrow(p_mat) != length(w_vec))) 
    stop("w_vec length must equal nrow(p_mat)")
  
  # cap/floor predicted probabilities
  p_mat <- apply(p_mat, 2, function(j) {
    # j -> [10e-15, (1 - 10e-15)]
    j <- ifelse(j < 10e-15, 10e-15, ifelse(j> (1-10e-15), (1-10e-15), j))
  })
  
  ll <- matrix(NA, nrow= nrow(p_mat), ncol= ncol(p_mat))
  # calculate logloss w/o weights
  if (is.null(w_vec) | length(w_vec) != nrow(p_mat)) {
    for (j in 1:ncol(ll)) {
      ll[,j] <- class_mat[,j] * log(p_mat[,j])
    }
    return(sum(ll) / nrow(ll))
  }
  else { # calculate logloss w/ weights
    for (j in 1:ncol(ll)) {
      ll[,j] <- w_vec[j] * class_mat[,j] * log(p_mat[,j])
    }
    return(sum(ll) / nrow(ll))
  }
}

#' @title Calculate ROC curve metrics (two-class case)
#' @description Calcluates the trade-off between true positive rate (tpr) and 
#' false discovery rate (fdr), true negative rate (tnr), and overall accuracy 
#' for a set of threshold probabilities.
#' @param pred_vec A numeric vector of predicted probabilities
#' @param labels A factor vector of class labels (must use \code{TRUE} and \code{FALSE})
#' @param threshholds a numeric of threshholds to test.
#' @return a three column matrix with the true positive rate (tpr), false discovery,
#' rate (fdr), and true negative rate (tnr) associated with each threshold value.
#' @export
calc_roc <- function(pred_vec, labels, thresholds= seq(0.01,1,.01)) {
  len <- length(thresholds)
  acc <- tnr <- tpr <- fdr <- fnr <- vector("numeric", length= len)
  for (i in 1:len) {
    t <- table(labels, pred_vec > thresholds[i])
    if (any(pred_vec > thresholds[i]) & any(pred_vec <= thresholds[i])) {
      t <- t[c("FALSE", "TRUE"), c("FALSE", "TRUE")]  
      tpr[i] <- plyr::try_default(t["TRUE","TRUE"] / sum(t["TRUE",]), as.double(0), quiet= TRUE)
      fdr[i] <- plyr::try_default(t["FALSE","TRUE"] / sum(t["FALSE",]), as.double(0), quiet= TRUE)
      tnr[i] <- plyr::try_default(t["FALSE","FALSE"] / sum(t[,"FALSE"]), as.double(0), quiet= TRUE)
      fnr[i] <- plyr::try_default(t["TRUE","FALSE"] / sum(t[,"TRUE"]), as.double(0), quiet= TRUE)
      acc[i] <- sum(diag(t)) / sum(t)
    }
    else if (sum(pred_vec > thresholds[i]) == 0) {
      tpr[i] <- 0; fdr[i] <- 0; tnr[i] <- 1
      acc[i] <- sum(labels == FALSE) / length(labels)
    } else if (sum(pred_vec <= thresholds[i]) == 0) {
      tpr[i] <- 1; fdr[i] <- 1; tnr[i] <- 0
      acc[i] <- sum(labels == TRUE) / length(labels)
    }
  }
  out <- cbind(tpr= tpr, tnr= tnr, fdr= fdr, fnr= fnr, acc= acc)
  rownames(out) <- thresholds
  return(out)
}