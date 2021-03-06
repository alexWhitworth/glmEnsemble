% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict.glmEnsemble}
\alias{predict.glmEnsemble}
\title{Predict Method for GLM Ensembles}
\usage{
\method{predict}{glmEnsemble}(object, newdata = NULL, type = c("link",
  "response"))
}
\arguments{
\item{object}{a fitted object of class "glmEnsemble".}

\item{newdata}{A data frame in which to look for variables with which 
to predict.}

\item{type}{the type of prediction required. The default, "link", is on the scale of the 
linear predictors; the alternative "response" is on the scale of the response variable.}
}
\description{
Obtains prediction from a fitted glmEnsemble model object.
}

