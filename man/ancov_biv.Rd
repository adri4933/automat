% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ancov_biv.R
\name{ancov_biv}
\alias{ancov_biv}
\title{regression with 2 independant variables}
\usage{
ancov_biv(model, showShapiro = T, showVarianceTest = T)
}
\arguments{
\item{model}{a model object}

\item{showShapiro}{if True, Shapiro test is realized on residuals to test for normality}

\item{showVarianceTest}{if True, Levene's or Bartlett's test is realized on residuals to test for equality of variance}
}
\value{
function returns a list of 3 object : "model" containing model summary, "shapiro pvalue" and "bartlett pvalue"
}
\description{
regression with 2 independant variables
}
