% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/significance.R
\name{test_pval}
\alias{test_pval}
\title{Tests whether vector is free text or not}
\usage{
test_pval(dataset, x, y)
}
\arguments{
\item{x}{the vector to optimize for running a random forest}

\item{replace_NA_with}{the character string used to replace NA as category in categorical responses. Defaults to "Not provided"}
}
\description{
Tests whether vector is free text or not
}
\examples{
# example code
data(icecream)
test_pval(icecream,"age","creme_glacee_rhumraisins")
}
