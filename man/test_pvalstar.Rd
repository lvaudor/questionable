% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/significance.R
\name{test_pvalstar}
\alias{test_pvalstar}
\title{Replace p-value with stars (***:<0.001, **:<0.01, *:<0.05, .:<0.1, not significant:>0.1)}
\usage{
test_pvalstar(pvalue)
}
\arguments{
\item{pvalue}{the p-value to replace with stars}
}
\description{
Replace p-value with stars (***:<0.001, **:<0.01, *:<0.05, .:<0.1, not significant:>0.1)
}
\examples{
# example code
data(icecream)
pvalue=test_pval(icecream,"age","creme_glacee_rhumraisins")
test_pvalstar(pvalue)
}
