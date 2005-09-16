\name{impute.norm}
\alias{impute.norm}
\title{Elementary Imputation Method: Linear Regression Analysis}
\usage{
impute.norm(y, ry, x)
}
\description{Imputes univariate missing data using linear regression analysis 
}
\arguments{
  \item{y}{Incomplete data vector of length n}
  \item{ry}{Vector of missing data pattern (FALSE=missing, TRUE=observed)}
  \item{x}{Matrix (n x p) of complete covariates.}

}
\value{A vector of length nmis with imputations.
}
\details{
    Draws values of beta and sigma for Bayesian linear regression imputation 
    of y given x according to Rubin p. 167.
}
\note{
    Using impute.norm for all columns gives results similar to Schafer's norm 
    method (Schafer, 1997), though much slower.
}
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
Brand, J.P.L. (1999). Development, Implementation and Evaluation of Multiple Imputation Strategies for the Statistical Analysis of Incomplete Data Sets. Ph.D. Thesis, TNO Prevention and Health/Erasmus University Rotterdam. ISBN 90-74479-08-1. 

Schafer, J.L. (1997). Analysis of incomplete multivariate data. London: Chapman & Hall.

}

\author{Stef van Buuren, Karin Oudshoorn, 2000} 
\keyword{misc} 
