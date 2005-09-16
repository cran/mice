\name{mice.impute.pmm}
\alias{mice.impute.pmm}
\title{Elementary Imputation Method: Linear Regression Analysis}
\usage{
mice.impute.pmm(y, ry, x)
}
\description{Imputes univariate missing data using predictive mean matching
}
\arguments{
  \item{y}{ Incomplete data vector of length n}
  \item{ry}{    Vector of missing data pattern (FALSE=missing, TRUE=observed)}
  \item{x}{ Matrix (n x p) of complete covariates.}
  
}
\value{\item{imp}{  A vector of length nmis with imputations.}
}
\details{
  Imputation of y by predictive mean matching, based on
  Rubin (p. 168, formulas a and b).
  The procedure is as follows:
  \begin{enumerate}
  \item Draw beta and sigma from the proper posterior
  \item Compute predicted values for yobs and ymis
  \item For each ymis, find the observation with closest predicted
  value, and take its observed y as the imputation.
  \end{enumerate}
  The matching is on yhat, NOT on y, which deviates from formula b.
  
}
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
  
  Rubin, D.B. (1987). Multiple imputation for nonresponse in surveys. New York: Wiley.
}


\author{Stef van Buuren, Karin Oudshoorn, 2000} 
\keyword{misc} 
