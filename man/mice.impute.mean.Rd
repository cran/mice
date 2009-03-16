\name{mice.impute.mean}
\alias{mice.impute.mean}
\title{Elementary Imputation Method: Simple Mean Imputation}

\description{
  Imputes the arithmetic mean of the observed data
}
\usage{
    mice.impute.mean(y, ry, x=NULL)
}
\arguments{
  \item{y}{Incomplete data vector of length n}
  \item{ry}{Vector of missing data pattern (FALSE=missing, TRUE=observed)}
  \item{x}{Matrix (n x p) of complete covariates.
  }
}
\value{
   A vector of length nmis with imputations.
}

\section{Warning}{
  Imputing the mean of a variable rarely produces appropriate inferences.
  See Little and Rubin (1987). 
}
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
  
  Little, R.J.A. and Rubin, D.B. (1987). Statistical Analysis with Missing Data. 
  New York: John Wiley and Sons.
}
\seealso{
  \code{\link{mice}}, \code{\link{mean}}
}
\author{Stef van Buuren, Karin Oudshoorn, 2000} 
\keyword{misc} 
