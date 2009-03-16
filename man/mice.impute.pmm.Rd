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
  \enumerate{
    \item Draw $\beta$ and $\sigma$ from the proper posterior
    \item Compute predicted values for yobs and ymis
    \item For each $y_{mis}$, find the observation with closest predicted
  value, and take its observed $y$ as the imputation.
  The matching is on $\hat{y}$, NOT on $y$, which deviates from formula b.
  }
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, C.G.M. (2000) 
\emph{Multivariate Imputation by Chained Equations:  MICE V1.0 User's manual.} 
Leiden: TNO Quality of Life.
\url{http://www.stefvanbuuren.nl/publications/MICE V1.0 Manual TNO00038 2000.pdf}
  
  Rubin, D.B. (1987). Multiple imputation for nonresponse in surveys. New York: Wiley.
}

\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000} 
\keyword{misc} 
