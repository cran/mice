\name{mice.impute.norm}
\alias{mice.impute.norm}
\title{Elementary Imputation Method: Linear Regression Analysis}
\usage{
mice.impute.norm(y, ry, x)
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
    Using mice.impute.norm for all columns gives results similar to Schafer's norm 
    method (Schafer, 1997), though much slower.
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, C.G.M. (2000) 
\emph{Multivariate Imputation by Chained Equations:  MICE V1.0 User's manual.} 
Leiden: TNO Quality of Life.
\url{http://www.stefvanbuuren.nl/publications/MICE V1.0 Manual TNO00038 2000.pdf}

Brand, J.P.L. (1999) 
\emph{Development, implementation and evaluation of multiple imputation strategies for the statistical analysis of incomplete data sets.} 
Dissertation. Rotterdam: Erasmus University.

Schafer, J.L. (1997). Analysis of incomplete multivariate data. London: Chapman & Hall.

}

\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000} 
\keyword{misc} 
