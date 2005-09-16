\name{mice.impute.sample}
\alias{mice.impute.sample}
\title{Elementary Imputation Method: Simple Random Sample}
\usage{
mice.impute.sample(y, ry, x=NULL)
}
\description{Imputes a random sample from the observed y data
}
\arguments{
  \item{y}{ Incomplete data vector of length n}
  \item{ry}{    Vector of missing data pattern (FALSE=missing, TRUE=observed)}
  \item{x}{ Matrix (n x p) of complete covariates.}
  
}
\value{A vector of length nmis with imputations.
}
\details{
  This function takes a simple random sample from the observed values in
  y, and returns these as imputations. 
}
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
  
}
\author{Stef van Buuren, Karin Oudshoorn, 2000}     
\keyword{misc} 
