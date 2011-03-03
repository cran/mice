\name{mice.impute.sample}
\alias{mice.impute.sample}
\title{Imputation by Simple Random Sampling}
\usage{
mice.impute.sample(y, ry, x=NULL, ...)
}
\description{Imputes a random sample from the observed \code{y} data
}
\arguments{
  \item{y}{ Incomplete data vector of length \code{n}}
  \item{ry}{ Vector of missing data pattern (\code{FALSE}=missing, \code{TRUE}=observed)}
  \item{x}{ Matrix (\code{n} x \code{p}) of complete covariates.}
  \item{...}{Other named arguments.}
}
\value{A vector of length \code{nmis} with imputations.
}
\details{
  This function takes a simple random sample from the observed values in
  \code{y}, and returns these as imputations. 
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, K. (2011) 
MICE: Multivariate Imputation by Chained Equations in R. 
\emph{Journal of Statistical Software}, forthcoming.
\url{http://www.stefvanbuuren.nl/publications/MICE in R - Draft.pdf}
 
}
\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000}     
\keyword{datagen} 
