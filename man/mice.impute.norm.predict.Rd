\name{mice.impute.norm.predict}
\alias{mice.impute.norm.predict}
\alias{norm.predict}
\title{Imputation by Linear Regression, Prediction Method}
\usage{
mice.impute.norm.predict(y, ry, x, ridge=0.00001, ...)
}
\description{Imputes univariate missing data using the predicted value from a linear regression 
}
\arguments{
  \item{y}{Incomplete data vector of length \code{n}}
  \item{ry}{Vector of missing data pattern (\code{FALSE}=missing, \code{TRUE}=observed)}
  \item{x}{Matrix (\code{n} x \code{p}) of complete covariates.}
  \item{ridge}{Ridge parameter}
  \item{...}{Other named arguments.}

}
\value{A vector of length \code{nmis} with imputations.
}
\details{
Calculates regression 
weights from the observed data and and return predicted values to as imputations. The \code{ridge} parameter adds 
a penalty term \code{ridge*diag(xtx)} to the variance-covariance matrix \code{xtx}.
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, K. (2011) 
MICE: Multivariate Imputation by Chained Equations in R. 
\emph{Journal of Statistical Software}, forthcoming.
\url{http://www.stefvanbuuren.nl/publications/MICE in R - Draft.pdf}

}

\author{Stef van Buuren, 2011} 
\keyword{datagen} 
