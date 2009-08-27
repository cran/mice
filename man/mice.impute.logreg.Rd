\name{mice.impute.logreg}
\alias{mice.impute.logreg}
\title{Imputation by Logistic Regression}

\description{
  Imputes univariate missing data using logistic regression.
}
\usage{
    mice.impute.logreg(y, ry, x)
}
\arguments{
  
  \item{y}{Incomplete data vector of length \code{n}}
  \item{ry}{Vector of missing data pattern of length \code{n} (\code{FALSE}=missing,
    \code{TRUE}=observed) }
  \item{x}{Matrix (\code{n} x \code{p}) of complete covariates.}
}

\value{
  \item{imp}{A vector of length \code{nmis} with imputations (0 or 1).}
}

\details{
  Imputation for binary response variables by the Bayesian 
  logistic regression model. See Rubin (1987, p. 169-170) for
  a description of the method.
  The method consists of the following steps:
  \enumerate{
    \item Fit a logit, and find (bhat, V(bhat))
    \item Draw BETA from N(bhat, V(bhat))
    \item Compute predicted scores for m.d., i.e. logit-1(X BETA)
    \item Compare the score to a random (0,1) deviate, and impute.}
  The method relies on the standard \code{glm.fit} function.
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, K. (2009) 
MICE: Multivariate Imputation by Chained Equations in R. 
\emph{Journal of Statistical Software}, forthcoming.
\url{http://www.stefvanbuuren.nl/publications/MICE in R - Draft.pdf}
  
  Brand, J.P.L. (1999). Development, Implementation and Evaluation of Multiple Imputation Strategies for 
  the Statistical Analysis of Incomplete Data Sets. Ph.D. Thesis, 
  TNO Prevention and Health/Erasmus University Rotterdam. ISBN 90-74479-08-1. 
  
  Venables, W.N. & Ripley, B.D. (1997). Modern applied statistics with S-Plus (2nd ed). Springer, Berlin.

}
\seealso{\code{\link{mice}}, \code{\link{glm}}, \code{\link{glm.fit}}}

\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000} 

\keyword{datagen}
