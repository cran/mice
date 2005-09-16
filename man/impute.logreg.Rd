\name{impute.logreg}
\alias{impute.logreg}
\title{Elementary Imputation Method: Logistic Regression}

\description{
  Imputes univariate missing data using logistic regression.
}
\usage{
    impute.logreg(y, ry, x)
}
\arguments{
  
  \item{y}{Incomplete data vector of length n}
  \item{ry}{Vector of missing data pattern of length n (FALSE=missing,
    TRUE=observed) }
  \item{x}{Matrix (n x p) of complete covariates.}
}

\value{
  \item{imp}{A vector of length nmis with imputations (0 or 1).}
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
  The method relies on the standard glm.fit function.
}
\note{
  An alternative is impute.logreg2.
}
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
  
  Brand, J.P.L. (1999). Development, Implementation and Evaluation of Multiple Imputation Strategies for 
  the Statistical Analysis of Incomplete Data Sets. Ph.D. Thesis, 
  TNO Prevention and Health/Erasmus University Rotterdam. ISBN 90-74479-08-1. 
}
\seealso{\code{\link{mice}}, \code{\link{glm}}, \code{\link{glm.fit}},
  \code{\link{impute.logreg2}} }

\author{Stef van Buuren, Karin Oudshoorn, 2000} 

\keyword{misc}
