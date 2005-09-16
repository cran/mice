\name{mice.impute.logreg2}
\alias{mice.impute.logreg2}
\title{Elementary Imputation Method: Logistic Regression}

\description{
  Imputes univariate missing data using logistic regression.
}
\usage{
    imp <- mice.impute.logreg2(y, ry, x)
}
\arguments{
  \item{y}{Incomplete data vector of length n}
  \item{ry}{Vector of missing data pattern of length n (FALSE=missing,
    TRUE=observed)}
    \item{x}{Matrix (n x p) of complete covariates.
    }
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
    \item Compare the score to a random (0,1) deviate, and mice.impute.
  }
  This method uses direct minimization of the likelihood function
  by means of V&R function logitreg (V&R, 2nd ed, p. 293).
}

\note{
  An alternative is mice.impute.logreg.
}
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
  
  Brand, J.P.L. (1999). Development, Implementation and Evaluation
  of Multiple
  Imputation Strategies for the Statistical Analysis of Incomplete
  Data Sets.
  Ph.D. Thesis, TNO Prevention and Health/Erasmus University Rotterdam. ISBN 90-74479-08-1. 
  
  Venables, W.N. & Ripley, B.D. (1997). Modern applied statistics with S-Plus (2nd ed). Springer, Berlin.
}
\seealso{
  \code{\link{mice}}, \code{\link{logitreg}}, \code{\link{mice.impute.logreg}}
}
\author{Stef van Buuren, Karin Oudshoorn, 2000} 

\keyword{misc}

