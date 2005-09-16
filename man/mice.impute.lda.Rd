\name{mice.impute.lda}
\alias{mice.impute.lda}
\title{Elementary Imputation Method: Linear Discriminant Analysis}
\usage{
mice.impute.lda(y, ry, x)
}
\description{Imputes univariate missing data using linear discriminant analysis 
}
\arguments{
  \item{y}{ Incomplete data vector of length n}
  \item{ry}{    Vector of missing data pattern (FALSE=missing, TRUE=observed)}
  \item{x}{ Matrix (n x p) of complete covariates.}
  
}
\value{A vector of length nmis with imputations.
}
\details{
  Imputation of categorical response variables by linear discriminant
  analysis. This function uses the Venables/Ripley functions
  lda and predict.lda to compute posterior probabilities for
  each incomplete case, and draws the imputations from this 
  posterior. 
}
\section{Warning}{
  The function does not incorporate the variability of the discriminant 
  weight, so it is not 'proper' in the sense of Rubin. For small samples
  and rare categories in the y, variability of the mice.imputed data could 
  therefore be somewhat underestimated. 
}
\note{  This function can be called from within the Gibbs sampler by specifying 
  'lda' in the imputationMethod argument. 
  This method is usually faster and uses less resources than 
  \code{\link{mice.impute.polyreg}}.
}   
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
  
  Brand, J.P.L. (1999). Development, Implementation and Evaluation of
  Multiple Imputation Strategies for the Statistical Analysis of
  Incomplete Data Sets.
  Ph.D. Thesis, TNO Prevention and Health/Erasmus University Rotterdam. ISBN 90-74479-08-1. 
  
  Venables, W.N. & Ripley, B.D. (1999). Modern applied statistics with S-Plus (3rd ed). Springer, Berlin.
}
\seealso{ \code{\link{mice}}, \code{\link{lda}}, \code{\link{predict.lda}}
}

\author{Stef van Buuren, Karin Oudshoorn, 2000} 
\keyword{misc} 
