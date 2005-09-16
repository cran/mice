\name{mice.impute.norm.improper}
\alias{mice.impute.norm.improper}
\title{Elementary Imputation Method: Linear Regression Analysis (improper)}
\usage{
    mice.impute.norm.improper(y, ry, x)
}
\description{Imputes univariate missing data using linear regression analysis (improper version)
}
\arguments{
  \item{y}{Incomplete data vector of length n}
  \item{ry}{Vector of missing data pattern (FALSE=missing, TRUE=observed)}
  \item{x}{Matrix (n x p) of complete covariates.}
}
\value{A vector of length nmis with imputations.
}
\details{This creates imputation using the spread around the fitted 
  linear regression line of y given x, as fitted on the observed data.
  
}
\section{Warning}{
  The function does not incorporate the variability of the regression
  weights, so it is not 'proper' in the sense of Rubin. For small samples, 
  variability of the mice.imputed data is therefore somewhat underestimated. 
}
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
  
  Brand, J.P.L. (1999). Development, Implementation and Evaluation of
  Multiple Imputation Strategies for the Statistical Analysis of
  Incomplete Data Sets. Ph.D. Thesis, TNO Prevention
  and Health/Erasmus University Rotterdam. ISBN 90-74479-08-1. 
  
}
\note{  This function is provided mainly to allow comparison between proper
  and improper norm methods.
}
\seealso{\code{\link{mice}},  \code{\link{mice.impute.norm}}
}


\author{Stef van Buuren, Karin Oudshoorn, 2000}

\keyword{misc} 
