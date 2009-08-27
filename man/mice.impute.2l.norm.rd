\name{mice.impute.2l.norm}
\alias{mice.impute.2l.norm}
\title{Imputation by a Two-Level Normal Model}
\usage{
mice.impute.2l.norm(y, ry, x, type)
}
\description{Imputes univariate missing data using a two-level normal model}
\arguments{
  \item{y}{Incomplete data vector of length \code{n}}
  \item{ry}{Vector of missing data pattern (\code{FALSE}=missing, \code{TRUE}=observed)}
  \item{x}{Matrix (\code{n} x \code{p}) of complete covariates.}
  \item{type}{Vector of length \code{ncol(x)} identifying random and class variables. 
      Random variables are identified by a '2'. The class variable (only one 
      is allow) is code as '-2'. Random variables always include the fixed 
      effect.}
}

\value{A vector of length \code{nmis} with imputations.
}
\details{
    Implements the Gibbs sampler for the linear multilevel model with heterogeneous
    with-class variance (Kasim and Raudenbush, 1998). Imputations are drawn as an 
    extra step to the algorithm. For statistical properties see Van Buuren (2010).
}
\references{

Kasim RM, Raudenbush SW. (1998). Application of Gibbs sampling to nested variance 
components models with heterogeneous within-group variance. Journal of Educational 
and Behavioral Statistics, 23(2), 93--116.

Van Buuren, S., Groothuis-Oudshoorn, K. (2009) 
MICE: Multivariate Imputation by Chained Equations in R. 
\emph{Journal of Statistical Software}, forthcoming.
\url{http://www.stefvanbuuren.nl/publications/MICE in R - Draft.pdf}

Van Buuren, S. (2010) 
Multiple imputation of multilevel data.
In Hox, J.J. and and Roberts, K. (Eds.), 
\emph{The Handbook of Advanced Multilevel Analysis},
Milton Park, UK: Routledge.

}

\author{Roel de Jong, 2008} 
\keyword{datagen} 

