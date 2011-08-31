\name{mice.impute.2L.norm}
\alias{mice.impute.2L.norm}
\alias{mice.impute.2l.norm}
\alias{2L.norm}

\title{Imputation by a Two-Level Normal Model}
\usage{
mice.impute.2L.norm(y, ry, x, type, intercept=TRUE, ...)
mice.impute.2l.norm(y, ry, x, type, intercept=TRUE, ...)
}
\description{Imputes univariate missing data using a two-level normal model}
\arguments{
  \item{y}{Incomplete data vector of length \code{n}}
  \item{ry}{Vector of missing data pattern (\code{FALSE}=missing, \code{TRUE}=observed)}
  \item{x}{Matrix (\code{n} x \code{p}) of complete covariates.}
  \item{type}{Vector of length \code{ncol(x)} identifying random and class variables. 
      Random variables are identified by a '2'. The class variable (only one 
      is allowed) is coded as '-2'. Random variables also include the fixed 
      effect.}
  \item{intercept}{Logical determining whether the intercept is
      automatically added.}
  \item{...}{Other named arguments.}

}

\value{A vector of length \code{nmis} with imputations.
}
\details{
    Implements the Gibbs sampler for the linear multilevel model with heterogeneous
    with-class variance (Kasim and Raudenbush, 1998). Imputations are drawn as an 
    extra step to the algorithm. For simulation work see Van
    Buuren (2011). 

    The random intercept is automatically added in
    \code{mice.impute.2L.norm()}. 
}
\references{

Kasim RM, Raudenbush SW. (1998). Application of Gibbs sampling to nested variance 
components models with heterogeneous within-group variance. Journal of Educational 
and Behavioral Statistics, 23(2), 93--116.

Van Buuren, S., Groothuis-Oudshoorn, K. (2011) 
MICE: Multivariate Imputation by Chained Equations in R. 
\emph{Journal of Statistical Software}, forthcoming.
\url{http://www.stefvanbuuren.nl/publications/MICE in R - Draft.pdf}

Van Buuren, S. (2011) 
Multiple imputation of multilevel data.
In Hox, J.J. and and Roberts, J.K. (Eds.), 
\emph{The Handbook of Advanced Multilevel Analysis}, Chapter 10, pp. 173--196.
Milton Park, UK: Routledge.

}

\author{Roel de Jong, 2008} 
\keyword{datagen} 

