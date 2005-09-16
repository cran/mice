\name{lm.mids}
\alias{lm.mids}
\title{Linear Regression on Multiply Imputed Data}
\usage{
lm.mids(formula, data, weights, subset, na.action, method="qr", model=FALSE, 
        x=FALSE, y=FALSE, contrasts=NULL, ...)
}
\description{Performs repeated linear regression on multiply imputed data set
}
\arguments{
  \item{formula}{       a formula object, with the response on the left of a ~ operator, and the 
    terms, separated by + operators, on the right.}

\item{data}{
    An object of type 'mids', which stands for 'multiply imputed data set', typically
    created by function \code{mice()}.}

\item{weights}{     see \code{\link{lm}} }
\item{subset}{      see \code{\link{lm}} }
\item{na.action}{           see \code{\link{lm}} }
\item{method}{      see \code{\link{lm}} }
\item{model}{       see \code{\link{lm}} }
\item{x}{       see \code{\link{lm}} }
\item{y}{       see \code{\link{lm}} }
\item{contrasts}{   see \code{\link{lm}} }
\item{...}{not used}
}
\value{
    An objects of class 'mira', which stands for 'multiply imputed repeated analysis'.
    This object contains m lm.objects, plus some descriptive information.

}
\references{Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.

}
\seealso{ \code{\link{lm}}, \code{\link{mids}}, \code{\link{mira}}
}
\examples{
data(nhanes)
imp <- mice(nhanes)     # do default multiple imputation on a numeric matrix
fit <- lm.mids(bmi~hyp+chl,data=imp)
}
\author{Stef van Buuren, Karin Oudshoorn, 2000}     

\keyword{misc}
