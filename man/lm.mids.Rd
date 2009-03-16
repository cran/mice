\name{lm.mids}
\alias{lm.mids}
\title{Linear Regression on Multiply Imputed Data}
\usage{
lm.mids(formula, data, ...)
}
\description{Applies \code{lm()} to  multiply imputed data set
}
\arguments{
  \item{formula}{       a formula object, with the response on the left of a ~ operator, and the 
    terms, separated by + operators, on the right. See the documentation
    of \code{\link{lm}} and \code{\link{formula}} for details.}

\item{data}{
    An object of type 'mids', which stands for 'multiply imputed data set', typically
    created by a call to function \code{mice()}.}

\item{...}{	Additional parameters passed to \code{\link{lm}}}
}

\value{
    An objects of class 'mira', which stands for 'multiply imputed repeated analysis'.
    This object contains \code{data$m} distinct \code{lm.objects}, plus some descriptive information.

}
\references{
Van Buuren, S., Groothuis-Oudshoorn, C.G.M. (2000). 
\emph{Multivariate Imputation by Chained Equations:  MICE V1.0 User's manual.} 
Leiden: TNO Quality of Life.
\url{http://www.stefvanbuuren.nl/publications/MICE V1.0 Manual TNO00038 2000.pdf}

}
\seealso{ \code{\link{lm}}, \code{\link{mids}}, \code{\link{mira}}
}
\examples{
data(nhanes)
imp <- mice(nhanes)
fit <- lm.mids(bmi~hyp+chl,data=imp)
}
\author{Stef van Buuren, Karin Oudshoorn, 2000}     

\keyword{misc}
