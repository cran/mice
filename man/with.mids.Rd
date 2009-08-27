\name{with.mids}
\alias{with.mids}
\title{Evaluate an expression in multiple imputed datasets}
\usage{
\method{with}{mids}(data, expr, \dots)
}
\description{Performs a computation of each of imputed datasets in data.
}
\arguments{
\item{data}{
    An object of type \code{mids}, which stands for 'multiply imputed data set', typically
    created by a call to function \code{mice()}.}
  \item{expr}{An expression with a formula object, with the response on the left of 
    a \code{~} operator, and the
    terms, separated by \code{+} operators, on the right. See the documentation
    of \code{\link{lm}} and \code{\link{formula}} for details.}
  \item{\dots}{	Additional parameters passed to \code{expr}}
}

\value{
  \item{call}{  The call that created the object.}
  \item{call1}{ The call that created the \code{mids} object that was used in \code{call}.}
  \item{nmis}{  An array containing the number of missing observations per column.}
  \item{analyses}{ A list of \code{m} components containing the individual fit objects from each of the \code{m} complete data analyses.}
  \item{formula}{ The formula of the call that created the object.}
}

\references{
Van Buuren, S., Groothuis-Oudshoorn, K. (2009) 
MICE: Multivariate Imputation by Chained Equations in R. 
\emph{Journal of Statistical Software}, forthcoming.
\url{http://www.stefvanbuuren.nl/publications/MICE in R - Draft.pdf}

}
\seealso{ \code{\link{lm.mids}}, \code{\link{glm.mids}}, \code{\link{mids}}, \code{\link{mira}},
  \code{\link{pool}}, \code{\link{pool.compare}}, \code{\link{pool.r.squared}}
}

\examples{

imp <- mice(nhanes2)
fit1 <- with(data=imp,exp=lm(bmi~age+hyp+chl))
fit2 <- with(data=imp,exp=glm(hyp~age+bmi+chl,family=binomial))
anova.imp <- with(data=imp,exp=anova(lm(bmi~age+hyp+chl)))
}

\author{Karin Oudshoorn, 2009}

\keyword{multivariate}
