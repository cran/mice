\name{mice.impute.passive}
\alias{mice.impute.passive}
\title{Elementary Imputation Method: Passive Imputation}
\usage{
mice.impute.passive(data, func)
}
\description{Derive a new variable based on the imputed data
}
\arguments{
  \item{data}{A data frame}
  \item{func}{A formula specifying the transformations on data}
}
\value{\item{t}{The tranformed data.}
}
\details{
  This is a special imputation function for so-called passive imputation.
  Using this function, the user can specify, at any point in the \code{mice} 
  Gibbs sampling algorithm, a function on the imputed data. 
  This is useful, for example, to compute a cubic version
  of a variable, a transformation like $Q = W/H^2$ based on two variables, 
  or a mean variable like $(x_1+x_2+x_3)/3$. The so derived variables might be
  used in other places in the imputation model.
  The function allows to dynamically derive virtually any function 
  of the imputed data at virtually any time. 
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, C.G.M. (2000) 
\emph{Multivariate Imputation by Chained Equations:  MICE V1.0 User's manual.} 
Leiden: TNO Quality of Life.
\url{http://www.stefvanbuuren.nl/publications/MICE V1.0 Manual TNO00038 2000.pdf}
}
\seealso{
  \code{\link{mice}}
}


\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000} 
\keyword{misc} 
