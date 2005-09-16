\name{impute.passive}
\alias{impute.passive}
\title{Elementary Imputation Method: Passive Imputation}
\usage{
impute.passive(data, func)
}
\description{Derive a new variable based on the imputed data
}
\arguments{
  \item{data}{A data frame}
  \item{func}{  A formula specifying the transformations on data}
}
\value{\item{t}{The tranformed data.}
}
\details{
  This is a special imputation function for so-called passive imputation.
  Using this function, the user can specify, at any point in the mice 
  Gibbs sampling algorithm, a function on the (imputed) data. 
  This is useful, for example, to compute a cubic version
  of a variable, a transformation like $Q=W/H^2$ based on two variables, 
  or a mean variable like $(x1+x2+x3)/3$. The so derived variables might be
  used in other places in the imputation model.
  The function allows to dynamically derive virtually any function 
  of the imputed data at virtually any time. 
}
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
}
\seealso{
  \code{\link{mice}}
}


\author{Stef van Buuren, Karin Oudshoorn, 2000} 
\keyword{misc} 
