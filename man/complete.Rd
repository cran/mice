\name{complete}
\alias{complete}
\title{Produces Imputed Flat Files
  from Multiply Imputed Data Set (mids)
}
\description{
  Takes an object of type mids, fills in the missing data, and
  returns the completed data in a specified format.
}
\usage{
    complete(x, action=1)
}
\arguments{
  \item{x}{An object of class \code{'mids'}
    (created by the function \code{mice()}).}
  \item{action}{If action is a scalar between 1 and \code{x$m},
    the function returns the data with the action's
    imputation filled in. Thus, \code{action=1} returns
    the first
    completed data set. 
    The can also be one of the following
    strings: \code{"long"}, \code{"broad"}, \code{"repeated"}.
    This has the following meaning:
    \begin{description}
    \item[\code{action="long"}] produces a long matrix with n*m
    rows,containing all imputed data plus two additional
    variables \code{"_ID_"} (containing the row.names)
    and \code{"_IMP_"} (containing the imputation number).
    \item[\code{action="broad"}] produces a broad matrix with m times
    the number of columns in the original data.
    The first ncol(\code{x$data}) columns contain the first
    imputed data matrix. Column names are changed to
    reflect the imputation number.
    \item[\code{action="repeated"}] produces a broad matrix with m times
    \code{ncol(x$data)} columns. The first m columns
    give the filled-in first variable. Column names are
    changed to reflect the imputation number.
    \end{description}
  }
}
\value{
  A data frame with the imputed values filled in.
}
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
}
\seealso{\code{\link{mice}}, \code{\link{mids}} }
\examples{
data(nhanes)
imp <- mice(nhanes)     # do default multiple imputation on a numeric matrix
mat <- complete(imp)    # fills in the first imputation
mat <- complete(imp, 3) # fills in the third imputation
mat <- complete(imp, "long") # produces a long matrix with stacked complete data
mat <- complete(imp, "b") # a broad matrix
cor(mat)                # for numeric mat, produces a blocked correlation matrix, where
            # each m*m block contains of the same variable pair over different
            # multiple imputations.
}




\author{Stef van Buuren, Karin Oudshoorn, 2000} 

\keyword{misc} 
