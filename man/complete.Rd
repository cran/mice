\name{complete}
\alias{complete}
\title{Creates a Complete Flat File from a Multiply Imputed Data Set}
\description{
  Takes an object of class \code{mids}, fills in the missing data, and
  returns the completed data in a specified format.
}
\usage{
    complete(x, action=1)
}
\arguments{
  \item{x}{An object of class \code{mids}
    as created by the function \code{mice()}.}
  \item{action}{If action is a scalar between 1 and \code{x$m},
    the function returns the data with the action's
    imputation filled in. Thus, \code{action=1} returns
    the first completed data set, \code{action=2} returns the second 
    completed data set, and so on. 
    The value of \code{action} can also be one of the following
    strings: \code{"long"}, \code{"broad"}, \code{"repeated"}. 
    See 'Details' for the interpretation.} 
}
\details{
    The argument \code{action} can also be a string, which is partially matched as follows:
    \describe{
        \item{\code{"long"}}{
        produces a long data frame of vertically stacked imputed data sets with \code{nrow(x$data) * x$m}
        rows and \code{ncol(x$data)+2} columns. The two additional columns are labeled
        \code{.id} containing the row names of \code{x$data}, 
        and \code{.imp} containing the imputation number.}
    
        \item{\code{"broad"}}{
        produces a broad data frame with \code{nrow(x$data)} rows and \code{ncol(x$data) * x$m} columns. 
        Columns are ordered such that the first \code{ncol(x$data)} columns corresponds to the first 
        imputed data matrix. The imputation number is appended to each column name.}

        \item{\code{"repeated"}}{
        produces a broad data frame with \code{nrow(x$data)} rows and \code{ncol(x$data) * x$m} columns. 
        Columns are ordered such that the first \code{x$m} columns correspond to the \code{x$m} imputed 
        versions of the first column in \code{x$data}. 
        The imputation number is appended to each column name.}
    }
}

\value{
  A data frame with the imputed values filled in.
}

\seealso{\code{\link{mice}}, \code{\link{mids}} }

\examples{
data(nhanes)

# do default multiple imputation on a numeric matrix
imp <- mice(nhanes)          

# obtain first imputated matrix
mat <- complete(imp)         

# fill in the third imputation
mat <- complete(imp, 3)      

# long matrix with stacked complete data
mat <- complete(imp, "long") 

# repeated matrix with complete data
mat <- complete(imp, "r")    

# for numeric data, produces a blocked correlation matrix, where
# each block contains of the same variable pair over different
# multiple imputations.
cor(mat)               
}

\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000} 

\keyword{misc} 
