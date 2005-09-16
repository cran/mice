\name{mice.mids}
\alias{mice.mids}
\title{Multivariate Imputation by Chained Equations (Iteration Step)}
\usage{
mice.mids(obj, maxit=1, diagnostics=TRUE, printFlag=TRUE)
}
\description{Takes a "mids"-object, and produces an new object of class "mids".
}
\arguments{
  \item{obj}{   An object of class "mids", typically produces by a previous call
    to \code{mice()} or \code{mice.mids()} }
  \item{maxit}{ The number of additional Gibbs sampling iterations. }
  \item{diagnostics}{A Boolean flag. If TRUE, diagnostic information will be appended to 
    the value of the function. If FALSE, only the imputed data are saved. 
    The default is TRUE.}
  \item{printFlag}{A Boolean flag. If TRUE, diagnostic information during the Gibbs sampling
    iterations will be written to the command window.  The default is TRUE.}
}
\details{
  This function enables the user to split up the computations of the 
  Gibbs sampler into smaller parts. This is useful for the following
  reasons:
  \begin{itemize}
  \item RAM memory may become easily exhausted if the number of iterations is 
  large. Returning to prompt/session level may alleviate these problems.
  \item The user can compute customized convergence statistics at specific
  points, e.g. after each iteration, for monitoring convergence.
  - For computing a 'few extra iterations'.
  \end{itemize}
  Note: The imputation model itself is specified in the mice() function
  and cannot be changed with mice.mids.
  The state of the random generator is saved with the mids-object.
  
}
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
  
}
\seealso{
}
\examples{
data(nhanes)
imp1 <- mice(nhanes,maxit=1)
imp2 <- mice.mids(imp1)

# yields the same result as
imp <- mice(nhanes,maxit=2)

# for example:
# 
# > imp$imp$bmi[1,]
#      1    2    3    4    5 
# 1 30.1 35.3 33.2 35.3 27.5
# > imp2$imp$bmi[1,]
#      1    2    3    4    5 
# 1 30.1 35.3 33.2 35.3 27.5
# 
}
\keyword{}  

\author{Stef van Buuren, Karin Oudshoorn, 2000}     

\keyword{misc} 
