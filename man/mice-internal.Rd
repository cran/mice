\name{mice.internal}

\alias{check.imputationMethod}
\alias{check.predictorMatrix}
\alias{check.visitSequence}
\alias{data.frame.to.matrix}
\alias{is.mids}
\alias{is.mipo}
\alias{is.mira}
\alias{is.passive}
\alias{logitreg}
\alias{mipo}
\alias{.norm.draw}
\alias{padModel}
\alias{.pmm.match}
\alias{sampler}

\title{internal mice functions}

\description{
Internal functions for package mice.
}

\usage{
check.imputationMethod(imputationMethod, defaultImputationMethod, visitSequence, data, nmis, nvar)
check.predictorMatrix(predictorMatrix, nmis, nvar)
check.visitSequence(visitSequence, nmis, nvar)
data.frame.to.matrix(x)
is.mids(data)
is.mipo(data)
is.mira(data)
is.passive(string)
logitreg(x, y, wt = rep(1, length(y)), intercept = TRUE, start, trace = TRUE, ...)
.norm.draw(y, ry, x)
padModel(data, imputationMethod, predictorMatrix, visitSequence, nmis, nvar)
.pmm.match(z, yhat = yhat, y = y)
sampler(p, data, m, imp, r, visitSequence, maxit, printFlag)
}

\arguments{
    \item{imputationMethod, defaultImputationMethod, visitSequence, data, nmis, nvar,
        predictorMatrix, x, y, ry, string, wt, intercept, start, trace, z, yhat, p, m,
        imp, r, maxit, printFlag, ...}{arguments}
}

\author{Stef van Buuren, Karin Oudshoorn, 2000} 

\keyword{misc}  
