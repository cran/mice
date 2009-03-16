\name{mice.internal}

\alias{data.frame.to.matrix}
\alias{is.mids}
\alias{is.mipo}
\alias{is.mira}
\alias{is.passive}
\alias{logitreg}
\alias{.norm.draw}
\alias{padModel}
\alias{.pmm.match}
\alias{sampler}

\title{Internal mice functions}

\description{
Internal functions for package \code{mice}.
}

\usage{
data.frame.to.matrix(x)
is.mids(data)
is.mipo(data)
is.mira(data)
is.passive(string)
logitreg(x, y, wt = rep(1, length(y)), intercept = TRUE, start, trace = TRUE, \dots)
.norm.draw(y, ry, x)
padModel(data, imputationMethod, predictorMatrix, visitSequence, nmis, nvar)
.pmm.match(z, yhat = yhat, y = y)
sampler(p, data, m, imp, r, visitSequence, maxit, printFlag)
}

\arguments{
    \item{imputationMethod}{}
    \item{visitSequence}{}
    \item{data}{}
    \item{nmis}{}
    \item{nvar}{}
    \item{predictorMatrix}{}
    \item{x}{}
    \item{y}{}
    \item{ry}{}
    \item{string}{}
    \item{wt}{}
    \item{intercept}{}
    \item{start}{}
    \item{trace}{}
    \item{z}{}
    \item{yhat}{}
    \item{p}{}
    \item{m}{}
    \item{imp}{}
    \item{r}{}
    \item{maxit}{}
    \item{printFlag}{}
    \item{\dots}{}
}

\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000} 

\keyword{misc}  
