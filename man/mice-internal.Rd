\name{mice.internal}

\alias{is.passive}
\alias{.norm.draw}
\alias{.norm.fix}
\alias{padModel}
\alias{.pmm.match}
\alias{sampler}
\alias{squeeze}
\alias{augment}
\alias{remove.lindep}
\alias{find.collinear}
\alias{updateLog}
\alias{mice.df}
\alias{df.residual.mira}
\alias{df.residual.lme}
\alias{df.residual.mer}
\alias{df.residual.default}


\title{Internal mice functions}

\description{
Internal functions for package \code{mice}.
}

\usage{
is.passive(string)
.norm.draw(y, ry, x, ridge = 0.00001, ...)
.norm.fix(y, ry, x, ridge = 0.00001, ...)
padModel(data, method, predictorMatrix, visitSequence, post, 
 nmis, nvar)
.pmm.match(z, yhat = yhat, y = y, donors=3, ...)
sampler(p, data, m, imp, r, visitSequence, fromto, printFlag,...)
squeeze(x, bounds = c(min(x[r]), max(x[r])), r = rep(TRUE, 
 length(x)), ...)
augment(y, ry, x, maxcat=50, ...)
remove.lindep(x, y, ry, eps = 0.0001, maxcor = 0.99, ...) 
find.collinear(x, threshold=0.999, ...)
updateLog(out=NULL, meth=NULL, frame=2)
mice.df(m, lambda, dfcom)
df.residual.mira(object, ...)
df.residual.lme(object, ...)
df.residual.mer(object, ...)
df.residual.default(object, q=1.3, ...)
}

\arguments{
    \item{method}{}
    \item{visitSequence}{}
    \item{data}{}
    \item{nmis}{}
    \item{nvar}{}
    \item{predictorMatrix}{}
    \item{x}{}
    \item{y}{}
    \item{ry}{}
    \item{string}{}
    \item{ridge}{}	
    \item{z}{}
    \item{yhat}{}
    \item{p}{}
    \item{m}{}
    \item{imp}{}
    \item{r}{}
    \item{post}{}
    \item{fromto}{}
    \item{printFlag}{}
    \item{\dots}{}
    \item{bounds}{}
    \item{nam}{}
    \item{eps}{}
    \item{maxcor}{}
    \item{threshold}{}	
    \item{out}{}
    \item{frame}{}
    \item{meth}{}
    \item{gamma}{}
    \item{dfcom}{}
    \item{object}{}
    \item{q}{}
}

\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2011} 

\keyword{internal}  
