\name{mice.auxiliary}

\alias{long2mids}
\alias{ifdo}
\alias{appendbreak}
\alias{extractBS}
\alias{wp.twin}

\title{Auxiliary functions used in FIMD}

\description{
Auxiliary function that are not (yet) part of \code{mice}, but that are needed to execute the code for the 
book Flexible Imputation of Missing Data (FIMD).
}

\usage{
long2mids(x)
ifdo(cond, action)
appendbreak(data, brk, warp.model = warp.model, id=NULL, typ="pred")
extractBS(fit)
wp.twin(obj1, obj2=NULL, xvar=NULL, xvar.column=2,
           n.inter=16, show.given=FALSE, ylim.worm=0.5,
           line=FALSE, cex=1, col1="black", col2="orange",
           warnings=FALSE, ...)
}

\arguments{
    \item{x}{}
    \item{cond}{}
    \item{action}{}
    \item{data}{}
    \item{brk}{}
    \item{warp.model}{}
    \item{id}{}
    \item{typ}{}
    \item{obj1}{}
    \item{obj2}{}
    \item{xvar}{}
    \item{xvar.columns}{}
    \item{n.inter}{}
    \item{show.given}{}
    \item{ylim.worm}{}
    \item{line}{}
    \item{cex}{}
    \item{col1}{}
    \item{col2}{}
    \item{warnings}{}
    \item{\dots}{}
}

\author{Stef van Buuren, 2012} 

\keyword{internal}  
