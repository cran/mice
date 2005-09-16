\name{mice}
\alias{mice}
\title{Multivariate Imputation by Chained Equations}
\description{
  Produces an object of class "mids", which stands
  for 'multiply imputed data set'.
}

\usage{
mice(data, m = 5, 
    imputationMethod = vector("character",length=ncol(data)), 
    predictorMatrix = (1 - diag(1, ncol(data))),
    visitSequence = (1:ncol(data))[apply(is.na(data),2,any)], 
    defaultImputationMethod=c("pmm","logreg","polyreg"),
    maxit = 5, 
    diagnostics = TRUE, 
    printFlag = TRUE,
    seed = NA)
}
\arguments{
 \item{data}{A data frame or a matrix containing the incomplete
        data. Missing values are coded as NA's.}
  \item{m}{Number of multiple imputations.
           If omitted, m=5 is used.}
  \item{imputationMethod}{Can be either a string,
or a vector of strings with length ncol(data),
specifying the elementary imputation method to be used
for each column in data. If specified as a single
string, the given method will be used for all columns.
The default imputation method (when no argument is specified)
depends on the measurement level of the target column and
are specified by the \code{defaultImputationMethod} argument.
Columns that need not be imputed have method \code{""}.
See details for more inromation.}

   \item{predictorMatrix}{
A square matrix of size \code{ncol(data)} containing 0/1 data specifying
the set of predictors to be used for each target column. Rows correspond
to target variables (i.e. variables to be imputed), in the sequence as
they appear in data. A value of '1' means that the column variable is
used as a predictor for the target variable (in the rows). The diagonal
of \code{predictorMatrix} must be zero. The default for \code{predictorMatrix} is that
all other columns are used as predictors (sometimes called massive
imputation).} 

   \item{visitSequence}{
A vector of integers of arbitrary length, specifying the column indices
of the visiting sequence. The visiting sequence is the column order that
is used to impute the data during one iteration of the algorithm. A
column may be visited more than once. All incomplete columns that are
used as predictors should be visited, or else the function will stop
with an error. The default sequence 1:ncol(data) implies that columns
are imputed from left to right.} 

   \item{defaultImputationMethod}{
A vector of three strings containing the default imputation methods for numerical columns, factor 
columns with 2 levels, and factor columns with more than two levels, respectively. If nothing is 
specified, the following defaults will be used:
   \code{pmm}, predictive mean matching (numeric data);
   \code{logreg}, logistic regression imputation (binary data, factor with 2 levels);
   \code{polyreg}, polytomous regression imputation categorical data (factor >= 2 levels).}
   
   \item{maxit}{A scalar giving the number of iterations. The default is 5.}

   \item{diagnostics}{A Boolean flag. If \code{TRUE}, diagnostic
information will be appended to the value of the function. If
\code{FALSE}, only the imputed data are saved. The default is \code{TRUE}.} 

   \item{printFlag}{}

   \item{seed}{An integer between 0 and 1000 that is used by the
set.seed function for offsetting the random number generator. Default is to leave the random number generator alone.} 
}

\value{An object of class mids, which stands for 'multiply imputed data set'. For 
a description of the object, see the documentation on \code{\link{mids}}.}


\details{
Generates multiple imputations for incomplete multivariate data by Gibbs
Sampling. Missing data can occur anywhere in the data. The algorithm
imputes an incomplete column (the target column) by generating
oappropriate imputation values given other columns in the data. Each
incomplete column must act as a target column, and has its own specific
set of predictors. The default predictor set consists of all other
columns in the data. For predictors that are incomplete themselves, the
most recently generated imputations are used to complete the predictors
prior to imputation of the target column. 

A separate univariate imputation model can be specified for each
column. The default imputation method depends on the measurement level
of the target column. In addition to these, several other methods are
provided. Users may also write their own imputation functions, and call
these from within the algorithm. 

In some cases, an imputation model may need transformed data in addition
to the original data (e.g. log or quadratic transforms). In order to
maintain consistency among different transformations of the same data,
the function has a special built-in method using the \code{~} mechanism. This
method can be used to ensure that a data transform always depends on the
most recently generated imputations in the untransformed (active)
column.  

The data may contain categorical variables that are used in a
regressions on other variables. The algorithm creates dummy variables
for the categories of these variables, and imputes these from the
corresponding categorical variable. 

Built-in imputation methods are:

\describe{
  \item{norm}{Bayesian linear regression (Numeric)}
  \item{pmm}{Predictive mean matching (Numeric)}   
  \item{mean}{Unconditional mean imputation (Numeric)}
  \item{logreg}{Logistic regression (2 categories)}        
  \item{logreg2}{Logistic regression (direct minimization)(2 categories)}
  \item{polyreg}{Polytomous logistic regression (>= 2 categories)}
  \item{lda}{Linear discriminant analysis (>= 2 categories)}        
  \item{sample}{Random sample from the observed values (Any)}
  \item{Special method}{If the first character of the elementary
method is a \code{~}, then the string is interpreted as the formula argument
in a call to \code{model.frame(formula, data[!r[,j],])}. This provides a simple
mechanism for specifying a large variety of dependencies among the
variables. For example transformed versions of imputed variables,
recodes, interactions, sum scores, and so on, that may themselves be
needed in other parts of the algoritm, can be specified in this
way. Note that the \code{~} mechanism works only on those entries which have
missing values in the target column. The user should make sure that the
combined observed and imputed parts of the target column make sense. One
easy way to create consistency is by coding all entries in the target as
\code{NA}, but for large data sets, this could be inefficient. Moreover, this
will not work in S-Plus 4.5. Though not strictly needed, it is often
useful to specify \code{visitSequence} such that the column that is imputed by
the \code{~} mechanism is visited each time after one of its predictors was
visited. In that way, deterministic relation between columns will always
be synchronized.}
}

For example, for the j'th column, the \code{impute.norm} function that implements the 
Bayesian linear regression method can be called by specifying the string "norm" 
as the j'th entry in the vector of strings. 

The user can write his or her own imputation function, say
\code{impute.myfunc}, and call it for all columns by specifying
\code{imputationMethod="myfunc"}, or for specific columns by specifying
\code{imputationMethod=c("norm","myfunc",...)}.

\emph{side effects:}
Some elementary imputation method require access to the nnet or MASS
libraries of Venables & Ripley. Where needed, these libraries will be
attached.
}

\references{
Van Buuren, S. and Oudshoorn, C.G.M.. (1999). Flexible multivariate
imputation by MICE. Report PG/VGZ/99.054, TNO Prevention and Health,
Leiden. 

Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by
Chained Equations:  
MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and
Health, Leiden. 

Van Buuren, S., Boshuizen, H.C. and Knook, D.L. (1999). Multiple
imputation of missing blood pressure covariates in survival
analysis. Statistics in Medicine, 18, 681-694. 

Brand, J.P.L. (1999). Development, implementation and evaluation of multiple imputation strategies for the statistical analysis of incomplete data sets. Dissertation, TNO Prevention and Health, Leiden and Erasmus University, Rotterdam.
} 

\seealso{\code{\link{complete}}, \code{\link{mids}}, \code{\link{lm.mids}}, \code{\link{set.seed}}}


\examples{
data(nhanes)
imp <- mice(nhanes)     # do default multiple imputation on a numeric matrix
imp
imp$imputations$bmi     # and list the actual imputations 
complete(imp)       # show the first completed data matrix
lm.mids(chl~age+bmi+hyp, imp)   # repeated linear regression on imputed data

data(nhanes2)
mice(nhanes2,im=c("sample","pmm","logreg","norm")) # imputation on mixed data with a different method per column
}
\author{Stef van Buuren, Karin Oudshoorn, 2000}     

\keyword{misc} 
