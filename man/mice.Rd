\name{mice}
\alias{mice}
\title{Multivariate Imputation by Chained Equations (MICE)}
\description{
  Produces an object of class \code{"mids"}, which stands
  for 'multiply imputed data set'. Multivariate missing data
  are imputed under Fully Conditional Specification (FCS).
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
    \item{data}{A data frame or a matrix containing the incomplete data. Missing values are coded as \code{NA}.}
    \item{m}{Number of multiple imputations, $m$. The default is $m=5$.}
    \item{imputationMethod}{Can be either a single string,
    or a vector of strings with length \code{ncol(data)},
    specifying the elementary imputation method to be used
    for each column in data. If specified as a single
    string, the same method will be used for all columns.
    The default imputation method (when no argument is specified)
    depends on the measurement level of the target column and
    are specified by the 
    \code{defaultImputationMethod} argument.
    Columns that need not be imputed have method \code{""}.
    See details for more information.}

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
is used to impute the data during one pass through the data. A
column may be visited more than once. All incomplete columns that are
used as predictors should be visited, or else the function will stop
with an error. The default sequence \code{1:ncol(data)} implies that columns
are imputed from left to right.} 

   \item{defaultImputationMethod}{
A vector of three strings containing the default imputation methods for numerical columns, factor 
columns with 2 levels, and factor columns with more than two levels, respectively. If nothing is 
specified, the following defaults will be used:
        \code{pmm}, predictive mean matching (numeric data)
        \code{logreg}, logistic regression imputation (binary data, factor with 2 levels)
        \code{polyreg}, polytomous regression imputation categorical data (factor >= 2 levels)
    }
   
   \item{maxit}{A scalar giving the number of iterations. The default is 5.}

   \item{diagnostics}{A Boolean flag. If \code{TRUE}, diagnostic
information will be appended to the value of the function. If
\code{FALSE}, only the imputed data are saved. The default is \code{TRUE}.} 

   \item{printFlag}{}

   \item{seed}{An integer between 0 and 1000 that is used by the \code{set.seed()} 
   for offsetting the random number generator. Default is to leave the random number generator alone.} 
}



\details{
Generates multiple imputations for incomplete multivariate data by Gibbs
sampling. Missing data can occur anywhere in the data. The algorithm
imputes an incomplete column (the target column) by generating
'plausible' synthetic values given other columns in the data. Each
incomplete column must act as a target column, and has its own specific
set of predictors. The default set of predictors for a given target consists 
of all other columns in the data. For predictors that are incomplete themselves, the
most recently generated imputations are used to complete the predictors
prior to imputation of the target column. 

A separate univariate imputation model can be specified for each
column. The default imputation method depends on the measurement level
of the target column. In addition to these, several other methods are
provided. You can also write their own imputation functions, and call
these from within the algorithm. 

The data may contain categorical variables that are used in a
regressions on other variables. The algorithm creates dummy variables
for the categories of these variables, and imputes these from the
corresponding categorical variable. The extended model containing the 
dummy variables is called the padded model. Its structure is stored in
the list component \code{pad}. 

Built-in elementary imputation methods are:

\describe{
  \item{\code{norm}}{Bayesian linear regression (numeric)}
  \item{\code{norm.improper}}{Linear regression ignoring model error (numeric)}
  \item{\code{pmm}}{Predictive mean matching (numeric)}   
  \item{\code{mean}}{Unconditional mean imputation (numeric)}
  \item{\code{logreg}}{Logistic regression (2 categories)}        
  \item{\code{logreg2}}{Logistic regression (direct minimization)(2 categories)}
  \item{\code{polyreg}}{Polytomous logistic regression (>= 2 categories)}
  \item{\code{lda}}{Linear discriminant analysis (>= 2 categories)}        
  \item{\code{sample}}{Random sample from the observed values (any)}
  }

These corresponding functions are coded in the \code{mice} library under 
names 
\code{paste("mice.impute.",method,sep="")}, where \code{method} 
is a string with the name of the elementary imputation method name, 
for example \code{method="norm"}. For the \code{j}'th column, the \code{mice} 
function constructs the function 
name 
\code{paste("mice.impute.",imputationMethod[j],sep="")} and calls the
function with this name that occurs on top of the search path. 
The mechanism allows uses to write customized imputation function, 
\code{mice.impute.myfunc}. To call it for all columns specify
\code{imputationMethod="myfunc"}. 
To call it only for, say, column 2 specify
\code{imputationMethod=c("norm","myfunc","logreg",\dots)}.

\emph{Passive imputation: }
\code{mice} supports a special built-in method, called passive imputation. This
method can be used to ensure that a data transform always depends on the
most recently generated imputations.  
In some cases, an imputation model may need transformed data in addition
to the original data (e.g. log, quadratic, recodes, interaction, sum scores,
and so on). 
Passive imputation maintains consistency among different transformations of the same data.
Passive imputation is invoked if \code{~} is specified as the first 
character of the string that specifies the elementary method.  
\code{mice} interprets the entire string, including the \code{~} character, 
as the formula argument
in a call to \code{model.frame(formula, data[!r[,j],])}. This provides a simple
mechanism for specifying determinstic dependencies among the
columns. For example, suppose that the missing entries in 
variables \code{data$height} and \code{data$weight} are imputed. The
body mass index (BMI) can be calculated within \code{mice} by 
specifying the string \code{"~I(weight/height^2)"} as the elementary 
imputation method for the target column \code{data$bmi}. 
Note that the \code{~} mechanism works only on those entries which have
missing values in the target column. You should make sure that the
combined observed and imputed parts of the target column make sense. An
easy way to create consistency is by coding all entries in the target as
\code{NA}, but for large data sets, this could be inefficient. 
Note that you may also need to adapt the default \code{predictorMatrix} to 
evade linear dependencies among the predictors that could cause errors 
like \code{Error in solve.default()} or \code{Error: system is exactly singular}.
Though not strictly needed, it is often
useful to specify \code{visitSequence} such that the column that is imputed by
the \code{~} mechanism is visited each time after one of its predictors was
visited. In that way, deterministic relation between columns will always
be synchronized.


\emph{Side effects: }
Some elementary imputation method require access to the \code{nnet} or \code{MASS}
libraries of Venables & Ripley. Where needed, these libraries will be
attached.
}

\value{Returns an object of class \code{"mids"} (multiply imputed data set) with components
  \item{call}{The call that created the object}
  \item{data}{A copy of the incomplete data set}
  \item{m}{The number of imputations}
  \item{nmis}{An array of length \code{ncol(data)} containing the number of missing observations 
  per column}
  \item{imp}{A list of \code{ncol(data)} components with the generated multiple imputations. 
    Each part of the list is a \code{nmis[j]} by \emph{m} matrix of imputed values for 
    variable \code{data[,j]}. The component equals \code{NULL} for columns without 
    missing data.}
  \item{imputationMethod}{A vector of strings of length \code{ncol(data)} specifying the elementary 
    imputation method per column}
  \item{predictorMatrix}{
    A square matrix of size \code{ncol(data)} containing 0/1 data specifying 
    the predictor set}
  \item{visitSequence}{The sequence in which columns are visited}
  \item{seed}{The seed value of the solution}
  \item{iteration}{Last Gibbs sampling iteration number}
  \item{lastSeedValue}{The most recent seed value}
  \item{chainMean}{
    A list of \emph{m} components. Each component is a \code{length(visitSequence)}
    by \code{maxit} matrix containing the mean of the generated multiple 
    imputations. The array can be used for monitoring convergence.
    Note that observed data are not present in this mean.}
  \item{chainCov}{
    A list with similar structure of \code{chainMean}, containing the covariances 
    of the imputed values.}
  \item{pad}{   A list containing various settings of the padded imputation model, 
    i.e. the imputation model after creating dummy variables. Normally, 
    this array is only useful for error checking.}
}

\author{Stef van Buuren (\email{stef.vanbuuren@tno.nl}), Karin Groothuis-Oudshoorn (\email{k.groothuis@rrd.nl}) (2000-2008) 
    with contributions of Roel de Jong, John Fox, Frank E. Harrell, Martijn Heijmans, Jaap Brand, Peter Malewski.}     


\references{
Van Buuren, S., Groothuis-Oudshoorn, C.G.M. (2000) 
\emph{Multivariate Imputation by Chained Equations:  MICE V1.0 User's manual.} 
Leiden: TNO Quality of Life.
\url{http://www.stefvanbuuren.nl/publications/MICE V1.0 Manual TNO00038 2000.pdf}

Van Buuren, S. (2007) 
Multiple imputation of discrete and continuous data by fully conditional specification. 
\emph{Statistical Methods in Medical Research}, \bold{16}, 3, 219--242.
\url{http://www.stefvanbuuren.nl/publications/MI by FCS - SMMR 2007.pdf}

Van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn C.G.M., Rubin, D.B. (2006) 
Fully conditional specification in multivariate imputation. 
\emph{Journal of Statistical Computation and Simulation}, \bold{76}, 12, 1049--1064. 
\url{http://www.stefvanbuuren.nl/publications/FCS in multivariate imputation - JSCS 2006.pdf}

Van Buuren, S., Boshuizen, H.C., Knook, D.L. (1999) 
Multiple imputation of missing blood pressure covariates in survival analysis. 
\emph{Statistics in Medicine}, \bold{18}, 681--694. 
\url{http://www.stefvanbuuren.nl/publications/Multiple imputation - Stat Med 1999.pdf}

Brand, J.P.L. (1999) 
\emph{Development, implementation and evaluation of multiple imputation strategies for the statistical analysis of incomplete data sets.} 
Dissertation. Rotterdam: Erasmus University.
}

\seealso{\code{\link{complete}}, \code{\link{mids}}, \code{\link{lm.mids}}, \code{\link{set.seed}}}


\examples{
data(nhanes)

# do default multiple imputation on a numeric matrix
imp <- mice(nhanes)
imp

# list the actual imputations for BMI
imp$imputations$bmi     

# first completed data matrix
complete(imp)       

# repeated linear regression on imputed data
lm.mids(chl~age+bmi+hyp, imp)   


# imputation on mixed data with a different method per column
data(nhanes2)
mice(nhanes2,im=c("sample","pmm","logreg","norm")) 
}

\keyword{misc} 
