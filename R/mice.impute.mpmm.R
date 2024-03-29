#' Imputation by multivariate predictive mean matching
#'
#' Imputes multivariate incomplete data among which there are specific relations,
#' for instance, polynomials, interactions, range restrictions and sum scores.
#' @aliases mice.impute.mpmm mpmm
#' @param data matrix with exactly two missing data patterns
#' @param format A character vector specifying the type of object that should
#' be returned. The default is \code{format = "imputes"}.
#' @param ... Other named arguments.
#' @return A matrix with imputed data, which has \code{ncol(y)} columns and
#' \code{sum(wy)} rows.
#' @details
#' This function implements the predictive mean matching and applies canonical
#' regression analysis to select donors fora set of missing variables. In general,
#' canonical regressionanalysis looks for a linear combination of covariates that
#' predicts a linear combination of outcomes (a set of missing variables)
#' optimally in a least-square sense (Israels, 1987). The predicted
#' value of the linear combination of the set of missing variables
#' would be applied to perform predictive mean matching.
#'
#' @note
#' The function requires variables in the block have the same missingness pattern.
#' If there are more than one missingness pattern, the function will return
#' a warning.
#' @author Mingyang Cai and Gerko Vink

# @author Mingyang Cai (University of Utrecht), \email{g.vink#uu.nl}
#' @seealso \code{\link{mice.impute.pmm}}
#' Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/sec-knowledge.html#sec:quadratic}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#' @family univariate imputation functions
#' @keywords datagen
#' @examples
#' # simulate data
#' beta2 <- beta1 <- .5
#' x <- rnorm(1000)
#' e <- rnorm(1000, 0, 1)
#' y <- beta1 * x + beta2 * x^2 + e
#' dat <- data.frame(y = y, x = x, x2 = x^2)
#' m <- as.logical(rbinom(1000, 1, 0.25))
#' dat[m, c("x", "x2")] <- NA
#'
#' # impute
#' blk <- list("y", c("x", "x2"))
#' meth <- c("", "mpmm")
#' imp <- mice(dat, blocks = blk, method = meth, print = FALSE,
#'     m = 2, maxit = 2)
#'
#' # analyse and check
#' summary(pool(with(imp, lm(y ~ x + x2))))
#' with(dat, plot(x, x2, col = mdc(1)))
#' with(complete(imp), points(x[m], x2[m], col = mdc(2)))
#' @export
#'
mice.impute.mpmm <- function(data, format = "imputes", ...) {
  order <- dimnames(data)[[1]]
  res <- mpmm.impute(data, ...)
  return(single2imputes(res[order, ], is.na(data)))
}


mpmm.impute <- function(data, ...) {
  data <- as.data.frame(data)
  r <- !is.na(data)
  mpat <- apply(r, 1, function(x) paste(as.numeric(x), collapse = ""))
  nmpat <- length(unique(mpat))
  if (nmpat != 2) stop("There are more than one missingness patterns")
  r <- unique(r)
  r <- r[rowSums(r) < ncol(r), ]
  y <- data[, which(r == FALSE), drop = FALSE]
  ry <- !is.na(y)[, 1]
  x <- data[, which(r == TRUE), drop = FALSE]
  wy <- !ry
  ES <- eigen(solve(cov(y[ry, , drop = FALSE], y[ry, , drop = FALSE])) %*% cov(y[ry, , drop = FALSE], x[ry, , drop = FALSE])
    %*% solve(cov(x[ry, , drop = FALSE], x[ry, , drop = FALSE])) %*% cov(x[ry, , drop = FALSE], y[ry, , drop = FALSE]))
  parm <- as.matrix(ES$vectors[, 1])
  z <- as.matrix(y) %*% parm
  imp <- mice.impute.pmm(z, ry, x)
  zstar <- as.matrix(imp)
  y[wy, ] <- y[ry, , drop = FALSE][match(zstar, z[ry]), ]
  data[colnames(y)] <- y
  return(data)
}
