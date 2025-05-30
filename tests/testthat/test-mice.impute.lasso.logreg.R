context("mice.impute.lasso.logreg")

#########################
# TEST 1: Simple problem #
#########################
set.seed(123)

# generate data
n <- 1e3
y <- rnorm(n)
x <- y * .3 + rnorm(n, 0, .25)
x2 <- x + rnorm(n, 2, 3)
x <- cbind(x, x2)
y <- as.numeric(cut(y, 2)) - 1

# make missingness
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps <- mice.impute.lasso.logreg(y, ry, x)

test_that("Returns a matrix of dimensionality sum(wy) x 1", {
  expect_true(is.matrix(imps))
  expect_equal(dim(imps), c(sum(wy), 1))
})

#########################
# TEST 2: Use it within mice call #
#########################

# Generate some dichotomous data
n <- 1e2
p <- 4
Sigma <- matrix(.7, nrow = p, ncol = p)
diag(Sigma) <- 1
X <- as.data.frame(MASS::mvrnorm(n, rep(0, p), Sigma))

# Discretize and impose miss
for (j in 1:2) {
  X[, j] <- cut(X[, j], 2) # Make it discrete
  X[sample(1:n, n * .3), j] <- NA # Impose missings
}

# Imputations
meth <- make.method(X)
meth[1:2] <- "lasso.logreg"
durr_default <- mice(X,
  m = 2, maxit = 2, method = meth,
  print = FALSE
)
durr_custom <- mice(X,
  m = 2, maxit = 2, method = meth,
  nfolds = 5,
  print = FALSE
)
meth[1:2] <- "logreg"
logreg_default <- mice(X,
  m = 2, maxit = 2, method = meth,
  print = FALSE
)

# Tests
test_that("mice call works", {
  expect_equal(class(durr_custom), "mids")
})

test_that("mice call works w/ custom arguments", {
  expect_equal(class(durr_custom), "mids")
})

test_that("same class as logreg default method", {
  expect_equal(
    class(complete(logreg_default)[, 1]),
    class(complete(durr_default)[, 1])
  )
})

#########################
# TEST 3: Perfect Prediction / Complete Separation #
#########################
set.seed(123)

# Generate some dichotomous data
n <- 1e2
p <- 4
Sigma <- matrix(.7, nrow = p, ncol = p)
diag(Sigma) <- 1
x <- MASS::mvrnorm(n, rep(0, p), Sigma)

# Create Perfect Predictor
y <- factor(x[, 1] < 0, labels = c("y", "n"))

# Missing values
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Imputation well behaved
wellBehaved <- tryCatch(
  expr = {
    mice.impute.lasso.logreg(y = y, ry = ry, x = x[, -1])
  },
  error = function(e) {
    e
  },
  warning = function(w) {
    w
  }
)

# Imputation perfect prediction
perfectPred <- tryCatch(
  expr = {
    mice.impute.lasso.logreg(y = y, ry = ry, x = x)
  },
  error = function(e) {
    e
  },
  warning = function(w) {
    w
  }
)

# Test
test_that("Complete separation results in same class as well behaved case", {
  expect_true(all.equal(class(wellBehaved), class(perfectPred)))
})


context("mice.impute.lasso.select.logreg")

#########################
# TEST 1: Simple problem #
#########################
set.seed(123)

# generate data
n <- 1e3
y <- rnorm(n)
x <- y * .3 + rnorm(n, 0, .25)
x2 <- x + rnorm(n, 2, 3)
x <- cbind(x, x2)
y <- as.numeric(cut(y, 2)) - 1

# make missingness
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps_t1 <- mice.impute.lasso.select.logreg(y, ry, x)

test_that("Returns requested length", {
  expect_equal(length(imps_t1), sum(!ry))
})
test_that("Returns dichotomous imputations", {
  expect_equal(length(unique(imps_t1)), 2)
})

#########################
# TEST 2: Nothing is important #
#########################

set.seed(20211013)
n <- 1e4
p <- 3
b0 <- 1
bs <- rep(0, p)
x <- cbind(matrix(rnorm(n * p), n, p))
y <- b0 + x %*% bs + rnorm(n)
y <- as.numeric(cut(y, 2)) - 1

# Missing values
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps_t2 <- mice.impute.lasso.select.logreg(y, ry, x)

test_that("Returns dichotomous imputations", {
  expect_equal(length(unique(imps_t2)), 2)
})

#########################
# TEST 3: Everything is important #
#########################

n <- 1e4
p <- 10
b0 <- 1
bs <- rep(1, p)
x <- cbind(matrix(rnorm(n * p), n, p))
y <- b0 + x %*% bs + rnorm(n)
y <- as.numeric(cut(y, 2)) - 1

# Missing values
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps_t3 <- mice.impute.lasso.select.logreg(y, ry, x)

test_that("Works when all predictors are important", {
  expect_equal(length(unique(imps_t3)), 2)
})

#########################
# TEST 4: Use it within mice call #
#########################

# Generate some dichotomous data
n <- 1e2
p <- 4
Sigma <- matrix(.7, nrow = p, ncol = p)
diag(Sigma) <- 1
X <- as.data.frame(MASS::mvrnorm(n, rep(0, p), Sigma))

# Discretize and impose miss
for (j in 1:2) {
  X[, j] <- cut(X[, j], 2) # Make it discrete
  X[sample(1:n, n * .3), j] <- NA # Impose missings
}

# Imputations
meth <- make.method(X)
meth[1:2] <- "lasso.select.logreg"
iurr_default <- mice(X,
                     m = 2, maxit = 2, method = meth,
                     print = FALSE
)
iurr_custom <- mice(X,
                    m = 2, maxit = 2, method = meth,
                    nfolds = 5,
                    print = FALSE
)
meth[1:2] <- "logreg"
logreg_default <- mice(X,
                       m = 2, maxit = 2, method = meth,
                       print = FALSE
)

# Tests
test_that("mice call works", {
  expect_equal(class(iurr_custom), "mids")
})

test_that("mice call works w/ custom arguments", {
  expect_equal(class(iurr_custom), "mids")
})

test_that("same class as logreg default method", {
  expect_equal(
    class(complete(logreg_default)[, 1]),
    class(complete(iurr_default)[, 1])
  )
})

#########################
# TEST 5: Perfect Prediction / Complete Separation #
#########################
set.seed(123)

# Generate some dichotomous data
n <- 1e2
p <- 4
Sigma <- matrix(.7, nrow = p, ncol = p)
diag(Sigma) <- 1
x <- MASS::mvrnorm(n, rep(0, p), Sigma)

# Create Perfect Predictor
y <- factor(x[, 1] < 0, labels = c("y", "n"))

# Missing values
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Imputation well behaved
wellBehaved <- tryCatch(
  expr = {
    mice.impute.lasso.select.logreg(y = y, ry = ry, x = x[, -1])
  },
  error = function(e) {
    e
  },
  warning = function(w) {
    w
  }
)

# Imputation perfect prediction
perfectPred <- tryCatch(
  expr = {
    mice.impute.lasso.select.logreg(y = y, ry = ry, x = x)
  },
  error = function(e) {
    e
  },
  warning = function(w) {
    w
  }
)

# Test
test_that("Complete separation results in same class as well behaved case", {
  expect_true(all.equal(class(wellBehaved), class(perfectPred)))
})
