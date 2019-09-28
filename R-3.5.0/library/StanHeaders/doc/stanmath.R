## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
Sys.setenv(USE_CXX14 = "1")
set.seed(12345)

## ------------------------------------------------------------------------
x <- optim(rnorm(3), fn = f, gr = g, a = 1:3, method = "BFGS", hessian = TRUE)
x$par
x$hessian
H(x$par, a = 1:3)
J(x$par, a = 1:3)
solution(a = 1:3, guess = rnorm(3))

## ------------------------------------------------------------------------
StanHeaders_pkg_libs <- system.file(ifelse(.Platform$OS.type == "windows", "libs", "lib"), 
                                    .Platform$r_arch, package = "StanHeaders")
Sys.setenv(PKG_LIBS = paste(paste0("-L", shQuote(StanHeaders_pkg_libs)), "-lStanHeaders"))

## ------------------------------------------------------------------------
all.equal(1, Cauchy(rexp(1)), tol = 1e-15)

## ------------------------------------------------------------------------
A <- matrix(c(0, -1, 1, -runif(1)), nrow = 2, ncol = 2)
y0 <- rexp(2)
all.equal(nonstiff(A, y0), c(0, 0), tol = 1e-5)
all.equal(   stiff(A, y0), c(0, 0), tol = 1e-8)

## ------------------------------------------------------------------------
Sys.setenv(PKG_CXXFLAGS = "-DSTAN_THREADS")
Sys.setenv(STAN_NUM_THREADS = 2) # specify -1 to use all available cores

## ------------------------------------------------------------------------
odd <- seq.int(from = 2^25 - 1, to = 2^26 - 1, by = 2)
tail(psapply(n = as.list(odd))) == 1 # check your process manager while this is running

## ---- echo = FALSE, comment = ""-----------------------------------------
cat(readLines("sparselm_stan.hpp"), sep = "\n")

## ---- message = FALSE----------------------------------------------------
library(Rcpp)
tf <- tempfile(fileext = "Module.cpp")
exposeClass("sparselm_stan",
      constructors = list(c("Eigen::Map<Eigen::SparseMatrix<double> >", 
                            "Eigen::VectorXd")),
      fields = c("X", "y"),
      methods = c("log_prob<>", "gradient<>"),
      rename = c(log_prob = "log_prob<>", gradient = "gradient<>"),
      header = c("// [[Rcpp::depends(BH)]]",
                 "// [[Rcpp::depends(RcppEigen)]]",
                 "// [[Rcpp::depends(StanHeaders)]]",
                 "// [[Rcpp::plugins(cpp14)]]",
                 paste0("#include <", file.path(getwd(), "sparselm_stan.hpp"), ">")),
      file = tf,
      Rfile = FALSE)
Sys.unsetenv("PKG_CXXFLAGS") # don't specify -DSTAN_THREADS if you are not using them
Sys.setenv(PKG_CPPFLAGS = paste0("-I",
                                 system.file("include", "src", 
                                             package = "StanHeaders", mustWork = TRUE)))
sourceCpp(tf)
sparselm_stan

## ------------------------------------------------------------------------
dd <- data.frame(a = gl(3, 4), b = gl(4, 1, 12))
X <- Matrix::sparse.model.matrix(~ a + b, data = dd)
X

## ------------------------------------------------------------------------
sm <- new(sparselm_stan, X = X, y = rnorm(nrow(X)))
sm$log_prob(c(beta = rnorm(ncol(X)), log_sigma = log(pi)))
round(sm$gradient(c(beta = rnorm(ncol(X)), log_sigma = log(pi))), digits = 4)

