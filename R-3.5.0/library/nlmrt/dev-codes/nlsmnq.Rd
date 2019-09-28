\name{nlsmnq}
\alias{nlsmnq}
\title{Nash variant of Marquardt nonlinear least squares solution via
     qr linear solver.
}
\description{
   Given a nonlinear model expressed as an expression of the form
         lhs ~ formula_for_rhs
   and a start vector where parameters used in the model formula are named,
   attempts to find the minimum of the residual sum of squares using the
   Nash variant (Nash, 1979) of the Marquardt algorithm, where the linear 
   sub-problem is solved by a qr method.
}
\usage{
   nlsmnq(formula, start, trace=FALSE, data, control, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
     This is a modeling formula of the form (as in \code{nls})
           lhsvar ~ rhsexpression
     for example,
           y ~ b1/(1+b2*exp(-b3*T))
     You may also give this as a string.
}
  \item{start}{
     A named parameter vector. For our example, we could use
        start=c(b1=1, b2=2.345, b3=0.123)
}
  \item{trace}{
     Logical TRUE if we want intermediate progress to be reported. Default is FALSE.
} 
  \item{data}{
     A data frame containing the data of the variables in the formula. This data may,
     however, be supplied directly in the parent frame. 
} 
  \item{control}{ 
     A list of controls for the algorithm. These are:
%%     ~~Describe \code{control} here~~
}
  \item{\dots}{
     Any data needed for computation of the residual vector from the expression
       rhsexpression - lhsvar. Note that this is the negative of the usual residual,
     but the sum of squares is the same.
}
}
\details{
    \code{nlsmnq} attempts to solve the nonlinear sum of squares problem by using
    a variant of Marquardt's approach to stabilizing the Gauss-Newton method using
    the Levenberg-Marquardt adjustment. This is explained in Nash (1979 or 1990) in
    the sections that discuss Algorithm 23. (?? do we want a vignette. Yes, because
    folk don't have access to book easily, but finding time.)

    In this code, we solve the (adjusted) Marquardt equations by use of the 
    \code{qr.solve()}. Rather than forming the J'J + lambda*D matrix, we augment
    the J matrix with extra rows and the y vector with null elements. 
}
\value{
  A list of the following items
  \item{coeffs}{A named vector giving the parameter values at the supposed solution.}
  \item{ssquares}{The sum of squared residuals at this set of parameters.}
  \item{resid}{The residual vector at the returned parameters.}
  \item{jacobian}{The jacobian matrix (partial derivatives of residuals w.r.t. the parameters)
           at the returned parameters.}
  \item{feval}{The number of residual evaluations (sum of squares computations) used.}
  \item{jeval}{The number of Jacobian evaluations used.}
}
\references{
%% ~put references to the literature/web site here ~
     Nash, J. C. (1979, 1990) _Compact Numerical Methods for Computers.
     Linear Algebra and Function Minimisation._ Adam Hilger./Institute
     of Physics Publications

     others!!

}
\author{
John C Nash <nashjc@uottawa.ca>
}
\note{
   Special notes, if any, will appear here.
}

\seealso{
    Function \code{nls()}, packages \code{\link{optim}} and \code{optimx}.
}

\examples{

ydat<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 
          38.558, 50.156, 62.948, 75.995, 91.972) # for testing
y<-ydat  # for testing
tdat<-1:length(ydat) # for testing
# WARNING -- using T can get confusion with TRUE
t<-tdat
start1<-c(b1=1, b2=1, b3=1)
eunsc<- y ~ b1/(1+b2*exp(-b3*t))

an1<-try(nls(eunsc, start=start1, trace=TRUE))
print(an1)

cat("GLOBAL DATA\n")
an1q<-try(nlsmnq(eunsc, start=start1, trace=TRUE))
print(an1q)

rm(y, t)

cat("LOCAL DATA\n")
ydata1<-data.frame(y=ydat, t=tdat)
ydata2<-data.frame(y=1.5*ydat, t=tdat)
an1ql1<-try(nlsmnq(eunsc, start=start1, trace=TRUE, data=ydata1))
print(an1ql1)

an1ql2<-try(nlsmnq(eunsc, start=start1, trace=TRUE, data=ydata2))
print(an1ql2)

cat("GLOBAL DATA AGAIN -- should fail due to no data\n")
an1q<-try(nlsmnq(eunsc, start=start1, trace=TRUE))
print(an1q)



}
\keyword{ nonlinear least squares }

