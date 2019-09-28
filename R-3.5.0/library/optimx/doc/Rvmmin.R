## ------------------------------------------------------------------------
cyq.f <- function (x) {
  rv<-cyq.res(x)
  f<-sum(rv*rv)
}

cyq.res <- function (x) {
# Fletcher's chebyquad function m = n -- residuals 
   n<-length(x)
   res<-rep(0,n) # initialize
   for (i in 1:n) { #loop over resids
     rr<-0.0
     for (k in 1:n) {
  z7<-1.0
  z2<-2.0*x[k]-1.0
        z8<-z2
        j<-1
        while (j<i) {
            z6<-z7
            z7<-z8
            z8<-2*z2*z7-z6 # recurrence to compute Chebyshev polynomial
            j<-j+1
        } # end recurrence loop
        rr<-rr+z8
      } # end loop on k
      rr<-rr/n
      if (2*trunc(i/2) == i) { rr <- rr + 1.0/(i*i - 1) }
      res[i]<-rr
    } # end loop on i
    res
}

cyq.jac<- function (x) {
#  Chebyquad Jacobian matrix
   n<-length(x)
   cj<-matrix(0.0, n, n)
   for (i in 1:n) { # loop over rows
     for (k in 1:n) { # loop over columns (parameters)
       z5<-0.0
       cj[i,k]<-2.0
       z8<-2.0*x[k]-1.0 
       z2<-z8
       z7<-1.0
       j<- 1
       while (j<i) { # recurrence loop
         z4<-z5
         z5<-cj[i,k]
         cj[i,k]<-4.0*z8+2.0*z2*z5-z4
         z6<-z7
         z7<-z8
         z8<-2.0*z2*z7-z6
         j<- j+1
       } # end recurrence loop
       cj[i,k]<-cj[i,k]/n
     } # end loop on k
   } # end loop on i
   cj
}


cyq.g <- function (x) {
   cj<-cyq.jac(x)
   rv<-cyq.res(x)
   gg<- as.vector(2.0* rv %*% cj)
}

require(optimx)
nn <- 4
xx0 <- 1:nn
xx0 <- xx0 / (nn+1.0) # Initial value suggested by Fletcher

# cat("aed\n")
# aed <- Rvmminu(xx0, cyq.f, cyq.g, control=list(trace=2, checkgrad=FALSE))
# print(aed)
#================================
# Now build a table of results for different values of eps and acc
veps <- c(1e-3, 1e-5, 1e-7, 1e-9, 1e-11)
vacc <- c(.1, .01, .001, .0001, .00001, .000001)
resdf <- data.frame(eps=NA, acctol=NA, nf=NA, ng=NA, fval=NA, gnorm=NA)
for (eps in veps) {
  for (acctol in vacc) {
    ans <- Rvmminu(xx0, cyq.f, cyq.g, 
          control=list(eps=eps, acctol=acctol, trace=0))
    gn <- as.numeric(crossprod(cyq.g(ans$par)))
    resdf <- rbind(resdf, 
              c(eps, acctol, ans$counts[1], ans$counts[2], ans$value, gn))
  }
}
resdf <- resdf[-1,]
# Display the function value found for different tolerances
xtabs(formula = fval ~ acctol + eps, data=resdf)
# Display the gradient norm found for different tolerances
xtabs(formula = gnorm ~ acctol + eps, data=resdf)
# Display the number of function evaluations used for different tolerances
xtabs(formula = nf ~ acctol + eps, data=resdf)
# Display the number of gradient evaluations used for different tolerances
xtabs(formula = ng ~ acctol + eps, data=resdf)


## ------------------------------------------------------------------------
sq<-function(x, exfs=1){
  nn<-length(x)
  yy<-(1:nn)*pi/4
  f<-(10^exfs)*sum((yy-x)^2)
  f
}
sq.g <- function(x, exfs=1){
  nn<-length(x)
  yy<-(1:nn)*pi/4
  gg<- 2*(x - yy)*(10^exfs)
}
require(optimx)
nn <- 4
xx0 <- rep(pi, nn) # crude start

# Now build a table of results for different values of eps and acc
veps <- c(1e-3, 1e-5, 1e-7, 1e-9, 1e-11)
exfsi <- 1:6
resdf <- data.frame(eps=NA, exfs=NA, nf=NA, ng=NA, fval=NA, gnorm=NA)
for (eps in veps) {
  for (exfs in exfsi) {
    ans <- Rvmminu(xx0, sq, sq.g, 
                   control=list(eps=eps, trace=0), exfs=exfs)
    gn <- as.numeric(crossprod(sq.g(ans$par)))
    resdf <- rbind(resdf, 
                   c(eps, exfs, ans$counts[1], ans$counts[2], ans$value, gn))
  }
}
resdf <- resdf[-1,]
# Display the function value found for different tolerances
xtabs(formula = fval ~ exfs + eps, data=resdf)
# Display the gradient norm found for different tolerances
xtabs(formula = gnorm ~ exfs + eps, data=resdf)
# Display the number of function evaluations used for different tolerances
xtabs(formula = nf ~ exfs + eps, data=resdf)
# Display the number of gradient evaluations used for different tolerances
xtabs(formula = ng ~ exfs + eps, data=resdf)

## ------------------------------------------------------------------------
ssq.f<-function(x){
   nn<-length(x)
   yy <- 1:nn
   f<-sum((yy-x/10^yy)^2)
   f
}
ssq.g <- function(x){
   nn<-length(x)
   yy<-1:nn
   gg<- 2*(x/10^yy - yy)*(1/10^yy)
}

xy <- c(1, 1/10, 1/100, 1/1000)
# note: gradient was checked using numDeriv
veps <- c(1e-3, 1e-5, 1e-7, 1e-9, 1e-11)
vacc <- c(.1, .01, .001, .0001, .00001, .000001)
resdf <- data.frame(eps=NA, acctol=NA, nf=NA, ng=NA, fval=NA, gnorm=NA)
for (eps in veps) {
  for (acctol in vacc) {
    ans <- Rvmminu(xy, ssq.f, ssq.g, 
          control=list(eps=eps, acctol=acctol, trace=0))
    gn <- as.numeric(crossprod(ssq.g(ans$par)))
    resdf <- rbind(resdf, 
              c(eps, acctol, ans$counts[1], ans$counts[2], ans$value, gn))
  }
}
resdf <- resdf[-1,]
# Display the function value found for different tolerances
xtabs(formula = fval ~ acctol + eps, data=resdf)
# Display the gradient norm found for different tolerances
xtabs(formula = gnorm ~ acctol + eps, data=resdf)
# Display the number of function evaluations used for different tolerances
xtabs(formula = nf ~ acctol + eps, data=resdf)
# Display the number of gradient evaluations used for different tolerances
xtabs(formula = ng ~ acctol + eps, data=resdf)

## ------------------------------------------------------------------------
## hobbstarts.R -- starting points for Hobbs problem 
hobbs.f<- function(x){ # # Hobbs weeds problem -- function
    if (abs(12*x[3]) > 500) { # check computability
       fbad<-.Machine$double.xmax
       return(fbad)
    }
    res<-hobbs.res(x)
    f<-sum(res*res)
##    cat("fval =",f,"\n")
##    f
}
hobbs.res<-function(x){ # Hobbs weeds problem -- residual
# This variant uses looping
    if(length(x) != 3) stop("hobbs.res -- parameter vector n!=3")
    y<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 
         38.558, 50.156, 62.948, 75.995, 91.972)
    t<-1:12
    if(abs(12*x[3])>50) {
       res<-rep(Inf,12)
    } else {
       res<-x[1]/(1+x[2]*exp(-x[3]*t)) - y
    }
}

hobbs.jac<-function(x){ # Jacobian of Hobbs weeds problem
   jj<-matrix(0.0, 12, 3)
   t<-1:12
    yy<-exp(-x[3]*t)
    zz<-1.0/(1+x[2]*yy)
     jj[t,1] <- zz
     jj[t,2] <- -x[1]*zz*zz*yy
     jj[t,3] <- x[1]*zz*zz*yy*x[2]*t
   return(jj)
}

hobbs.g<-function(x){ # gradient of Hobbs weeds problem
    # NOT EFFICIENT TO CALL AGAIN
    jj<-hobbs.jac(x)
    res<-hobbs.res(x)
    gg<-as.vector(2.*t(jj) %*% res)
    return(gg)
}
require(optimx)
set.seed(12345)
nrun<-100
sstart<-matrix(runif(3*nrun, 0, 5), nrow=nrun, ncol=3)
ustart<-sstart %*% diag(c(100, 10, 0.1))
nsuccR <- 0
nsuccO <- 0
vRvm <- rep(NA, nrun)
voptim <- vRvm
fRvm <- vRvm
gRvm <- vRvm
foptim <- vRvm
goptim <- vRvm

for (irun in 1:nrun) {
  us <- ustart[irun,]
#  print(us)
#  ans <- Rvmminu(us, hobbs.f, hobbs.g, control=list(trace=1))
#  ans <- optim(us, hobbs.f, hobbs.g, method="BFGS")
  ans <- Rvmminu(us, hobbs.f, hobbs.g, control=list(trace=0))
  ao <- optim(us, hobbs.f, hobbs.g, method="BFGS", 
               control=list(maxit=3000))
# ensure does not max function out

# cat(irun,"  Rvmminu value =",ans$value,"  optim:BFGS value=",ao$value,"\n")
  if (ans$value < 2.5879) nsuccR <- nsuccR + 1
  if (ao$value < 2.5879) nsuccO <- nsuccO + 1
#  tmp <- readline()
  vRvm[irun] <- ans$value
  voptim[irun] <- ao$value
  fRvm[irun] <- ans$counts[1]
  gRvm[irun] <- ans$counts[2]
  foptim[irun] <- ao$counts[1]
  goptim[irun] <- ao$counts[2]

}
cat("Rvmminu: number of successes=",nsuccR,"  propn=",nsuccR/nrun,"\n")
cat("optim:BFGS no. of successes=",nsuccO,"  propn=",nsuccO/nrun,"\n")
fgc <- data.frame(fRvm, foptim, gRvm, goptim)
summary(fgc)

## ------------------------------------------------------------------------
nsuccR <- 0
nsuccO <- 0
for (irun in 1:nrun) {
  us <- ustart[irun,]
#  print(us)
#  ans <- Rvmminu(us, hobbs.f, hobbs.g, control=list(trace=1))
#  ans <- optim(us, hobbs.f, hobbs.g, method="BFGS")
  ans <- Rvmminu(us, hobbs.f, hobbs.g, control=list(trace=0))
  ao <- optim(us, hobbs.f, hobbs.g, method="BFGS")
# ensure does not max function out

# cat(irun,"  Rvmminu value =",ans$value,"  optim:BFGS value=",ao$value,"\n")
  if (ans$value < 2.5879) nsuccR <- nsuccR + 1
  if (ao$value < 2.5879) nsuccO <- nsuccO + 1
#  tmp <- readline()
  vRvm[irun] <- ans$value
  voptim[irun] <- ao$value
  fRvm[irun] <- ans$counts[1]
  gRvm[irun] <- ans$counts[2]
  foptim[irun] <- ao$counts[1]
  goptim[irun] <- ao$counts[2]

}
cat("Rvmminu: number of successes=",nsuccR,"  propn=",nsuccR/nrun,"\n")
cat("optim:BFGS no. of successes=",nsuccO,"  propn=",nsuccO/nrun,"\n")
fgc <- data.frame(fRvm, foptim, gRvm, goptim)
summary(fgc)

## ------------------------------------------------------------------------
bt.f<-function(x){
 sum(x*x)
}

bt.g<-function(x){
  gg<-2.0*x
}

lower <- c(0, 1, 2, 3, 4)
upper <- c(2, 3, 4, 5, 6)
bdmsk <- rep(1,5)
xx <- rep(0,5) # out of bounds
ans <- Rvmmin(xx, bt.f, bt.g, lower=lower, upper=upper, bdmsk=bdmsk)
ans


## ------------------------------------------------------------------------
sq.f<-function(x){
   nn<-length(x)
   yy<-1:nn
   f<-sum((yy-x)^2)
   f
}
sq.g <- function(x){
   nn<-length(x)
   yy<-1:nn
   gg<- 2*(x - yy)
}

xx0 <- rep(pi,3)
bdmsk <- c(1, 0, 1) # Middle parameter fixed at pi
cat("Check final function value (pi-2)^2 = ", (pi-2)^2,"\n")

require(optimx)
ans <- Rvmmin(xx0, sq.f, sq.g, lower=-Inf, upper=Inf, bdmsk=bdmsk,
               control=list(trace=2))
ans
ansnog <- Rvmmin(xx0, sq.f, lower=-Inf, upper=Inf, bdmsk=bdmsk,
               control=list(trace=2))
ansnog

