## ------------------------------------------------------------------------
x0<-c(1,2,3,4)
fnt <- function(x, fscale=10){
  yy <- length(x):1
  val <- sum((yy*x)^2)*fscale
}
grt <- function(x, fscale=10){
  nn <- length(x)
  yy <- nn:1
  #    gg <- rep(NA,nn)
  gg <- 2*(yy^2)*x*fscale
  gg
}

hesst <- function(x, fscale=10){
  nn <- length(x)
  yy <- nn:1
  hh <- diag(2*yy^2*fscale)
  hh
}

require(optimx)
t1 <- snewton(x0, fnt, grt, hesst, control=list(trace=0), fscale=3.0)
print(t1)
# we can also use nlm and nlminb
fght <- function(x, fscale=10){
  ## combine f, g and h into single function for nlm
     ff <- fnt(x, fscale)
     gg <- grt(x, fscale)
     hh <- hesst(x, fscale)
     attr(ff, "gradient") <- gg
     attr(ff, "hessian") <- hh
     ff
}

t1nlm <- nlm(fght, x0, fscale=3.0, hessian=TRUE, print.level=0)
print(t1nlm)

## BUT ... it looks like nlminb is NOT using a true Newton-type method
t1nlminb <- nlminb(x0, fnt, gradient=grt, hessian=hesst, fscale=3.0, 
                   control=list(trace=0))
print(t1nlminb)

# and call them from optimx (i.e., test this gives same results)
t1so <-  optimr(x0, fnt, grt, hess=hesst, method="snewton", fscale=3.0, 
                 control=list(trace=0))
proptimr(t1so)

t1nlmo <- optimr(x0, fnt, grt, hess=hesst, method="nlm", fscale=3.0, 
                 control=list(trace=0))
proptimr(t1nlmo)

tst <- try(t1nlminbo <- optimr(x0, fnt, grt, hess=hesst, method="nlminb", 
                               fscale=3.0, control=list(trace=0)))
if (class(tst) == "try-error"){
    cat("try-error on attempt to run nlminb in optimr()\n")
} else { proptimr(t1nlminbo) }


## ------------------------------------------------------------------------
require(optimx)
#Rosenbrock banana valley function
f <- function(x){
return(100*(x[2] - x[1]*x[1])^2 + (1-x[1])^2)
}
#gradient
gr <- function(x){
return(c(-400*x[1]*(x[2] - x[1]*x[1]) - 2*(1-x[1]), 200*(x[2] - x[1]*x[1])))
}
#Hessian
h <- function(x) {
a11 <- 2 - 400*x[2] + 1200*x[1]*x[1]; a21 <- -400*x[1]
return(matrix(c(a11, a21, a21, 200), 2, 2))
}
x0 <- c(-1.2, 1)
# sink("mbrn1-170408.txt", split=TRUE)
t1 <- snewton(x0, fn=f, gr=gr, hess=h, control=list(trace=0))
print(t1)

# we can also use nlm and nlminb
fght <- function(x){
  ## combine f, g and h into single function for nlm
     ff <- f(x)
     gg <- gr(x)
     hh <- h(x)
     attr(ff, "gradient") <- gg
     attr(ff, "hessian") <- hh
     ff
}

# COULD TRY: t1nlm <- nlm(fght, x0, hessian=TRUE, print.level=2, iterlim=10000)
t1nlmo <- optimr(x0, f, gr, hess=h, method="nlm", control=list(trace=0))
proptimr(t1nlmo)

t1so <- optimr(x0, f, gr, hess=h, method="snewton", control=list(trace=0))
proptimr(t1so)
  
t1smo <-  optimr(x0, f, gr, hess=h, method="snewtonm", control=list(trace=0))
proptimr(t1smo)


## Again, nlminb probably not using hessian
tst <- try(t1nlminbo <- optimr(x0, f, gr, hess=h, method="nlminb", 
                               control=list(trace=0)))
if (class(tst) == "try-error"){
    cat("try-error on attempt to run nlminb in optimr()\n")
} else { proptimr(t1nlminbo) }




## ------------------------------------------------------------------------
#Example: Wood function
#
wood.f <- function(x){
  res <- 100*(x[1]^2-x[2])^2+(1-x[1])^2+90*(x[3]^2-x[4])^2+(1-x[3])^2+
    10.1*((1-x[2])^2+(1-x[4])^2)+19.8*(1-x[2])*(1-x[4])
  return(res)
}
#gradient:
wood.g <- function(x){
  g1 <- 400*x[1]^3-400*x[1]*x[2]+2*x[1]-2
  g2 <- -200*x[1]^2+220.2*x[2]+19.8*x[4]-40
  g3 <- 360*x[3]^3-360*x[3]*x[4]+2*x[3]-2
  g4 <- -180*x[3]^2+200.2*x[4]+19.8*x[2]-40
  return(c(g1,g2,g3,g4))
}
#hessian:
wood.h <- function(x){
  h11 <- 1200*x[1]^2-400*x[2]+2;    h12 <- -400*x[1]; h13 <- h14 <- 0
  h22 <- 220.2; h23 <- 0;    h24 <- 19.8
  h33 <- 1080*x[3]^2-360*x[4]+2;    h34 <- -360*x[3]
  h44 <- 200.2
  H <- matrix(c(h11,h12,h13,h14,h12,h22,h23,h24,
                h13,h23,h33,h34,h14,h24,h34,h44),ncol=4)
  return(H)
}

wood.fgh <- function(x){
      fval <- wood.f(x)
      gval <- wood.g(x)
      hval <- wood.h(x)
      attr(fval,"gradient") <- gval
      attr(fval,"hessian")<- hval
      fval
}
 
#################################################
x0 <- c(-3,-1,-3,-1) # Wood standard start

require(optimx)
# In 100 iterations, not converged
t1nlm <- nlm(wood.fgh, x0, print.level=0)
print(t1nlm)
# But both newton approaches do work
wd <- snewton(x0, fn=wood.f, gr=wood.g, hess=wood.h, control=list(trace=0))
print(wd)
wdm <- snewtonm(x0, fn=wood.f, gr=wood.g, hess=wood.h, control=list(trace=0))
print(wdm)

## AND again nlminb not likely using hessian information
## t1nlminb <- nlminb(x0, wood.f, gradient=wood.g, hess=wood.h, control=list(trace=0))
## print(t1nlminb)
# and call them from optimx (i.e., test this gives same results)

# But optimr uses a larger iteration limit, and gets to solution
t1nlmo <- optimr(x0, wood.f, wood.g, hess=wood.h, method="nlm", control=list(trace=0))
proptimr(t1nlmo)

tst<-try(t1nlminbo <- optimr(x0, wood.f, wood.g, hess=wood.h, method="nlminb", control=list(trace=0)))
if (class(tst) == "try-error"){
    cat("try-error on attempt to run nlminb in optimr()\n")
} else { proptimr(t1nlminbo) }

## ------------------------------------------------------------------------
# genrosa function code -- attempts to match the rosenbrock at gs=100 and x=c(-1.2,1)
genrosa.f<- function(x, gs=NULL){ # objective function
## One generalization of the Rosenbrock banana valley function (n parameters)
	n <- length(x)
        if(is.null(gs)) { gs=100.0 }
        # Note do not at 1.0 so min at 0
	fval<-sum (gs*(x[1:(n-1)]^2 - x[2:n])^2 + (x[1:(n-1)] - 1)^2)
}

genrosa.g <- function(x, gs=NULL){
# vectorized gradient for genrose.f
# Ravi Varadhan 2009-04-03
	n <- length(x)
        if(is.null(gs)) { gs=100.0 }
	gg <- as.vector(rep(0, n))
	tn <- 2:n
	tn1 <- tn - 1
	z1 <- x[tn] - x[tn1]^2
	z2 <- 1 - x[tn1]
        # f = gs*z1*z1 + z2*z2
	gg[tn] <- 2 * (gs * z1)
	gg[tn1] <- gg[tn1] - 4 * gs * x[tn1] * z1 - 2 *z2 
	return(gg)
}

genrosa.h <- function(x, gs=NULL) { ## compute Hessian
   if(is.null(gs)) { gs=100.0 }
	n <- length(x)
	hh<-matrix(rep(0, n*n),n,n)
	for (i in 2:n) {
		z1<-x[i]-x[i-1]*x[i-1]
#		z2<-1.0 - x[i-1]
                hh[i,i]<-hh[i,i]+2.0*(gs+1.0)
                hh[i-1,i-1]<-hh[i-1,i-1]-4.0*gs*z1-4.0*gs*x[i-1]*(-2.0*x[i-1])
                hh[i,i-1]<-hh[i,i-1]-4.0*gs*x[i-1]
                hh[i-1,i]<-hh[i-1,i]-4.0*gs*x[i-1]
	}
        return(hh)
}

require(optimx)
cat("Generalized Rosenbrock tests\n")

cat("original n and x0")

x0 <- c(-1.2, 1)
solorig <- snewton(x0, genrosa.f, genrosa.g, genrosa.h)
print(solorig)
print(eigen(solorig$Hess)$values)
solorigm <- snewtonm(x0, genrosa.f, genrosa.g, genrosa.h)
print(solorigm)
print(eigen(solorigm$Hess)$values)

cat("Start with 50 values of pi and scale factor 10\n")
x0 <- rep(pi, 50)
sol50pi <- optimr(x0, genrosa.f, genrosa.g, genrosa.h, method="snewton", gs=10)
proptimr(sol50pi)
hhi <- genrosa.h(sol50pi$par, gs=10)
print(eigen(hhi)$values)
sol50pim <- optimr(x0, genrosa.f, genrosa.g, genrosa.h, method="snewtonm", gs=10)
proptimr(sol50pim)
hhm <- genrosa.h(sol50pim$par, gs=10)
print(eigen(hhm)$values)

## ------------------------------------------------------------------------
## Optimization test function HOBBS
## ?? refs (put in .doc??)
## Nash and Walker-Smith (1987, 1989) ...
require(optimx)

hobbs.f<- function(x){ # # Hobbs weeds problem -- function
    if (abs(12*x[3]) > 500) { # check computability
       fbad<-.Machine$double.xmax
       return(fbad)
    }
    res<-hobbs.res(x)
    f<-sum(res*res)
}


hobbs.res<-function(x){ # Hobbs weeds problem -- residual
# This variant uses looping
    if(length(x) != 3) stop("hobbs.res -- parameter vector n!=3")
    y<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 38.558, 50.156, 62.948,
         75.995, 91.972)
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


hobbs.rsd<-function(x) { # Jacobian second derivative
    rsd<-array(0.0, c(12,3,3))
    t<-1:12
    yy<-exp(-x[3]*t)
    zz<-1.0/(1+x[2]*yy)
    rsd[t,1,1]<- 0.0
    rsd[t,2,1]<- -yy*zz*zz
    rsd[t,1,2]<- -yy*zz*zz
    rsd[t,2,2]<- 2.0*x[1]*yy*yy*zz*zz*zz
    rsd[t,3,1]<- t*x[2]*yy*zz*zz
    rsd[t,1,3]<- t*x[2]*yy*zz*zz
    rsd[t,3,2]<- t*x[1]*yy*zz*zz*(1-2*x[2]*yy*zz)
    rsd[t,2,3]<- t*x[1]*yy*zz*zz*(1-2*x[2]*yy*zz)
##    rsd[t,3,3]<- 2*t*t*x[1]*x[2]*x[2]*yy*yy*zz*zz*zz
    rsd[t,3,3]<- -t*t*x[1]*x[2]*yy*zz*zz*(1-2*yy*zz*x[2])
    return(rsd)
}


hobbs.h <- function(x) { ## compute Hessian
#   cat("Hessian not yet available\n")
#   return(NULL)
    H<-matrix(0,3,3)
    res<-hobbs.res(x)
    jj<-hobbs.jac(x)
    rsd<-hobbs.rsd(x)
##    H<-2.0*(t(res) %*% rsd + t(jj) %*% jj)
    for (j in 1:3) {
       for (k in 1:3) {
          for (i in 1:12) {
             H[j,k]<-H[j,k]+res[i]*rsd[i,j,k]
          }
       }
    }
    H<-2*(H + t(jj) %*% jj)
    return(H)
}

require(optimx)
x0 <- c(200, 50, .3)
cat("Start for Hobbs:")
print(x0)
solx0 <- snewton(x0, hobbs.f, hobbs.g, hobbs.h)
## Note that we exceed count limit, but have answer
print(solx0)
print(eigen(solx0$Hess)$values)
## Note that we exceed count limit, but have answer

## Setting relative check offset larger gets quicker convergence
solx0a <- snewton(x0, hobbs.f, hobbs.g, hobbs.h, control=list(offset=1000.))
print(solx0a)


x1s <- c(100, 10, .1)
cat("Start for Hobbs:")
print(x1s)
solx1s <- snewton(x1s, hobbs.f, hobbs.g, hobbs.h, control=list(trace=0))
print(solx1s)
print(eigen(solx1s$Hess)$values)
solx1m <- snewton(x1s, hobbs.f, hobbs.g, hobbs.h, control=list(trace=0))
print(solx1m)
print(eigen(solx1m$Hess)$values)

cat("Following test fails in snewton with ERROR -- Why?\n")
x3 <- c(1, 1, 1)
cat("Start for Hobbs:")
print(x3)
ftest <- try(solx3 <- snewton(x3, hobbs.f, hobbs.g, hobbs.h, control=list(trace=0)))
if (class(ftest) != "try-error") {
   print(solx3)
   print(eigen(solx3$Hess)$values)
}
cat("But Marquardt variant succeeds\n")
solx3m <- snewtonm(x3, hobbs.f, hobbs.g, hobbs.h, control=list(trace=0))
print(solx3m)
print(eigen(solx3m$Hess)$values)



# we can also use nlm and nlminb and call them from optimx


