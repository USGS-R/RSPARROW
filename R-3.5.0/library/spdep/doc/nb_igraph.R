## ------------------------------------------------------------------------
library(spdep)

## ------------------------------------------------------------------------
dothis <- TRUE
if (!suppressPackageStartupMessages(require(rgdal, quietly=TRUE))) {
  message("install the rgdal package")
  dothis <- FALSE
}
if (dothis) {
  if (exists("rgdal_extSoftVersion")) rgdal_extSoftVersion()
  else getGDALVersionInfo()
}

## ---- echo=dothis, eval=dothis-------------------------------------------
library(rgdal)
columbus <- readOGR(system.file("shapes/columbus.shp", package="spData")[1])
row.names(columbus)[1:10]

## ---- echo=dothis, eval=dothis-------------------------------------------
nb_q <- poly2nb(columbus)
nb_q
attr(nb_q, "region.id")[1:10]
is.symmetric.nb(nb_q)

## ---- echo=dothis, eval=dothis-------------------------------------------
col2 <- droplinks(nb_q, 21)
nb_q[[21]]
col2[[21]]
col2
is.symmetric.nb(col2)
coords <- coordinates(columbus)
plot(nb_q, coords, col="grey")
plot(col2, coords, add=TRUE)

## ---- echo=dothis, eval=dothis-------------------------------------------
nb_B <- nb2listw(col2, style="B", zero.policy=TRUE)
nb_B$style

## ---- echo=dothis, eval=dothis-------------------------------------------
library(Matrix)
B <- as(nb_B, "CsparseMatrix")
all(B == t(B))
str(B)
rownames(B)[1:10]

## ---- echo=dothis, eval=dothis-------------------------------------------
nb_B1 <- mat2listw(as(B, "dgTMatrix"))
nb_B1$style
all.equal(nb_B1$neighbours, col2, check.attributes=FALSE)
all.equal(attr(nb_B1$neighbours, "region.id"), attr(nb_B$neighbours, "region.id"))

## ---- echo=dothis, eval=dothis-------------------------------------------
rho <- 0.1
sum(log(1 - rho * eigenw(nb_B)))

## ---- echo=dothis, eval=dothis-------------------------------------------
n <- nrow(B)
I <- Diagonal(n)
class(I - rho * B)
c(determinant(I - rho * B, logarithm=TRUE)$modulus)

## ---- echo=dothis, eval=dothis-------------------------------------------
nW <- -B
nChol <- Cholesky(nW, Imult=8)
n * log(rho) + (2 * c(determinant(update(nChol, nW, 1/rho))$modulus))

## ---- echo=dothis, eval=dothis-------------------------------------------
nb_W <- nb2listw(col2, style="W", zero.policy=TRUE)
W <- as(nb_W, "CsparseMatrix")
str(W)
all(W == t(W))

## ---- echo=dothis, eval=dothis-------------------------------------------
set.seed(1)
x <- runif(n)
r1 <- as.numeric(W %*% x)
r2 <- lag(nb_W, x, zero.policy=TRUE)
all.equal(r1, r2, check.attributes=FALSE)
plot(x, r1, ylim=c(0,1))
c(x[21], r1[21])

## ---- echo=dothis, eval=dothis-------------------------------------------
rho <- 0.5
sum(log(1 - rho * eigenw(nb_W)))
class(I - rho * W)
c(determinant(I - rho * W, logarithm=TRUE)$modulus)

## ---- echo=dothis, eval=dothis-------------------------------------------
LU <- lu(I - rho * W)
sum(log(abs(diag(slot(LU, "U")))))

## ---- echo=dothis, eval=dothis-------------------------------------------
d <- attr(nb_W$weights, "comp")$d
all.equal(d, card(col2))

## ---- echo=dothis, eval=dothis-------------------------------------------
dW <- Diagonal(n, d) %*% W
all(dW == t(dW))
isd <- Diagonal(n, 1/sqrt(d))
isd[21,21]
Ws <- as(isd %*% dW %*% isd, "symmetricMatrix")
rowSums(Ws)[21]
class(Ws)
c(determinant(I - rho * Ws, logarithm=TRUE)$modulus)

## ---- echo=dothis, eval=dothis-------------------------------------------
1/range(eigenw(nb_B))
if (!require("RSpectra", quietly=TRUE)) dothis <- FALSE
1/c(eigs(B, k=1, which="SR")$values, eigs(B, k=1, which="LR")$values)

## ---- echo=dothis, eval=dothis-------------------------------------------
1/range(eigenw(nb_W))
1/Re(c(eigs(W, k=1, which="SR")$values, eigs(W, k=1, which="LR")$values))

## ---- echo=dothis, eval=dothis-------------------------------------------
class(B)
object.size(B)
if (!require("igraph", quietly=FALSE)) dothis <- FALSE
g1 <- graph.adjacency(B, mode="undirected")
class(g1)
object.size(g1)

## ---- echo=dothis, eval=dothis-------------------------------------------
B1 <- get.adjacency(g1)
class(B1)
object.size(B1)
all.equal(B, as(as(B1, "dgTMatrix"), "symmetricMatrix"))

## ---- echo=dothis, eval=dothis-------------------------------------------
res <- n.comp.nb(col2)
table(res$comp.id)

## ---- echo=dothis, eval=dothis-------------------------------------------
c1 <- clusters(g1)
c1$no == res$nc
all.equal(c1$membership, res$comp.id)
all.equal(c1$csize, c(table(res$comp.id)), check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
W <- as(nb2listw(col2, style="W", zero.policy=TRUE), "CsparseMatrix")
g1W <- graph.adjacency(W, mode="directed", weighted="W")
c1W <- clusters(g1W)
all.equal(c1W$membership, res$comp.id)

## ---- echo=dothis, eval=dothis-------------------------------------------
is.connected(g1)
dg1 <- diameter(g1)
dg1
sp_mat <- shortest.paths(g1)
str(sp_mat)

## ---- echo=dothis, eval=dothis-------------------------------------------
nbl10 <- nblag(col2, maxlag=10)
vals <- sapply(nbl10, function(x) sum(card(x)))
zero <- which(vals == 0)
zero[1]-1

## ---- echo=dothis, eval=dothis-------------------------------------------
lmat <- lapply(nbl10[1:(zero[1]-1)], nb2mat, style="B", zero.policy=TRUE)
mat <- matrix(0, n, n)
for (i in seq(along=lmat)) mat = mat + i*lmat[[i]]
mat[mat==0] <- Inf
diag(mat) <- 0
all.equal(mat, sp_mat, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
nb_r <- cell2nb(7, 7, type="rook")
nb_rW <- nb2listw(nb_r, style="W")
spdep:::find_q1_q2(nb_rW)

## ---- echo=dothis, eval=dothis-------------------------------------------
1/range(Re(eigenw(similar.listw(nb_rW))))

## ---- echo=dothis, eval=dothis-------------------------------------------
spdep:::find_q1_q2(nb_W)
1/range(Re(eigenw(similar.listw(nb_W))))

