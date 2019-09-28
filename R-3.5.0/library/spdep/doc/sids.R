## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

## ---- echo=FALSE,eval=TRUE,warning=FALSE, message=FALSE------------------
library(spdep)

## ----echo=TRUE,eval=TRUE-------------------------------------------------
library(spdep)
nc <- st_read(system.file("shapes/sids.shp", package="spData")[1], quiet=TRUE)
st_crs(nc) <- "+proj=longlat +datum=NAD27"
row.names(nc) <- as.character(nc$FIPSNO)

## ----echo=TRUE-----------------------------------------------------------
plot(st_geometry(nc), axes=TRUE)
text(st_coordinates(st_centroid(st_geometry(nc), of_largest_polygon=TRUE)), label=nc$FIPSNO, cex=0.5)

## ----echo=TRUE,eval=TRUE-------------------------------------------------
names(nc)
summary(nc)

## ----echo=TRUE,eval=TRUE-------------------------------------------------
library(sf)
nc_sf <- st_read(system.file("shape/nc.shp", package="sf"),
                 quiet=TRUE)
st_crs(nc_sf)
nc <- st_read(system.file("shapes/sids.shp",
                 package="spData"), quiet=TRUE)
st_crs(nc)

## ----echo=TRUE,eval=TRUE-------------------------------------------------
st_crs(nc) <- "+proj=longlat +datum=NAD27"

## ----echo=TRUE,eval=TRUE-------------------------------------------------
suppressWarnings(st_crs(nc_sf) <- st_crs(nc))
xx <- st_equals(nc, nc_sf, sparse=FALSE)
all(diag(xx)) && sum(xx) == 100L

## ----echo=TRUE,eval=TRUE-------------------------------------------------
td <- tempdir()
download.file("https://s3.amazonaws.com/geoda/data/sids.zip", file.path(td, "sids.zip"), quiet=TRUE)
unzip(file.path(td, "sids.zip"), c("sids/sids.dbf", "sids/sids.prj", "sids/sids.shp", "sids/sids.shx"), exdir=td)
sids_sf <- st_read(file.path(td, "sids/sids.shp"), quiet=TRUE)
download.file("https://s3.amazonaws.com/geoda/data/sids2.zip", file.path(td, "sids2.zip"), quiet=TRUE)
unzip(file.path(td, "sids2.zip"), c("sids2/sids2.dbf", "sids2/sids2.prj", "sids2/sids2.shp", "sids2/sids2.shx"), exdir=td)
sids2_sf <- st_read(file.path(td, "sids2/sids2.shp"), quiet=TRUE)

## ----echo=TRUE,eval=TRUE-------------------------------------------------
st_crs(sids_sf)
st_crs(sids2_sf)

## ----echo=TRUE,eval=TRUE-------------------------------------------------
suppressWarnings(st_crs(sids_sf) <- st_crs(nc_sf))
xx <- st_equals(sids_sf, nc_sf, sparse=FALSE)
all(diag(xx)) && sum(xx) == 100L

## ----echo=TRUE,eval=TRUE-------------------------------------------------
suppressWarnings(st_crs(sids2_sf) <- st_crs(nc_sf))
xx <- st_equals(sids2_sf, nc_sf, sparse=FALSE)
all(diag(xx)) && sum(xx) == 100L

## ----echo=TRUE, eval=TRUE------------------------------------------------
all.equal(as.data.frame(nc_sf)[,1:14], as.data.frame(sids_sf)[,1:14])
all.equal(as.data.frame(nc_sf)[,1:14], as.data.frame(sids2_sf)[,1:14])

## ----echo=TRUE, eval=TRUE------------------------------------------------
all.equal(as.data.frame(nc_sf)[,1:14], as.data.frame(nc)[,c(2,3,4,1,5:14)])

## ----echo=TRUE, eval=TRUE------------------------------------------------
which(!(nc_sf$NWBIR74 == nc$NWBIR74))
c(nc$NWBIR74[21], nc_sf$NWBIR74[21])

## ----echo=TRUE, eval=TRUE------------------------------------------------
gal_file <- system.file("weights/ncCR85.gal", package="spData")[1]
ncCR85 <- read.gal(gal_file, region.id=nc$FIPSNO)
ncCR85
gal_file <- system.file("weights/ncCC89.gal", package="spData")[1]
ncCC89 <- read.gal(gal_file, region.id=nc$FIPSNO)
ncCC89

## ----label=plot-CC89.nb, echo=TRUE---------------------------------------
plot(st_geometry(nc), border="grey")
plot(ncCC89, st_centroid(st_geometry(nc), of_largest_polygon), add=TRUE, col="blue")

## ----echo=TRUE-----------------------------------------------------------
r.id <- attr(ncCC89, "region.id")
ncCC89[[match("37001", r.id)]]
r.id[ncCC89[[match("37001", r.id)]]]

## ----echo=TRUE-----------------------------------------------------------
as.character(nc$NAME)[card(ncCC89) == 0]

## ----echo=TRUE-----------------------------------------------------------
ch <- choynowski(nc$SID74, nc$BIR74)
nc$ch_pmap_low <- ifelse(ch$type, ch$pmap, NA)
nc$ch_pmap_high <- ifelse(!ch$type, ch$pmap, NA)
prbs <- c(0,.001,.01,.05,.1,1)
nc$high = cut(nc$ch_pmap_high, prbs)
nc$low = cut(nc$ch_pmap_low,prbs )

## ------------------------------------------------------------------------
is_tmap <- FALSE
if (require(tmap, quietly=TRUE)) is_tmap <- TRUE
is_tmap

## ----choymap, echo=TRUE, eval=is_tmap------------------------------------
library(tmap)
tm_shape(nc) + tm_fill(c("low", "high"), palette="Set1", title="p-values") +
  tm_facets(free.scales=FALSE) + tm_layout(panel.labels=c("low", "high"))

## ----echo=TRUE-----------------------------------------------------------
pmap <- probmap(nc$SID74, nc$BIR74)
nc$pmap <- pmap$pmap

## ---- eval=is_tmap, echo=TRUE--------------------------------------------
brks <- c(0,0.001,0.01,0.025,0.05,0.95,0.975,0.99,0.999,1)
tm_shape(nc) + tm_fill("pmap", breaks=brks, midpoint=0.5, palette="RdBu") + tm_layout(legend.outside=TRUE)

## ----label=poishist, echo=TRUE-------------------------------------------
hist(nc$pmap, main="")

## ----echo=TRUE-----------------------------------------------------------
res <- glm(SID74 ~ offset(log(BIR74)), data=nc, family="quasipoisson")
nc$stdres <- rstandard(res)

## ---- eval=is_tmap, echo=TRUE--------------------------------------------
brks <- c(-4, -3, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 3, 4)
tm_shape(nc) + tm_fill("stdres", breaks=brks, midpoint=0, palette="RdBu") + tm_layout(legend.outside=TRUE)

## ----echo=TRUE-----------------------------------------------------------
global_rate <- sum(nc$SID74)/sum(nc$BIR74)
nc$Expected <- global_rate * nc$BIR74
res <- EBlocal(nc$SID74, nc$Expected, ncCC89, zero.policy=TRUE)
nc$EB_loc <- res$est

## ---- eval=is_tmap-------------------------------------------------------
brks <- c(0, 0.25, 0.5, 0.75, 1, 2, 3, 4, 5)
nc_miss <- st_centroid(st_geometry(nc[card(ncCC89) == 0,]), of_largest_polygon)
tm_shape(nc) + tm_fill("EB_loc", breaks=brks, midpoint=1, palette="RdBu") + tm_layout(legend.outside=TRUE) + tm_shape(nc_miss) + tm_symbols(shape=8, size=0.5)


## ----echo=TRUE-----------------------------------------------------------
set.seed(1)
EBImoran.mc(nc$SID74, nc$BIR74, nb2listw(ncCC89, style="B", zero.policy=TRUE), nsim=999, zero.policy=TRUE)

## ----echo=TRUE-----------------------------------------------------------
nc$both <- factor(paste(nc$L_id, nc$M_id, sep=":"))
nboth <- length(table(unclass(nc$both)))

## ---- eval=is_tmap-------------------------------------------------------
tm_shape(nc) + tm_fill("both", palette="Set1", title="rough\nrectangles") + tm_layout(legend.outside=TRUE)

## ----echo=TRUE-----------------------------------------------------------
nc$ft.SID74 <- sqrt(1000)*(sqrt(nc$SID74/nc$BIR74) + sqrt((nc$SID74+1)/nc$BIR74))
stem(round(nc$ft.SID74, 1), scale=2)

## ----echo=TRUE,eval=TRUE-------------------------------------------------
mBIR74 <- tapply(nc$BIR74, nc$both, sum)
mSID74 <- tapply(nc$SID74, nc$both, sum)

## ----echo=TRUE,eval=TRUE-------------------------------------------------
mFT <- sqrt(1000)*(sqrt(mSID74/mBIR74) + sqrt((mSID74+1)/mBIR74))
mFT1 <- t(matrix(mFT, 4, 4, byrow=TRUE))
med <- medpolish(mFT1, na.rm=TRUE, trace.iter=FALSE)
med

## ----echo=TRUE,eval=TRUE-------------------------------------------------
mL_id <- model.matrix(~ as.factor(nc$L_id) -1)
mM_id <- model.matrix(~ as.factor(nc$M_id) -1)
nc$pred <- c(med$overall + mL_id %*% med$row + mM_id %*% med$col)
nc$mp_resid <- nc$ft.SID74 - nc$pred

## ---- eval=is_tmap-------------------------------------------------------
out1 <- tm_shape(nc) + tm_fill(c("ft.SID74", "pred")) + tm_facets(free.scales=FALSE) + tm_layout(panel.labels=c("Observed", "Median polish prediction"))
out2 <- tm_shape(nc) + tm_fill("mp_resid", midpoint=0) + tm_layout(legend.outside=TRUE)
tmap_arrange(out1, out2, ncol=1)

