## ---- eval=FALSE---------------------------------------------------------
#  library(sf)
#  sf_bna <- st_read("t8_36.bna", stringsAsFactors=FALSE)
#  table(st_is_valid(sf_bna))
#  sf_bna$AREAKEY <- gsub("\\.", "", sf_bna$Primary.ID)
#  data(NY_data, package="spData")
#  key <- as.character(nydata$AREAKEY)
#  sf_bna1 <- sf_bna[match(key, sf_bna$AREAKEY), c("AREAKEY")]
#  sf_bna2 <- merge(sf_bna1, nydata, by="AREAKEY")
#  sf_bna2_utm18 <- st_transform(sf_bna2, "+proj=utm +zone=18 +datum=NAD27")
#  st_write(sf_bna2_utm18, "NY8_bna_utm18.gpkg")

## ---- echo=FALSE---------------------------------------------------------
rv <- R.Version()
dothis <- TRUE
if (rv$major > "3" || (rv$major == "3" && !(rv$minor >= "3.0"))) dothis=FALSE

## ---- echo=dothis, eval=dothis-------------------------------------------
if (!suppressPackageStartupMessages(require(sf, quietly=TRUE))) {
  message("install the sf package")
  dothis <- FALSE
}
if (dothis) sf_extSoftVersion()

## ---- echo=dothis, eval=dothis-------------------------------------------
if (!suppressPackageStartupMessages(require(rgdal, quietly=TRUE))) {
  message("install the rgdal package")
  dothis <- FALSE
}
if (dothis) {
  if (exists("rgdal_extSoftVersion")) rgdal_extSoftVersion()
  else getGDALVersionInfo()
}

## ---- echo=dothis, eval=dothis-------------------------------------------
if (!suppressPackageStartupMessages(require(rgeos, quietly=TRUE))) {
  message("install the rgeos package")
  dothis <- FALSE
}
if (dothis) {
  if (exists("rgeos_extSoftVersion")) rgeos_extSoftVersion()
  else version_GEOS()
}

## ---- echo=dothis, eval=dothis-------------------------------------------
NY8_sf <- st_read(system.file("shapes/NY8_bna_utm18.gpkg", package="spData"), quiet=TRUE)
table(st_is_valid(NY8_sf))

## ---- echo=dothis, eval=dothis-------------------------------------------
NY8_sp <- readOGR(system.file("shapes/NY8_bna_utm18.gpkg", package="spData"), verbose=FALSE)
table(gIsValid(NY8_sp, byid=TRUE))

## ---- echo=dothis, eval=dothis-------------------------------------------
suppressPackageStartupMessages(library(spdep))
reps <- 10
eps <- sqrt(.Machine$double.eps)
system.time(for(i in 1:reps) NY8_sp_1_nb <- poly2nb(NY8_sp, queen=TRUE, snap=eps))/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
NY8_sp_1_nb

## ---- echo=dothis, eval=dothis-------------------------------------------
STRQ <-system.time(for(i in 1:reps) a2 <- gUnarySTRtreeQuery(NY8_sp))/reps
system.time(for(i in 1:reps) NY8_sp_1_fB_nb <- poly2nb(NY8_sp, queen=TRUE, snap=eps, foundInBox=a2))/reps + STRQ

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(NY8_sp_1_fB_nb, NY8_sp_1_nb, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for(i in 1:reps) NY8_sf_1_sgbp <- st_queen(NY8_sf))/reps

## ---- echo=dothis, eval=dothis, eval=FALSE-------------------------------
#  system.time(for (i in 1:reps) NY8_sf_dist_nb <- st_is_within_distance(NY8_sf, NY8_sf, dist=eps))/reps

## ---- echo=dothis, eval=dothis, eval=FALSE-------------------------------
#  NY8_sf_dist_nb1 <- lapply(1:length(NY8_sf_dist_nb), function(i) NY8_sf_dist_nb[[i]][-match(i, NY8_sf_dist_nb[[i]])])
#  all.equal(NY8_sf_dist_nb1, NY8_sp_1_nb, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
as.nb.sgbp <- function(x, ...) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

## ---- echo=dothis, eval=dothis-------------------------------------------
NY8_sf_1_nb <- as.nb.sgbp(NY8_sf_1_sgbp)
all.equal(NY8_sf_1_nb, NY8_sp_1_nb, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for(i in 1:reps) as(NY8_sf, "Spatial"))/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for(i in 1:reps) sp_touches <- gTouches(NY8_sp, byid=TRUE, returnDense=FALSE))/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(sp_touches, NY8_sp_1_nb, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for(i in 1:reps) NY8_sf_1_touch <- st_touches(NY8_sf, NY8_sf))/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
class(NY8_sf_1_touch) <- "sgbp"
all.equal(as.nb.sgbp(NY8_sf_1_touch), NY8_sp_1_nb, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
plot(NY8_sp, border="grey", lwd=0.5)
plot(NY8_sp_1_nb, coordinates(NY8_sp), points=FALSE, add=TRUE, lwd=0.7)

## ---- echo=dothis, eval=dothis-------------------------------------------
NY8_sp_old <- readOGR(system.file("shapes/NY8_utm18.shp", package="spData"), verbose=FALSE)
if (suppressPackageStartupMessages(require(rgeos, quietly=TRUE))) suppressWarnings(table(gIsValid(NY8_sp_old, byid=TRUE)))

## ---- echo=dothis, eval=dothis-------------------------------------------
try(NY8_sp_old_1_nb <- poly2nb(NY8_sp_old), silent = TRUE)
all.equal(NY8_sp_old_1_nb, NY8_sp_1_nb, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
NY8_sp_old_buf <- gBuffer(NY8_sp_old, width=0, byid=TRUE)
table(gIsValid(NY8_sp_old_buf, byid=TRUE))

## ---- echo=dothis, eval=dothis-------------------------------------------
try(NY8_sp_old_1_nb_buf <- poly2nb(NY8_sp_old_buf), silent = TRUE)
all.equal(NY8_sp_old_1_nb_buf, NY8_sp_1_nb, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(NY8_sp_old_1_nb_buf, NY8_sp_old_1_nb, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
NY8_sf_old <- st_read(system.file("shapes/NY8_utm18.shp", package="spData"), quiet=TRUE)
suppressWarnings(table(st_is_valid(NY8_sf_old)))

## ---- echo=dothis, eval=dothis-------------------------------------------
NY8_sf_old_1_sgbp <- try(st_queen(NY8_sf_old), silent = TRUE)
if (class(NY8_sf_old_1_sgbp) == "try-error") cat(NY8_sf_old_1_sgbp)

## ---- echo=dothis, eval=dothis-------------------------------------------
NY8_sf_old_buf <- st_buffer(NY8_sf_old, dist=0)
table(st_is_valid(NY8_sf_old_buf))

## ---- echo=dothis, eval=dothis-------------------------------------------
try(NY8_sf_old_1_nb_buf <- st_queen(NY8_sf_old_buf), silent = TRUE)
all.equal(NY8_sf_old_1_nb_buf, NY8_sf_1_nb, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(NY8_sf_old_1_nb_buf, NY8_sp_old_1_nb_buf, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for (i in 1:reps) coords <-  coordinates(NY8_sp))/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
IDs <- row.names(NY8_sp)
is.projected(NY8_sp)

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for (i in 1:reps) if (class(st_geometry(NY8_sf))[1] == "sfc_MULTIPOLYGON") {
  NY8_pt_sf <- st_centroid(st_geometry(NY8_sf), of_largest_polygon=TRUE)
} else {
  NY8_pt_sf <- st_centroid(st_geometry(NY8_sf))
})/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
if (!isTRUE(all.equal(st_crs(NY8_pt_sf), st_crs(NY8_sf)))) st_crs(NY8_pt_sf) <- st_crs(NY8_sf)
class(st_geometry(NY8_pt_sf))[1]

## ---- echo=dothis, eval=dothis-------------------------------------------
zm <- class(st_geometry(NY8_pt_sf)[[1]])[1]
if (zm %in% c("XYM", "XYZM"))
  NY8_pt_sf <- st_zm(NY8_pt_sf, drop=TRUE, what="ZM")
if (zm %in% c("XYZ"))
  NY8_pt_sf <- st_zm(NY8_pt_sf, drop=TRUE, what="ZM")

## ---- echo=dothis, eval=dothis-------------------------------------------
st_is_longlat(NY8_pt_sf)

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for (i in 1:reps) coords_sf <- st_coordinates(NY8_pt_sf))/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(coords, coords_sf, check.attributes=FALSE, scale=1)

## ---- echo=dothis, eval=dothis-------------------------------------------
diffs2d <- coords - coords_sf
diffs1d <- apply(diffs2d, 1, function(x) mean(abs(x)))
diffs <- unname(which(diffs1d > 1e-3))
cbind(coords, coords_sf)[diffs, c(1,3,2,4)]

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(coords[-diffs,], coords_sf[-diffs,], check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
plot(NY8_sp[diffs,])
points(coords[diffs,], pch=3, col="red")
points(coords_sf[diffs,], pch=4, col="blue")

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for (i in 1:reps) gcoords <- coordinates(gCentroid(NY8_sp, byid=TRUE)))/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(coords_sf, gcoords, check.attributes=FALSE, scale=1)

## ---- echo=dothis, eval=dothis-------------------------------------------
gdiffs2d <- gcoords - coords_sf
gdiffs1d <- apply(gdiffs2d, 1, function(x) mean(abs(x)))
gdiffs <- unname(which(gdiffs1d > 1e-3))
cbind(gcoords, coords_sf)[gdiffs, c(1,3,2,4)]

## ---- echo=dothis, eval=dothis-------------------------------------------
plot(NY8_sp[gdiffs,])
points(coords[gdiffs,], pch=3, col="red")
points(coords_sf[gdiffs,], pch=4, col="blue")

## ---- echo=dothis, eval=dothis-------------------------------------------
suppressPackageStartupMessages(require(deldir))
NY84_nb <- tri2nb(coords, row.names=IDs)
if (require(rgeos, quietly=TRUE) && require(RANN, quietly=TRUE)) {
  NY85_nb <- graph2nb(soi.graph(NY84_nb, coords), row.names=IDs)
} else NY85_nb <- NULL
NY86_nb <- graph2nb(gabrielneigh(coords), row.names=IDs)
NY87_nb <- graph2nb(relativeneigh(coords), row.names=IDs)

## ---- echo=dothis, eval=dothis-------------------------------------------
NY84_nb_sf <- tri2nb(coords_sf)
isTRUE(all.equal(NY84_nb_sf, NY84_nb, check.attributes=FALSE))

## ---- echo=dothis, eval=dothis-------------------------------------------
if (require(rgeos, quietly=TRUE) && require(RANN, quietly=TRUE)) {
  NY85_nb_sf <- graph2nb(soi.graph(NY84_nb_sf, coords_sf))
} else NY85_nb_sf <- NULL
if (!is.null(NY85_nb_sf) && !is.null(NY85_nb)) {
  isTRUE(all.equal(NY85_nb_sf, NY85_nb, check.attributes=FALSE))
}

## ---- echo=dothis, eval=dothis-------------------------------------------
if (!is.null(NY85_nb_sf) && !is.null(NY85_nb)) {
  diffs <- diffnb(NY85_nb_sf, NY85_nb)
  wdiffs <- which(card(NY85_nb_sf) != card(NY85_nb))
  wdiffs
}

## ---- echo=dothis, eval=dothis-------------------------------------------
if (!is.null(NY85_nb_sf) && !is.null(NY85_nb)) {
  c(dist(rbind(coords[97,], coords_sf[97,])))
}

## ---- echo=dothis, eval=dothis-------------------------------------------
if (!is.null(NY85_nb_sf) && !is.null(NY85_nb)) {
  plot(NY8_sp[wdiffs,], border="grey30", lwd=2)
  plot(NY8_sp, border="grey80", add=TRUE)
  plot(diffs, coordinates(NY8_sp), points=FALSE, add=TRUE, lwd=2)
  plot(NY85_nb, coordinates(NY8_sp), points=FALSE, add=TRUE, col="red")
  text(coordinates(NY8_sp), IDs, pos=1)
}

## ---- echo=dothis, eval=dothis-------------------------------------------
c(dist(rbind(gcoords[97,], coords_sf[97,])))

## ---- echo=dothis, eval=dothis-------------------------------------------
NY84g_nb <- tri2nb(gcoords, row.names=IDs)
if (require(rgeos, quietly=TRUE) && require(RANN, quietly=TRUE)) {
  NY85g_nb <- graph2nb(soi.graph(NY84g_nb, gcoords), row.names=IDs)
} else NY85g_nb <- NULL
if (!is.null(NY85_nb_sf) && !is.null(NY85_nb)) {
  isTRUE(all.equal(NY85_nb_sf, NY85g_nb, check.attributes=FALSE))
}

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for (i in 1:reps) coords_sf_sp <- coordinates(as(NY8_sf, "Spatial")))/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(as(NY8_sp, "SpatialPolygons"), as(as(NY8_sf, "Spatial"),"SpatialPolygons"))

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(coords_sf_sp, coords)

## ---- echo=dothis, eval=dothis-------------------------------------------
NY86_nb_sf <- graph2nb(gabrielneigh(coords_sf))
isTRUE(all.equal(NY86_nb_sf, NY86_nb, check.attributes=FALSE))

## ---- echo=dothis, eval=dothis-------------------------------------------
NY87_nb_sf <- graph2nb(relativeneigh(coords_sf))
isTRUE(all.equal(NY87_nb_sf, NY87_nb, check.attributes=FALSE))

## ---- echo=dothis, eval=dothis-------------------------------------------
NY88_nb <- knn2nb(knearneigh(coords, k=1), row.names=IDs)
NY88_nb_sf <- knn2nb(knearneigh(coords_sf, k=1))
isTRUE(all.equal(NY88_nb_sf, NY88_nb, check.attributes=FALSE))

## ---- echo=dothis, eval=dothis-------------------------------------------
NY89_nb <- knn2nb(knearneigh(coords, k=4), row.names=IDs)
NY89_nb_sf <- knn2nb(knearneigh(coords_sf, k=4))
isTRUE(all.equal(NY89_nb_sf, NY89_nb, check.attributes=FALSE))

## ---- echo=dothis, eval=dothis-------------------------------------------
dsts <- unlist(nbdists(NY88_nb_sf, coords_sf))
summary(dsts)
max_1nn <- max(dsts)

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for (i in 1:reps) NY811_nb <- dnearneigh(coords, d1=0, d2=0.75*max_1nn, row.names=IDs))/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for (i in 1:reps) NY811_nb_sf <- dnearneigh(coords_sf, d1=0, d2=0.75*max_1nn))/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
isTRUE(all.equal(NY811_nb_sf, NY811_nb, check.attributes=FALSE))

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for (i in 1:reps) {NY811_buf <- st_buffer(NY8_pt_sf, dist=0.75*max_1nn, nQuadSegs=30)
NY811_nb_sf_buff0 <- st_intersects(NY811_buf, NY8_pt_sf)
NY811_nb_sf_buff <- lapply(1:length(NY811_nb_sf_buff0), function(i) NY811_nb_sf_buff0[[i]][-match(i, NY811_nb_sf_buff0[[i]])])})/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(as.nb.sgbp(NY811_nb_sf_buff), NY811_nb, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for (i in 1:reps) {NY811_buf <- st_buffer(NY8_pt_sf, dist=0.75*max_1nn, nQuadSegs=90)
NY811_nb_sf_buff0 <- st_intersects(NY811_buf, NY8_pt_sf)
NY811_nb_sf_buff <- lapply(1:length(NY811_nb_sf_buff0), function(i) NY811_nb_sf_buff0[[i]][-match(i, NY811_nb_sf_buff0[[i]])])})/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(as.nb.sgbp(NY811_nb_sf_buff), NY811_nb, check.attributes=FALSE)

## ---- echo=dothis, eval=dothis-------------------------------------------
system.time(for (i in 1:reps) {NY811_nb_sf_dist0 <- st_is_within_distance(NY8_pt_sf, NY8_pt_sf, dist=0.75*max_1nn)
NY811_nb_sf_dist <- lapply(1:length(NY811_nb_sf_dist0), function(i) NY811_nb_sf_dist0[[i]][-match(i, NY811_nb_sf_dist0[[i]])])})/reps

## ---- echo=dothis, eval=dothis-------------------------------------------
all.equal(as.nb.sgbp(NY811_nb_sf_dist), NY811_nb, check.attributes=FALSE)

