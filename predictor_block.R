# get big block of predictors for phytolith analysis
library(maps)
library(fields)
library(raster)
library(dismo)
library(xtable)

setwd('C:/projects/phytolith/')

# bioclim 1
bio <- stack( list.files ("C:/data/clim/wc2-5/bio/wc2-5", full = TRUE, pattern = 'bil$') ) 

# bioclim 600 bp
bio4 <- stack('C:/projects/phytolith/data/raw/bioclim_600pb.tif')
names(bio4) <- gsub('600pb', '400AD', names(bio4))

# bioclim 6000 bp
bio6 <- stack('C:/projects/phytolith/data/raw/bioclim_6kpb.tif')
names(bio6) <- gsub('6kpb', '6kbp', names(bio6))

# bioclim 1000 bp
bio8 <- stack('C:/projects/phytolith/data/raw/bio_millenium.tif')
names(bio8) <- gsub('millenium', 'preind', names(bio8))

# soilgrids data
soil <- stack('C:/data/soil/soilgrids/ca/aggregated/ca_soil_30cm.grd')

# gssurgo data
wetland <- raster("C:/projects/phytolith/data/wetfinal.tif")
names(wetland) <- 'wetland'

# altitude
alt <- raster("C:/data/elev/wc_elev30_land.tif")

# calveg
veg <- raster("C:/data/veg/calveg.grd")
veg <- as.factor(round(veg))
names(veg) <- 'veg'

# wc1
wc <- stack( lapply(
					list.files( "C:/data/clim/wc2-5/", pattern = "prec.*?bil$",full = TRUE)[c(1,5:12,2:4)], raster )
			)

# wc Temperature			
wcT <- stack( lapply(
					list.files( "C:/data/clim/wc2-5/", pattern = "tmean.*?bil$",full = TRUE)[c(1,5:12,2:4)], raster )
			)


# get a mask of california for plotting results
mask <- shapefile('C:/Users/steve/Downloads/GEO2015/13/counties_2000.shp')
mask <- spTransform(mask, CRS(projection(bio)))

# crop data to mask
wc <- crop(wc, mask)
wcT <- crop(wcT, mask);wcT <- wcT/10
bio <- crop(bio, mask)
bio4 <- crop(bio4, mask)
bio6 <- crop(bio6, mask)
bio8 <- crop(bio8, mask)
soil <- crop(soil, mask)
veg <- crop(veg, mask)
alt <- crop(alt, mask)

# resample to resolution of 'alt'
wc <- resample(wc, alt, progress = 'text')
wcT <- resample(wcT, alt, progress = 'text')
bio <- resample(bio, alt, progress = 'text')
bio4 <- resample(bio4, alt, progress = 'text')
bio6 <- resample(bio6, alt, progress = 'text')
bio8 <- resample(bio8, alt, progress = 'text')
veg <- resample(veg, alt, progress = 'text')

slope <- terrain(alt, opt='slope', unit = 'degrees')
aspect <- terrain(alt, opt='aspect')
northness <- calc(aspect, function(x) pi - pmin( 2*pi - x, x-0))
eastness <- calc(aspect, function(x) pi - pmin( abs(.5*pi - x), abs(x-2.5*pi)))
names(eastness) <- 'eastness'
names(northness) <- 'northness'
hillshade <- hillShade(slope, aspect,40, 20)

# Generate summer precip
summer_precip <- sum(wc[[5:9]])
names(summer_precip) <- 'summer_precip'

#NPP from the 'Miami' equation
	NPPt <- 3000/( 1 + exp( 1.315 - .119* mean(wcT)))
	NPPp <- 3000*( 1 - exp( - 0.000664 * sum(wc)))
	NPP <- overlay( NPPt, NPPp, fun = function(x,y) pmin(x,y))
	names(NPP) = 'NPP'
# Distance to coast (takes a long time)
thefile <- "C:/projects/phytolith/data/dcoast.tif"
if( file.exists(thefile)) {
	dcoast <- raster(thefile)
} else {
	dcoast <- is.na(alt)
	dcoast[dcoast==0] <- NA
	dcoast <- distance(dcoast, progress = 'text', file = thefile)
}
names(dcoast) <- 'dcoast'

block <- stack(hillshade, alt, slope,northness, eastness,  dcoast, veg, soil,summer_precip, bio, bio4, bio6, bio8, wetland, NPP)
names(block)[c(1,2)] <- c('hillshade','alt')

#writeRaster(block, file = 'C:/projects/phytolith/data/predictors.grd', overwrite = TRUE)

mb <- mask(block, mask ,file = 'C:/projects/phytolith/data/processed/predictors.grd', overwrite = TRUE,  progress = 'text') 

mba <- aggregate(mb, 10, file = 'C:/projects/phytolith/data/processed/aggregated_predictors.grd', overwrite = TRUE, progress='text')

############
# Slope and aspect are way too smoothed out at this scale. Point values should be extracted from 90m product!

slope <- raster('C:/data/elev/90m/90m_slope.tif')
aspect <- raster('C:/data/elev/90m/90m_aspect.tif')

northness <- calc(aspect, function(x) 180 - pmin( abs(x-360), x) ) 
eastness <- calc( aspect, function(x)  pmin( abs(x - 270), (x + 90) ))

topo <- stack(slope, northness, eastness)
names(topo) <- c('slope', 'northness', 'eastness')
writeRaster(topo, file = 'C:/projects/phytolith/data/processed/fine_topo.grd', progress = 'text', overwrite = TRUE)
