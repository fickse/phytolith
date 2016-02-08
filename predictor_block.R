# get big block of predictors for phytolith analysis
library(maps)
library(fields)
library(raster)
library(dismo)
library(xtable)

setwd('C:/projects/phytolith/')

bio <- stack( list.files ("C:/data/clim/wc2-5/bio/wc2-5", full = TRUE, pattern = 'bil$') ) 
bio4 <- stack('C:/projects/phytolith/data/raw/bioclim_600pb.tif')
names(bio4) <- gsub('600pb', '400AD', names(bio4))
bio6 <- stack('C:/projects/phytolith/data/raw/bioclim_6kpb.tif')
names(bio6) <- gsub('6kpb', '6kbp', names(bio6))
bio8 <- stack('C:/projects/phytolith/data/raw/bio_millenium.tif')
names(bio8) <- gsub('millenium', 'preind', names(bio8))

soil <- stack('C:/data/soil/soilgrids/ca/aggregated/ca_soil_30cm.grd')
alt <- raster("C:/data/elev/wc_elev30_land.tif")
veg <- raster("C:/data/veg/calveg.grd")
veg <- as.factor(round(veg))
names(veg) <- 'veg'


# get a mask of california for plotting results
mask <- shapefile('C:/Users/steve/Downloads/GEO2015/13/counties_2000.shp')
mask <- spTransform(mask, CRS(projection(bio)))


bio <- crop(bio, mask)
bio4 <- crop(bio4, mask)
bio6 <- crop(bio6, mask)
bio8 <- crop(bio8, mask)
soil <- crop(soil, mask)
veg <- crop(veg, mask)
alt <- crop(alt, mask)

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


dcoast <- is.na(alt)
dcoast[dcoast==0] <- NA
dcoast <- distance(dcoast, progress = 'text')
#why log? not sure about this one
	#dcoast <- log(dcoast)
names(dcoast) <- 'dcoast'

block <- stack(hillshade, alt, slope,northness, eastness,  dcoast, veg, soil, bio, bio4, bio6, bio8)
names(block)[c(1,2)] <- c('hillshade','alt')

#writeRaster(block, file = 'C:/projects/phytolith/data/predictors.grd', overwrite = TRUE)

mb <- mask(block, mask ,file = 'C:/projects/phytolith/data/processed/predictors.grd', overwrite = TRUE,  progress = 'text') 

mba <- aggregate(mb, 10, file = 'C:/projects/phytolith/data/processed/aggregated_predictors.grd', overwrite = TRUE, progress='text')


