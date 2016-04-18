# Prepare phytolith data
library(raster)


z <- read.csv('C:/projects/phytolith/data/raw/dat.csv')
b <- brick('C:/projects/phytolith/data/processed/predictors.grd')

# remove stuff without location
z <- z[-which(is.na(z$lat)),]


z$bilobate_star <- 0
star <- grep('[*]', z$bilobate_ratio)
z$bilobate_star[star] <- 1

z$bilobate_ratio <- as.numeric(gsub('[*]', '', z$bilobate_ratio))

# extract fine-scale slope and aspect
topo <- stack('C:/projects/phytolith/data/processed/fine_topo.grd')

e.topo <- as.data.frame( extract( topo, z[, c('lon','lat')]))




# three observations have NAs in them (too close to coast)...
# extending slope and aspect rasters by 'focal' procedure, then filling in these missing values

e <- as.data.frame(extract(b, z[, c('lon','lat')]))

nas <- which( apply(e,1, function(x) any(is.na(x))))

# 
# e[109,'rootaws'] <- 128.55
# e[c(109,58,45),'wetland'] <- 0

# slope2 <- focal(b[['slope']], matrix(1, 7,7), fun = mean, na.rm=TRUE)
# e[nas,'slope'] <- extract(slope2, z[nas, c('lon','lat')])

# x <- focal(b[['northness']], matrix(1, 5,5), fun = mean, na.rm=TRUE)
# e[nas,'northness'] <- extract(x, z[nas, c('lon','lat')])

# x <- focal(b[['eastness']], matrix(1, 5,5), fun = mean, na.rm=TRUE)
# e[nas,'eastness'] <- extract(x, z[nas, c('lon','lat')])

e$slope <- e$northness <- e$eastness <- NULL
e <- cbind(e.topo, e)
a <- cbind(z,e)
a$dcoastkm <- a$dcoast/1000
write.csv(a , 'C:/projects/phytolith/data/processed/phyto.csv', row.names = F)

