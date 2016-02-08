library(raster)
setwd('C:/data/clim/')


ext <- new("Extent"

    , xmin = -125.352614639443
    , xmax = -113.109333543521
    , ymin = 31.5300559773918
    , ymax = 43.4376100891511
)
	cvars <- c('tmin', 'tmax', 'ppt')

	out <- list()
		
	for ( cvar in cvars) { 
		cat(cvar,'\n')
		
		if( cvar == 'tavg'){
			ccv <- 'tmean'
		} else if(cvar == 'ppt'){
			ccv <- 'prec'
		} else { ccv <- cvar}

		wc <- stack(paste0('wc2-5/', ccv,1:12, '.bil'))
		W <- crop(wc, ext)
		
		#if(cvar != 'ppt' ) { W <- W/10}
		
		
		# preind <- stack(paste0('ccsm/mean/', cvar,'_1850.tif'))
		# holo <- stack(paste0('pmip/mean/', cvar,'.tif'))
		
		preind <- stack(paste0('CCSM/CCSM4 1° Last Millennium 850-1850 simulation/out/', cvar, '.grd'))
		modern <- stack(paste0('CCSM/CCSM4 1° Last Millennium 1850-2005/out/', cvar, '.grd'))
		
		#extent(holo) <- extent(preind)
		
		#h2 <- resample(holo, preind)

		# delta <- h2 - preind
		delta <- preind - modern
		
		delta <- crop(delta, ext)
		
		d2 <- resample(delta, W, progress = 'text')

		
		out[[cvar]] <- d3 <-  d2+ W
		
		if( cvar == 'ppt') {
			d3[d3 < 0] <- 0
			out[[cvar]] <- d3
		}
		# writeRaster(d3, paste0('C:/data/clim/paleo/', cvar , '_6k.tif'), progress = 'text')
		
	}

library(dismo)
bb <- biovars(out[['ppt']], out[['tmin']], out[['tmax']])
# writeRaster(bb, filename = "C:/projects/phytolith/data/bioclim_6kpb.tif", overwrite = TRUE)
writeRaster(bb, filename = "C:/projects/phytolith/data/bio_millenium.tif", overwrite = TRUE)

#####################################3
######################################

# Now do the same for the difference between 1850 and late holocene (400's)

	cvars <- c('tmin', 'tmax', 'ppt')

	out <- list()
		
	for ( cvar in cvars) { 
		cat(cvar,'\n')
		
		if( cvar == 'tavg'){
			ccv <- 'tmean'
		} else if(cvar == 'ppt'){
			ccv <- 'prec'
		} else { ccv <- cvar}

		wc <- stack(paste0('wc2-5/', ccv,1:12, '.bil'))
		W <- crop(wc, ext)
		
		#if(cvar != 'ppt' ) { W <- W/10}
		
		
		preind <- stack(paste0('ccsm/mean/', cvar,'_1850.tif'))
		holo <- stack(paste0('ccsm/mean/', cvar,'.tif'))
		# holo <- stack(paste0('pmip/mean/', cvar,'.tif'))
		
		extent(holo) <- extent(preind)
		
		h2 <- resample(holo, preind)

		delta <- h2 - preind
		
		delta <- crop(delta, ext)

		d2 <- resample(delta, W, progress = 'text')

		out[[cvar]] <- d3 <- d2+ W
		

		# writeRaster(d3, paste0('C:/data/clim/paleo/', cvar , '_6k.tif'), progress = 'text')
		
	}

jj <- biovars(out[['ppt']], out[['tmin']], out[['tmax']])
writeRaster(jj, filename = "C:/projects/phytolith/data/bioclim_600pb.tif", overwrite = TRUE)



