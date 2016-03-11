############################
# PHYTOLITH ANALYSIS V 2.0 #
############################

# BASIC IDEA

# Recreate Clary (2012) analysis, using phytolith prevalence as a response variable.
# Do the (environmental) trends found in modern assemblages follow the pattern in Clary?

######################################################
# Paths

	#wd
	setwd('C:/projects/phytolith/')

	#data
	phyto_data <- 'data/processed/phyto.csv'
	clary_data <- 'data/processed/clary_data.csv'
	predictor_brick <- 'data/processed/predictors.grd'
	agg_predictors <- 'data/processed/aggregated_predictors.grd'

######################################################
# packages
 
  library(maps)
  library(knitr)
  library(raster)
  library(dismo)
  library(arm)
  library(MASS)
  library(glmnet)
  library(randomForest)
  library(rpart)
  library(spdep)
  library(xtable)
  require(rpart.plot)
  library(mgcv)

###################################################### 

	# Data

	phyto <- read.csv(phyto_data)
	
	clary <- read.csv(clary_data)

	predictors <- brick(predictor_brick)
	
	
	# names (predictors) <- c(
		# "dcoast", "hillshade", "alt", "slope", "northness", "eastness", "coast", 
		# "veg", "bulk", "carbon", "cec", "clay", "coarse", "ph", "sand", 
		# "silt", "bio1", "bio10", "bio11", "bio12", "bio13", "bio14", 
		# "bio15", "bio16", "bio17", "bio18", "bio19", "bio2", "bio3", 
		# "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bioclim_400AD.1", 
		# "bioclim_400AD.2", "bioclim_400AD.3", "bioclim_400AD.4", "bioclim_400AD.5", 
		# "bioclim_400AD.6", "bioclim_400AD.7", "bioclim_400AD.8", "bioclim_400AD.9", 
		# "bioclim_400AD.10", "bioclim_400AD.11", "bioclim_400AD.12", "bioclim_400AD.13", 
		# "bioclim_400AD.14", "bioclim_400AD.15", "bioclim_400AD.16", "bioclim_400AD.17", 
		# "bioclim_400AD.18", "bioclim_400AD.19", "bioclim_6kbp.1", "bioclim_6kbp.2", 
		# "bioclim_6kbp.3", "bioclim_6kbp.4", "bioclim_6kbp.5", "bioclim_6kbp.6", 
		# "bioclim_6kbp.7", "bioclim_6kbp.8", "bioclim_6kbp.9", "bioclim_6kbp.10", 
		# "bioclim_6kbp.11", "bioclim_6kbp.12", "bioclim_6kbp.13", "bioclim_6kbp.14", 
		# "bioclim_6kbp.15", "bioclim_6kbp.16", "bioclim_6kbp.17", "bioclim_6kbp.18", 
		# "bioclim_6kbp.19", "bio_preind.1", "bio_preind.2", "bio_preind.3", 
		# "bio_preind.4", "bio_preind.5", "bio_preind.6", "bio_preind.7", 
		# "bio_preind.8", "bio_preind.9", "bio_preind.10", "bio_preind.11", 
		# "bio_preind.12", "bio_preind.13", "bio_preind.14", "bio_preind.15", 
		# "bio_preind.16", "bio_preind.17", "bio_preind.18", "bio_preind.19"
	)
	
	aggP <- brick( agg_predictors)
	names(aggP) <- names(predictors)
	
	  wc <- stack( 
				lapply(
					list.files( "C:/data/clim/wc2-5/", pattern = "prec.*?bil$",full = TRUE)[c(1,5:12,2:4)], raster )
			)
	
	
######################################################

#reference

# dput (names(clary))
		# c("site", "Unit", "lat", "Longitude", "long.of.coast", "long.from.coast", 
		# "dcoast.km.", "weather.station", "Ann.ppt", "M.S.ppt", "O.A.ppt", 
		# "X.warm.season.ppt", "Cooling.Degree.Days..Ann.from.", "Cooling.degree.days..ann.from.70.", 
		# "Per.grass.hits", "Annual.grass", "Ann.forb", "Herb.leg", "Per.forb", 
		# "Bare.soil", "Rock", "Shrub", "n", "site.1", "p.grass.cover", 
		# "a.grass.cover", "a.forb.cover", "h.leg.cover", "p.forb.cover", 
		# "bare.soil", "Rock.1", "shrub", "Longitude.1", "lon", "X..per.grass.cover", 
		# "disturbance", "clry_slope", "clry_aspect", "X..WARM.SEASON.PPT", 
		# "dcoast"...)

# > dput(names(phyto)[1:12])
	# c("series", "County", "series_area_ha", "lat", "lon", "phyto_pct", 
	# "rondels_per1000gsoil", "bilobates_per1000gsoil", "short_cells_total_per1000gsoil", 
	# "total_phytoliths_1000g", "bilobate_ratio", "elev")

 bio_codes <- c(
	 bio1 = "Annual Mean Temperature",
	 bio2 = "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
	 bio3 = "Isothermality (diurnal/annual range)",
	 bio4 = "Temperature Seasonality (standard deviation)",
	 bio5 = "Max Temperature of Warmest Month",
	 bio6 = "Min Temperature of Coldest Month",
	 bio7 = "Temperature Annual Range",
	 bio8 = "Mean Temperature of Wettest Quarter",
	 bio9 = "Mean Temperature of Driest Quarter",
	 bio10 = "Mean Temperature of Warmest Quarter",
	 bio11 = "Mean Temperature of Coldest Quarter",
	 bio12 = "Annual Precipitation",
	 bio13 = "Precipitation of Wettest Month",
	 bio14 = "Precipitation of Driest Month",
	 bio15 = "Precipitation Seasonality (Coefficient of Variation)",
	 bio16 = "Precipitation of Wettest Quarter",
	 bio17 = "Precipitation of Driest Quarter",
	 bio18 = "Precipitation of Warmest Quarter",
	 bio19 = "Precipitation of Coldest Quarter" 
	 )

	
	
######################################################

# Ad Hoc    (FIX THESE FURTHER BACK IN PIPELINE)

hillshade <- hillShade( terrain( predictors$alt, opt = 'slope'), terrain( predictors$alt, opt = 'aspect'), 40, 270) 

######################################################

# Figure 0 : locations of points
	x11(width = 8.677, height = 9.48)
	plot(hillshade, col=grey(0:100/100, alpha = .5), legend=FALSE)
	plot(predictors$alt, col=terrain.colors(25, alpha=0.35), add=TRUE, legend = FALSE)
  
#  map('county', 'california', col = rgb(.5,.5,.5,1), lwd = 1, add = TRUE)
  points(phyto$lon, phyto$lat, cex = .5, lwd = .5,pch = 16, col = rgb(0,0,0,.7))
  points(clary$lon, clary$lat, cex = 1, lwd = .5, pch = 4, col= rgb(0,0,1,.7))
  legend('topright', legend = c( "Phytolith Abundance Data (Evett 2013)",'Grass Cover Data (Clary 2012)'), col = c('black','blue'), pch = c(16, 4))


######################################################

  # Recreate figures from Clary (using variables derived from raster data
  
  # Fig 2. Perennial Grass cover as a function of distance from the coast
  
  plot(clary$p.grass.cover~clary$dcoastkm, xlab = 'Distance from Pacific coast (m)', ylab = 'Perennial Grass cover') 
  plot(clary$p.grass.cover~clary$clry_slope, xlab = 'Distance from Pacific coast (m)', ylab = 'Perennial Grass cover') 

  
  # Most of Evett's data follow the same pattern as clary, w.r.t distance from coast. 
  plot(phyto$bilobate_ratio ~ phyto$dcoast)
  plot(phyto$phyto_pct ~ phyto$dcoast)
  plot(phyto$rondels_per1000gsoil ~ phyto$dcoast)
  plot(phyto$bilobates_per1000gsoil ~ phyto$dcoast)
  plot(phyto$total_phytoliths_1000g ~ phyto$dcoast)
  plot(phyto$short_cells_total_per1000gsoil ~ phyto$dcoast)


  # Plot of Perennial grass cover, annual grass cover and phytolith content as a function of distance from the coast. 
  # Phytolith content and perennial grass cover show the same pattern.
  
  i <- which(phyto$dcoast <= max(clary$dcoast))
  
  par(mar = c(5,5,4,2))
  plot( clary$p.grass.cover~clary$dcoastkm, col = rgb(1,0,0,.4), pch = 4, lwd =3, ylim = c(0,1), xlab = 'Distance from Pacific Coast (km)',ylab = 'Percent')
  points( clary$a.grass.cover~clary$dcoastkm, col = rgb(0,1,0,.4), pch = 3, lwd =3,  xlab = 'distance from coast',ylab = 'Percent')
  points(phyto_pct ~ dcoastkm, data = phyto, ylab = '', col = rgb(0,0,1,.4), lwd = 3)


	m.grass <- lm(clary$p.grass.cover~clary$dcoastkm)
	abline( m.grass, col = 'red', lwd = 2)
	summary(m.grass)

	m.exotic <- lm(clary$a.grass.cover~clary$dcoastkm)
	abline( m.exotic, col = 'green', lwd = 2)
	
	m.phyto <- lm(phyto_pct~dcoastkm,data= phyto[i,])
	abline( m.phyto, col = 'blue', lwd = 2)
  
  legend('topright', legend = c('Perennial Grass Cover (Clary 2012)', 'Annual Grass Cover (Clary 2012)', 'Total Phytolith (Evett 2013)'), col = c('red','green', 'blue'), pch = c(4,3,1), lwd = 2, bg = 'white')


#####################################################

# Curiosity: cooling degree days is one of clary's variables that correlates very strongly with dcoast and seems to explain a lot of perennial grass cover variation

  plot( clary$p.grass.cover~clary$Cooling.Degree.Days..Ann.from.)

  plot( clary[, c('p.grass.cover', 'Cooling.Degree.Days..Ann.from.', 'alt', 'dcoast')])

# what bioclim variable most strongly correlates to cooling degree days?

  which.max(abs( unlist( lapply(clary[, paste0('bio', 1:19)], function(x) cor(x, clary$Cooling.Degree.Days..Ann.from.)))))

  cor.test(clary$bio10, clary$Cooling.Degree.Days..Ann.from.)
  cor.test(clary$dcoast, clary$Cooling.Degree.Days..Ann.from.)
  
  # Appears to be bio 10, 'mean temperature of warmest quarter'. This makes quite a bit of sense. Lets substitute bio 10 for dcoast  and remake the plot above...
  
  plot(phyto_pct ~ bio10, data = phyto,  col = 'blue', lwd = 3,ylim = c(0,1), xlab = 'Mean Summertime Temperature (.1 degree C)',ylab = 'Percent')

  points( clary$p.grass.cover~clary$bio10, col = 'red', pch = 4, lwd =3, )
   points( clary$a.grass.cover~clary$bio10, col = 'green', pch = 3, lwd =3,  xlab = 'distance from coast',ylab = 'Percent')
  
  abline( lm(clary$p.grass.cover~clary$bio10), col = 'red', lwd = 2)
  abline( lm(clary$a.grass.cover~clary$bio10), col = 'green', lwd = 2)
  abline( lm(phyto_pct~bio10,data= phyto[i,]), col = 'blue', lwd = 2)

  polygon( c( 230, 230, 265, 265), c(.19, .63, .63, .18), lty = 2, lwd =3, border = rgb(.5,.5,.5,.5))
  
  legend('topright', legend = c('Perennial Grass Cover (Clary 2012)', 'Annual Grass Cover (Clary 2012)', 'Total Phytolith (Evett 2013)'), col = c('red','green', 'blue'), pch = c(4,3,1), lwd = 2, bg = 'white')

  ############################
  # lets do the same thing, only with precip in the warmest quarter)
  #may-sept
  #? is this summer temperature though?

  
  phyto$summer_precip <- rowSums( extract( wc, phyto[, c('lon','lat')])[, 5:9])
  clary$summer_precip <- rowSums( extract( wc, clary[, c('lon','lat')])[, 5:9])
  
  plot(phyto_pct ~ summer_precip, data = phyto[i,],  col = 'blue', lwd = 3,ylim = c(0,1), xlab = 'Summer precip (May-June)',ylab = 'Percent')

  points( p.grass.cover ~ summer_precip, col = 'red', pch = 4, lwd =3,data = clary )
  points( clary$a.grass.cover~clary$summer_precip, col = 'green', pch = 3, lwd =3,  xlab = 'distance from coast',ylab = 'Percent')
  
  abline( lm(clary$p.grass.cover~clary$summer_precip), col = 'red', lwd = 2)
  abline( lm(clary$a.grass.cover~clary$summer_precip), col = 'green', lwd = 2)
  abline( lm(phyto_pct~bio18,data= phyto[i,]), col = 'blue', lwd = 2)
  
  
  cor.test(clary$p.grass.cover,clary$summer_precip)
  cor.test(clary$a.grass.cover,clary$summer_precip)
  cor.test(phyto$phyto_pct,phyto$summer_precip)
  
  legend('topright', legend = c('Perennial Grass Cover (Clary 2012)', 'Annual Grass Cover (Clary 2012)', 'Total Phytolith (Evett 2013)'), col = c('red','green', 'blue'), pch = c(4,3,1), lwd = 2, bg = 'white')
  
  
  ########################
  # Same thing,but for slope
  
  plot(phyto_pct ~ slope, data = phyto,  col = 'blue', lwd = 3,ylim = c(0,1), xlab = 'slope (degrees)',ylab = 'Percent', xlim = c(0, 7))
	
  points( p.grass.cover ~ slope, col = 'red', pch = 4, lwd =3,data = clary )
  points( a.grass.cover ~ slope, data= clary, col = 'green', pch = 3, lwd =3,  xlab = 'distance from coast',ylab = 'Percent')
  
  abline( lm(clary$p.grass.cover~clary$slope), col = 'red', lwd = 2)
  abline( lm(clary$a.grass.cover~clary$slope), col = 'green', lwd = 2)
  abline( lm(phyto_pct~slope,data= phyto), col = 'blue', lwd = 2)
  

  
  cor.test(clary$p.grass.cover,clary$slope)
  cor.test(clary$a.grass.cover,clary$slope)
  cor.test(phyto$phyto_pct,phyto$slope)
  
  
  ########################
  # Testing for correlations among soil variables and responses
  soilv <- c('Bulk Density' = 'bulk', 'Soil Carbon' ='carbon', 'CEC' =  'cec', 'pH' = 'ph','Coarse Debris' = 'coarse', 'Sand' = 'sand', 'Silt' = 'silt', 'Clay'= 'clay')

  topov <- c('Slope' = 'slope', 'Elevation' = 'alt', 'North-ness' = 'northness', 'East-ness' ='eastness')

  bio2 <- names(bio_codes)
  names(bio2) <- bio_codes
  bio2 <- c('May-Sept Precip' = 'summer_precip', bio2) 
  vv <- c(topov, soilv, bio2)
  
   p.grass <- lapply( vv, function(x) cor.test( clary$p.grass.cover, clary[,x]) )
   a.grass <- lapply( vv, function(x) cor.test( clary$a.grass.cover, clary[,x]) )
   phyto.cor <- lapply( vv, function(x) cor.test( phyto$phyto_pct,phyto[,x] ))
   phytoC.cor <- lapply( vv, function(x) cor.test( phyto$phyto_pct[i],phyto[i,x] ))

   get_cors <- function(x) {
		n <- sprintf("%2.2f", round(x$estimate,2))
		cuts <- c(0,.001, .01, .05, 1, 100)
		stars <- c( '***', '** ', '*  ', '   ')
		sig <- stars[cut( x$p.value, cuts) ] 
		paste0(n, sig)
		#ifelse(x$p.value <= .05, paste0(n, '*'), paste0(n))
	}
   cors <- rbind(sapply(p.grass, get_cors), sapply(phytoC.cor, get_cors), sapply(phyto.cor, get_cors), sapply(a.grass, get_cors) ) 

	colnames(cors) <- names(vv)
	row.names(cors) <- c('Perennial grass cover', 'Phytolith Content \n (Restricted Range)', 'Phytolith content \n (Full Range)', 'Annual grass cover')

	t(cors)
	print.xtable(xtable(t(cors)), type = 'html', file = 'C:/projects/phytolith/out/tab/variables_cor.html') 

	######################
	# Testing for correlations with topographic variables
	topov <- c('slope', 'elev', 'northness', 'eastness')
	
	
	
  # cor.test(clary$p.grass.cover,clary$clay)
  # cor.test(clary$a.grass.cover,clary$clay)
  # cor.test(phyto$phyto_pct,phyto$clay)
  
  
  ########################
  # Ok, so for summer temps between 16 and ~22 degrees, they appear to follow the same trend.
  # But whats up with the high phytolith values for some plots between 23 and and 26 degrees?
  
  # Attempting to map these out -- maybe cluster around some known feature?
  
  w <- which(phyto$phyto_pct > .21 & phyto$bio10 > 230 & phyto$bio10 < 260) 
  
  # plot(hillshade, col=grey(0:100/100, alpha = .5), legend=FALSE)
  # plot(predictors$alt, col=terrain.colors(25, alpha=0.35), add=TRUE)
  
  map('county', 'california', col = rgb(0,0,0,.1), lwd = 1, fill = TRUE)
  points(phyto$lon, phyto$lat, cex = 1, lwd = .5,pch = 16, col = rgb(0,0,0,.7))
  points(phyto$lon[w], phyto$lat[w], cex = 2, lwd = 3,pch = 1, col = 'red')
  
  
  # Yes! these all cluster around a couple of counties in the foothills: Stanislaus and Merced Counties. ? what could be going on there. May have to consult an expert.
  # just for kicks, I wonder what the soil properties are like in these spots?

  # carbon looks like nothing special...
  plot(phyto$phyto_pct ~ phyto$carbon)
  points(phyto$phyto_pct[w] ~ phyto$carbon[w], col = 'red', cex = 2, lwd =2)

  # silt ok...
  plot(phyto$phyto_pct ~ phyto$silt)
  points(phyto$phyto_pct[w] ~ phyto$silt[w], col = 'red', cex = 2, lwd =2)
  
  # same with clay...
  plot(phyto$phyto_pct ~ phyto$clay)
  points(phyto$phyto_pct[w] ~ phyto$clay[w], col = 'red', cex = 2, lwd =2)
  
  #sand ok
  plot(phyto$phyto_pct ~ phyto$sand)
  points(phyto$phyto_pct[w] ~ phyto$sand[w], col = 'red', cex = 2, lwd =2)

  #cec ok
  plot(phyto$phyto_pct ~ phyto$cec)
  points(phyto$phyto_pct[w] ~ phyto$cec[w], col = 'red', cex = 2, lwd =2)
  
  # not pH
  plot(phyto$phyto_pct ~ phyto$ph)
  points(phyto$phyto_pct[w] ~ phyto$ph[w], col = 'red', cex = 2, lwd =2)


  # HMMM these values all tend to have a very low coarse-ness... but so do a lot of others
  plot(phyto$phyto_pct ~ phyto$coarse)
  points(phyto$phyto_pct[w] ~ phyto$coarse[w], col = 'red', cex = 2, lwd =2)
  
  # HMMM also tend to have a high bulk density
  plot(phyto$phyto_pct ~ phyto$bulk)
  points(phyto$phyto_pct[w] ~ phyto$bulk[w], col = 'red', cex = 2, lwd =2)
  
  #not alt
  plot(phyto$phyto_pct ~ phyto$alt)
  points(phyto$phyto_pct[w] ~ phyto$alt[w], col = 'red', cex = 2, lwd =2)
  
  #very flat
  plot(phyto$phyto_pct ~ phyto$slope)
  points(phyto$phyto_pct[w] ~ phyto$slope[w], col = 'red', cex = 2, lwd =2)
  
  #ugh, bioclim?
  
  biofun <- function(i) {
	v <- paste0('bio',i)
	plot(phyto$phyto_pct ~ phyto[,v], main = v)
	points(phyto$phyto_pct[w] ~ phyto[w,v], col = 'red', cex = 2, lwd =2)
	readline('ready?')
  }
  
  for (n in 1:19) biofun(n)
  
  # doesn't seem to elucidate much.
  
  # how about a PCA of soil variables
  
  soilv <- c('bulk', 'carbon', 'cec', 'clay','coarse', 'ph', 'sand','silt')

  pc <- princomp(phyto[, soilv], cor = TRUE)
  biplot(pc)
  plot(pc$scores[,1:2])
  points(pc$scores[w,1:2], lwd = 5,col = 'red')
  plot(pc$scores[,1:2],cex = 1 + scale(phyto$phyto_pct), lwd = 1,col = 'red')
  
  
  # PCA for bioclim variables? probably not...
  pc <- princomp(phyto[, paste0('bio',1:19)], cor = TRUE)
  biplot(pc)
  plot(pc$scores[,1:2])
  points(pc$scores[w,1:2], col = 'red', lwd =5 )
  plot(pc$scores[,1:2],cex = 1 + scale(phyto$phyto_pct), lwd = 1,col = 'red')
  points(pc$scores[w,1:2],pch = 16,cex = 1 + scale(phyto$phyto_pct), lwd = 1,col = rgb(1,0,0,.3))
  
  # Maybe I should just map them...
  library(plotKML)
  sp <- phyto[w,]
  coordinates(sp) <- ~ lon + lat
  projection(sp) <- CRS("+init=epsg:4326") 
  plotKML(sp)

##################################################################
##################################################################

# Another Curiosity: How well are is the range of bioclimatic and soil characteristics
# sampled?
	
    sr <- sampleRandom(predictors, 3000, xy = TRUE)
	i <- which(sr[,'alt'] < 1200 & sr[,'bio4'] < 7500)
	pcr <- princomp( sr[i ,c( paste0('bio',c(1:19)), 'ph', 'bulk', 'sand','clay','silt','cec')], cor = TRUE)
	biplot(pcr, xlabs = rep('', length(i)), scale = .7)
	
	plot(pcr$scores[,1:2], cex = .25, col = rgb(0,0,0,.5), pch = 16)
	pr <- predict( pcr, phyto)
	points(pr, pch = 1, col = rgb(1,0,0,1),cex = 2 + scale(phyto$phyto_pct), lwd = 1)
	points(pr, pch = 16, col = rgb(1,0,0,.05),cex = 2 + scale(phyto$phyto_pct), lwd = 1)
	summary(pcr)
	
	
	j <- rbind(pcr$scores[,1:3], pr[,1:3])
	plot3d(j, col = c(rep('black', nrow(pcr$scores)), rep('red', nrow(pr))))
	
	
	map('state','california')
	points(sr[i,c('x','y')], cex = .5, pch = 16, col = rgb(0,0,0,.5))
	points(phyto[,c('lon','lat')], pch = 1, col = rgb(1,0,0,.8), lwd = 2)
	
	
	# the samples do a decent job sampling the climate/soil space of inland and coastal california
	
##################################################################
##################################################################



# OK species distribution model time!
# What do the correlations between environmental variables and grass dominance tell us about predicted distribution? 

# Modeling Methods: (explain these?)

	# 1. Random Forest
	# 2. CART
	# 3. GAM
	# 4. Lasso Regression

#########################################################

 # variables
 
 pvars <- c( #'slope',
			#'northness',
			#'eastness',
			'bulk', 
			'carbon',
			'ph',
			'silt',
			'clay',
			'cec', 
#			'sand',
			 'bio1', # = "Annual Mean Temperature",
			 'bio2', # "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
			 'bio3',  # "Isothermality (bio2/bio7) (  100)",
#			 'bio4', # "Temperature Seasonality (standard deviation  100)",
			 'bio5',  # "Max Temperature of Warmest Month",
#			 'bio6',  # "Min Temperature of Coldest Month",
			 'bio7' ,# "Temperature Annual Range (bio5-bio6)",
#			 bio8 = "Mean Temperature of Wettest Quarter",
#			 'bio9' , # "Mean Temperature of Driest Quarter",
#			 'bio10' , # "Mean Temperature of Warmest Quarter",
#			 bio11 = "Mean Temperature of Coldest Quarter",
			 'bio12', # = "Annual Precipitation",
#			 'bio13', # = "Precipitation of Wettest Month",
#			 'bio14', # = "Precipitation of Driest Month",
			 'bio15',#, # = "Precipitation Seasonality (Coefficient of Variation)",
#			 'bio16', # = "Precipitation of Wettest Quarter",
#			 'bio17' # = "Precipitation of Driest Quarter",
#			 'bio18', # = "Precipitation of Warmest Quarter",
#			 'bio19'  # = "Precipitation of Coldest Quarter" 
#			'summer_precip'
			'alt',
			'dcoast'
			)

f1.gam <- as.formula( 
		paste0(
			"I(log(phyto_pct+ .005)) ~ s(", 
				paste( pvars, collapse = ') + s('), ')'
			)
		)

f1 <- as.formula( 
		paste0(
			"I(log(phyto_pct+ .005)) ~ ", 
				paste( pvars, collapse = ' + ')
			)
		)

# for the clary dataset
f2 <- update(f1, p.grass.cover ~ .)

f2.gam <- update(f1.gam, p.grass.cover ~ .)

		
#########################################################

	# Generate Folds for cross-validation
	set.seed(77)
	k <- 5
	k.p <- kfold(1:nrow(phyto), k = k)
	k.c <- kfold(1:nrow(clary), k = k)

#########################################################

	#Moran MC functions
	
	moran <- function(resids, xy) {
		require(spdep)
		d <- 1/pointDistance(xy,xy,allpairs = TRUE, lonlat = TRUE)
		diag(d) <- 1
		mat <- mat2listw(d)  
		moran.mc(resids, mat, 999, return_boot = FALSE)
	}
	
	moran.phyto <- list()
	moran.clary <- list()
	pred.phyto <- list()
	pred.clary <- list()
#########################################################

# Random Forest

	rf <- randomForest( y = log(phyto[,'phyto_pct'] + .005), x = phyto[, pvars], ntree = 5000, mtry = 2)
	rf
	varImpPlot(rf)

	
	rfc <- randomForest( y = clary$p.grass.cover, x = clary[, pvars], ntree = 5000, mtry = 1)
	rfc
	varImpPlot(rfc)
	
	p.rf <- exp(predict(aggP, rf, progress = 'text'))
	p.rfc <- predict(aggP, rfc, progress = 'text')
	
	spplot(stack(p.rf, p.rfc))
	
	pred.phyto$rf <-  predict(rf)
	pred.clary$rf <- predict(rfc)
	
	
	
	moran.phyto$rf <- moran( log(phyto$phyto_pct + .005) - predict(rfc), phyto[, c('lon','lat')] )
	
	moran.clary$rf <- moran( clary$p.grass.cover - predict(rfc), clary[, c('lon','lat')] )
	



	
	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	out <- list()
	
	for (j in 1:k){
			train <- which(k.p != j)
			test <- which( k.p == j)
			rf <- randomForest( y = log(phyto[train,'phyto_pct'] + .005), x = phyto[train, pvars], ntree = 5000, mtry = 2)
			out[[j]] <- predict(rf, newdata=phyto[test,pvars])        
	}
	
	out <- unlist(out)
	rf.phyto.cv <- list ( p =  out[order(as.numeric(names(out)))], o = log(phyto[,'phyto_pct'] + .005))
	
	# perennial grass data
	
	out <- list()
	for (j in 1:k){
			train <- which(k.c != j)
			test <- which( k.c == j)
			rf <- randomForest( y = clary$p.grass.cover[train] , x = clary[train, pvars], ntree = 5000, mtry = 2)
			out[[j]] <- predict(rf, newdata=clary[test,pvars])        
	}
	out <- unlist(out)
	rf.clary.cv <- list ( p =  out[order(as.numeric(names(out)))], o = clary$p.grass.cover)
	
	
#########################################################

# GAM

	f1.gam <- I(log(phyto_pct + .005)) ~  s(bio1) + s(bio12) + s(bio14) + s(bio3) + s(northness) + s(eastness)+ s(slope) + s(dcoast) + s(ph) + s(bulk) 
	f2.gam <- update(f1, p.grass.cover ~ .)
	
	g <- gam(f1.gam, data = phyto,select = TRUE)
	g.c <- gam(f2.gam, data = clary, select = TRUE)

	
	p.g <- exp(predict(aggP, g))
	p.gc <- exp(predict(aggP, g.c))
	p.gc[p.gc[] > 1] <- 1
	spplot(stack(p.g, p.gc))

	
	plot(p.g)
	
	pred.phyto$gam <-  predict(g)
	pred.clary$gam <- predict(g.c)
	
	moran.phyto$gam <- moran( log(phyto$phyto_pct + .001) - predict(g), phyto[, c('lon','lat')] )
	moran.clary$gam <- moran( clary$p.grass.cover - predict(g.c) , clary[, c('lon','lat')] )
	
	
	
	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	out <- list()
	
	for (j in 1:k){
			train <- which(k.p != j)
			test <- which( k.p == j)
			gj <- gam( f1, data = phyto[train, ])
			out[[j]] <- predict(gj, newdata=phyto[test,])        
	}
	
	out <- unlist(out)
	gam.phyto.cv <- list ( p =  out[order(as.numeric(names(out)))], o = log(phyto[,'phyto_pct'] + .005))
	
	# perennial grass data
	
	out <- list()
	for (j in 1:k){
			train <- which(k.c != j)
			test <- which( k.c == j)
			gj <- gam( f2, data = clary[train, ])
			out[[j]] <- predict(gj, newdata=clary[test,]) 
	}
	out <- unlist(out)
	gam.clary.cv <- list ( p =  out[order(as.numeric(names(out)))], o = clary$p.grass.cover)

	
###########################################################

	# Function for Generating rasterized predictions using gnet models (lasso and ridge)

	predict.gnet <- function(r, m, ...){
		xn <- row.names(m$beta)
		out <- raster(r)
		out <- writeStart(out, rasterTmpFile(), overwrite=TRUE)
		bs <- blockSize(r)
		pb <- pbCreate(bs$n, ...)
		for (i in 1:bs$n) {
			v <- getValues(r, row = bs$row[i], nrows = bs$nrows[i])
			p.v <- predict(m , v[,xn])
			out <- writeValues(out, p.v, bs$row[i])
			pbStep(pb, i)
		}
		out <- writeStop(out)
		pbClose(pb)
		return(out)
	}	
	

	# lasso Regression

	lso = glmnet( as.matrix(phyto[, pvars]),log(phyto[,'phyto_pct'] + .005),alpha =1, lambda =  cv.glmnet (as.matrix(as.matrix(phyto[, pvars])),log(phyto[,'phyto_pct'] + .005),alpha =1)$lambda.min )

	
	lso.c = glmnet( as.matrix(clary[, pvars]),clary$p.grass.cover,alpha =1, lambda =  cv.glmnet (as.matrix(as.matrix(clary[, pvars])),clary$p.grass.cover,alpha =1)$lambda.min )
	
	
	
	p.lso <- exp(predict.gnet(aggP, lso))
	p.lsoc <- predict.gnet(aggP, lso.c)
	dotchart(lso$beta)
	
	plot(p.lso)
	plot(p.lsoc)

	spplot(stack(p.lso, p.lsoc))
	
	pred.phyto$lasso <- predict(lso, newx = as.matrix(phyto[,pvars]))
	pred.clary$lasso <- predict(lso.c, newx = as.matrix(clary[,pvars]))
	
	moran.phyto$lasso <- moran( log(phyto$phyto_pct + .005) - pred.phyto$lasso, phyto[, c('lon','lat')] )

	moran.clary$lasso <- moran( clary$p.grass.cover - pred.clary$lasso, clary[, c('lon','lat')] )
	
	
	
	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	out <- c()
	
	for (j in 1:k){
			train <- which(k.p != j)
			test <- which( k.p == j)
			lsoj = glmnet( as.matrix(phyto[train, pvars]),log(phyto[train,'phyto_pct'] + .005),alpha =1, lambda =  cv.glmnet (as.matrix(as.matrix(phyto[train, pvars])),log(phyto[train,'phyto_pct'] + .005),alpha =1)$lambda.min )
			out[test] <- as.vector( predict(lsoj, newx=as.matrix(phyto[test,pvars])) )       
	}
	
	
	lso.phyto.cv <- list ( p =  out, o = log(phyto[,'phyto_pct'] + .005))
	
	# perennial grass data
	
	out <- c()
	
	for (j in 1:k){
			train <- which(k.c != j)
			test <- which( k.c == j)
			lsoj = glmnet( as.matrix(clary[train, pvars]),clary[train,'p.grass.cover'],alpha =1, lambda =  cv.glmnet (as.matrix(as.matrix(phyto[train, pvars])),clary[train,'p.grass.cover'],alpha =1)$lambda.min )
			out[test] <- as.vector( predict(lsoj, newx=as.matrix(clary[test,pvars])) )       
	}
	
	lso.clary.cv <- list ( p =  out, o = clary$p.grass.cover)
		
######################################################################
######################################################################

	#  CART
	
	# f1 <- I(log(phyto_pct+ .001)) ~  bio1 + bio12 + northness + slope + dcoast + veg
	# f2 <- update(f1, p.grass.cover ~ .)
	
	
	ct = rpart(f1, data = phyto)
	ctc = rpart(f2, data = clary)

	plot(ctc);text(ctc)
	plot(ct); text(ct)

	p.ct <- exp(predict(aggP, ct))
	p.ctc <- predict(aggP, ctc)

	plot(p.ct)
	plot(p.ctc)

	
	pred.phyto$cart <-  predict(ct)
	pred.clary$cart <- predict(ctc)
	
	moran.phyto$cart <- moran( log(phyto$phyto_pct + .005) - predict(ct), phyto[, c('lon','lat')] )
	moran.clary$cart <- moran( clary$p.grass.cover - predict(ctc) , clary[, c('lon','lat')] )
	
	
	
	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	out <- list()
	
	for (j in 1:k){
			train <- which(k.p != j)
			test <- which( k.p == j)
			rpj <- rpart( f1, data = phyto[train, ])
			out[[j]] <- predict(rpj, newdata=phyto[test,])        
	}
	
	out <- unlist(out)
	cart.phyto.cv <- list ( p =  out[order(as.numeric(names(out)))], o = log(phyto[,'phyto_pct'] + .005))
	
	# perennial grass data
	
	out <- list()
	for (j in 1:k){
			train <- which(k.c != j)
			test <- which( k.c == j)
			rpj <- rpart( f2, data = clary[train, ])
			out[[j]] <- predict(rpj, newdata=clary[test,]) 
	}
	out <- unlist(out)
	cart.clary.cv <- list ( p =  out[order(as.numeric(names(out)))], o = clary$p.grass.cover)



#########################################################################
#########################################################################
#GLM



#########################################################################
#########################################################################

# Cross Validation Results (!)

cv.phy <- list ( 'Random Forest' = rf.phyto.cv$p,
				 'CART' = cart.phyto.cv$p,
				 'GAM' = gam.phyto.cv$p,
				 'Lasso' = lso.phyto.cv$p
				)

cv.clary <- list('Random Forest' = rf.clary.cv$p,
				 'CART' = cart.clary.cv$p,
				 'GAM' = gam.clary.cv$p,
				 'Lasso' = lso.clary.cv$p
				)
				
				
# phyto

cv.phy <- lapply(cv.phy, function(x) exp(x) + .001)
obs <- exp(rf.phyto.cv$o) + .001

(stats <- t(cv_phy <- rbind(	
		'cor' = sapply(cv.phy, function(x) cor(x, obs)),
		'rmse' = sapply(cv.phy, function(x) sqrt(mean( (x - obs)^2))),
		'MAE' = sapply(cv.phy, function(x) mean( abs(x - obs))),
		'ME' = sapply(cv.phy, function(x) mean( x - obs)),
		'I' = sapply(moran.phyto, function(x)  round(x$statistic,4))
	)))

	
print.xtable( xtable(stats, digits = c(1,2,2,2,2,4)), type = 'html', file = 'C:/projects/phytolith/out/tab/validation_stats.html')
	
	
# are they correlated?
cor(do.call(cbind,cv.phy))

# residual correlations
resid_cors <- cor(do.call( cbind, lapply(pred.phyto, function(x) x - log(phyto$phyto_pct + .005)) ))

row.names(resid_cors) <- colnames(resid_cors) <- names(pred.phyto)
print.xtable( xtable( resid_cors, digits = 2), type = 'html', file = 'C:/projects/phytolith/out/tab/residual_correlations.html')





ROW <-3  # cor
ens <- p.lso*cv_phy[ROW,4]  +  p.ct* cv_phy[ROW,2] + p.g * cv_phy[ROW,3] + p.rf*cv_phy[ROW,1]
ens <- ens / sum(cv_phy[ROW,])

plot(ens)
ens[ens[] > 1] <- 1
plot(ens)
points(phyto[, c('lon','lat')])