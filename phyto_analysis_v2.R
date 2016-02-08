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
	 bio3 = "Isothermality (bio2/bio7) (  100)",
	 bio4 = "Temperature Seasonality (standard deviation  100)",
	 bio5 = "Max Temperature of Warmest Month",
	 bio6 = "Min Temperature of Coldest Month",
	 bio7 = "Temperature Annual Range (bio5-bio6)",
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

  # Recreate figures from Clary (using variables derived from raster data
  
  # Fig 2. Perennial Grass cover as a function of distance from the coast
  
  plot(clary$p.grass.cover~clary$dcoast, xlab = 'Distance from Pacific coast (m)', ylab = 'Perennial Grass cover') 
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

  plot( clary$p.grass.cover~clary$dcoast, col = 'red', pch = 4, lwd =3, ylim = c(0,1), xlab = 'Distance from Pacific Coast (m)',ylab = 'Percent')
  points( clary$a.grass.cover~clary$dcoast, col = 'green', pch = 3, lwd =3,  xlab = 'distance from coast',ylab = 'Percent')
  
  
  points(phyto_pct ~ dcoast, data = phyto, ylab = '', col = 'blue', lwd = 3)
  abline( lm(clary$p.grass.cover~clary$dcoast), col = 'red', lwd = 2)
  abline( lm(clary$a.grass.cover~clary$dcoast), col = 'green', lwd = 2)
  abline( lm(phyto_pct~dcoast,data= phyto[i,]), col = 'blue', lwd = 2)
  
  legend('topright', legend = c('Perennial Grass Cover (Clary 2012)', 'Annual Grass Cover (Clary 2012)', 'Total Phytolith (Evett 2013)'), col = c('red','green', 'blue'), pch = c(4,3,1), lwd = 2)


#####################################################

# Curiosity: cooling degree days is one of clary's variables that correlates very strongly with dcoast and seems to explain a lot of perennial grass cover variation

  plot( clary$p.grass.cover~clary$Cooling.Degree.Days..Ann.from.)

  plot( clary[, c('p.grass.cover', 'Cooling.Degree.Days..Ann.from.', 'alt', 'dcoast')])

# what bioclim variable most strongly correlates to cooling degree days?

  which.max(abs( unlist( lapply(clary[, paste0('bio', 1:19)], function(x) cor(x, clary$Cooling.Degree.Days..Ann.from.)))))

  # Appears to be bio 10, 'mean temperature of warmest quarter'. This makes quite a bit of sense. Lets substitute bio 10 for dcoast  and remake the plot above...
  
  plot(phyto_pct ~ bio10, data = phyto,  col = 'blue', lwd = 3,ylim = c(0,1), xlab = 'Mean Summertime Temperature (.1 degree C)',ylab = 'Percent')

  points( clary$p.grass.cover~clary$bio10, col = 'red', pch = 4, lwd =3, )
   points( clary$a.grass.cover~clary$bio10, col = 'green', pch = 3, lwd =3,  xlab = 'distance from coast',ylab = 'Percent')
  
  abline( lm(clary$p.grass.cover~clary$bio10), col = 'red', lwd = 2)
  abline( lm(clary$a.grass.cover~clary$bio10), col = 'green', lwd = 2)
  abline( lm(phyto_pct~bio10,data= phyto), col = 'blue', lwd = 2)
  
  legend('topright', legend = c('Perennial Grass Cover (Clary 2012)', 'Annual Grass Cover (Clary 2012)', 'Total Phytolith (Evett 2013)'), col = c('red','green', 'blue'), pch = c(4,3,1), lwd = 2)

  ############################
  # lets do the same thing, only with precip in the warmest quarter)
  
  
  plot(phyto_pct ~ I(bio16/bio12), data = phyto,  col = 'blue', lwd = 3,ylim = c(0,1), xlab = 'total precip',ylab = 'Percent')

  points( p.grass.cover ~ I(bio19/bio12), col = 'red', pch = 4, lwd =3,data = clary )
  points( clary$a.grass.cover~clary$bio12, col = 'green', pch = 3, lwd =3,  xlab = 'distance from coast',ylab = 'Percent')
  
  abline( lm(clary$p.grass.cover~clary$bio12), col = 'red', lwd = 2)
  abline( lm(clary$a.grass.cover~clary$bio12), col = 'green', lwd = 2)
  abline( lm(phyto_pct~bio12,data= phyto), col = 'blue', lwd = 2)
  
  legend('topright', legend = c('Perennial Grass Cover (Clary 2012)', 'Annual Grass Cover (Clary 2012)', 'Total Phytolith (Evett 2013)'), col = c('red','green', 'blue'), pch = c(4,3,1), lwd = 2)
  
  
  ########################
  # Ok, so for summer temps between 16 and ~22 degrees, they appear to follow the same trend.
  # But whats up with the high phytolith values for some plots between 23 and and 26 degrees?
  
  # Attempting to map these out -- maybe cluster around some known feature?
  
  w <- which(phyto$phyto_pct > .2 & phyto$bio10 > 230 & phyto$bio10 < 260) 
  map('county', 'california')
  points(phyto$lon[w], phyto$lat[w], cex = 2, lwd = 2,pch = 4, col = 'red')
  points(phyto$lon, phyto$lat, cex = 1, lwd = 1,pch = 1)
  
  
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

  pc <- princomp(phyto[, soilv])
  biplot(pc)
  plot(pc$scores[,1:2])
  points(pc$scores[w,1:2], lwd = 5,col = 'red')
  
  
  # PCA for bioclim variables? probably not...
  pc <- princomp(phyto[, paste0('bio',1:19)])
  plot(pc$scores[,1:2])
  points(pc$scores[w,1:2], col = 'red', lwd =5 )
  
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
	
	plot(pcr$scores[,1:2], cex = .5, col = rgb(0,0,0,.5), pch = 16)
	pr <- predict( pcr, phyto)
	points(pr, pch = 1, col = rgb(1,0,0,.5), lwd = 2)
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
 
 pvars <- c( 'alt', 'slope', 'northness', 'dcoast',
			'eastness', 'bulk', 'ph', 'silt', 'clay', 'cec', 'sand', 
			 'bio1', # = "Annual Mean Temperature",
#			 'bio2', # "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
#			 'bio3'  # "Isothermality (bio2/bio7) (  100)",
#			 'bio4', # "Temperature Seasonality (standard deviation  100)",
#			 'bio5',  # "Max Temperature of Warmest Month",
#			 'bio6',  # "Min Temperature of Coldest Month",
			 'bio7' ,# "Temperature Annual Range (bio5-bio6)",
#			 bio8 = "Mean Temperature of Wettest Quarter",
#			 'bio9' , # "Mean Temperature of Driest Quarter",
#			 'bio10' , # "Mean Temperature of Warmest Quarter",
#			 bio11 = "Mean Temperature of Coldest Quarter",
			 'bio12' # = "Annual Precipitation",
#			 'bio13', # = "Precipitation of Wettest Month",
#			 'bio14', # = "Precipitation of Driest Month",
#			 'bio15', # = "Precipitation Seasonality (Coefficient of Variation)",
#			 'bio16', # = "Precipitation of Wettest Quarter",
#			 'bio17' # = "Precipitation of Driest Quarter",
#			 'bio18', # = "Precipitation of Warmest Quarter",
#			 'bio19'  # = "Precipitation of Coldest Quarter" 
			) 
#########################################################

	# Generate Folds for cross-validation
	set.seed(77)
	k <- 5
	k.p <- kfold(1:nrow(phyto), k = k)
	k.c <- kfold(1:nrow(clary), k = k)


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

	f1 <- I(log(phyto_pct+ .001)) ~  s(bio1) + s(bio12) + s(bio14) + s(bio3) + s(northness) + s(eastness)+ s(slope) + s(dcoast) + s(ph) + s(bulk) 
	f2 <- update(f1, p.grass.cover ~ .)
	
	g <- gam(f1, data = phyto, select = TRUE)
	g.c <- gam(f2, data = clary, family = binomial(link = 'logit'))

	
	p.g <- exp(predict(aggP, g))
	p.gc <- predict(aggP, g.c)
	spplot(stack(p.g, p.gc))

	
	plot(p.g)

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

# Generate rasterized predictions using gnet models (lasso and ridge)

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
	
	f1 <- I(log(phyto_pct+ .001)) ~  bio1 + bio12 + northness + slope + dcoast + veg
	f2 <- update(f1, p.grass.cover ~ .)
	
	
	ct = rpart(f1, data = phyto)
	ctc = rpart(f2, data = clary)

	plot(ctc);text(ctc)
	plot(ct); text(ct)

	p.ct <- exp(predict(aggP, ct))
	p.ctc <- predict(aggP, ctc)

	plot(p.ct)
	plot(p.ctc)

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

(cv_phy <- rbind(	
		'cor' = sapply(cv.phy, function(x) cor(x, obs)),
		'rmse' = sapply(cv.phy, function(x) sqrt(mean( (x - obs)^2))),
		'MAE' = sapply(cv.phy, function(x) mean( abs(x - obs))),
		'ME' = sapply(cv.phy, function(x) mean( x - obs))
	))

# are they correlated?
cor(do.call(cbind,cv.phy))

ROW <-1  # cor
ens <- p.lso*cv_phy[ROW,4]  +  p.ct* cv_phy[ROW,2] + p.g * cv_phy[ROW,3] + p.rf*cv_phy[ROW,1]
ens <- ens / sum(cv_phy[ROW,])
