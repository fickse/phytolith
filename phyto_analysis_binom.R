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
  library(rasterVis)
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
  require(ggplot2)
  library(ggdendro)
  library(FNN)
  library(grid)
  library(e1071)
  library(ROCR)
	

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
	
	
	
	  phyto$summer_precip <- rowSums( extract( wc, phyto[, c('lon','lat')])[, 5:9])
	  clary$summer_precip <- rowSums( extract( wc, clary[, c('lon','lat')])[, 5:9])

	
# # # # # # # # # # # # # # # # # # # # # # 	
	
	
	
	the_y <- "bilobate_star"
	log_adj <- 1
	maxv <- 1000
	
	
	# phyto$bilobate <- phyto$bilobates_per1000gsoil > 
	# phyto$bilobate <- phyto$bilobate_ratio >= .1
	# the_y <- 'bilobate'
	
	phyto[,the_y] <- as.factor(phyto[,the_y])
	
	
# # # # # # # # # # # # # # # # # # # # # # 
	
	#scaled data
	j <- which( names(phyto) %in% c('series','County', 'phyto_pct', 'bilobate_star', the_y))
	phyto.scaled <- cbind( scale(phyto[,-j]), phyto[,j])
	
	dir.create( paste0( 'C:/projects/phytolith/out/', the_y), F)
	dir.create( paste0( 'C:/projects/phytolith/out/', the_y,'/tab'), F)
	dir.create( paste0( 'C:/projects/phytolith/out/', the_y,'/fig'), F)
	
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
	 bio2 = "Mean Diurnal Range \n(Mean of monthly (max temp - min temp))",
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
	 bio15 = "Precipitation Seasonality \n(Coefficient of Variation)",
	 bio16 = "Precipitation of Wettest Quarter",
	 bio17 = "Precipitation of Driest Quarter",
	 bio18 = "Precipitation of Warmest Quarter",
	 bio19 = "Precipitation of Coldest Quarter" 
	 )

	
	
	spec <- colorRampPalette( c("#256EC2","#3D9ABE","#42C695","#5DD64B","#AEE34E","#DDEC53","#F8EE49","#F6D53A","#F5B52D","#EE8913","#E16310","#CB4707","#B61904","#9B0000") )

######################################################

######################################################

  # Recreate figures from Clary (using variables derived from raster data
  
  # Fig 2. Perennial Grass cover as a function of distance from the coast
  
  plot(clary$p.grass.cover~clary$dcoastkm, xlab = 'Distance from Pacific coast (m)', ylab = 'Perennial Grass cover') 
  plot(clary$p.grass.cover~clary$clry_slope, xlab = 'Distance from Pacific coast (m)', ylab = 'Perennial Grass cover') 

  
  # Most of Evett's data follow the same pattern as clary, w.r.t distance from coast. 
  plot(phyto[,the_y] ~ phyto$dcoast)


  # Plot of Perennial grass cover, annual grass cover and phytolith content as a function of distance from the coast. 
  # Phytolith content and perennial grass cover show the same pattern.
  
  i <- which(phyto$dcoast <= max(clary$dcoast))


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
 
 pvars <- c(
			'slope',
			# 'northness',
			# 'eastness',
			'bulk', 
			'carbon',
			'ph',
			'silt',
			'clay',
			'cec', 
			'sand',
			'coarse',
			'wetland',
			 'bio1', # = "Annual Mean Temperature",
			 'bio2', # "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
			  'bio3',  # "Isothermality (bio2/bio7) (  100)",
			 'bio4', # "Temperature Seasonality (standard deviation  100)",
			 'bio5',  # "Max Temperature of Warmest Month",
			 'bio6',  # "Min Temperature of Coldest Month",
			  'bio7' ,# "Temperature Annual Range (bio5-bio6)",
			 'bio8', # = "Mean Temperature of Wettest Quarter",
			  'bio9' , # "Mean Temperature of Driest Quarter",
			 'bio10' , # "Mean Temperature of Warmest Quarter",
			 'bio11', #= "Mean Temperature of Coldest Quarter",
			 'bio12', # = "Annual Precipitation",
			 'bio13', # = "Precipitation of Wettest Month",
			  'bio14', # = "Precipitation of Driest Month",
			  'bio15',#, # = "Precipitation Seasonality (Coefficient of Variation)",
			 'bio16', # = "Precipitation of Wettest Quarter",
			  'bio17', # = "Precipitation of Driest Quarter",
			 'bio18', # = "Precipitation of Warmest Quarter",
			 'bio19',  # = "Precipitation of Coldest Quarter" 
			'summer_precip',
			# 'alt',
			'dcoast'
			)

f1.gam <- as.formula( 
		paste0(
			 the_y, " ~ s(", 
				paste( pvars, collapse = ') + s('), ')'
			)
		)

f1 <- as.formula( 
		paste0(
			the_y," ~ ", 
				paste( pvars, collapse = ' + ')
			)
		)

		
#########################################################

	# Generate Folds for cross-validation
	set.seed(77)
	k <- 5
	k.p <- kfold(1:nrow(phyto), k = k)

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
	pred.phyto <- list()
	auc <- list()

#########################################################

# Random Forest

	rf <- randomForest( y = phyto[,the_y], x = phyto[, pvars], ntree = 10000, mtry = 2)
	rf
	varImpPlot(rf)

	
	p.rf <- 1- predict(aggP, rf, progress = 'text', type = 'prob')
	
	pred.phyto$rf <-  p.rf
	
	plot(p.rf)
	
	

	
	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	out <- list()
	
	for (j in 1:k){
			train <- which(k.p != j)
			test <- which( k.p == j)
			rfj <- randomForest( y = phyto[train,the_y], x = phyto[train, pvars], ntree = 5000, mtry = 2)
			out[[j]] <- predict(rfj, newdata=phyto[test,pvars], type = 'prob')        
	}
	out <- lapply(out, function(x) x[,2])

	out <- unlist(out)
	out <- out[order(as.numeric(names(out)))]
	
	pred <- prediction(out, phyto[,the_y])
	AUC <- performance(pred, 'auc')@y.values[[1]]

	auc$rf <- out
	
#########################################################

# GAM

	f1.gam <- as.formula(paste0(the_y," ~  s(dcoast) + s(bio1) + s(bio7) + s(bio12) + s(bio3)  + s(ph) + s(bulk) + s(wetland) "))
	
	g <- gam(f1.gam, data = phyto,select = TRUE, family = binomial, method = 'REML')

	p.g <- predict(aggP, g, type = 'response')
	plot(p.g)

	pred.phyto$gam <-  predict(g, type = 'response')
	
	# moran.phyto$gam <- moran( log(phyto[,the_y] + log_adj )- predict(g), phyto[, c('lon','lat')] )
	
	
	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	out <- list()
	
	for (j in 1:k){
			train <- which(k.p != j)
			test <- which( k.p == j)
			gj <- gam( f1.gam, data = phyto[train, ],select = TRUE, family = binomial, method = 'REML')
			out[[j]] <- predict(gj, newdata=phyto[test,], type = 'response')        
	}
	
	out <- unlist(out)
	out <- out[order(as.numeric(names(out)))]
	auc$gam <- out
	
	
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
			p.v <- predict(m , v[,xn], type = 'response')
			out <- writeValues(out, p.v, bs$row[i])
			pbStep(pb, i)
		}
		out <- writeStop(out)
		pbClose(pb)
		return(out)
	}	
	

	# lasso Regression
	xmat <- as.matrix(phyto[,pvars])
	ymat <- as.vector(phyto[,the_y])

	lso = glmnet( xmat,ymat ,alpha =1,family = 'binomial', 
		lambda =  cv.glmnet (xmat,ymat,alpha =1, family = 'binomial')$lambda.min 
	)

	
	
	p.lso <- predict.gnet(aggP, lso)
		
	plot(p.lso)
	
	pred.phyto$lasso <- predict(lso, newx = as.matrix(phyto[,pvars]))
	
	# moran.phyto$lasso <- moran( log(phyto[,the_y] + log_adj ) - pred.phyto$lasso, phyto[, c('lon','lat')] )

	
	
	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	outx <- c()
	
	for (j in 1:k){
			train <- which(k.p != j)
			test <- which( k.p == j)
			lsoj = glmnet( xmat[train,],ymat[train], alpha =1, lambda =  cv.glmnet(xmat[train,],ymat[train],alpha =1, family = 'binomial')$lambda.min, family = 'binomial' )
			outx[test] <- as.vector( predict(lsoj, newx=as.matrix(phyto[test,pvars]), type = 'response') )       
	}
	
	
	auc$lso <- outx
	
######################################################################
######################################################################
	# SVM
	

	# t.out <- tune(svm, f1 , data = phyto, ranges=list(cost=10^(-1:2), gamma=c(.1,.3,.5,1,2)) )
	# # # sv <- svm( f1, data = phyto, cost = t.out$best.parameters$cost, gamma = t.out$best.parameter$gamma)
	# sv <- svm(  phyto[,pvars], phyto[,the_y], probability = TRUE)
	# j <- predict(sv, phyto, probability = TRUE)
	
	# p.sv <- predict(aggP, sv, probability = TRUE)
	# plot(p.sv)

	# ctr <- trainControl(method = 'repeatedcv', repeats = 5)
	# svm.tune <- train( f1, data = phyto, method = 'svmRadial', tunelength = 9, preProc = c('center', 'scale'), trControl = ctr)
	# p.sv <- predict(aggP, svm.tune$finalModel)
	
	
######################################################################
######################################################################

	#  CART
	
	# f1 <- I(log(phyto_pct+ .001)) ~  bio1 + bio12 + northness + slope + dcoast + veg
	# f2 <- update(f1, p.grass.cover ~ .)
	
	
	# ct = rpart(f1, data = phyto)
	
	# plot(ct); text(ct)

	# p.ct <- 1-predict(aggP, ct)

	# plot(p.ct)

	
	# pred.phyto$cart <-  predict(ct)
	
	
	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	# out <- list()
	
	# for (j in 1:k){
			# train <- which(k.p != j)
			# test <- which( k.p == j)
			# rpj <- rpart( f1, data = phyto[train, ])
			# out[[j]] <- predict(rpj, newdata=phyto[test,])        
	# }
	# out <- lapply(out, function(x) x[,2])
	# out <- unlist(out)
	# auc$cart <- out
	

#########################################################################
#########################################################################

#OLS

	# ols = stepAIC(glm(f1, data = phyto, family = binomial))
	
	# p.ols <- predict(aggP, ols, type = 'response')
	
	# pred.phyto$ols <-  predict(ols)
	# moran.phyto$ols <- moran( log(phyto[,the_y] + log_adj ) - predict(ols), phyto[, c('lon','lat')] )

	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	# out <- list()
	
	# for (j in 1:k){
			# train <- which(k.p != j)
			# test <- which( k.p == j)
			# rpj <- stepAIC(glm( f1, data = phyto[train, ], family = binomial))
			# out[[j]] <- predict(rpj, newdata=phyto[test,], type = 'response')        
	# }
	
	# out <- unlist(out)
	# auc$ols <- out[order(as.numeric(names(out)))]
	

#########################################################################
#########################################################################

# Cross Validation Results (!)

cv.auc <-lapply(auc, function(x) {
			performance(prediction( x,phyto[,the_y]), 'auc')@y.values[[1]]
		})
		
cv.auc


# ens <- p.lso*cv.auc$lso  +  p.ct* cv.auc$cart + p.g * cv.auc$gam + p.rf*cv.auc$rf+ p.ols*cv.auc$ols
ens <- p.lso*cv.auc$lso  + p.g * cv.auc$gam + p.rf*cv.auc$rf
ens <- ens / sum(unlist(cv.auc))

plot(ens)
points(phyto[, c('lon','lat')], pch = as.numeric(phyto$bilobate))

p.ens <- extract(ens, phyto[, c('lon','lat')])
performance(prediction( p.ens, phyto[,the_y]), 'auc')@y.values[[1]]

##############################################################################################
##############################################################################################
	
	# Fancy
	p.rff <- predict(predictors, rf, progress = 'text', type = 'prob')
#	p.ctf <- predict(predictors, ct, progress = 'text')
	p.gf <- predict(predictors,g, progress = 'text', type = 'response')
	p.lsof <- predict.gnet(predictors, lso)
#	p.olsf <- exp(predict(predictors, ols, progress = 'text'))
	
	
	ens.f <- 
			p.rff * cv.auc$rf +
			# p.ctf* abs(cv_phy[ROW,2])  + 
			p.gf * cv.auc$gam + 		
			p.lsof*cv.auc$lso #  +  
			# p.olsf * abs(cv_phy[ROW,5])*0
			
			
	ens.f <- ens.f / sum(unlist(cv.auc))

	points(phyto[, c('lon', 'lat')], pch = c(1,3)[phyto[,the_y]])
	
	
	
	# Now to Find PC distances
    sr <- sampleRandom(predictors, 3000, xy = TRUE)
	kp <- pvars
	pca <-  princomp( sr[ ,kp], cor = TRUE)
	pr <- predict(pca, phyto[,kp])
	pa.hi <- predict(pca, predictors[])
	pa.ok.hi <- which(!apply(pa.hi,1, function(x) any(is.na(x))))
	dis.hi <- knnx.dist( pr[,1:4],pa.hi[pa.ok.hi,1:4],3)
	dis.hi <- rowMeans(dis.hi)
	unc.hi <- predictors[[1]]
	unc.hi[] <- NA
	unc.hi[pa.ok.hi] <- dis.hi
	
	
	# Apply this surface to predicted distribution
	inside <- unc.hi < 3
	outside <- unc.hi >= 3
	
	ens.fin <- inside*ens.f
	

	
	f <- paste0('C:/projects/phytolith/out/', the_y,'final.tif')
	writeRaster( stack(ens.f, inside), file = f) 
	
	
	png( width = 7.281250 , height = 8.427083, units = 'in', res = 250, file = paste0('C:/projects/phytolith/out/',the_y,'/fig/ensemble.png'))
		plot(ens.f , col = rev(grey(0:100/100)))
		plot(inside, col = rainbow(1), alpha = .2, add = TRUE, legend = F)
	dev.off()

	
	
	
##############################################################################################s
##############################################################################################s

preds <- list( p.rff, p.ctf, p.gf,p.olsf, p.lsof, ens.f)
preds <- stack( lapply(preds, function(x) { x[x> maxv] <- maxv;x}))

names(preds) <- c('Random Forest', 'CART', 'GAM', 'OLS', 'Lasso', 'Ensemble')
myTheme=rasterTheme(region=spec(100))

png(width = 12.989583, height = 9.479167, units = 'in', res = 250, file = paste0('C:/projects/phytolith/out/',the_y,'/fig/models.png'))
levelplot(preds, contour = FALSE,par.settings=myTheme)
dev.off()

##############################################################################################s
##############################################################################################s

	white_red <- colorRampPalette(c(rgb(1,0,0,.1), rgb(0,0,1,.5)), alpha = TRUE)
	pal <- colorRampPalette( c( rgb(0,0,0,.1), rgb(0,0,0,.5)), alpha = TRUE)
	pal <- colorRampPalette( c( gray(.5), gray(1)))
	pal <- function(x) { rainbow(x, alpha = .3) }
	pal2 <- function(x) { rainbow(x, alpha = .7) }

	
	# Remove aspect -- too weird
	kp <- pvars[-c(2:3)]
	# kp <- pvars
	pca <-  princomp( sr[ ,kp], cor = TRUE)
	
	#Mkay how much variance do the PCs explain. Choose the # dimensions that make up 80$
	summary(pca)
	# 3 dimensions
	
	loads <- with(pca, unclass(loadings))
	
	interesting <- c( 'Annual Temp' = 'bio1', "Precip Seasonality" = 'bio15', 'Elevation' = 'alt',  'Annual Precipitation' = 'bio12', "Dry Season Precip." = 'bio17', 'pH'  = 'ph', 'CEC' = 'cec', "Sand" = 'sand')

	
	
	# find euclidean distance between each grid cell and nearest neighboring  cell in PC 1-3 space
	
	pa <- predict(pca, aggP[])
	
	pr <- predict(pca, phyto[,kp])
	pa.ok <- which(!apply(pa,1, function(x) any(is.na(x))))
	dis <- knnx.dist( pr[,1:4],pa[pa.ok,1:4],1)
	dis <- rowMeans(dis)
	
	
	pa.hi <- predict(pca, predictors[])
	pa.ok.hi <- which(!apply(pa.hi,1, function(x) any(is.na(x))))
	dis.hi <- knnx.dist( pr[,1:4],pa.hi[pa.ok.hi,1:4],3)
	dis.hi <- rowMeans(dis.hi)
	
	
	# Map this to 2d space. 

	unc <- aggP[[1]]
	unc[] <- NA
	unc[pa.ok] <- dis

	unc.hi <- predictors[[1]]
	unc.hi[] <- NA
	unc.hi[pa.ok.hi] <- dis.hi
	
	brks <- c(seq(0, 12, length.out = 13))
	
	
	png(file = paste0('C:/projects/phytolith/out/',the_y,'/fig/pca.png'), width = 13, height = 8.5, unit = 'in', res = 200)
	par(mfrow = c(1,2))
	plot(pa[pa.ok,1:2], cex = .1 + dis, pch = 16, col = pal(12)[cut(dis,brks, right = FALSE)], xlab = 'PCA 1', ylab = 'PCA 2')
	
	# plot(pa.hi[pa.ok.hi,1:2], cex = .1 + dis.hi, pch = 16, col = rev(rainbow(5, alpha = .5))[cut(dis.hi,brks, right = FALSE)], xlab = 'PCA 1', ylab = 'PCA 2')

	
	sapply(interesting, function(x) {
		i <- which(x == row.names(loads))
#		text( loads[i,1]*pca$scale[i]**.85, loads[i,2]* pca$scale[i]**.85, x)
		text( loads[i,1]*20, loads[i,2]*20, names(interesting)[match(x,interesting)])
	})
	
	
	
#	points(pr[,1:2], cex = .1 + phyto$phyto_pct, pch = 3, lwd = 1.5, col = rgb(0,0,0,1))
	points(pr[,1:2], cex =  1 + 2*phyto$phyto_pct/max(phyto$phyto_pct), pch = 1, col = rgb(0,0,0,1))
		#library(rgl)
	#plot3d(pa[pa.ok,], col = terrain.colors(5)[cut(dis,5)], cex = 2)

	
	plot(unc.hi , col = pal2(12), breaks = brks)
	points(phyto[,c('lon','lat')], cex = 2, pch =3, lwd = 1.5, col = rgb(0,0,0,.3))
	points(phyto[,c('lon','lat')], cex = .5, pch =1, lwd = 1.5, col = rgb(0,0,0,.2))
	
	dev.off()

	levelplot(unc, contour = TRUE)
	unc.hi <- unc
	
	# Apply this surface to predicted distribution
	plot(unc)
	out <- unc > 2
	out2 <- 1/unc * out
	int <- unc <= 2
	int [ int[] < 1 ] <- NA 

	plot(ens ,col = rev(terrain.colors(10, alpha = .4)), legend = F)
	plot(ens * int,add = TRUE)
	
##############################################################################################s
##############################################################################################s
 soilv <- c('Bulk Density' = 'bulk', 'Soil Carbon' ='carbon', 'CEC' =  'cec', 'pH' = 'ph','Coarse Debris' = 'coarse', 'Sand' = 'sand', 'Silt' = 'silt', 'Clay'= 'clay')

  topov <- c('Slope' = 'slope', 'Elevation' = 'alt', 'North-ness' = 'northness', 'East-ness' ='eastness', 'Coast Distance' = 'dcoast')

  bio2 <- names(bio_codes)
  names(bio2) <- bio_codes
  bio2 <- c('May-Sept Precip' = 'summer_precip', bio2) 
  vv <- c(topov, soilv, bio2)

	
# Variable Importance

# create a figure /table that shows importance scores (somehow)

# Random Forest

	i.rf <- importance(rf)[,1]
	#i.rf.size <- scale(i.rf, center = min(i.rf))
	i.rf.size <- i.rf/ max(i.rf)

	i.rf.d <- data.frame( rowv = names(i.rf), colv = 'Random Forest', size = i.rf.size, color = 1)
	
# 	cart

	i.cart <- ct$variable.importance
	i.cart.size <- i.cart/max(i.cart)

	i.cart.d <- data.frame(rowv = names(i.cart), colv = 'CART', size = i.cart.size,color = 1)
	
# OLS

	i.ols <- coef(update(ols, . ~., data = phyto.scaled))[-1]
	
	i.ols.d <- data.frame(rowv = names(i.ols), colv = 'OLS', size = abs(i.ols) / max(abs(i.ols)), color = ifelse( i.ols < 0, 3, 2))
	
# GAM

	i.gam <- anova(g)$s.table[,3]
	i.gam <- anova(g)$chi.sq
	i.gam <- i.gam[ -which( round(i.gam, 1) == 0) ]
	names(i.gam) <- gsub('[()]', '', names(i.gam))
	names(i.gam) <- gsub('^s', '', names(i.gam))
	
	i.gam.d <- data.frame(rowv = names(i.gam), colv = 'GAM', size = i.gam/max(i.gam), color = 1) 
	
	
# LASSO

	i.lasso <- coef(lso)[,1][-1]
	i.lasso <- i.lasso[ -which( i.lasso == 0) ]
	
	i.las.size <- abs( i.lasso / max(abs(i.lasso)))

	i.las.d <- data.frame(rowv = names(i.lasso), colv = 'Lasso', size = i.las.size, color = ifelse( i.lasso < 0, 3, 2))

	
	
	# dataf <- rbind(i.rf.d, i.cart.d, i.gam.d, i.ols.d, i.las.d)
	dataf <- rbind(i.rf.d,  i.gam.d, i.ols.d, i.las.d)
#dataf$rowv <- as.factor(dataf$rowv)

# change names


	# Bios
	dataf$rowv <- as.character(dataf$rowv)
	dataf$rown <- names(vv)[match(dataf$rowv, vv)]
	
	dataf$rvar <- factor(dataf$rown, levels = rev(names(vv)))
	
	
	# Correlations
	
	cmat <- abs(cor(phyto[,pvars]))
	dissim <- 1 - cmat
	distance <- as.dist(dissim)
	h <- hclust(distance)
	dd.row <- as.dendrogram(h)
	dx <- dendro_data(dd.row)
	#plot(h)
	
	sn <- names(vv)[match(as.character(dx$labels$label),vv)]
	dx$labels$label <- factor(sn, levels = sn)
	dataf$dvar <- factor(dataf$rown, levels = dx$labels$label)
	
	
	
	ggdend <- function(df) {
	  ggplot() +
		geom_segment(data = df, aes(x=x, y=y, xend=xend, yend=yend),colour="light grey", size = 1.3) +
		labs(x = "", y = "") + theme_minimal() +
		theme(axis.text = element_blank(), axis.ticks = element_blank(),
			  panel.grid = element_blank())
	}
		
	the_dendro <- ggdend(dx$segments)+ coord_flip()
	
	
cbPalette <- c("#999999", "#56B4E9", "#D55E00")
the_plot <- ggplot(dataf, aes(y = dvar, x = factor(colv))) +	
  geom_point(aes(size = size, colour = factor(color)))+
  #theme with white background
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank()
   # ,panel.grid.major = element_blank()
   #,panel.grid.minor = element_blank()
   ,panel.border = element_blank()
   ,legend.position="none"
   ,axis.ticks = element_blank()
   ,axis.text.x = element_text(angle = 330, hjust = 0, colour = "grey50")
   # ,axis.text.x = element_blank()
   ,axis.text.y = element_text( hjust = 0, colour = "grey50")
   ,axis.title.x = element_blank()
   ,axis.title.y = element_blank()
	)+
	scale_colour_manual(values=cbPalette)	+
    scale_size_continuous(range = c(0, 13)) 
	# scale_size_area()
	

	png(file = paste0('C:/projects/phytolith/out/', the_y,'/fig/model_importances.png'), height= 10, width=10, units = 'in', res = 200)
	#x11(height=10, width  =10)
	grid.newpage()
	print(the_plot, vp = viewport(.7,1, x = .35, y = .5))
	print(the_dendro, vp=viewport(0.35, 1.001, x = 0.8, y = 0.523))
	
	
	dev.off()
