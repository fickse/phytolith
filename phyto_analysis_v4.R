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
	# )
	
	aggP <- brick( agg_predictors)
	names(aggP) <- names(predictors)
	
	  wc <- stack( 
				lapply(
					list.files( "C:/data/clim/wc2-5/", pattern = "prec.*?bil$",full = TRUE)[c(1,5:12,2:4)], raster )
			)
	
	
	
	  phyto$summer_precip <- rowSums( extract( wc, phyto[, c('lon','lat')])[, 5:9])
	  clary$summer_precip <- rowSums( extract( wc, clary[, c('lon','lat')])[, 5:9])

	
# # # # # # # # # # # # # # # # # # # # # # 	
	
	# variables
 
 pvars <- c(
			'slope',
		#	'northness',
		#	'eastness',
			'bulk', 
			#'carbon',
			'ph',
			'silt',
			'clay',
			'cec', 
			'sand',
			'coarse',
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
			#'alt',
			# 'aws'#,
			'wetland',
			# 'rootaws'
			'dcoast'
			)

	
# # # # # # # # # # # # # # # # # # # # # # 
	
	#scaled data
	j <- which( names(phyto) %in% c('series','County', 'phyto_pct', 'bilobate_star', the_y))
	phyto.scaled <- cbind( scale(phyto[,-j]), phyto[,j])
	
	dir.create( paste0( 'C:/projects/phytolith/out/', the_y), F)
	dir.create( paste0( 'C:/projects/phytolith/out/', the_y,'/tab'), F)
	dir.create( paste0( 'C:/projects/phytolith/out/', the_y,'/fig'), F)
	
######################################################

#reference


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
	spec2 <- colorRampPalette( c("gray","#256EC2","#3D9ABE","#42C695","#5DD64B","#AEE34E","#DDEC53","#F8EE49","#F6D53A","#F5B52D","#EE8913","#E16310","#CB4707","#B61904","#9B0000") )
######################################################

######################################################

  # Recreate figures from Clary (using variables derived from raster data
  
  # Fig 2. Perennial Grass cover as a function of distance from the coast
  
  # plot(clary$p.grass.cover~clary$dcoastkm, xlab = 'Distance from Pacific coast (m)', ylab = 'Perennial Grass cover') 
  # plot(clary$p.grass.cover~clary$clry_slope, xlab = 'Distance from Pacific coast (m)', ylab = 'Perennial Grass cover') 

  
  # Most of Evett's data follow the same pattern as clary, w.r.t distance from coast. 
  # plot(phyto[,the_y] ~ phyto$dcoast)


  # Plot of Perennial grass cover, annual grass cover and phytolith content as a function of distance from the coast. 
  # Phytolith content and perennial grass cover show the same pattern.
  
  i <- which(phyto$dcoast <= max(clary$dcoast))


#####################################################

 
  # Testing for correlations among soil variables and responses
  soilv <- c('Bulk Density' = 'bulk', 'Soil Carbon' ='carbon', 'CEC' =  'cec', 'pH' = 'ph','Coarse Debris' = 'coarse', 'Sand' = 'sand', 'Silt' = 'silt', 'Clay'= 'clay', 'Wetland Area' = 'wetland')

  topov <- c('Slope' = 'slope', 'Elevation' = 'alt', 'North-ness' = 'northness', 'East-ness' ='eastness', 'Coast Distance' = 'dcoast')

  bio2 <- names(bio_codes)
  names(bio2) <- bio_codes
  bio2 <- c('May-Sept Precip' = 'summer_precip', bio2) 
  vv <- c(topov, soilv, bio2)
  vv <- vv[which(vv %in% pvars)]
  
  
  method = 'pearson' # doesn't depend on bivariate normal independent
  
   p.grass <- lapply( vv, function(x) cor.test( clary$p.grass.cover, clary[,x], method = method) )
   a.grass <- lapply( vv, function(x) cor.test( clary$a.grass.cover, clary[,x], method = method) )
   phyto.cor <- lapply( vv, function(x) cor.test( phyto[, the_y],phyto[,x], method = method))
   phytoC.cor <- lapply( vv, function(x) cor.test( phyto[i,the_y],phyto[i,x], method = method ))

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
	print.xtable(xtable(t(cors)), type = 'html', file = paste0('C:/projects/phytolith/out/',the_y,'/tab/variables_cor.html')) 


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

 
f1.gam <- as.formula( 
		paste0(
			"I(log(", the_y, "+", log_adj, ")) ~ s(", 
				paste( pvars, collapse = ') + s('), ')'
			)
		)

f1 <- as.formula( 
		paste0(
			"I(log(", the_y,"+", log_adj, ")) ~ ", 
				paste( pvars, collapse = ' + ')
			)
		)

# for the clary dataset
f2 <- update(f1, p.grass.cover ~ .)

f2.gam <- update(f1.gam, p.grass.cover ~ .)

		
#########################################################

	# Generate Folds for cross-validation
	set.seed(777)
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

	rf <- randomForest( y = log(phyto[,the_y] + log_adj ), x = phyto[, pvars], ntree = 10000, mtry = 2)
	rf
	varImpPlot(rf)

	
	rfc <- randomForest( y = clary$p.grass.cover, x = clary[, pvars], ntree = 5000, mtry = 1)
	rfc
	varImpPlot(rfc)
	
	p.rf <-exp( predict(aggP[[pvars]], rf, progress = 'text'))
	p.rfc <- predict(aggP[[pvars]], rfc, progress = 'text')
	
	# spplot(stack(p.rf, p.rfc))
    plot(p.rf)
	pred.phyto$rf <-  predict(rf)
	pred.clary$rf <- predict(rfc)
	
	
	
	moran.phyto$rf <- moran( log(phyto[,the_y] + log_adj ) - predict(rf), phyto[, c('lon','lat')] )
	
	moran.clary$rf <- moran( clary$p.grass.cover - predict(rfc), clary[, c('lon','lat')] )
	



	
	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	out <- list()
	
	for (j in 1:k){
			train <- which(k.p != j)
			test <- which( k.p == j)
			rfj <- randomForest( y = log(phyto[train,the_y] + log_adj ), x = phyto[train, pvars], ntree = 5000, mtry = 2)
			out[[j]] <- predict(rfj, newdata=phyto[test,pvars])        
	}
	
	out <- unlist(out)
	rf.phyto.cv <- list ( p =  out[order(as.numeric(names(out)))], o = log(phyto[,the_y] + log_adj ))
	
	# perennial grass data
	
	out <- list()
	for (j in 1:k){
			train <- which(k.c != j)
			test <- which( k.c == j)
			rfj <- randomForest( y = clary$p.grass.cover[train] , x = clary[train, pvars], ntree = 5000, mtry = 2)
			out[[j]] <- predict(rfj, newdata=clary[test,pvars])        
	}
	out <- unlist(out)
	rf.clary.cv <- list ( p =  out[order(as.numeric(names(out)))], o = clary$p.grass.cover)
	
	
#########################################################

# GAM
	pcgam.clim <- princomp( phyto[, c(paste0('bio',1:19), 'summer_precip')], cor = TRUE)
	pcgam.soil <- princomp( phyto[, soilv], cor = TRUE)

	phyto[, paste0("pcClim", 1:3)] <- pcgam.clim$scores[,1:3]
	phyto[, paste0("pcSoil", 1:3)] <- pcgam.clim$scores[,1:3]

	aggP[["pcClim1"]] <- aggP[["pcClim2"]] <- aggP[["pcClim3"]] <- aggP[["pcSoil1"]] <- aggP[["pcSoil2"]] <-aggP[["pcSoil3"]] <- aggP[[2]]
	aggP[[paste0('pcClim',1)]][] <- predict(pcgam.clim,aggP[])[,1]
	aggP[[paste0('pcClim',2)]][] <- predict(pcgam.clim,aggP[])[,2]
	aggP[[paste0('pcClim',3)]][] <- predict(pcgam.clim,aggP[])[,3]
	aggP[[paste0('pcSoil',1)]][] <- predict(pcgam.soil,aggP[])[,1]
	aggP[[paste0('pcSoil',2)]][] <- predict(pcgam.soil,aggP[])[,2]
	aggP[[paste0('pcSoil',3)]][] <- predict(pcgam.soil,aggP[])[,3]

	PRG <- aggP[[ c(paste0('pcClim',1:3), paste0('pcSoil',1:3), 'dcoast')]]
	
	f1.gam <- I(log(phyto[,the_y] + log_adj )) ~  s(dcoast) + s(bio1) + s(bio3) + s(bio7) + s(bio12)  + s(bio15) + s(ph) + s(bulk) + s(sand) + s(wetland) 
	# f1.gam <- I(log(phyto[,the_y] + log_adj )) ~  s(dcoast) + s(pcClim1) + s(pcClim2) + s(pcClim3) + s(pcSoil1) + s(pcSoil2) + s(pcSoil3) 
	f2.gam <- update(f1, p.grass.cover ~ .)
	
	g <- gam(f1.gam, data = phyto,select = TRUE)
	g.c <- gam(f2.gam, data = clary, select = TRUE)

	
	# p.g <- exp(predict(PRG, g))
	p.g <- exp(predict(aggP[[pvars]], g))
	plot(p.g)
	
	p.gc <- exp(predict(aggP, g.c))
	p.gc[p.gc[] > 1] <- 1
#	spplot(stack(p.g, p.gc))

	
	
	pred.phyto$gam <-  predict(g)
	pred.clary$gam <- predict(g.c)
	
	moran.phyto$gam <- moran( log(phyto[,the_y] + log_adj )- predict(g), phyto[, c('lon','lat')] )
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
	gam.phyto.cv <- list ( p =  out[order(as.numeric(names(out)))], o = log(phyto[,the_y] + log_adj ))
	
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

	lso = glmnet( as.matrix(phyto[, pvars]),log(phyto[,the_y] + log_adj ),alpha =1, lambda =  cv.glmnet (as.matrix(as.matrix(phyto[, pvars])),log(phyto[,the_y] + log_adj ),alpha =1)$lambda.min )

	
	lso.c = glmnet( as.matrix(clary[, pvars]),clary$p.grass.cover,alpha =1, lambda =  cv.glmnet (as.matrix(as.matrix(clary[, pvars])),clary$p.grass.cover,alpha =1)$lambda.min )
	
	
	
	p.lso <- exp(predict.gnet(aggP[[pvars]], lso))
	p.lsoc <- predict.gnet(aggP, lso.c)
	
	 plot(p.lso)
	# plot(p.lsoc)

	# spplot(stack(p.lso, p.lsoc))
	
	pred.phyto$lasso <- predict(lso, newx = as.matrix(phyto[,pvars]))
	pred.clary$lasso <- predict(lso.c, newx = as.matrix(clary[,pvars]))
	
	moran.phyto$lasso <- moran( log(phyto[,the_y] + log_adj ) - pred.phyto$lasso, phyto[, c('lon','lat')] )

	moran.clary$lasso <- moran( clary$p.grass.cover - pred.clary$lasso, clary[, c('lon','lat')] )
	
	
	
	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	out <- c()
	
	for (j in 1:k){
			train <- which(k.p != j)
			test <- which( k.p == j)
			lsoj = glmnet( as.matrix(phyto[train, pvars]),log(phyto[train,the_y] + log_adj ),alpha =1, lambda =  cv.glmnet (as.matrix(as.matrix(phyto[train, pvars])),log(phyto[train,the_y] + log_adj ),alpha =1)$lambda.min )
			out[test] <- as.vector( predict(lsoj, newx=as.matrix(phyto[test,pvars])) )       
	}
	
	
	lso.phyto.cv <- list ( p =  out, o = log(phyto[,the_y] + log_adj ))
	
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
	# SVM
	
if(FALSE){
	t.out <- tune("svm", f1 , data = phyto, ranges = list(cost = c(.1,1,10,100,1000), gamma = c(.001,.5, 1, 2, 3,4)) )
#   sv <- svm( f1, data = phyto, cost = t.out$best.parameters$cost, gamma = t.out$best.parameter$gamma)
	sv <- svm( f1, data = phyto, cost = 1, gamma = .001)
	p.sv <- exp(predict(aggP[[pvars]], sv))
	plot(p.sv)
	# plot(p.sv)
}
	


######################################################################
######################################################################

	#  CART
	
	# f1 <- I(log(phyto_pct+ .001)) ~  bio1 + bio12 + northness + slope + dcoast + veg
	# f2 <- update(f1, p.grass.cover ~ .)
	
	
	ct = rpart(f1, data = phyto)
	ctc = rpart(f2, data = clary)

	# plot(ctc);text(ctc)
	 plot(ct); text(ct)

	# p.ct <- exp(predict(aggP, ct))
	# p.ctc <- predict(aggP, ctc)

	# spplot(stack(p.ct,p.ctc))

	
	# pred.phyto$cart <-  predict(ct)
	# pred.clary$cart <- predict(ctc)
	
	# moran.phyto$cart <- moran( log(phyto[,the_y] + log_adj ) - predict(ct), phyto[, c('lon','lat')] )
	# moran.clary$cart <- moran( clary$p.grass.cover - predict(ctc) , clary[, c('lon','lat')] )
	
	
	
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
	
	# out <- unlist(out)
	# cart.phyto.cv <- list ( p =  out[order(as.numeric(names(out)))], o = log(phyto[,the_y] + log_adj ))
	
	# perennial grass data
	
	# out <- list()
	# for (j in 1:k){
			# train <- which(k.c != j)
			# test <- which( k.c == j)
			# rpj <- rpart( f2, data = clary[train, ])
			# out[[j]] <- predict(rpj, newdata=clary[test,]) 
	# }
	# out <- unlist(out)
	# cart.clary.cv <- list ( p =  out[order(as.numeric(names(out)))], o = clary$p.grass.cover)



#########################################################################
#########################################################################

#OLS

	ols = stepAIC(lm(f1, data = phyto))

	p.ols <- exp(predict(aggP[[pvars]], ols))-log_adj
	pred.phyto$ols <-  predict(ols)
	moran.phyto$ols <- moran( log(phyto[,the_y] + log_adj ) - predict(ols), phyto[, c('lon','lat')] )
	
	###################
	# Cross- Validate #
	###################
	

	# phytolith data
	
	out <- list()
	
	for (j in 1:k){
			train <- which(k.p != j)
			test <- which( k.p == j)
			rpj <- stepAIC(lm( f1, data = phyto[train, ]))
			# rpj <- update(ols, . ~. , data = phyto[train,])
			out[[j]] <- predict(rpj, newdata=phyto[test,])        
	}
	
	out <- unlist(out)
	ols.phyto.cv <- list ( p =  out[order(as.numeric(names(out)))], o = log(phyto[,the_y] + log_adj ))



#########################################################################
#########################################################################

# Cross Validation Results (!)

cv.phy <- list ( 'Random Forest' = rf.phyto.cv$p,
				# 'CART' = cart.phyto.cv$p,
				 'GAM' = gam.phyto.cv$p,
				 'Lasso' = lso.phyto.cv$p#,
			#	'OLS' = ols.phyto.cv$p
				)

				
				
# phyto

	# determine what the ensemble would have predicted for each fold, based on cor
	# out <- list()
	# for (j in 1:k){
			# train <- which(k.p != j)
			# test <- which( k.p == j)
			# cors <- sapply(cv.phy, function(x) cor(exp(x[test]) + .005, obs[test]))
			# p1 <- rowSums(mapply(function(x,y) x[test]*y, cv.phy, cors))
			# out[[j]] <- p1 / sum(cors)
	# }
	# out <- unlist(out)
	# cv.phy[['Ensemble']] <- out[order(as.numeric(names(out)))]

cv.phy <- lapply(cv.phy, function(x) exp(x) + log_adj)
obs <- exp(rf.phyto.cv$o) + log_adj

(stats <- t(cv_phy <- rbind(	
		'cor' = sapply(cv.phy, function(x) cor(x, obs)) ,
		'R2' = sapply(cv.phy, function(x) cor(x, obs)**2),
		'rmse' = sapply(cv.phy, function(x) sqrt(mean( (x - obs)^2))),
		'MAE' = sapply(cv.phy, function(x) mean( abs(x - obs))),
		'ME' = sapply(cv.phy, function(x) mean( x - obs)),
		'Median' = sapply(cv.phy, function(x) median( x - obs)),
		'I' = sapply(moran.phyto, function(x)  round(x$p.value,4))
	)))

	
print.xtable( xtable(stats, digits = c(1,4,4,2,2,2,2,4)), type = 'html', file = paste0('C:/projects/phytolith/out/',the_y,'/tab/validation_stats.html'))
	
	
# are they correlated?
cor(do.call(cbind,cv.phy))

# residual correlations
(resid_cors <- cor(do.call( cbind, lapply(pred.phyto, function(x) x - log(phyto[,the_y] + log_adj )) )))

row.names(resid_cors) <- colnames(resid_cors) <- names(pred.phyto)
print.xtable( xtable( resid_cors, digits = 2), type = 'html', file = paste0('C:/projects/phytolith/out/',the_y,'/tab/residual_correlations.html'))





ROW <-1  # R2
ens <- p.lso*abs(cv_phy[ROW,'Lasso'])   + p.g * abs(cv_phy[ROW,"GAM"])*1 + p.rf*abs(cv_phy[ROW,"Random Forest"]) #+ p.ols*abs(cv_phy[ROW,"OLS"])*0
ens <- ens / sum(abs(cv_phy[ROW,]))

# plot(ens)
# ens[ens[] > maxv] <- maxv

png( file = paste0('C:/projects/phytolith/out/', the_y, '/fig/quickEnsemble.png'))
 plot(ens)
points(phyto[, c('lon','lat')], cex = 1)

dev.off()

p.ens <- extract(ens, phyto[, c('lon','lat')])
ens.stat <- c( 'cor' = cor( p.ens, obs), 
	'R2' = cor( p.ens, obs)**2,
	'rmse' = sqrt(mean( (p.ens - obs)^2)),
	'MAE' = mean( abs(p.ens - obs)),
	'ME' = mean( p.ens - obs),
	'MEDE' = median( p.ens - obs),
	'I' = round(moran( p.ens - obs, phyto[, c('lon','lat')] )$statistic,4)
	)

	print.xtable( xtable( rbind(ens.stat), digits = 5), type = 'html', file = paste0('C:/projects/phytolith/out/',the_y,'/tab/ensemble_stat.html'))
##############################################################################################
##############################################################################################
	
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
	i.gam <- i.gam[ -which( round(i.gam, 10) == 0) ]
	names(i.gam) <- gsub('[()]', '', names(i.gam))
	names(i.gam) <- gsub('^s', '', names(i.gam))
	
	i.gam.d <- data.frame(rowv = names(i.gam), colv = 'GAM', size = i.gam/max(i.gam), color = 1) 
	
	
# LASSO

	i.lasso <- coef(lso)[,1][-1]
	i.lasso <- i.lasso[ -which( i.lasso == 0) ]
	
	i.las.size <- abs( i.lasso / max(abs(i.lasso)))

	i.las.d <- data.frame(rowv = names(i.lasso), colv = 'Lasso', size = i.las.size, color = ifelse( i.lasso < 0, 3, 2))

	
	
	dataf <- rbind(i.rf.d, 
					i.gam.d, 
					# i.ols.d, 
					i.las.d)
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
    scale_size_continuous(range = c(0, 10)) 
	# scale_size_area()
	

	png(file = paste0('C:/projects/phytolith/out/', the_y,'/fig/model_importances.png'), height= 10, width=10, units = 'in', res = 200)
#	x11(height=10, width  =10)
	grid.newpage()
	print(the_plot, vp = viewport(.7,1, x = .35, y = .5))
	print(the_dendro, vp=viewport(0.35, 1.031, x = 0.8, y = 0.51))
	
	
	dev.off()
	

######################################################################################	
######################################################################################	
	# Fancy
	p.rff <- exp(predict(predictors, rf, progress = 'text'))
	# p.ctf <- exp(predict(predictors, ct, progress = 'text'))
	p.gf <- exp(predict(predictors,g, progress = 'text'))
	p.lsof <- exp(predict.gnet(predictors, lso))
	# p.olsf <- exp(predict(predictors, ols, progress = 'text'))
	
	p.lsofa <- p.lsof


	# p.lsofa[p.lsofa> 1000] <- 1000
	
	ROW <-1  # cor
	ens.f <- 
			p.rff * abs(cv_phy[ROW,'Random Forest']) +
			# p.ctf* abs(cv_phy[ROW,2])  + 
			p.gf * abs(cv_phy[ROW,'GAM'])  + 		
			p.lsofa*abs(cv_phy[ROW,'Lasso'])  #+  
			# p.olsf * abs(cv_phy[ROW,5])*0
			
			
	ens.f <- ens.f / sum(abs(cv_phy[ROW,]))

	
	# Now to Find PC distances
    sr <- sampleRandom(predictors, 3000, xy = TRUE)
	# kp <- pvars[-which(pvars %in% c('northness','eastness'))]
	kp <- pvars
	pca <-  princomp( sr[ ,pvars], cor = TRUE)
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
	
	
	f <- paste0('C:/projects/phytolith/out/', the_y,'/fig/final.tif')
	writeRaster( stack(ens.f, inside), file = f) 

	# plot(ens.f*unc.hi)
	# maxv <- 2500 
#	ens.f[ens.f > maxv] <- maxv
#	plot(ens.f ,col = spec(100),alpha = .7, legend = F)
	
	png( width = 7.281250 , height = 8.427083, units = 'in', res = 250, file = paste0('C:/projects/phytolith/out/',the_y,'/fig/ensemble.png'))
#		plot(ens.fin , col = rev(grey(0:100/100)))
		plot(ens.fin , col = spec2(100))
		#map('state','california', add=T)
		points(phyto[,c('lon','lat')],  pch = as.numeric(phyto$phyto_pct>.3) + 1, cex = 1, col = rgb(0,0,0,.5))
#		plot(ens.fin , col = spec(100))
#		plot(inside, col = gray(.5), alpha = 1, add = TRUE, legend = F)
		legend('topright', pch = c( 1,2), legend = c(' < 0.3 %', '>= 0.3%'), title = 'phytolith percent')
	dev.off()
##############################################################################################s
##############################################################################################s

spec2 <- colorRampPalette( c("gray","#256EC2","#3D9ABE","#42C695","#5DD64B","#AEE34E","#DDEC53","#F8EE49","#F6D53A","#F5B52D","#EE8913","#E16310","#CB4707","#B61904","#9B0000") )
# preds <- list( p.rff, p.ctf, p.gf,p.olsf, p.lsof, ens.f)
preds <- list( p.rff, p.gf, p.lsof, ens.f)

preds <- ( lapply(preds, function(x) { x[x> maxv] <- maxv;x}))
preds <- stack( lapply(preds, function(x) { x* inside}))

names(preds) <- c('Random Forest', 'GAM', 'Lasso', 'Ensemble')
myTheme=rasterTheme(region=spec2(100))

png(width = 12.989583, height = 9.479167, units = 'in', res = 250, file = paste0('C:/projects/phytolith/out/',the_y,'/fig/models.png'))
levelplot(preds, contour = FALSE,par.settings=myTheme)
dev.off()
	
######################################################################################	
######################################################################################	

	# NPP comparison
	
	NPP <- aggP[['NPP']]
	npp <- predictors[['NPP']]
	plot(NPP[], ens[])
	m <- lm(ens[] ~ NPP[])
	abline(m, col = 'red')
	p.npp <- coef(m)[2]*NPP + coef(m)[1]
	p.npp <- coef(m)[2]*npp + coef(m)[1]
	
	x11(h =10, w = 10)
	v <- ens.fin - p.npp
	v <- mask(v, inside, maskvalue = 0)
	plot(inside + outside, col = grey(.8), legend = F)
	plot(v, col = spec(100), add=TRUE)
	
	u <- par("usr")
	v <- c(
	  grconvertX(u[1:2], "user", "ndc"),
	  grconvertY(u[3:4], "user", "ndc")
	)
	v <- c( (v[1]+v[2])*1.9/3, v[2], (v[3]+v[4])*1.9/3, v[4] )
	par( fig=v, new=TRUE, mar=c(0,0,0,0) )
	plot(NPP[], ens[], xlab="NPP", ylab="Phytolith Content", cex = .25)
	abline(m, col = 'red')
	
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

	
	# plot(unc.hi , col = pal2(12), breaks = brks)
	points(phyto[,c('lon','lat')], cex = 2, pch =3, lwd = 1.5, col = rgb(0,0,0,.3))
	points(phyto[,c('lon','lat')], cex = .5, pch =1, lwd = 1.5, col = rgb(0,0,0,.2))
	
	dev.off()

	# levelplot(unc, contour = TRUE)
	unc.hi <- unc
	
	# Apply this surface to predicted distribution
	# plot(unc)
	out <- unc > 2
	out2 <- 1/unc * out
	int <- unc <= 2
	int [ int[] < 1 ] <- NA 

	# plot(ens ,col = rev(terrain.colors(10, alpha = .4)), legend = F)
	# plot(ens * int,add = TRUE)
	
##############################################################################################s
##############################################################################################s


