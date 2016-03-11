# Prepare Clary Data

library(raster)

z <- read.csv('C:/projects/phytolith/data/processed/clary_data.csv')

a <- read.csv('C:/projects/phytolith/data/raw/clary_transect.csv', check.names = FALSE)
j <- read.csv('C:/projects/phytolith/data/raw/clary_transects_slope_aspect.csv', check.names = TRUE)

names(j) <- c("site", "Unit", "disturbance", "slope", "aspect", "unk1", "unk2", "% WARM SEASON PPT")

# move point on border
a$Long[49] <- -120.8882

# merge

# in same order, just moving columns from j to a

a$disturbance <- j$disturbance
a$clry_slope <- j$slope
a$clry_aspect <- j$aspect
a$`% WARM SEASON PPT` <- j$`% WARM SEASON PPT`


# extract predictors
b <- brick('C:/projects/phytolith/data/processed/predictors.grd')
ca <- shapefile('C:/Users/steve/Downloads/GEO2015/13/counties_2000.shp')

sp <- a
coordinates(sp) <- ~Long + Latitude


# extract fine-scale slope and aspect
topo <- stack('C:/projects/phytolith/data/processed/fine_topo.grd')
e.topo <- as.data.frame( extract( topo, sp))


e <- as.data.frame(extract(b, sp))


# some points on the slope dont have hillshade, slope, northness and eastness

		# nas <- which( apply(e,1, function(x) any(is.na(x))))

		# slope2 <- focal(b[['slope']], matrix(1, 7,7), fun = mean, na.rm=TRUE)
		# e[nas,'slope'] <- extract(slope2, sp[nas,])

		# x <- focal(b[['northness']], matrix(1, 5,5), fun = mean, na.rm=TRUE)
		# e[nas,'northness'] <- extract(x, sp[nas,])

		# x <- focal(b[['eastness']], matrix(1, 5,5), fun = mean, na.rm=TRUE)
		# e[nas,'eastness'] <- extract(slope2, sp[nas,])

		# e[15,'slope'] <- 0 # bodega
		# e[15,'northness'] <- 2.7 # bodega
		# e[15,'eastness'] <- 2.7 # bodega

		
e$slope <- e$northness <- e$eastness <- NULL
e <- cbind(e.topo, e)
a <- cbind(z,e)
a$dcoastkm <- a$dcoast/1000	

names(a)[names(a)=='Long'] <- 'lon'
names(a)[names(a)=='Latitude'] <- 'lat'


write.csv(a, 'C:/projects/phytolith/data/processed/clary_data.csv', row.names=F)

###############

# Other random stuff (not necessary)


library(maps); library(xtable)
plot(z$x, z$y, pch = 3);map('state','california', add=TRUE)


plot(z[ , c('dcoast', 'Ann.ppt', 'M.S.ppt', 'O.A.ppt', 'X.warm.season.ppt', 'p.grass.cover', 'a.grass.cover', 'slope')] )

library(plotKML)
library(sp)
library(raster)
sp <- z
coordinates(sp) <- ~x + y
projection(sp) <- CRS("+init=epsg:4326")
plotKML(sp)


mb <- brick('C:/projects/phytolith/data/mpredictors.grd')




# Extract from location
	X <- as.data.frame(extract(mb, sp))
	i <- which(is.na(X$slope))
	if(length(i) > 0){
		X$slope[i] <- 0
		X$northness[i] <- X$eastness[i] <-pi/2
	}
	z$alt <- X$alt
library(mgcv)
bm <- gam(p.grass.cover ~ s(dcoast) + s(CDD)+ s(CDD70)+ s(alt), family=betar(link="logit"),data=sp)
bm <- gam(p.grass.cover ~ s(dcoast) + s(CDD)+ s(CDD70)+ s(alt) + s(X.warm.season.ppt), family=betar(link="logit"),data=z)


	
 ### Some further covariate processing
  
  # remove veg and hillshade columns
  XX <- X[ , -which(names(X) %in% c('veg', 'hillshade', 'slope', 'northness', 'eastness'))]
  
  #only use pre-industrial bioclim variables
  XX <- XX[ , -grep('bio[1-9]',names(XX))]
  XX <- XX[ , -grep('bioclim_6',names(XX))]
  XX <- XX[ , -grep('bioclim_4',names(XX))]
  
  #exclude some soil covariates which likely vary on short timescales / be highly correlated with bioclim variables
  XX <- XX[ , -which(names(XX) %in% c('carbon','ph','cec'))]
  
  ### Load Functions
  
  source('code/functions.R')
 
  # identify response variable
  XX$y <- sp$p.grass.cover
  depvarname <- 'total phytolith content'
  
   XX <- na.omit(XX)
   omit <- attr(XX, 'na.action')
   if (length(omit) > 0) sp <- sp [ -omit,]

   
# create a distance matrix between points to consider spatial autocorrelation in models
  dd <- as.matrix(as.dist(pointDistance(sp, lonlat = TRUE, allpairs=TRUE)))
  dd.inv <- 1/dd
  diag(dd.inv) <- 0

 
 
  f <- y ~ .  
  xn <- which(names (XX) != 'y')
  yn <- which(names(XX) == 'y')
  
  
  
  
  
  # do an initial kfold cv to determine weighting scheme for ensemble
  kf.results <- list()
  for(i in 1:3){
    cat(i);flush.console()
    kf <- kfoldcv(f, XX, XX[,xn], XX[,yn], wts = NULL, k = 5, seed = i)
    kf.results[[i]] <- kf$stats
  }
  
  initial.kf <- kf.results <- Reduce('+', kf.results)/length(kf.results)
  
  # get weights for models 
  wts <- getWts(kf.results[1:(nrow(kf.results)-1),'MSE'], low = TRUE)
  
  # now check kfold again, also inspecting the ensemble 
  
  kf <- kfoldcv(f, XX, XX[,xn], XX[,yn], wts = wts, k = 5, seed = 999)
  second.kf <- kf$stats
  
  # fit the ensemble model
  ens <- ensemble.fit(f, XX, XX[,xn],XX[,yn])
  
  # Make a prediction
  p.ens <- ensemble.pred(ens, XX, XX[,xn], XX[,yn], wts = wts)
  
  # look at how tightly each model fits the data (random forest = very strong)
  (fitted.cors <- lapply(p.ens, function(x) cor(x,XX$y)))
  
  # look for spatial autcorrelation in residuals
  mor <- apply(p.ens-XX$y, 2, function(e) moran(e))
  
  # moran test of model residuals
  moran.result <- sapply(mor, function(x) cbind(x$i, x$lowerp,x$upperp))
  row.names(moran.result) <- c('I','lower P-value', 'upper P-value')
  
  moran.out <- xtable(t(moran.result))
  
  #correlation of model residuals
  model.cors <- cor(XX$y-p.ens)
 
 
 #random forest prediction
	p.rf <- predict(mb, ens$RF)

	#OLS prediction
	p.ols <- predict(mb, ens$OLS)
	
	#CT prediction
	p.ct <- predict(mb, ens$CT)

	#ridge prediction
	p.ridge <- predict.gnet(mb, ens$ridge)
	
	#lasso prediction
	p.lasso <- predict.gnet(mb, ens$lass)
	
	#ensemble prediction
	m.ens <- p.ols * wts['OLS'] + p.ct * wts['CT'] + p.rf * wts['RF'] + p.ridge* wts['ridge'] + p.lasso*wts['lass'] 


	
	# take out very extreme values
	m.ens[m.ens > 1] <- 1

	plot(m.ens)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
#
	plot(mb$hillshade, col=grey(0:100/100),legend = FALSE, main = 'Phytolith Sample Locations')
  plot(mb$alt, col = rainbow(25, alpha = .25), add=TRUE)
  scalebar(200, divs = 4, below = 'km', type = 'bar')
  points (sp, pch =1, cex = 1 + sp$p.grass.cover)
 
sp$slope2 <- X$slope
sp$alt <- X$alt

