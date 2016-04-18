# Functions for analyzing phytolith data
# Steve Fick
# 10/2/2015

library(MASS)
library(glmnet)
library(randomForest)
library(rpart)


spec <- colorRampPalette( c("#256EC2","#3D9ABE","#42C695","#5DD64B","#AEE34E","#DDEC53","#F8EE49","#F6D53A","#F5B52D","#EE8913","#E16310","#CB4707","#B61904","#9B0000") )


# Fit an ensemble model

	ensemble.fit <- function(f,dat,X,Y){
	  list(
		OLS = stepAIC( lm(f, data = dat), trace =0),
		CT = rpart(f, data =dat),
		RF = randomForest(f, data = dat, ntree=2000, mtry = 2),
		ridge = glmnet(as.matrix(X),Y,alpha =0,  lambda = cv.glmnet (as.matrix(X),Y,alpha =0)$lambda.min),
		lass = glmnet( as.matrix(X),Y,alpha =1, lambda =  cv.glmnet (as.matrix(X),Y,alpha =1)$lambda.min )
	  )
	}

# Generate predictions from ensemble model

	ensemble.pred <- function(ens, ndata, nX, nY, wts = NULL){
		with( ens, {
			d <- data.frame(
				OLS = predict(OLS, ndata),
				CT = predict(CT, ndata),
				RF = predict(RF, ndata),
				ridge = as.vector(predict(ridge, as.matrix(nX))),
				lass = as.vector(predict(lass, as.matrix(nX)))
				)
			if (is.null(wts)) d$ens <- rowMeans(d) else d$ens <- apply(d, 1, function(x) sum(x*wts))
			d
		})
	}


# Moran monte carlo function

	moran <- function(resids, d = dd.inv) {
		require(spdep)
		mat <- mat2listw(d)  
		lower <- moran.mc(resids, mat, 999, alternative = 'less')$p.value
		upper <- moran.mc(resids, mat, 999)
		return(list ( upperp = upper$p.value, lowerp = lower, i = upper$statistic, res = upper$res))
	}

# K-fold cross validation function
	kfoldcv <- function(f,dat,X,Y, seed=77, k = 5, wts = NULL){
		require(dismo)
		i <- kfold(1:nrow(dat), k = k)
		  
		out <- list()
		  
		for (j in 1:k){
			# cat( '\r', j , ' / ', k); flush.console()
			train <- which(i != j)
			test <- which( i == j)
			fit <- ensemble.fit(f,dat[train,],X,Y)
			out[[j]] <- ensemble.pred(fit, dat[test,], X[test,], Y[test], wts = wts)        
		}
	  
		p.out <- do.call(rbind,out)
		p.out <- p.out[order(as.numeric(row.names(p.out))),]

		resp.name <- all.vars(f)[1]
		COR <- apply(p.out,2, function(x) cor( x, Y))
		Err <-  Y- p.out
		SSE <- apply(Err, 2, function(e) sum(e ^ 2))
		MSE <- apply(Err, 2, function(e) mean(e ^ 2))
		MAE <- apply(Err, 2, function(e) mean(abs(e)))
		ME <- apply(Err, 2, function(e) mean(e))
		RMSE <- sqrt(MSE)
		mor <- apply(Err, 2, function(e) moran(e))
		mor.up <- sapply(mor,function(x)x$upperp)
		mor.dn <- sapply(mor,function(x)x$lowerp)

		return( list( stats = cbind('cor'= COR, 'SSE' = SSE, 'MSE' = MSE, 'RMSE' = RMSE, 'ME' = ME, 'MAE' = MAE, "moran lower" = mor.dn,  'moran upper' = mor.up ), predicted = p.out, obs = Y))
	}


# Generate Weights from list of statistics

	getWts <- function(x, low = TRUE){ if (low) 1/x /(sum(1/x)) else x / sum(x) }


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
	

	changeVarNames <- function(vn) {
		vn <- gsub('bioclim_400AD.', 'bio', vn)
		vn <- gsub("bio_preind.", 'bio', vn)
		vn <- gsub('bioclim_6kbp.', 'bio', vn)
		vn[ match(names(codex), vn) ] <- codex
		vn[ which(vn == 'clay')] <- "Clay content"
		vn[ which(vn == 'bulk')] <- "Bulk density"
		vn[ which(vn == 'alt')] <- "Elevation"
		vn
	}
		
	# for translating bioclim variables
	codex <- c(
		bio1 = "Annual Mean Temperature",
		bio2 = "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
		bio3 = "Isothermality (diurnal range/annual range) (  100)",
		bio4 = "Temperature Seasonality (standard deviation  100)",
		bio5 = "Max Temperature of Warmest Month",
		bio6 = "Min Temperature of Coldest Month",
		bio7 = "Temperature Annual Range (max-min)",
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
		
		


	ensemble.fit.bin <- function(f,dat,X,Y){
	  list(
		OLS = stepAIC(glm(f, data = dat, family = 'binomial'), trace = 0),
		CT = rpart(f, data =dat),
		RF = randomForest(f, data = dat, ntree=2000, mtry = 2)#,
	   # ridge = glmnet(as.matrix(X),Y,alpha =0,  lambda = cv.glmnet (as.matrix(X),Y,alpha =0)$lambda.min),
		#lass = glmnet( as.matrix(X),Y,alpha =1, lambda =  cv.glmnet (as.matrix(X),Y,alpha =1)$lambda.min )
	  )
	}


	ensemble.pred.bin <- function(ens, ndata, nX, nY, wts = NULL){
	   with( ens, {
			  d <- data.frame(
				#OLS = predict(OLS, ndata),
				CT = predict(CT, ndata),
				RF = predict(RF, ndata)#,
				#ridge = as.vector(predict(ridge, as.matrix(nX))),
				#lass = as.vector(predict(lass, as.matrix(nX)))
				)
			 # if (is.null(wts)) d$ens <- rowMeans(d) else d$ens <- apply(d, 1, function(x) sum(x*wts))
			  d
			  })
	  }
			
		


