library(raster)
spec <- colorRampPalette( c("#256EC2","#3D9ABE","#42C695","#5DD64B","#AEE34E","#DDEC53","#F8EE49","#F6D53A","#F5B52D","#EE8913","#E16310","#CB4707","#B61904","#9B0000") )
pa <- stack("C:/projects/phytolith/out/bilobatefinal.tif")
a <- stack("C:/projects/phytolith/out/bilob_soil_gt1final.tif")
p <- read.csv("C:/projects/phytolith/data/processed/phyto.csv")

bilobate <- pa[[1]] * a[[1]] * pa[[2]] * a[[2]]
plot(bilobate)

# for plotting
b2 <- bilobate
b2[b2==0] <- NA

msk <- pa[[2]]*a[[2]]
msk[msk > 0 ] <- NA

brks <- c(-1,0,.1, exp(seq(1,5, length.out = 100)))
nb <- length(brks) - 1
#plot(b2, breaks = brks, col = rev(terrain.colors(nb)), lab.breaks = exp(1:5))

png( "C:/projects/phytolith/out/bilobate/fig/bilobate_predictions.png", width = 7, height = 7, units = 'in', res = 150)

plot(b2, breaks = brks, col = spec(nb), legend = FALSE)
plot(msk, col = grey(.80), add=TRUE, legend = F)

l.cuts <- c( 0, 5, 20, 50, 100 )
l.name <- paste0(l.cuts, '-', l.cuts[-1])
l.name[ length(l.name) ]  <- paste0 ( '> ', l.cuts[length(l.cuts)])
l.col <- spec(nb)[ sapply(l.cuts, function(x) which.min(abs(x - brks))) ]
legend("topright", legend = l.name, fill = l.col, title = 'Predicted Bilobates')

i <- which(p$bilobate_ratio > 0)
cex <- .5 +  p$bilobates_per1000gsoil[i] / max(  p$bilobates_per1000gsoil[i] )*3
points(p[ i, c('lon','lat')], pch = 1, cex  = cex, col = rgb(0,0,0,.5))
legend('bottomleft', pch =1 , pt.cex = .5 + 3* l.cuts / max( p$bilobates_per1000gsoil[i] ), legend = l.name, title = 'Observed Bilobates')
dev.off()

