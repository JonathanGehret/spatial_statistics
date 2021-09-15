library(spatstat) 
library(onpoint)
library(raster) # for loading raster files
library(maptools) # for using as.im.Rasterlayer()

penguins = read.csv(file = "data/penguins/nest_locations_EPSG3031.csv",sep=",")

plot(penguins)

#defining coords?
plot_area = ripras(x = penguins$Easting..m., y = penguins$Northing..m., shape = "rectangle")

#combining both into one!
penguins_ppp = as.ppp(X = penguins, W = plot_area)
summary(penguins_ppp)
plot(penguins_ppp, add = T)

# o-ring statistic
oring_statistic = estimate_o_ring(x = penguins_ppp, r = seq(from = 0, to=45,by = 0.5))
plot(oring_statistic$r, oring_statistic$iso, type = "l")
lines(oring_statistic$r, oring_statistic$the,col="red")

# envelope!
simulation_envelope_39 = envelope(Y = penguins_ppp, fun = estimate_o_ring, r = seq(from = 0, to = 45, by = 0.5), 
                                  nsim = 39, nrank = 1)
plot(simulation_envelope_39)
#quantum colors show if its outside theenevelope (yellow), above (purple), inside (green)
plot_quantums(simulation_envelope_39,quantum_size = 0.001)


#random labeling to see differences in qualitites!
#...
#  EPSG:3031 (Antarctic Stereographic).

# to-do: add coordinate system: EPSG:3031 (Antarctic Stereographic)

# load elevation
elevation = raster(x = "data/penguins/elevation/elevation.tif")
crs(elevation) = CRS('+init=EPSG:3031')
#plot(elevation)

# load cost distance raster
cdr = raster(x = "data/penguins/cost_distance/cost_distance.tif")
crs(cdr) = CRS('+init=EPSG:3031')
#plot(cdr)

# load flow accumulation (original scale)
flacc = raster(x = "data/penguins/flow_acc_01/flow_acc_01.tif")
crs(flacc) = CRS('+init=EPSG:3031')
#plot(flacc, add = T)

flacc_16 = raster(x = "data/penguins/flow_acc_16/flow_acc_16.tif")
plot(flacc_16)


# plotting in one place
par(mfrow=c(1,3))
plot(elevation, main = "Elevation")
plot(penguins_ppp, add = T)
plot(cdr, main = "Cost distance")
plot(penguins_ppp, add = T)
plot(flacc, main = "Flow accumulation")
plot(penguins_ppp, add = T)

# are the points signifcantly correlated with an abiotic covariate
# later in programita: cut the studdy area? but isn't it already onyl using the  study area!
?im
?as.im
?spatstat
?berman.test()

delev = disaggregate(elevation, fact = 20)
plot(delev)

# raster to im for use with berman.test (spatstat)
flim = as.im.RasterLayer(flacc_16)
cdrim = as.im.RasterLayer(cdr)

# berman.test for interactions 
#Tests the goodness-of-fit of a Poisson point process model using methods of Berman (1986).

bt_fl_z1 = berman.test(penguins_ppp,flim, which = "Z1")
bt_cdr_z1 = berman.test(penguins_ppp,cdrim, which = "Z1")
bt_fl_z2 = berman.test(penguins_ppp,flim, which = "Z2")
bt_cdr_z2 = berman.test(penguins_ppp,cdrim, which = "Z2")

bt_cdr
bt_fl

# plotting of bermant tests
par(mfrow = c(2,2))
plot(bt_cdr_z1, xlab = "Cost Distance")
plot(bt_fl_z1, xlab = "Flow Accumulation")
plot(bt_cdr_z2, xlab = "Cost Distance")
plot(bt_fl_z2, xlab = "Flow Accumulation")
