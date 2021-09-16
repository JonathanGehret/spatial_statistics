library(spatstat) 
library(onpoint)
library(raster) # for loading raster files
library(maptools) # for using as.im.Rasterlayer()

pengcounts = read.csv(file ="data/penguinmap/Model_results_by_site_V_3.0.csv")

lock_counts = pengcounts[pengcounts$site.name == "Port Lockroy",]
lock_counts_coords = data.frame(lock_counts$Longitude.EPSG.4326,lock_counts$Latitude.EPSG.4326)

plot_area = ripras(x = lock_counts$Longitude.EPSG.4326, y = lock_counts$Latitude.EPSG.4326, shape = "rectangle")

#combining both into one!
penguins_ppp = as.ppp(X = lock_counts_coords, W = plot_area)
summary(penguins_ppp)
plot(penguins_ppp, add = T)