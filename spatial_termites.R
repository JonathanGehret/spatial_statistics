#library(spatstat)
library(onpoint)

termites = read.csv(file = "data/Brazil_1.txt",sep = "\t", header = FALSE)

plot(termites)
plot(bre.dryad)

rdat

bees = read.csv(file = "data/NestDorsata.csv", sep = ";", header = T)

plot(bees$Lat ~ bees$Long)



