#Brief introduction to R
#lern how to calculate O-ring statistic and L-function
#download file DouglasFir_LiveDead_OGN.txt from studip
#create a folder and save in it 
#Set the location of the folder as working directory 
setwd("C:/Users/admin/.....")
#install.packages(c("spatstat", "tidyverse"))
library(spatstat)
library(tidyverse)

# import data using read_delim to save it as tibble (advanced version of dataframe) for spatstat
# take a look at the file to set delim or sep depending on the version
douglas_fir <- read_delim(file = "DouglasFir_LiveDead_OGN.txt",
                          delim = ";")

# take a look to the data
# change marks 1 to alive and 2 to dead
#library(dplyr)
douglas_fir <- mutate(douglas_fir,
                      mark = case_when(mark == 1 ~ "alive",
                                       mark == 2 ~ "dead"),
                      mark = as.factor(mark))

#change tibble to ppp for spatstat
#first use ripras() to obtain an owin object
plot_area <- ripras(x = douglas_fir$x, y = douglas_fir$y,
                    shape = "rectangle")

#now convert tibble to ppp
douglas_fir_ppp <- as.ppp(X = douglas_fir, W = plot_area)

#take a look to douglas_fir_ppp
summary(douglas_fir_ppp)

#plot all points
plot(douglas_fir_ppp, main = "Douglas fir")

#analyse living trees
alive <- subset(douglas_fir_ppp, marks == "alive")

#install devtools
library(devtools)
#install_github(repo = "r-spatialecology/onpoint")
library(onpoint)

# Calculate L-function for living trees
l_function <- center_l_function(x = alive, 
                                r = seq(from = 0, to = 45, by = 0.5))

# plot(l_function, xlim = c(0,45), 
#      main = "Univariate L-function\nliving trees", 
#      legend = FALSE)

#Calculate O-ring statistic for living trees

plot(l_function$r,l_function$iso, main = c("Univariate L-function", "living trees"), 
     type = "l", xlab = expression(italic(r)), ylab = expression(italic(L(r)-r)))
lines(l_function$r,l_function$theo, type ="l",lty=2,col = "red")

oring_statistic <- estimate_o_ring(x = alive, 
                                   r = seq(from = 0, to = 45, 
                                           by = 0.5))
# plot(oring_statistic, xlim = c(0,45), 
#      main = "Univariate O-ring statistic\nliving trees", 
#      legend = FALSE)

plot(oring_statistic$r,oring_statistic$iso, 
     main = c("Univariate O-ring statistic", "living trees"), type = "l", 
     xlab = expression(italic(r)), ylab = expression(italic(g(r))*lambda))
lines(oring_statistic$r,oring_statistic$theo, type ="l",lty=2,col = "red")

#1-Compare both results (L-function and O-Ring statistics). 
#   Where are the biggest differences? 
#   What can be a reason for these discrepancies?
#2-Calculate the L-function and the O-Ring statistic for dead trees. 
#   Subsequently, plot their results.

simulation_envelope_39 <- envelope(Y = alive, fun = estimate_o_ring,
                                   r = seq(from = 0, to = 45, 
                                           by = 0.5),
                                   nsim = 39, nrank = 1)

plot(simulation_envelope_39)

# a better Visualization

plot_quantums(simulation_envelope_39, 
              xlab = "r [m]", ylab = "O(r)",
              title = "Univariate O-ring statistic (living trees) \nnsim=39, nrank=1",
              base_size = 10, quantum_size = 0.001)

#3-Calculate the confidence envelope of 95% for living trees by 199 simulations
#   using the O-ring statistic?
#   Which nrank value did you use this time? Ajust it according to
#   the number of simulations and p-value (nrank = s of the Lecture HomoNullModels slide 23)
#4-How does the confidence envelope change and what impact does it have on the p-value?
#5-Calculate the confidence envelope of dead trees? 
#   Are there differences between living and dead trees?

#Extra material
#####################################################################
#For better data description and visualization


library(ggplot2)
library(reshape2)


#Douglas fir graph point locations
#black and white
ggplot(douglas_fir, aes(x=x,y=y,shape=mark)) + 
  geom_point(colour = "black", size = 3) +
  geom_point(colour = "white", size = 1)+
  xlab("") + 
  ylab("") +
  ggtitle("Douglas fir") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
#Colors
ggplot(douglas_fir, aes(x=x,y=y,group=mark,shape=mark, color=mark)) + 
  geom_point() +   
  xlab("") + 
  ylab("") +
  ggtitle("Douglas fir") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.title=element_blank(),axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

#L-function
ggplot(l_function, aes(r)) +
  geom_line(aes(y=iso), color="black") +
  geom_line(aes(y=theo), color='red',lty=2) +
  xlab(expression(italic(r))) + 
  ylab(expression(italic(L(r))-r)) +
  ggtitle("Univariate L-function statistic \n living trees") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#O-ring statistic 
ggplot(oring_statistic, aes(r))+
  geom_line(aes(y=iso), color="black")+
  geom_line(aes(y=theo), color='red',lty=2)+
  xlab(expression(italic(r))) +
  ylab(expression(italic(g(r))*lambda)) +
  ggtitle("Univariate O-ring statistic \n living trees")+ 
  theme_bw()+ 
  theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 

#Simulation envelopes O-ring statistic
ggplot(simulation_envelope_39, aes(r))+
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey70") +
  geom_line(aes(y=obs))+
  geom_line(aes(y=theo),color="red",linetype="dashed")+
  xlab(expression(italic(r)))+
  ylab(expression(italic(g(r)*lambda)))+
  ggtitle("Univariate O-ring statistic \n nsim=39, nrank=1")+ 
  theme_bw()+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(legend.title=element_blank(),legend.position = c(0.12,0.80), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) 
