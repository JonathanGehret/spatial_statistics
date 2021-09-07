#some analysis about o-ring statsitcs, l, graphs, create null model, inside grpahic we will be able to create confidence envelopes
# envelope: if it's inside

library(spatstat)
library(tidyverse)

# either dead or living trees, we want to know: is there some correlation
#read_delim imports as "tibble" class with very good compatibility with spatstat
douglas_fir = read_delim(file = "data/DouglasFir_LiveDead_OGN.txt", delim = ";")

#change 1 and 2 to dead and alive for more clarity using mutate
douglas_fir = mutate(douglas_fir, mark = case_when(mark == 1 ~ "alive", mark == 2 ~ "dead"), mark = as.factor(mark))

#defining coords?
plot_area = ripras(x = douglas_fir$x, y = douglas_fir$y, shape = "rectangle")

#combining both into one!
douglas_fir_ppp = as.ppp(X = douglas_fir, W = plot_area)
summary(douglas_fir_ppp)
plot(douglas_fir_ppp)

#separate deaed and alive for easier analysis
alive = subset(douglas_fir_ppp, marks == "alive")
plot(alive)

library(devtools)
install_github(repo="r-spatialecology/onpoint")
library(onpoint)

# using the l_function!
l_function = center_l_function(x = alive, r = seq(from = 0, to = 45, by = 0.5))
# get all the information inside the lfunction!
plot(l_function$r, l_function$iso, type = "l")
lines(l_function$r, l_function$the, col = "red")

# o-ring statistic
oring_statistic = estimate_o_ring(x = alive, r = seq(from = 0, to=45,by = 0.5))
plot(oring_statistic$r, oring_statistic$iso, type = "l")
lines(oring_statistic$r, oring_statistic$the,col="red")

# envelope!
simulation_envelope_39 = envelope(Y = alive, fun = estimate_o_ring, r = seq(from = 0, to = 45, by = 0.5), nsim = 39, nrank = 1)
plot(simulation_envelope_39)
#quantum colors show if its outside theenevelope (yellow), above (purple), inside (green)
plot_quantums(simulation_envelope_39,quantum_size = 0.001)
