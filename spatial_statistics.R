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

# univariate: trees stay in position
# bivariate: poisitoin is changed of something,  one of the characteristics

random_labeling = envelope(douglas_fir_ppp, fun = pcfcross, i = "alive", j ="dead",
                           r=seq(from = 0, to=45,by = 0.5), nsim = 39, nrank = 1,
                           simulate = expression(rlabel(douglas_fir_ppp)))
plot(random_labeling)
# envelope looks different than the other function: independt, almost the same shape as  the o-ring-statistic

nsim = c(39,79)
nrank = c(1,2)

param_envelop = expand.grid(nsim = nsim, nrank = nrank)
param_envelop = arrange(param_envelop, nsim, nrank)

# load library for using map function
library(purrr)

random_labeling_mult = purrr::map(1:nrow(param_envelop),function(i) {
  as_tibble(envelope(douglas_fir_ppp, fun = pcfcross, i = "alive", j ="dead",
                     r=seq(from = 0, to=45,by = 0.5), nsim = param_envelop[i,1], nrank = param_envelop[i,2],
                     simulate = expression(rlabel(douglas_fir_ppp))))
})

names(random_labeling_mult) = paste0("n_sim",param_envelop$nsim,"_nrank_",param_envelop$nrank)

# turnign to dataframe!
random_labeling_mult = bind_rows(random_labeling_mult, .id = "id")

library(ggplot2)
library(reshape2)

ggplot(data = random_labeling_mult) +
  geom_ribbon(aes(x = r, ymin = lo, ymax = hi, fill = "Simulation envelope"), alpha = 0.75) +
  geom_line(aes(x = r, y = obs, col = "Observed value")) +
  scale_fill_manual(name = "", values = c("Simulation envelope" = "grey")) +
  scale_color_manual(name = "", values = c("Observed value" = "black"))+
  facet_wrap(~id)+
  theme_classic()
  