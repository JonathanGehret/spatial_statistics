#some analysis about o-ring statsitcs, l, graphs, create null model, inside grpahic we will be able to create confidence envelopes
# envelope: if it's inside

library(spatstat)
library(tidyverse)
library(onpoint)

set.seed(1)

# either dead or living trees, we want to know: is there some correlation
#read_delim imports as "tibble" class with very good compatibility with spatstat
hemlock = read_delim(file = "data/Hemlock_Adults_vs_SmallSaps.txt", delim = ";")

#change 1 and 2 to adults and saplings for more clarity using mutate
hemlock = mutate(hemlock, mark = case_when(mark == 1 ~ "adults", mark == 2 ~ "saplings"), mark = as.factor(mark))

#defining coords?
plot_area = ripras(x = hemlock$x, y = hemlock$y, shape = "rectangle")

#combining both into one!
hemlock_ppp = as.ppp(X = hemlock, W = plot_area)

adults = subset.ppp(hemlock_ppp, marks == "adults", drop = TRUE)

envelopes_adults = envelope(hemlock_ppp, fun = estimate_o_ring,
                           r = seq(from=0, to = 45, by = 0.5), nsim = 39, nrank = 1)

plot(envelopes_adults)

# graph for the area of the intensity of the heterogeneity (like in programita)
lambda_xy = density(x = adults, 15) # the number is radius!

plot(lambda_xy)

simulated_heterogenous_pattern = rpoispp(lambda = lambda_xy, nsim = 39)

plot(simulated_heterogenous_pattern)




#below: old code!

#o-ring-statistic function
oest_cross = function(input,i,j,r = NULL,correction ="Ripley",divisor ="d",...) {
  gij = pcfcross(input, i=i, j=j, r=r, correction = correction, divisor = divisor, ...)
  lambda = intensity(input)[j]
  eval.fv(gij * lambda)
}

null_model_pattern = list()

seedlings = subset.ppp(douglas_fir_ppp, marks == "seedlings", drop=TRUE)

adults = subset.ppp(douglas_fir_ppp, marks == "adults", drop = TRUE)

for(i in 1:39){
  random_seedlings = rpoint(n = seedlings$n, win = seedlings$window)
  overall_pattern = superimpose(adults = unmark(adults), seedlings = random_seedlings)
  null_model_pattern[i] = list(overall_pattern)
}

antecedent_conditions = envelope(douglas_fir_ppp, fun = oest_cross, i="adults", j = "seedlings",
                                 r= seq(from=0, to=45,by=0.5), nsim = 39,nrank = 1,
                                 simulate = null_model_pattern)

plot(antecedent_conditions)

toroidal_shift =  envelope(douglas_fir_ppp, fun = oest_cross, i="adults", j = "seedlings",
                           r= seq(from=0, to=45,by=0.5), nsim = 39,nrank = 1,
                           simulate = expression(rshift(douglas_fir_ppp, which = "seedlings",
                                                        edge = "torus")))

plot(toroidal_shift)

comp = list()
comp[1] = list(as_tibble(antecedent_conditions))
comp[2] = list(as_tibble(toroidal_shift))

nms = c("CSR","Toroidal")
names_graphs = expand.grid(nms=nms)
names(comp) = paste0(names_graphs$nms)

comp = bind_rows(comp, .id = "id")

ggplot(data = comp) +
  geom_ribbon(aes(x = r, ymin = lo, ymax = hi, fill = "Simulation envelope"), alpha = 0.75) +
  geom_line(aes(x = r, y = obs, col = "Observed value")) +
  scale_fill_manual(name = "", values = c("Simulation envelope" = "grey")) +
  scale_color_manual(name = "", values = c("Observed value" = "black"))+
  facet_wrap(~id)+
  theme_classic()
