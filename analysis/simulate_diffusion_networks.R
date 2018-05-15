# required for simulation
library(ergm)

# read in ERGM objects
load("../data/ergm_results/ergm_results_threshold_8_pval_0.025.RData")

set.seed(12345)

# simulate networks from spatial ERGM
spatial.networks <- simulate(simple.homophily.ergm, nsim=100,constraints = ~ edges,
                             control=control.simulate.ergm(
                                 MCMC.interval=400000,
                                 MCMC.burnin=80000000))

# simulate networks from directional ERGM, take 1000
directional.networks <- simulate(ideological.hierarchy.ergm, nsim=100,constraints = ~ edges,
                                 control=control.simulate.ergm(
                                     MCMC.interval=400000,
                                     MCMC.burnin=80000000))

# save simulation results
save(list=c("spatial.networks","directional.networks"), 
     file="ergm_simulation_results.RData")

# access the adjacency matrix from a network
# amat <- directional.networks[[1]][,]
# see vertex ids
# colnames(amat)



