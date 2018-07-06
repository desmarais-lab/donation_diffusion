# required for simulation
library(ergm)
library(boxr)

P_VALUE = 0.05

# read in ERGM objects
box_auth()
if(P_VALUE == 0.05) {
    # Load 'ergm_results_threshold_8_pval_0.025.RData'
    box_load(file_id = '302655556413')   
} else if(P_VALUE == 0.025) {
    # Load 'ergm_results_threshold_8_pval_0.05.RData'
    box_load(file_id = '302644314208')
}

set.seed(12345)
# simulate networks from spatial ERGM
spatial.networks <- simulate(simple.homophily.ergm, nsim = 100, 
                             constraints = ~ edges,
                             control = control.simulate.ergm(
                                MCMC.interval = 400000,
                                MCMC.burnin = 80000000)
                             )

# simulate networks from directional ERGM, take 1000
directional.networks <- simulate(ideological.hierarchy.ergm, nsim = 100,
                                 constraints = ~ edges,
                                 control=control.simulate.ergm(
                                    MCMC.interval = 400000,
                                    MCMC.burnin = 80000000)
                                 )

# save simulation results
out = list('spatial.networks' = spatial.networks,
           'directional.networks' = directional.networks)

## Write files to box in dir 'Strategic_Donors/final_paper_data/'
fname = paste0('ergm_simulation_results_8_pval_', P_VALUE, '.RData')
box_write(out, filename = fname, dir_id = '50855821402')
