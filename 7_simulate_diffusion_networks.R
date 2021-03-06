# required for simulation
library(ergm)
library(yaml)
library(boxr)

config = yaml.load_file('0_config.yml')
LOCAL_DATA = config$LOCAL_DATA
P_VALUE = config$P_VALUE
THRESHOLD = config$ISOLATE_THRESHOLD

# read in ERGM objects
if(!is.null(LOCAL_DATA)) {
    fname = paste0('ergm_results_', THRESHOLD, '_pval_', P_VALUE, '.RData')
    load(paste0(LOCAL_DATA, fname))
} else {
    box_auth()
    if(P_VALUE == 0.05) {
        # Load 'ergm_results_threshold_8_pval_0.05.RData'
        box_load(file_id = '') # TODO: Run ergms with 0.05
    } else if(P_VALUE == 0.025) {
        # Load 'ergm_results_threshold_8_pval_0.025.RData'
        box_load(file_id = '308416663144')     
    }
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

fname = paste0('ergm_simulation_results_', THRESHOLD, '_pval_', P_VALUE, 
               '.RData')
if(!is.null(LOCAL_DATA)) {
    save(out, file = paste0(LOCAL_DATA, fname))
} else {
    ## Write to box in dir 'Strategic_Donors/final_paper_data/'
    box_write(out, filename = fname, dir_id = '50855821402')
}
