## Packages
library(foreach)
library(doParallel)
library(network)
library(fields)
library(texreg)
library(ergm)
library(tidyverse)
library(yaml)
library(boxr)

config = yaml.load_file('0_config.yml')
LOCAL_DATA = config$LOCAL_DATA
P_VALUE = config$P_VALUE
ISOLATE_THRESHOLD = config$ISOLATE_THRESHOLD

fname = paste0('netinf_network_threshold_', config$ISOLATE_THRESHOLD,
               '.RData')
if(!is.null(LOCAL_DATA)) {
    # Read the netinf network
    load(paste0(LOCAL_DATA, fname))
    netinf_network = network
    df = read_csv(paste0(LOCAL_DATA, 'data_for_netinf_threshold_', 
                        config$ISOLATE_THRESHOLD, '.csv'))
    vertex.data = read_csv(paste0(LOCAL_DATA, 'VLC_16_full.csv'))
} else {
    box_auth()
    # Read the netinf network
    box_load(file_id = '307983798270')
    netinf_network = network
    
    df = box_read_csv(file_id = config[[paste0('data_for_netinf_threshold_',
                                               config$ISOLATE_THRESHOLD, 
                                               '.csv')]])
    # Read 'VLC_16_full.csv' from box
    ## This is temporary: I couldn't reprocude VCL_16_full.csv so far and it 
    ## doesn't match VCL_16.csv procuded in 1_...
    vertex.data = box_read(file_id = '308095557675', read_fun = read_csv) 
}
  
# Extract vertex id
vertex.id = as.character(vertex.data$Actor_ID)

# edgelist from netinf
donor.edgelist = cbind(netinf_network$origin_node,
                       netinf_network$destination_node)
# Remove edges with p-values large than P_VALUE
donor.edgelist = donor.edgelist[1:min(which(netinf_network$p_value > P_VALUE)),]
    
# master list of nodes included in netinf
# the following assumes no isolates
uniqueDonors = unique(df$Donor_ID)

# throw out vertex data for nodes not included in the network
net.vertex.data = vertex.data[is.element(vertex.id, uniqueDonors), ]
nrow(net.vertex.data)/length(uniqueDonors)

donor.edgelist = donor.edgelist[donor.edgelist[, 1] %in% net.vertex.data$Actor_ID, ]
donor.edgelist = donor.edgelist[donor.edgelist[, 2] %in% net.vertex.data$Actor_ID, ]

# construct network object
influence.network = network.initialize(nrow(net.vertex.data))
network.vertex.names(influence.network) = as.character(net.vertex.data$Actor_ID)
edges = as.matrix(cbind(as.character(donor.edgelist[,1]), 
                        as.character(donor.edgelist[,2])))
network.ids = as.character(net.vertex.data$Actor_ID)
for(i in 1:nrow(edges)){
	sender = which(network.ids == edges[i, 1])
	receiver = which(network.ids == edges[i, 2])
	influence.network[sender, receiver] = 1
	if((i %% 500) == 0) print(i)
}
    
######## Finished data merging ##########
########## Now ERGM analyses ############

# Add node attributes to network object
set.vertex.attribute(influence.network, "ideology", 
                     as.character(net.vertex.data$ideology))
set.vertex.attribute(influence.network, "cd", 
                     as.character(net.vertex.data$ind_cd))
set.vertex.attribute(influence.network, "state", 
                     as.character(net.vertex.data$ind_state))

individual.network = influence.network

# Remove all PACs from the network
delete.vertices(individual.network, 
                which(as.character(net.vertex.data$Ent_Typ) != "IND"))

ideology.interval = get.vertex.attribute(individual.network,"ideology")
ideology.interval[which(ideology.interval == "")] = "50.01–55.0"
ideology.interval.start = as.numeric(substr(ideology.interval,1,2))
set.vertex.attribute(individual.network, "ideology.numeric", 
                     ideology.interval.start)

individual.network.narm = individual.network

delete.vertices(individual.network.narm,
                which(is.na(ideology.interval.start)))
    
# matrix form of ideology
ideology = get.vertex.attribute(individual.network.narm, "ideology.numeric")
ideoi = matrix(0,nrow(individual.network.narm[,]),
               nrow(individual.network.narm[,]))
ideoj = ideoi
for(i in 1:length(ideology)){
  for(j in 1:length(ideology)){
    ideoi[i,j] = ideology[i] - 50
    ideoj[i,j] = ideology[j] - 50
  }
}

diag(ideoi) <- 1
diag(ideoj) <- 1

diffun <- function(i,j){
  1.05^(sign(i)*(i-j))*sign((i+1)/(j+1))
}

### Estimate the ERGMs
set.seed(123)
mod = individual.network.narm ~ mutual + 
                                transitiveties +
                                gwidegree(0.5,fixed=T) + 
                                gwodegree(0.5,fixed=T) +
                                edges +
                                nodematch("state") + 
                                nodematch("cd") + 
                                edgecov(abs(ideoi-ideoj)) + 
                                edgecov(ideoi) + 
                                edgecov(ideoj)
simple.homophily.ergm = ergm(mod, 
                             control = control.ergm(MCMC.samplesize = 10000))

set.seed(1234)
mod = individual.network.narm ~ mutual + 
                                transitiveties +
                                gwidegree(0.5,fixed=T) +
                                gwodegree(0.5,fixed=T) +
                                edges +
                                nodematch("state") + 
                                nodematch("cd") + 
                                edgecov(eval(diffun(ideoi,ideoj))) + 
                                edgecov(ideoi) + 
                                edgecov(ideoj)
ideological.hierarchy.ergm = ergm(mod, 
                                  control = control.ergm(MCMC.samplesize = 10000))

set.seed(123)
mod = individual.network.narm ~ mutual + 
                                transitiveties +
                                gwidegree(0.5,fixed=T) +
                                gwodegree(0.5,fixed=T) +
                                edges+nodematch("state") + 
                                nodematch("cd") + 
                                edgecov(abs(ideoi-ideoj)) + 
                                edgecov(ideoi) + 
                                edgecov(ideoj) + 
                                edgecov(eval(diffun(ideoi,ideoj)))
full.ideology.ergm = ergm(mod, control = control.ergm(MCMC.samplesize = 10000))

## Write files to box in dir 'Strategic_Donors/final_paper_data/'
out = list(simple.homophily.ergm = simple.homophily.ergm, 
           ideological.hierarchy.ergm = ideological.hierarchy.ergm, 
           full.ideology.ergm = full.ideology.ergm,
           individual.network.narm = individual.network.narm,
           ideoi = ideoi, ideoj = ideoj, diffun = diffun)
fname = paste0('ergm_results_', ISOLATE_THRESHOLD, '_pval_', P_VALUE, '.RData')
if(!is.null(LOCAL_DATA)) {
    save(out, file = fname)
} else {
    box_write(out, filename = fname, dir_id = '50855821402')
}
    
mple.data.full <- ergmMPLE(full.ideology.ergm$formula)
mple.data.spatial <- ergmMPLE(simple.homophily.ergm$formula)
    
X.full <- mple.data.full$predictor
X.spatial <- mple.data.spatial$predictor
    
# replace with mean control variable values
X.full.control <- X.full
X.spatial.control <- X.spatial
for(i in 1:7){
  X.full.control[,i] <- mean(X.full.control[,i])
  X.spatial.control[,i] <- mean(X.spatial.control[,i])
}

prob.full.control <- 1/(1+exp(-(X.full.control%*%coef(full.ideology.ergm))))
prob.spatial.control <- 1/(1+exp(-(X.spatial.control%*%coef(simple.homophily.ergm))))

full.ideoi <- X.full.control[,"edgecov.ideoi"]
full.ideoj <- X.full.control[,"edgecov.ideoj"]
spatial.ideoi <- X.spatial.control[,"edgecov.ideoi"]
spatial.ideoj <- X.spatial.control[,"edgecov.ideoj"]

full.ideoi.seq <- sort(unique(full.ideoi))
full.ideoj.seq <- sort(unique(full.ideoj))

zmat.full <- matrix(0,length(full.ideoi.seq),length(full.ideoj.seq))
for(i in 1:length(full.ideoi.seq)){
  for(j in 1:length(full.ideoj.seq)){
    zmat.full[i,j] <- mean(prob.full.control[which((full.ideoi==full.ideoi.seq[i]) & (full.ideoj==full.ideoj.seq[j])) ])
  }
}

spatial.ideoi.seq <- sort(unique(spatial.ideoi))
spatial.ideoj.seq <- sort(unique(spatial.ideoj))

zmat.spatial <- matrix(0,length(spatial.ideoi.seq),length(spatial.ideoj.seq))
for(i in 1:length(spatial.ideoi.seq)){
  for(j in 1:length(spatial.ideoj.seq)){
    zmat.spatial[i,j] <- mean(prob.spatial.control[which((spatial.ideoi==spatial.ideoi.seq[i]) & (spatial.ideoj==spatial.ideoj.seq[j])) ])
  }
}

zmat.empirical <- matrix(0,length(spatial.ideoi.seq),length(spatial.ideoj.seq))
for(i in 1:length(spatial.ideoi.seq)){
  for(j in 1:length(spatial.ideoj.seq)){
    response <- mple.data.spatial$response[which((spatial.ideoi==spatial.ideoi.seq[i]) & (spatial.ideoj==spatial.ideoj.seq[j])) ]
    weight <- mple.data.spatial$weights[which((spatial.ideoi==spatial.ideoi.seq[i]) & (spatial.ideoj==spatial.ideoj.seq[j])) ]
    weight <- weight/sum(weight)
    zmat.empirical[i,j] <- sum(response*weight)
  }
}

smooth.empirical <- image.smooth(zmat.empirical,theta=2)
image.plot(spatial.ideoi.seq,spatial.ideoj.seq,smooth.empirical$z,ylab="Receiver Liberalism",xlab="Sender Liberalism")

pval = gsub('\\.', '_', P_VALUE)
plot_file1 <- paste0("paper/figures/spatial_model_tieprob_", ISOLATE_THRESHOLD,
                     "_pval_", pval, ".pdf")
plot_file2 <- paste0("paper/figures/empirical_tieprob_", ISOLATE_THRESHOLD,
                     "_pval_", pval, ".pdf")
plot_file3 <- paste0("paper/figures/full_model_tieprob_", ISOLATE_THRESHOLD,
                     "_pval_", pval, ".pdf")

pdf(file=plot_file1,height=4.5,width=6)
par(las=1,mar=c(4,4,1,4))
image.plot(spatial.ideoi.seq, spatial.ideoj.seq, zmat.spatial, 
           ylab="Receiver Liberalism", xlab="Sender Liberalism")
dev.off()

pdf(file=plot_file2,height=4.5,width=6)
par(las=1,mar=c(4,4,1,4))
image.plot(spatial.ideoi.seq, spatial.ideoj.seq, smooth.empirical$z,
           ylab="Receiver Liberalism", xlab="Sender Liberalism", legend.mar=7)
dev.off()

pdf(file=plot_file3,height=4.5,width=6)
par(las=1,mar=c(4,4,1,4))
image.plot(full.ideoi.seq,full.ideoj.seq,zmat.full,ylab="Receiver Liberalism",xlab="Sender Liberalism")
dev.off()

ergm_table_file <- paste0("paper/tables/ergm_results_table_", ISOLATE_THRESHOLD, 
                          "_pval_", pval, ".tex")
ergm.table = texreg(list(full.ideology.ergm, ideological.hierarchy.ergm, 
                          simple.homophily.ergm), digits=4)
print(ergm.table, file = ergm_table_file)
