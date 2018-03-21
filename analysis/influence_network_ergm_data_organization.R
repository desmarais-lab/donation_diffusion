## Packages
dir.create(Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
install.packages("foreach", Sys.getenv("R_LIBS_USER"), repos = "https://cran.cnr.berkeley.edu/")
install.packages("doParallel", Sys.getenv("R_LIBS_USER"), repos = "https://cran.cnr.berkeley.edu/")
library(foreign,lib.loc=Sys.getenv("R_LIBS_USER"))
library(foreach,lib.loc=Sys.getenv("R_LIBS_USER"))
library(doParallel,lib.loc=Sys.getenv("R_LIBS_USER"))


registerDoParallel(cores = 20)

# find all thresholds...
netinf.results.files <- dir("../data/results")

file.parts <- gsub(".RData","",netinf.results.files)
file.parts <- do.call('rbind',strsplit(file.parts,"_"))
threshold <- as.numeric(file.parts[,3])
iteration <- as.numeric(file.parts[,5])
max.iters <- c(by(iteration,threshold,max))
files.to.analyze <- paste("netinf_threshold_",names(max.iters),"_iter_",max.iters,".RData",sep="")
thresholds <- as.numeric(names(max.iters))

# loop over 0.01, 0.05, 0.10, 0.25 p-vals
all.p.val <- c(0.01,0.05,0.10,0.25)

all.combinations <- expand.grid(files.to.analyze,all.p.val)

all.combinations <- paste(all.combinations[,1],"_",all.combinations[,2],sep="")

# code assumes the working directory is set to 'donation_diffusion/analysis'
# and there is a sim link to the Strategic_Donors/Data folder
# run in terminal, the following two lines
#  ln -s /Users/bbd5087/Box\ Sync/Box/Research/donor_networks_netinf/Strategic_Donors/2016_Data_Match  2016_data_match
#  ln -s /Users/bbd5087/Box\ Sync/Box/Research/donor_networks_netinf/Strategic_Donors/Data  data


netinf.ergm <- function(netinf.file_p.val){

p.val <- as.numeric(strsplit(netinf.file_p.val,"_")[[1]][6])

netinf.file <- paste(strsplit(netinf.file_p.val,"_")[[1]][1:5],collapse="_")  
  
source('../data_processing/remove_isolates.R')

# load the netinf results
load(paste("../data/results/",netinf.file,sep=""))

isolate_threshold <- as.numeric(strsplit(netinf.file,"_")[[1]][3])

# Read the preprocessed data (see `make_netinf_data.R` for details)
cat('threshold: ', isolate_threshold, '\n')
df <- read_csv('../data/data_for_netinf.R')
df <- remove_isolates(df, isolate_threshold)

cat('Number of donors', length(unique(df$Donor_ID)), '\n')

# load in vertex level data
vertex.data <- read.csv("../2016_data_match/VLC_16_full.csv",stringsAsFactors=F)

## Zack's notes
# - activist - some secret model of “likelihood to take an action to advance a progressive cause”, so this may not be a good measure of general political activism; 0 - 100 (bins of 10; high values = higher activism relative to others, not a probability)

#- age - bins of 5 years

#- hhwealth - model of household total wealth, random bins

#- ideology - how liberal? 0 to 100 (bins of 5; 95-100 = most liberal)

#- income - individual income, random bins

#- partisanship - predicted probability of identifying as Dem, 0 - 100 (bins of 5)

#- pid - party ID, text

#- race - text

#These variable strings are pretty messy right now (e.g. weird characters, categorical variables), so I can change them to ordered numeric variables if you’d like - just let me know the best format for that.

# Extract vertex id
vertex.id <- as.character(vertex.data$Actor_ID)

# edgelist from netinf
donor.edgelist <- cbind(out$netinf_out$origin_node,out$netinf_out$destination_node)[1:min(which(out$netinf_out$p_value > p.val)),]

# node-level data for netinf
# netinf.data <- out$netinf_out$data

# master list of nodes included in netinf
# uniqueDonors <- unique(as.character(netinf.data$Donor_ID))
# the following assumes no isolates
uniqueDonors <- unique(df$Donor_ID)

# throw out vertex data for nodes not included in the network
net.vertex.data <- vertex.data[which(is.element(vertex.id,uniqueDonors)),]
nrow(net.vertex.data)/length(uniqueDonors)
# looks like all nodes in the netinf data can be found in the vertex id data

# construct network object
library(network)
influence.network <- network.initialize(nrow(net.vertex.data))
network.vertex.names(influence.network) <- as.character(net.vertex.data$Actor_ID)
edges <- as.matrix(cbind(as.character(donor.edgelist[,1]),as.character(donor.edgelist[,2])))
network.ids <- as.character(net.vertex.data$Actor_ID)
for(i in 1:nrow(edges)){
	sender <- which(network.ids==edges[i,1])
	receiver <- which(network.ids==edges[i,2])
	influence.network[sender,receiver] <- 1
	if(i/500==round(i/500)) print(i)
}

rm(list="vertex.data")

save(list=c("influence.network","net.vertex.data"),file="../data/influence.network.RData")
rm(list=ls())
gc()

######## Finished data merging ##########
########## Now ERGM analyses ############



# path to data-- ./Data/results
# file name is threshold_#_iter_#
# want to use max iteration

# code assumes the working directory is set to Strategic_Donors
# load the netinf results



load("../data/influence.network.RData")
set.vertex.attribute(influence.network,"ideology",as.character(net.vertex.data$ideology))

set.vertex.attribute(influence.network,"cd",as.character(net.vertex.data$ind_cd))

set.vertex.attribute(influence.network,"state",as.character(net.vertex.data$ind_state))

save(list="influence.network",file="../data/influence.network.RData")

# extract individual network
individual.network <- influence.network
delete.vertices(individual.network, which(as.character(net.vertex.data$Ent_Typ) != "IND"))

ideology.interval <- get.vertex.attribute(individual.network,"ideology")
ideology.interval[which(ideology.interval=="")] <- "50.01–55.0"
ideology.interval.start <- as.numeric(substr(ideology.interval,1,2))
set.vertex.attribute(individual.network,"ideology.numeric",ideology.interval.start)

individual.network.narm <- individual.network
delete.vertices(individual.network.narm,which(is.na(ideology.interval.start)))

# matrix form of ideology
ideology <- get.vertex.attribute(individual.network.narm,"ideology.numeric")
ideoi <- matrix(0,nrow(individual.network.narm[,]),nrow(individual.network.narm[,]))
ideoj <- ideoi
for(i in 1:length(ideology)){
  for(j in 1:length(ideology)){
    ideoi[i,j] <- ideology[i] - 50
    ideoj[i,j] <- ideology[j] - 50
  }
}

diag(ideoi) <- 1
diag(ideoj) <- 1


diffun <- function(i,j){
  1.05^(sign(i)*(i-j))*sign((i+1)/(j+1))
}

### Estimate the ERGMs
set.seed(123)
simple.homophily.ergm <- ergm(individual.network.narm~mutual+transitiveties+gwidegree(0.5,fixed=T)+gwodegree(0.5,fixed=T)+edges+nodematch("state")+nodematch("cd")+edgecov(abs(ideoi-ideoj))+edgecov(ideoi)+edgecov(ideoj), control=control.ergm(MCMC.samplesize=10000))

set.seed(123)
ideological.hierarchy.ergm <- ergm(individual.network.narm~mutual+transitiveties+gwidegree(0.5,fixed=T)+gwodegree(0.5,fixed=T)+edges+nodematch("state")+nodematch("cd")+edgecov(eval(diffun(ideoi,ideoj)))+edgecov(ideoi)+edgecov(ideoj), control=control.ergm(MCMC.samplesize=10000))

set.seed(123)
full.ideology.ergm <- ergm(individual.network.narm~mutual+transitiveties+gwidegree(0.5,fixed=T)+gwodegree(0.5,fixed=T)+edges+nodematch("state")+nodematch("cd")+edgecov(abs(ideoi-ideoj))+edgecov(ideoi)+edgecov(ideoj)+edgecov(eval(diffun(ideoi,ideoj))), control=control.ergm(MCMC.samplesize=10000))

ergm_results_file <- paste("../data/ergm_results/ergm_results_threshold_",isolate_threshold,"_pval_",p.val,".RData",sep="")

save(list=c("simple.homophily.ergm","full.ideology.ergm","ideological.hierarchy.ergm"),file=ergm_results_file)

mple.data.full <- ergmMPLE(full.ideology.ergm$formula)
mple.data.spatial <- ergmMPLE(simple.homophily.ergm$formula)

X.full <- mple.data.full$predictor
X.spatial <- mple.data.spatial$predictor
# 1 through 7 are non-ideology variables

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

library(fields)

plot_file1 <- paste("../data/ergm_results/spatial_model_tieprob_",isolate_threshold,"_pval_",p.val,".pdf",sep="")
plot_file2 <- paste("../data/ergm_results/empirical_tieprob_",isolate_threshold,"_pval_",p.val,".pdf",sep="")
plot_file3 <- paste("../data/ergm_results/full_model_tieprob_",isolate_threshold,"_pval_",p.val,".pdf",sep="")


pdf(file=plot_file1,height=4.5,width=6)
par(las=1,mar=c(4,4,1,4))
image.plot(spatial.ideoi.seq,spatial.ideoj.seq,zmat.spatial,ylab="Receiver Liberalism",xlab="Sender Liberalism")
dev.off()

pdf(file=plot_file2,height=4.5,width=6)
par(las=1,mar=c(4,4,1,4))
image.plot(spatial.ideoi.seq,spatial.ideoj.seq,smooth.empirical$z,ylab="Receiver Liberalism",xlab="Sender Liberalism",legend.mar=7)
dev.off()

pdf(file=plot_file3,height=4.5,width=6)
par(las=1,mar=c(4,4,1,4))
image.plot(full.ideoi.seq,full.ideoj.seq,zmat.full,ylab="Receiver Liberalism",xlab="Sender Liberalism")
dev.off()

ergm_table_file <- paste("../data/ergm_results/ergm_results_table_",isolate_threshold,"_pval_",p.val,".tex",sep="")


library(texreg)
ergm.table <- texreg(list(full.ideology.ergm,ideological.hierarchy.ergm,simple.homophily.ergm),digits=4)
print(ergm.table, file=ergm_table_file)

}



stopImplicitCluster()




