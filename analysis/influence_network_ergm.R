# path to data-- ./Data/results
# file name is threshold_#_iter_#
# want to use max iteration

# code assumes the working directory is set to Strategic_Donors
# load the netinf results



load("influence.network.RData")
set.vertex.attribute(influence.network,"ideology",as.character(net.vertex.data$ideology))

set.vertex.attribute(influence.network,"cd",as.character(net.vertex.data$ind_cd))

set.vertex.attribute(influence.network,"state",as.character(net.vertex.data$ind_state))

save(list="influence.network",file="influence.network.RData")

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

save(list=c("simple.homophily.ergm","full.ideology.ergm","ideological.hierarchy.ergm"),file="ergm_results.RData")

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

pdf(file='spatial_model_tieprob.pdf',height=4.5,width=6)
par(las=1,mar=c(4,4,1,4))
image.plot(spatial.ideoi.seq,spatial.ideoj.seq,zmat.spatial,ylab="Receiver Liberalism",xlab="Sender Liberalism")
dev.off()

pdf(file='empirical_tieprob.pdf',height=4.5,width=6)
par(las=1,mar=c(4,4,1,4))
image.plot(spatial.ideoi.seq,spatial.ideoj.seq,smooth.empirical$z,ylab="Receiver Liberalism",xlab="Sender Liberalism",legend.mar=7)
dev.off()

pdf(file='full_model_tieprob.pdf',height=4.5,width=6)
par(las=1,mar=c(4,4,1,4))
image.plot(full.ideoi.seq,full.ideoj.seq,zmat.full,ylab="Receiver Liberalism",xlab="Sender Liberalism")
dev.off()

library(texreg)
ergm.table <- texreg(list(full.ideology.ergm,ideological.hierarchy.ergm,simple.homophily.ergm),digits=4)
print(ergm.table, file="ergm_table.tex")








