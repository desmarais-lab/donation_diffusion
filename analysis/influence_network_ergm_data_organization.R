# code assumes the working directory is set to 'donation_diffusion/analysis'
# and there is a sim link to the Strategic_Donors/Data folder
# run in terminal, the following two lines
#  ln -s /Users/bbd5087/Box\ Sync/Box/Research/donor_networks_netinf/Strategic_Donors/2016_Data_Match  2016_data_match
#  ln -s /Users/bbd5087/Box\ Sync/Box/Research/donor_networks_netinf/Strategic_Donors/Data  data
netinf.results.files <- dir("../data/results")

netinf.file <- "netinf_threshold_10_iter_7.RData"

source('../data_processing/remove_isolates.R')

# load the netinf results
load(paste("../data/results/",netinf.file,sep=""))

isolate_threshold = 10

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
donor.edgelist <- cbind(out$netinf_out$origin_node,out$netinf_out$destination_node)

# node-level data for netinf
# netinf.data <- out$netinf_out$data

# master list of nodes included in netinf
# uniqueDonors <- unique(as.character(netinf.data$Donor_ID))
# the following assumes no isolates
uniqueDonors <- unique(c(donor.edgelist))

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

save(list=c("influence.network","net.vertex.data"),file="influence.network.RData")
rm(list=ls())
gc()
