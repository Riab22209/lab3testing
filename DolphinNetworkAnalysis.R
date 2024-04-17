#import the library we need
library(igraph)
library(readxl)

#Read the dolphins.gml file and plot the network
setwd('C:/Users/USER/Desktop/dolphins_network/dolphins')
G=read_graph(file='Dolphins.gml', format="gml")
plot(G)


png("dolphine.png", width = 1500, height = 1500)

dev.off()

#Count the number of nodes
print(paste("Number of nodes in the network: ",gorder(G)))

#Count the number of edges
print(paste("Number of edges in the network: ",gsize(G)))

#Convert a graph to the adjacency matrix
A=as_adjacency_matrix(G)
print(A)
graph_from_adjacency_matrix(
  A,
  mode = c("directed", "undirected", "max", "min", "upper", "lower", "plus"),
  weighted = NULL,
  diag = TRUE,
  add.colnames = NULL,
  add.rownames = NA
)

#check for acyclic
ifelse(is_dag(G)==TRUE,"ACYCLIC NETWORK","CYCLIC NETWORK")

#check for planarity
#Install RBGL 
if (!require("BiocManager", quietly = TRUE))
  #install.packages("BiocManager")
BiocManager::install("RBGL")
#Use the RBGL library
library(RBGL)

G1 <- as_graphnel(G) # Convert igraph object to graphNEL object for planarity testing
boyerMyrvoldPlanarityTest(G1)

#Diameter of the network
print(paste("Diameter of the network : " ,diameter(G, directed = FALSE))) #Infinite values are neglected 

#Is it a connected network
print(paste("Is it a connected network : ",is_connected(G)))

#using a for loop to obtain the lengths of the shortest paths across each pair of nodes
s_p_len<-vector(mode="list", length=gorder(G))
for (i in 1: gorder(G))
  {
  s_p_len[[i]]<-shortest.paths(G,i)
}

#components of the network 
components(G)


#The density of network G
print(paste("The density of G is ", edge_density(G, loops=FALSE)))

#Degree of the network 
degrees=degree(G,V(G),loops=FALSE)
print(degrees)

#Compute the mean degree of G
print(paste("The mean degree of G is ", sum(degree(G, loops=FALSE))/length(V(G))))

#degree centrality

# degree distributions of G

G_degree_distriution<-data.frame(degree=0:max(degree(G, loops=FALSE)), 
                                  freq=degree_distribution(G, cumulative = FALSE, loops=FALSE))

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
barplot(G_degree_distriution$freq,
        main = "G",
        xlab = "degree",
        ylab = "frequency",
        ylim = c(0,0.4),
        names.arg = as.character(G_degree_distriution$degree),
        col = "darkred",
        horiz = FALSE)

dev.off()

closen =closeness(G)
between=betweenness(G)   

df <- data.frame(node=V(G)$label, degree=degrees, closeness=closen, betweenness=between)

print(paste("The nodes with maximum degrees is/are ", max(degrees), 
            "The maximum degree is", V(G)$label[which(degrees==max(degrees))]))
print(paste("The nodes with maximum closeness centrality is/are ", max(closen), 
            "The maximum closeness centrality is", V(G)$label[which(closen==max(closen))]))
print(paste("The nodes with maximum betweenness centrality is/are ", max(between), 
            "The maximum betweenness centrality is", V(G)$label[which(between==max(between))]))

cliques(G)

#Find k-cores
core=coreness(G)
index=which(core==max(core))
color=rep("cyan", length=length(V(G)))
color[index]="red"
V(G)$color=color

png("dolphin1.png", width = 1500, height = 1500)
plot(G,vertex.size=5)
dev.off()

#Global clustering coefficient
transitivity(G, type="global")
#Local clustering coefficient
transitivity(G, type="local")
#Average clustering coefficient
mean(transitivity(G, type="local"))
