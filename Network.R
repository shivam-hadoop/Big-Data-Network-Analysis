#Initializing Pre installed Libraries
library(readxl)
library(igraph)

#Set working directory
setwd("/Users/shivam/Downloads/epgdaba/network project/Network Analysis in Big Data")
#Reading the file
big_data <-read_excel("Dataset.xlsx")

#Converting the file to adjacency matrix
bd <- as.matrix(big_data)
rownames(bd) <- colnames(bd)

#Creating a Graph 
bigdata_network <- graph_from_adjacency_matrix(bd, mode="undirected", weighted = NULL)

#Plotting the Graph
plot(bigdata_network, 
     edge.arrow.size=.3, edge.curved=0.1, 
     vertex.size=2,margin=-0.20, width=10,
     vertex.label.cex=0.5, 
     vertex.label.dist=-0.37, 
     vertex.label.color="black",vertex.color="Orange",vertex.frame.color="gray" )

#Now we will find node specific centralities

#Degree Centrality

Degree<- degree(bigdata_network, mode="all")

plot(bigdata_network, edge.arrow.size=.3,edge.curved=0.1,vertex.size=Degree,
     vertex.color="SkyBlue", vertex.label=NA,
     vertex.frame.color="gray" )

write.csv(Degree, file = "degree.csv",quote = FALSE)

#Betweenness
Betweenness <- betweenness(bigdata_network)

plot(bigdata_network, edge.arrow.size=.3, vertex.size=0.03*Betweenness,
     vertex.color="SkyBlue", vertex.label=NA,
     vertex.frame.color="gray" )

write.csv(Betweenness, file = "betweenness.csv")

#Closeness

Closeness <- closeness(bigdata_network)

plot(bigdata_network, edge.arrow.size=.3, vertex.size=700*Closeness,
     vertex.color="SkyBlue", vertex.label=NA,
     vertex.frame.color="gray" )

write.csv(Closeness, file = "closeness.csv")

#Eigenvector

Eigen <- eigen_centrality(bigdata_network)$vector

plot(bigdata_network, edge.arrow.size=.3, vertex.size=10*Eigen,
     vertex.color="SkyBlue", vertex.label=NA,
     vertex.frame.color="gray" )

write.csv(Eigen, file = "eigen_centrality.csv")

#Average Path length
path <- average.path.length(bd_n)
path
