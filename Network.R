#Initializing Pre installed Libraries

library(corrplot)
library(readxl)
library(igraph)

#Set working directory
setwd("/Users/shivam/git_repos/network-analysis-big-data/")
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
     vertex.label.cex=0.9, 
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

#Calculating Correlations
correlation <-read.csv("Correlation.csv")
z<-cor(x=correlation)

#Drawing Correlogram

corrplot(z,type = "upper", order = "hclust", t1.col = "black", t1.srt = 45)

# Drawing scatter plots

pairs(z[,1:4],pch=19,horInd=10)


#Part 3
l <- layout_with_fr(bigdata_network)
plot(bigdata_network,layout = l,   
     vertex.size=2,margin=-0.20, width=10,
     vertex.label.cex=0.9, 
     vertex.label.dist=-0.37)

Print("Empirical Network")

deg_cen <- centr_degree(bigdata_network)$centralization
deg_clo <- centr_clo(bigdata_network)$centralization
deg_bet <- centr_betw(bigdata_network)$centralization
deg_eig <- centr_eigen(bigdata_network)$centralization

print(deg_cen)
print(deg_clo)
print(deg_bet)
print(deg_eig)

print("Mean Measures")

print(mean(Degree))
print(mean(Closeness))
print(mean(Betweenness))
print(mean(Eigen))

cat("Average Path Length",average.path.length(bigdata_network))

# Barabasi-Albert Model
print("Barabasi-Albert Model")

barabasi <- sample_pa(n=50, power = 1, m = NULL, 
                      out.pref = FALSE, zero.appeal = 1, directed = FALSE,
                      algorithm ="psumtree", start.graph = NULL)

plot(barabasi, vertex.label= NA, vertex.size = 0.5)

print(centr_degree(barabasi)$centralization)
print(centr_clo(barabasi)$centralization)
print(centr_betw(barabasi)$centralization)
print(centr_eigen(barabasi)$centralization)

print(mean(degree(barabasi)))
print(mean(closeness(barabasi)))
print(mean(betweenness(barabasi)))
print(mean(eigen_centrality(barabasi)$vector))

print(average.path.length(barabasi))

# Small World Model
small_world <- watts.strogatz.game(dim=1, size = 50, nei = 1,p = 0.1, loops = FALSE, multiple = FALSE)
plot(small_world, vertex.label= NA,vertex.size = 0.5)

circle <- layout_in_circle(bigdata_network)
plot(bigdata_network, layout=circle, vertex.size = 0.5, vertex.label=NA)

print(centr_degree(small_world)$centralization)
print(centr_clo(small_world)$centralization)
print(centr_betw(small_world)$centralization)
print(centr_eigen(small_world)$centralization)

print(mean(degree(small_world)))
print(mean(closeness(small_world)))
print(mean(betweenness(small_world)))
print(mean(eigen_centrality(small_world)$vector))

cat("Average Path Length",average.path.length(small_world))

# Random Network

# No of nodes = 50
# No of edges = 135
n <- 50
m <- gsize(bigdata_network)
ran.graph <- erdos.renyi.game(n, m, type="gnm", directed = FALSE)
plot(ran.graph, vertex.label= NA, vertex.size = 0.5)

# Simulation
# Generate 100 random graphs of the same size
g.deg.cen <- c()
g.clo.cen <- c()
g.bet.cen <- c()
g.eig.cen <- c()
avg.deg.cen <- c()
avg.clo.cen <- c()
avg.bet.cen <- c()
avg.eig.cen <- c()

hundred_random <- function()
{
        ran.graph <- sample_gnm(n, m, directed = FALSE)
        
        deg.cen <<- c(g.deg.cen, centr_degree(ran.graph)$centralization)
        clo.cen <<- c(g.clo.cen, centr_clo(ran.graph)$centralization)
        bet.cen <<- c(g.bet.cen, centr_betw(ran.graph)$centralization)
        eig.cen <<- c(g.eig.cen, centr_eigen(ran.graph)$centralization)
        
        avg.deg.cen <<- c(avg.deg.cen, mean(degree(ran.graph)))
        avg.clo.cen <<- c(avg.clo.cen, mean(closeness(ran.graph)))
        avg.bet.cen <<- c(avg.bet.cen, mean(betweenness(ran.graph)))
        avg.eig.cen <<- c(avg.eig.cen, mean(eigen_centrality(ran.graph)$vector))
        
}

replicate(100,hundred_random())

rg.deg.cen <- mean(deg.cen)
rg.clo.cen <- mean(clo.cen)
rg.bet.cen <- mean(bet.cen)
rg.eig.cen <- mean(eig.cen)

print(rg.deg.cen)
print(rg.clo.cen)
print(rg.bet.cen)
print(rg.eig.cen)


print(mean(avg.deg.cen))
print(mean(avg.clo.cen))
print(mean(avg.bet.cen))
print(mean(avg.eig.cen))

cat("Average Path Length",average.path.length(ran.graph))
