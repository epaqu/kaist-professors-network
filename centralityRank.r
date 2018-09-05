library(sna)
library(igraph)

setwd("/Users/shaun/desktop/codes/centralityRanking_SC")
tab=read.csv("ex_total_150408.csv", header=T, sep=",")
rownames(tab, do.NULL = TRUE, prefix = "row")
rownames(tab)=colnames(tab); mia
options(digits=3)

## Eigenvector Centrality ##
eig=eigen(tab)
eigRaw=eig$vectors[,which.max(eig$values)]*-1
eigTable=matrix(eigRaw,nrow=1,byrow=T)
rownames(eigTable)=c("eigenVector_centrality")
colnames(eigTable)=c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p")
t(eigTable)

## Network Visualization: http://www.shizukalab.com/toolkits/sna/weighted-edges ##
setwd("/Users/shaun/desktop/codes/centralityRanking_SC")
status=read.csv("status.csv", header=T, sep=",")

m=as.matrix(tab)
net=graph.adjacency(m, mode="undirected", weighted=TRUE, diag=FALSE); summary(net)

E(net)$weight
V(net)$status=as.character(status$status[match(V(net)$name,status$name)])
V(net)$color=V(net)$status
V(net)$color=gsub("phd","red",V(net)$color)
V(net)$color=gsub("ms","yellow",V(net)$color)
V(net)$color=gsub("else","green",V(net)$color)

plot.igraph(net, vertex.size=15, vertex.frame.color="black", vertex.label=V(net)$name, vertex.label.color="blue",
            vertex.label.cex=1.5, layout=layout.fruchterman.reingold, edge.color="black", edge.width=2*E(net)$weight)