library(igraph)
library(pvclust)


#increment
inc <- function(x){
eval.parent(substitute(x <- x + 1)) }

#plotting
data <- read.csv("Data Frame.csv")
mtx <- read.csv("Data yes Loop.csv")
class <- mtx$Class
dpt <- mtx$Department
mtx$X = NULL
mtx$Department <- NULL
mtx$Class <- NULL
graph <- graph.data.frame(data, directed=FALSE)
igraph.options(vertex.size=7, layout=layout.fruchterman.reingold)
tkplot(graph)

#hierarchical clustering
D <- dist(t(mtx), method = "euclidean")
Dclust <- hclust(d, method="ward.D") 
plot(Dclust)
groups <- cutree(Dclust, k=7)
rect.hclust(Dclust, k=7, border="red")

#pvclustering
pvc <- pvclust(mtx, method.hclust="ward.D", method.dist="euclidean")
plot(pvc)
pvrect(pvc, alpha=.95, border="red")


#kmeans
results <- kmeans(mtx, 7)
table(class, results$cluster)


#coloring
V(graph)$color <- "white"
V(graph)[9]$color <- "red"
V(graph)[2]$color <- "red"
V(graph)[7]$color <- "red"
V(graph)[3]$color <- "red"
V(graph)[36]$color <- "red"
V(graph)[54]$color <- "orange"
V(graph)[4]$color <- "blue"
V(graph)[38]$color <- "black"

