#기타
for (i in 1:130)
for (j in 1:93)
if (temp[j,1] == data[i,1])
for (k in 1:93)
if (table[1,k] == data[i,2])
{
count <- as.numeric(table[j,k])
inc(count)
table[j,k] <- count
}



for (i in 2:93)
for (j in 2:93)
if (table[i,j] == 1)
table[j,i] <- 1




auto <- read.csv("Data Table.csv")


for (i in 1:93)
if (auto[i,24] == 1)
auto[i,2] <- c("College of Engineering - EE")



# increment

inc <- function(x)
{
 eval.parent(substitute(x <- x + 1))
}




# 방향성 제거 방법

setwd("C:/Users/Jaewoo/Desktop/")
temp <- read.csv("Table.csv", header=FALSE, stringsAsFactors=FALSE)
for (i in (2:184)) {
for (j in (2:i)) {
if (i != j) {
temp[i,j] <- as.numeric(temp[i,j]) + as.numeric(temp[j,i])
temp[j,i] <- temp[i,j]
}
if (i == j) {
temp[i,j] <- as.numeric(temp[i,j])
}
}}
write.csv(temp, "New.csv")



# 네트워크 그리는 법

library(igraph)
library(statnet)
library(ggplot2)
setwd("C:/Users/Jaewoo/Desktop/")
mtx <- read.csv("binary.csv", header=FALSE, stringsAsFactors=FALSE)
network <- graph.data.frame(mtx, directed=FALSE)
plot(network)




# 0과 1 로만 나타내는 법

setwd("C:/Users/Jaewoo/Desktop/")
temp <- read.csv("New.csv", header=FALSE, stringsAsFactors=FALSE)
for (i in (2:184)) {
for (j in (2:184)) {
if (as.numeric(temp[i,j]) <= 1) {
temp[i,j] <- as.numeric(0) }
if (as.numeric(temp[i,j]) > 1) {
temp[i,j] <- as.numeric(1) }
}}
write.csv(temp, "binary.csv")




# 네트워크 그리는 법 추가

temp <- read.csv(".csv", header=FALSE, stringsAsFactors = FALSE,row.names=1)
a = matrix(0,0,0)
for (i in (2:184)) {
for (j in (2:i)) {
if (as.numeric(temp[i,j]) != 0 & i != j) 
a = c(a, temp[i,1], temp[1,j])
}}
a <- matrix(a, 2)

result <- graph.data.frame(t(a), directed=TRUE)
plot(result)


#인터렉티브 네트워크
tkplot(result)




# Binary Data Frame 으로 네트워크 그리는 법 추가

mtx <- read.csv("Binary Data Frame.csv", row.names=1)
View(mtx)
result <- graph.data.frame(mtx, directed=TRUE)
plot(result)





# 데이터 분석법

Data = read.csv("exp.csv")
Data.features = Data
Data.features$Class = NULL
Data.features$Department = NULL
results <- kmeans(Data.features, 7)
table(Data$Class, results$cluster)





# 미디언 계산

for (i in 1:183)
for (j in 1:183) {
if (Data.features[i,j] != 0)
list <- c(list, Data.features[i,j])
}
list
median(list)



# 미디언값이 1일때 정리.

for (i in (1:183)) {
for (j in (1:183)) {
if (Data.features[i,j] <= 1) {
Data.features[i,j] <- 0 }}}
View(Data.features)



# 루프 삭제

for (i in (1:183))
for (j in (1:183))
if (i == j)
Data.features[i,j] <- 0




# 색깔 입히기



dataFrame <- read.csv(file="/Users/jungeunyoo/Downloads/Desktop/Data_Frame.csv", header=TRUE, fileEncoding="EUC-KR")
dataFrame$X = NULL
network <- graph.data.frame(dataFrame, directed=FALSE)
mat = as.matrix(dataFrame)


##initialize count##
for (j in 1:92){
	V(network)$count[j] <- 0
}

for (i in 1:92){
	#print (V(network)$name[i])
	for (j in 1:128){
		if (V(network)$name[i] == mat[j,2]){
			V(network)$count[i] <- V(network)$count[i] + 1
		}
	}
	
}


V(network)[count == 1]$color <- "cyan"
V(network)[count == 3]$color <- "green"
V(network)[count > 5]$color <- "blue"
V(network)[count > 7]$color <- "red"









# 크기 바꾸기

loop <- read.csv(file="/Users/jungeunyoo/Downloads/Desktop/wd.csv", head=TRUE, fileEncoding="EUC-KR")

loop$Class = NULL
lmat = as.matrix(loop)

##initialize loop count##
for (j in 1:92){
	V(network)$lcount[j] <- 0
}

for (i in 1:92){
	for (j in 1:183){
		if (V(network)$name[i] == lmat[j,1]){
			V(network)$lcount[i] <- lmat[j,j+1]
		}
	}
	
}
#remove unnecessary ones

for (i in 1:92){
	if (V(network)$lcount[i] == "1" || is.na(V(network)$lcount[i])){
		V(network)$lcount[i] <- 0
	}
}
V(network)[lcount < 10]$size <- 7
V(network)[lcount >= 10]$size <- 8
V(network)[lcount >= 20]$size <- 9

tkplot(network)
'






#클러스터링 1
mtx <- read.csv("Data Truth.csv")
mtx$X <- NULL
mtx$Class <- NULL
mtx$Department <- NULL
mtx <- t(mtx)
d <- dist(mtx, method ="euclidean")
fit <-hclust(d, method="ward.D")
plot(fit)
rect.hclust(fit, k=7, border="red")
