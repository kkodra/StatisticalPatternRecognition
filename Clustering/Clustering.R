# Clustering of nutrient data
# Nutrient data provided. Also available from R libraries

# Set directory first.
# setwd("C:/Users/...")

library(ggplot2)
library(cluster)

nutrient <- read.csv(file="nut1.csv",row.names=1)

# Scale nutrient data
nutrient.scaled <- scale(nutrient)

# Check head of scaled data
head(nutrient.scaled,4)

d <- dist(nutrient.scaled)
as.matrix(d)[1:4,1:4]

# Pair plot of data
dev.new()
pairs(nutrient.scaled)

# Cluster using dendrogram and plot the data
fit.dendro <- hclust(d, method="average")
plot(fit.dendro, hang=-1, cex=0.8, main="Average Linkage Clustering")

# Cluster using k-means
fit.kmeans_d5 <- kmeans(nutrient.scaled, 3) # 5 cluster solution

dissE <- daisy(nutrient.scaled) 
dE2   <- dissE^2
sk2   <- silhouette(fit.kmeans_d5$cl, dE2)
dev.new()
plot(sk2)


