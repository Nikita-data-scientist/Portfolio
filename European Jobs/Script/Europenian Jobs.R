library(ggplot2)
library(cluster)

europenian <- read.csv("European Jobs_data.csv", header = T, sep = ";", dec = ".")

#Опредиление числа кластеров
wss <- (nrow(europenian[, 2:10])-1)*sum(apply(europenian[, 2:10],2,var))
for (i in 2:15) wss[i] <- kmeans(europenian[, 2:10], 
                                 centers=i)$tot.withinss
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


str(europenian)

normalize <- (europenian[, 2:10] - min(europenian[, 2:10]))/max(europenian[, 2:10])-min(europenian[, 2:10])


europenian.kmeans <- kmeans(normalize, centers = 3, iter.max = 100)
europenian.kmeans2 <- kmeans(normalize, centers = 2, iter.max = 100)
names(europenian.kmeans)
options(digits = 2)


t(europenian_kmeans$centers)

europenian.dist <- dist(europenian[, 2:10])
europenian.mds <- cmdscale(europenian.dist)

plot(europenian.mds, col = europenian.kmeans$cluster, xlab = "Index", ylab = "Y")
plot(europenian.mds, col = europenian.kmeans2$cluster, xlab = "Index", ylab = "Y")

table(europenian.kmeans$cluster,europenian.kmeans2$cluster)

clusplot(europenian, europenian.kmeans$cluster,
         color = T, shade = T,
         labels = 2, lines = 0, 
         main = "Кластеры")

clusplot(europenian, europenian.kmeans2$cluster,
         color = T, shade = T,
         labels = 2, lines = 0, 
         main = "Кластеры")

#Сравнение 2 кластеризаций
table(europenian.kmeans$cluster, europenian.kmeans2$cluster)
