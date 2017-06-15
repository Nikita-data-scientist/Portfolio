europe <- read.csv("europe.csv", header = T, sep = ",", dec = ".")

head(europe)

sum(is.na(europe))

europe.2 <- europe[, 2:8]

min <- apply(europe.2, 2, min)
max <- apply(europe.2, 2, max)
scale.europe <- scale(europe.2, center = min, scale = max - min)

options(digits = 3)

clust.europe <- hclust(dist(scale.europe), "ward.D")
clust.europe

plot(clust.europe, labels = europe$Country)
plot(clust.europe, hang = -1, labels = europe$Country)
rect.hclust(clust.europe, k = 7, border = "red")

#Разделение странн на группы
groups <- cutree(clust.europe, k = 7)
groups

#Процент в каждом кластере
colMeans(scale.europe[groups==1,1:7])*100
colMeans(scale.europe[groups==2,1:7])*100
colMeans(scale.europe[groups==3,1:7])*100
colMeans(scale.europe[groups==4,1:7])*100
colMeans(scale.europe[groups==5,1:7])*100
colMeans(scale.europe[groups==6,1:7])*100
colMeans(scale.europe[groups==7,1:7])*100
