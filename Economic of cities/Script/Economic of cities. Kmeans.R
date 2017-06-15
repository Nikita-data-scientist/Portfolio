library(cluster)
library(ggplot2)

cities <- read.delim("Econom_Cities_data.txt", header = T,   
                     sep = "", dec = ",")

#Объявление прощунных значений
cities$Work[cities$Work == -9999] <- NA
cities$Salary[cities$Salary == -9999] <- NA

#Сума пропущеных значений
sum(is.na(cities))
#Очистка
cities <- na.omit(cities)
#Структура
str(cities)

#Нормализация
normalize.cities <- (cities[, 2:4] - min(cities[, 2:4])) / max(cities[, 2:4]) - min(cities[, 2:4])

wss <- (nrow(cities[, 2:4])-1)*sum(apply(cities[, 2:4],2,var))
for (i in 2:15) wss[i] <- kmeans(cities[, 2:4], 
                                 centers=i)$tot.withinss                                 
plot(1:15, wss, type="b", xlab="Clusters nomber",
     ylab="Within groups sum of squares")


#Кластерный анализ
cities.kmeans <- kmeans(normalize.cities, centers = 3, iter.max = 100)

summary(cities.kmeans)

#В какой кластер попал каждый город
table(cities$City, cities.kmeans$cluster)
#Центры кластеров
t(cities.kmeans$centers)

cities$cluster <- cities.kmeans$cluster


cities.dist <- dist(normalize.cities)
cities.mds <- cmdscale(cities.dist)

#Визуализация 
plot(cities_mds, col = cities.kmeans$cluster, xlab = "Index", ylab = "Y")

clusplot(cities, cities.kmeans$cluster,
         color = T, shade = T,
         labels = 2, lines = 0)

#Визуализация в ggplot2
ggplot(cities, aes(Price, Work, col = factor(cities$cluster))) + 
  geom_point(aes(shape = factor(cities$cluster))) +
  geom_text(aes(label = cities$City))







