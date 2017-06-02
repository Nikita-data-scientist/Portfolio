country <- read.csv("zzz.csv", header = T, sep = ";", dec = ",")
country_2 <- country[, 2:10]


min <- apply(country_2, 2, min)
max <- apply(country_2, 2, max)
scale_country <- scale(country_2, center = max, scale = max - min)


dist_country <- dist(scale_country)

clust_country <- hclust(dist_country, "ward.D")
clust_country

plot(clust_country, hang = -1, labels = country$Country, 
     main = "Country cluster")
rect.hclust(clust_country, k=5, border="red")

groups <- cutree(clust_country, k = 5)
groups