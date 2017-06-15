library(randomForest)

wine <- read.table("Wine.txt", header=T, sep="", dec=".")
names(wine) <- c("Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium",
                 "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity",
                 "Hue", "OD280_OD315_of_diluted_wines", "Proline", "Wine_type")
names(wine)

#Предикторы
x <- wine[1:13]
#Отклик
y <- wine[, 14]
y.1 <- as.factor(y)

table(y)

#Датчик случайных чисел
set.seed(3217)

#Число деревьев в лесе
ntree.1 <- 30

#
nodesize.1 <- 1

#Включение результатов в лес
keep.forest.1 <- T

#Построение модели
rf.wine <- randomForest(x, y=y.1, ntree=ntree.1, mtry=floor(sqrt(ncol(wine))),
                        replace=FALSE, nodesize = nodesize.1,
                        importance=TRUE, localImp=FALSE,
                        proximity=FALSE, norm.votes=TRUE, do.trace=ntree.1/10,
                        keep.forest=keep.forest.1, corr.bias=FALSE, keep.inbag=FALSE)


#Опредиление числа деревьев
wine.predict <- predict(rf.wine, newdata = x)
#Или
wine.predict.matrix <- predict(rf.wine, newdata = x, type = "prob")

table(y, wine.predict)

import.wine <- importance(rf.wine, type=NULL, class=1, scale=TRUE)
import.wine.2 <- as.data.frame(import.wine)

varImpPlot(rf.wine, sort=F)
