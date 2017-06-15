library(rpart) 
library(rpart.plot) 

wine <- read.table("Wine.txt", header=T, sep="", dec=".")
#Переименуем переменные
names(wine) <- c("Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium",
                 "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity",
                 "Hue", "OD280_OD315_of_diluted_wines", "Proline", "Wine_type")

#Проверим имена 
names(wine)
#Структура 
str(wine) 

#Построение модели
wine.model <- rpart(Wine_type~., wine, method = "class", 
                    control = rpart.control(minsplit = 25, 
                                            minbucket = 10, 
                                            maxdepth = 4, cp = .005)) 
print(wine.model, digits = 2)
summary(wine.model)
printcp(wine.model) 
names(wine.model)

extra_val <- 109 

# 1вариант визуализации 
rpart.plot(wine.model, type = 2, extra = 1) 
# 2 вариант визуализации в % соотношении
rpart.plot(wine.model, type = 2, extra = extra_val) 
# 3 вариант визуализации
prp(wine.model, type = 2, extra = 1) 


#Прогнозирование
wine.predict <- predict(wine.model, wine[ , -14], type="class")

predict(wine.model, wine[ , -14])[ , 2]

#Проверка результатов
table(wine[ , 14], wine.predict)
