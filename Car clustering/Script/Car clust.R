library(rpart) 
library(rpart.plot) 

cars <- read.table("Car_Data.dat", header = T, sep = ";") 


head(cars, n = 10)

#Отбираем переменные 
cars.2 <- cars[, 2:14]

#Структура cars_2                
str(cars.2) 


#Построение модели 
model <- rpart(C~T+R77, cars.2, method = "class", 
               control = rpart.control(minsplit = 25, 
                                       minbucket = 10, 
                                       maxdepth = 4, 
                                       cp = .005)) 
# Информация о построенной модели
printcp(model) 
print(model, digits = 2)
summary(m)

extra.val <- 109 

# 1 вариант визуализации
rpart.plot(model, type = 2, extra = 1) 
# 2 вариант визуализации в % соотношении
rpart.plot(model, type = 2, extra = extra.val)
# 3 вариант визуализации
prp(model, type = 2, extra = 1)


#Использование дерева
cars.predict <- predict(model.2, cars[ , -14], type="class")
predict(model.2, cars[ , -14])[ , 2]
table(cars[ , 14], cars.predict)    
