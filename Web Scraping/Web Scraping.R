library(rvest)
url <- ("http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature")

#Чтение html-страницы
webpage <- read_html(url)

#Использование CSS селектора для сбора рагнов фильмов 
rank_data_html <- html_nodes(webpage, '.text-primary')

#Конвертация rank_data в текст
rank_data <- html_text(rank_data_html)

head(rank_data)

rank_data <- as.numeric(rank_data)


title_data_html <- html_nodes(webpage, '.lister-item-header a')
title_data <- html_text(title_data_html)
head(title_data)

description_data_html <- html_nodes(webpage, '.ratings-bar+ .text-muted')
description_data <- html_text(description_data_html)
head(description_data, n = 10)
#Удаление'\n'
description_data<-gsub("\n", "", description_data)
head(description_data)


runtime_data_html <- html_nodes(webpage, '.text-muted .runtime')
runtime_data <- html_text(runtime_data_html)
#Удаление min и конвертация в imeric
runtime_data<-gsub(" min","",runtime_data)
runtime_data <- as.numeric(runtime_data)
head(runtime_data)

genre_data_html <- html_nodes(webpage, '.genre')
genre_data <- html_text(genre_data_html)
head(genre_data)
#Удаление \n
genre_data<-gsub("\n","",genre_data)
#Взять только первую запись в каждом фильме
genre_data<-gsub(",.*","",genre_data)
#Конвертация в фактор
genre_data<-as.factor(genre_data)
head(genre_data)

rating_data_html <- html_nodes(webpage, '.ratings-imdb-rating strong')
rating_data <- html_text(rating_data_html)
rating_data <- as.numeric(rating_data)
head(rating_data)

votes_data_html <- html_nodes(webpage, '.sort-num_votes-visible span:nth-child(2)')
votes_data <- html_text(votes_data_html)
head(votes_data)
#Удаление лишних запятых
votes_data<-gsub(",","",votes_data)
votes_data <- as.numeric(votes_data)

directors_data_html <- html_nodes(webpage, '.text-muted+ p a:nth-child(1)')
directors_data <- html_text(directors_data_html)
directors_data <- as.factor(directions_data)
head(directors_data)

actors_data_html <- html_nodes(webpage, '.lister-item-content .ghost+ a')
actors_data <- html_text(actors_data_html)
actors_data <- as.factor(actors_data)
head(actors_data)

metascore_data_html <- html_nodes(webpage, '.metascore')
metascore_data <- html_text(metascore_data_html)
head(metascore_data)
metascore_data<-gsub(" ","",metascore_data)
#Проверка длинны metascore 
length(metascore_data)

for (i in c(14,42,89)){
  a<-metascore_data[1:(i-1)]
  b<-metascore_data[i:length(metascore_data)]
  metascore_data <- append(a, list("NA"))
  metascore_data <- append(metascore_data, b)
}

metascore_data <- as.numeric(metascore_data)

length(metascore_data)

summary(metascore_data)


gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')
gross_data <- html_text(gross_data_html)
head(gross_data)

#Удаление '$' и 'M' 
gross_data<-gsub("M","",gross_data)
gross_data<-substring(gross_data,2,6)
length(gross_data)



#Заполнение пропущенных значений NA
for (i in c(14, 42, 43, 52, 58, 72, 79, 80, 89, 93, 96, 99, 100)){
  
  a<-gross_data[1:(i-1)]
  
  b<-gross_data[i:length(gross_data)]
  
  gross_data<-append(a,list("NA"))
  
  gross_data<-append(gross_data,b)
  
}


length(gross_data)
summary(gross_data)
str(gross_data)
gross_data <- gross_data[1:100]
#Конвертация gross в numerical
gross_data<-as.numeric(gross_data)

#Создание data frame
movies_df <- data.frame(Rank = rank_data, Title = title_data,
                        Description = description_data, 
                        Runtime = runtime_data, Genre = genre_data,
                        Rating = rating_data, Metascore = metascore_data,
                        Votes = votes_data, Gross_Earning_in_Mil = gross_data,
                        Director = directors_data, Actor = actors_data)
str(movies_df)


#Краткая визуализация 
library(ggplot2)

#Длительность фильмов по жанрам
qplot(data = movies_df, Runtime, fill = Genre, bins = 30)

#Рейтинг фильмов по жанрам
ggplot(movies_df, aes(x = Runtime, y = Rating)) +
  geom_point(aes(size = Votes, col = Genre))

#Кассовые сборы фильмов по жанрам 
ggplot(movies_df, aes(x = Runtime, y = Gross_Earning_in_Mil)) +
  geom_point(aes(size = Rating, col = Genre))



 


