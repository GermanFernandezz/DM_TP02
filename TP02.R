library(mongolite) #para cargar las bases de datos de mongo
library(arules) #para hacer feature engineering


lyrics_features = mongo(collection = "lyrics_features", db = "Spotify_DM_TP" ) #cargo la base de datos con letras y features
lyrics_features <- lyrics_features$find() #paso a data frame
names(lyrics_features)
str(lyrics_features)

#Parametros para discretizar
variable = lyrics_features$mejorPosicion
breaks = 5

# discretización de igual frecuencia
lyrics_features$disc_variable_frequency = arules::discretize(variable, method = "frequency", breaks = breaks, labels=c("muy_bajo", "bajo", "medio", "alto", "muy_alto"))
str(lyrics_features)

#Cargo la base de datos de solo features (sin letras)
features_mejorPosicion = mongo(collection = "features_avg_mejorPosition_red", db = "Spotify_DM_TP" ) #cargo los festures con mejor posicion
features_mejorPosicion <- features_mejorPosicion$find() #paso a data frame
names(features_mejorPosicion)

# Genero una variable target según la posicion en el ranking
features_mejorPosicion$top = as.factor(ifelse(features_mejorPosicion$mejorPosition<=100,"S","N" ))

variable = features_mejorPosicion$mejorPosition
breaks = 5

# discretización de igual frecuencia
features_mejorPosicion$disc_variable_frequency = arules::discretize(variable, method = "frequency", breaks = breaks, labels=c("muy_bajo", "bajo", "medio", "alto", "muy_alto"))

# discretización de igual ancho
features_mejorPosicion$disc_variable_interval = arules::discretize(variable, method = "interval", breaks = breaks, labels=c("muy_bajo", "bajo", "medio", "alto", "muy_alto"))

# discretización por kmeans
features_mejorPosicion$disc_variable_cluster = arules::discretize(variable, method = "cluster", breaks = breaks, labels=c("muy_bajo", "bajo", "medio", "alto", "muy_alto"))

str(features_mejorPosicion)

library(randomForest)
#Por discretizacion de igual frecuencia
model_rf<-randomForest(features_mejorPosicion$disc_variable_frequency ~ ., data = na.exclude(features_mejorPosicion[,c(4:11)]), importance=TRUE)

#Por discretizacion de igual ancho
model_rf<-randomForest(features_mejorPosicion$disc_variable_interval ~ ., data = na.exclude(features_mejorPosicion[,c(4:11)]), importance=TRUE)

#Por discretizacion de kmeans
model_rf<-randomForest(features_mejorPosicion$disc_variable_cluster ~ ., data = na.exclude(features_mejorPosicion[,c(4:11)]), importance=TRUE)

#Por top 100 en posiciones
model_rf<-randomForest(features_mejorPosicion$top ~ ., data = na.exclude(features_mejorPosicion[,c(4:11)]), importance=TRUE)


importance(model_rf)
varImpPlot(model_rf)


#Cargo la colección lyrics
lyrics = mongo(collection = "lyrics", db = "Spotify_DM_TP" ) #cargo la base de datos solo con letras
lyrics <- lyrics$find() #paso a data frame

# Filtramos las que el idioma es español
library("textcat")
spa_lyrics = lyrics[textcat(lyrics$lyrics)=="spanish",]
eng_lyrics = lyrics[textcat(lyrics$lyrics)=="english",]

#Preprocesamiento de textos
library(tm) #Text Mining Package

#Generacion del corpus
corpus = Corpus(VectorSource(enc2utf8(lyrics$lyrics)))
inspect(corpus[1])

# Eliminamos espacios
corpus.pro <- tm_map(corpus, stripWhitespace)
inspect(corpus.pro[1])

# Elimino todo lo que aparece antes del primer []
corpus.pro <- tm_map(corpus.pro, content_transformer(
  function(x) sub('^.+?\\[.*?\\]',"", x)))
inspect(corpus.pro[1])

# Elimino las aclaraciones en las canciones, por ejemplo:
# [Verso 1: etc etc]
corpus.pro <- tm_map(corpus.pro, content_transformer(
  function(x) gsub('\\[.*?\\]', '', x)))
inspect(corpus.pro[1])

# Elimino todo lo que aparece luego de 'More on Genius'
corpus.pro <- tm_map(corpus.pro, content_transformer(function(x) gsub("More on Genius.*","", x)))
inspect(corpus.pro[1])

# Convertimos el texto a minúsculas
corpus.pro <- tm_map(corpus.pro, content_transformer(tolower))
inspect(corpus.pro[1])

# removemos números
corpus.pro <- tm_map(corpus.pro, removeNumbers)
inspect(corpus.pro[1])

# Removemos palabras vacias en español e ingles
corpus.pro <- tm_map(corpus.pro, removeWords, stopwords("spanish"))
corpus.pro <- tm_map(corpus.pro, removeWords, stopwords("english"))
inspect(corpus.pro[1])

# Removemos puntuaciones
corpus.pro <- tm_map(corpus.pro, removePunctuation)
inspect(corpus.pro[1])

# Removemos todo lo que no es alfanumérico
library(tidyverse)
corpus.pro <- tm_map(corpus.pro, content_transformer(function(x) str_replace_all(x, "[[:punct:]]", " ")))
inspect(corpus.pro[1])

# En tm_map podemos utilizar funciones prop
library(stringi)
replaceAcentos <- function(x) {stri_trans_general(x, "Latin-ASCII")}
corpus.pro <- tm_map(corpus.pro, replaceAcentos)
inspect(corpus.pro[1])

# Eliminamos espacios que se van generando con los reemplazos
corpus.pro <- tm_map(corpus.pro, stripWhitespace)
inspect(corpus.pro[1])


#Matriz Termino-Documento
dtm <- TermDocumentMatrix(corpus.pro, 
                          control = list(weighting = "weightTf"))

# Resumen de la dtm (document-term matrix)
dtm

matriz_td <- as.matrix(dtm)
View(matriz_td)

# Calculamos la frecuencia de cada término en el corpus
freq_term <- sort(rowSums(matriz_td),decreasing=TRUE)

# Generamos un dataframe con esta sumatoria (de rows)
df_freq <- data.frame(termino = names(freq_term), frecuencia=freq_term)

# Reseteamos el índice (sino era el término "dueño" de la frecuencia)
row.names(df_freq) <- NULL

# Dataframe con frecuencia de términos
df_freq

# Gráfico de barras con los N más frecuentes
N=15
barplot(df_freq[1:N,]$frecuencia, las = 2, names.arg = df_freq[1:N,]$termino,
        col ="lightblue", main ="Palabras más frecuentes",
        ylab = "Frecuencia de palabras", ylim = c(0, max(df_freq$frecuencia)+300))

