#Cargo la colección lyrics
library(mongolite)
lyrics = mongo(collection = "lyrics", db = "Spotify_DM_TP" ) #cargo la base de datos solo con letras
lyrics <- lyrics$find() #paso a data frame

###Uso de la funcion
#data = el data frame que contiene las letras, la letras deben estar en una columna llamada lyrics
#Por defecto está seteado para filtrar letras en inglés y generar corpus en idioma ingles
#Si se quiere trabajar en español especificiar pro.ingles = FALSE, pro.espanol = TRUE, idioma_stoprwords = "spanish"
df2corpus.pro <-function(data, pro.ingles=TRUE, pro.espanol=FALSE, pro.genius=TRUE, pro.symbols=TRUE, 
                         pro.stopwords=TRUE, idioma_stopwords = "english",
                         pro.min=TRUE, pro.num=TRUE, pro.accents=TRUE,
                         pro.spaces=TRUE, pro.stemm=TRUE) {
  
  library("textcat")
  library(tm)
  library(stringi)
  library(stringr)
  library(SnowballC) # para Stemming
  
  if(pro.espanol){
    lyrics.pro <- data[textcat(data$lyrics)=="spanish",]
  }
  
  if(pro.ingles){
    lyrics.pro <- lyrics[textcat(lyrics$lyrics)=="english",]
  }
  
  corpus = Corpus(VectorSource(enc2utf8(lyrics.pro$lyrics)))
  
  if(pro.genius){
    # Elimino todo lo que aparece antes del primer []
    corpus.pro <- tm_map(corpus, content_transformer(
      function(x) sub('^.+?\\[.*?\\]',"", x)))
    
    # Elimino las aclaraciones en las canciones, por ejemplo:
    # [Verso 1: Luis Fonsi & Daddy Yankee]
    corpus.pro <- tm_map(corpus.pro, content_transformer(
      function(x) gsub('\\[.*?\\]', '', x)))
    
    # Elimino todo lo que aparece luego de 'More on Genius'
    corpus.pro <- tm_map(corpus.pro, content_transformer(function(x) gsub("More on Genius.*","", x)))
  }
  
  if(pro.min){
    # Convertimos el texto a minúsculas
    corpus.pro <- tm_map(corpus.pro, content_transformer(tolower))
  }
  
  if(pro.stopwords){
    # Removemos palabras vacias en español
    corpus.pro <- tm_map(corpus.pro, removeWords, stopwords(idioma_stopwords))
  }
  
  if(pro.num){
    # removemos números
    corpus.pro <- tm_map(corpus.pro, removeNumbers)
  }
  
  if(pro.symbols){
    # Removemos puntuaciones
    corpus.pro <- tm_map(corpus.pro, removePunctuation)
    
    # Removemos todo lo que no es alfanumérico
    corpus.pro <- tm_map(corpus.pro, content_transformer(function(x) str_replace_all(x, "[[:punct:]]", " ")))
    
    # Removemos puntuaciones
    corpus.pro <- tm_map(corpus.pro, removePunctuation)
  }
  
  if(pro.accents){
    replaceAcentos <- function(x) {stri_trans_general(x, "Latin-ASCII")}
    corpus.pro <- tm_map(corpus.pro, replaceAcentos)
  }
  
  if(pro.spaces){
    # Se eliminan los espacios adicionales
    corpus.pro <- tm_map(corpus.pro, stripWhitespace)
  }
  
  if(pro.stemm){
    # Se eliminan los espacios adicionales
    corpus.pro <- tm_map(corpus.pro, stemDocument, language="spanish")
  }
  
  return(corpus.pro)
}

corpus = df2corpus.pro(lyrics, pro.stemm = F, pro.ingles = F, pro.espanol = T, idioma_stopwords = "spanish")
inspect(corpus[1])
