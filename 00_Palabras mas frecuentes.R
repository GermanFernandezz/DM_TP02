# Calculamos la frecuencia de cada término en el corpus
freq_term <- sort(colSums(matriz),decreasing=TRUE)

# Generamos un dataframe con esta sumatoria (de rows)
df_freq <- data.frame(termino = names(freq_term), frecuencia=freq_term)

# Reseteamos el índice (sino era el término "dueño" de la frecuencia)
row.names(df_freq) <- NULL
# Dataframe con frecuencia de términos
df_freq

# Gráfico de barras con los N más frecuentes
N=30
barplot(df_freq[1:N,]$frecuencia, las = 2, names.arg = df_freq[1:N,]$termino,
        col ="lightblue", main ="Palabras más frecuentes",
        ylab = "Frecuencia de palabras", ylim = c(0, max(df_freq$frecuencia)+50))

#Wordcloud
topK = head(df_freq, 200)

# Visualización de los resultados
# Nube de Etiquetas
library("wordcloud")
library("RColorBrewer")

par(bg="grey30") # Fijamos el fondo en color gris

set.seed(1234)
wordcloud(words = topK$termino, freq = topK$frecuencia, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(4, "Dark2"))
dev.off()
