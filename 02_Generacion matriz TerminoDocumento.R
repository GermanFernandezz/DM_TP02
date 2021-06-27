corpus.pro2tdm <- function(corpus, ponderacion, n_terms) {
  
  # Genero la matriz TD y la transformo en una matriz
  dtm <- TermDocumentMatrix(corpus, control = list(weighting = ponderacion))
  matriz_td <- as.matrix(dtm)
  
  # Me quedo con los n_terms más frecuentes
  terminos_frecuentes = head(sort(rowSums(matriz_td), decreasing = T), n_terms)
  
  # Me quedo con la matriz transpuesta de los n_terms más frecuentes
  # Cada fila es un tema, cada columna un término
  matriz_mf = t(matriz_td[sort(names(terminos_frecuentes)),])
  
  # Paso a binaria la matriz (está o no está el término)
  matriz_mf[matriz_mf > 0] <- 1
  
  return(matriz_mf)
}

matriz <- corpus.pro2tdm(corpus, "weightTfIdf", 100000)
dim(matriz)


