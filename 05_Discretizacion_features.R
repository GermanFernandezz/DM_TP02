

names(df_ly_feat_ok[11275:11290])

##Discretización de Posicion
#Discretizaciones basadas en Quantile
df_ly_feat_ok$cat_posicion = cut(df_ly_feat_ok$Posicion,
                                 breaks = quantile(df_ly_feat_ok$Posicion), 
                                 labels = c("Muy Alta", "Alta","Media", "Baja"))
table(df_ly_feat_ok$cat_posicion)
hist(df_ly_feat_ok$Posicion, breaks = 50)
#Discretización manualmente basado en el histograma
df_ly_feat_ok$cat_posicion = cut(df_ly_feat_ok$position,
                                 breaks = c(1, 25, 100,200), 
                                 labels = c("Muy Alta","Media", "Baja")) 
table(df_ly_feat_ok$cat_posicion)

##Discretización de Valence
#Discretizaciones basadas en Quantile
df_ly_feat_ok$cat_valence = cut(df_ly_feat_ok$Valence,
                                 breaks = quantile(df_ly_feat_ok$Valence), 
                                 labels = c("Muy Alta", "Alta","Media", "Baja"))
table(df_ly_feat_ok$cat_valence)
hist(df_ly_feat_ok$Valence, breaks = 50)



