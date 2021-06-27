df_ly_feat_disc = df_ly_feat
names(df_ly_feat_disc[11875:11898])
head(df_ly_feat_disc[1:5,11884:11898])
##Discretización de Valence
#Discretizaciones basadas en Quantile
df_ly_feat_disc$cat_valence = cut(df_ly_feat_disc$Valence,
                                breaks = quantile(df_ly_feat_disc$Valence), 
                                labels = c("Muy Alta", "Alta","Media", "Baja"))
table(df_ly_feat_disc$tiene_voc_romantic, df_ly_feat_disc$tiene_voc_hiphop_drug)
hist(df_ly_feat_ok$Valence, breaks = 50)