
names(df_ly_feat_ok)[11275:11290]
# Los features que conservo son los terminos y las categóricas
features2 = names(df_ly_feat_ok)[c(1:11277,11289:11290)]
names(df_ly_feat_ok[11275:11290])


# Para los terminos necesito filtrar las frecuencias=0
df_single = reshape2::melt(data = df_ly_feat_ok[,features2], id.vars = c("tid") ) #transposicion 
df_single = df_single[df_single$value!=0,] #selecciono los terminos que no son igual a 0

df_single_txt = df_single[df_single$value==1,] #nuevo data frame, solo de las textuales
df_single_txt$variable =  paste0("TERM_",df_single_txt$variable) #Agrego TERM_ a cada item txt

df_single_cat = df_single[df_single$value!=1,] #nuevo data frame, solo de continuas categorizadas
df_single_cat$variable =  paste0(df_single_cat$variable, "=", as.character(df_single_cat$value)) #acomodo categoricas para que queden como si fueran items

#df_single_vocabulario = reshape2::melt(data = df_ly_feat[,c("tid","tiene_voc_tipo")], id.vars = c("tid") ) PUEDO AGREGAR LA CANTIDAD DE VOCABULARIO QUE QUIERA
#df_single_vocabulario$variable = df_single_vocabulario$value
#df_single_tipo = df_single_vocab[as.character(df_single_vocab$variable)=="VOC_TIPO_SI",]
df_single_vocabulario = reshape2::melt(data = df_ly_feat[,c("tid","tiene_voc_romantic", "tiene_voc_hiphop_drug")], id.vars = c("tid") ) 
df_single_vocabulario$variable = df_single_vocabulario$value
df_single_romantic = df_single_vocabulario[as.character(df_single_vocabulario$variable)=="VOC_ROMANTIC_SI",]
df_single_hiphop_drug = df_single_vocabulario[as.character(df_single_vocabulario$variable)=="VOC_HIPHOP_DRUG_SI",]

#union de categoricas, textuales y vocabulario
#df_single = rbind(df_single_cat, df_single_txt, df_single_tipo) AGREGO TODOS LOS VOCABULARIOS QUE QUIERA
df_single = rbind(df_single_cat, df_single_txt, df_single_romantic, df_single_hiphop_drug) 

df_single = na.omit(df_single[,-c(3)])
names(df_single ) = c("TID", "item")

write.table(df_single, file = "transacciones-lyrics-features.txt", row.names = FALSE) #guardo en formato tabla para luego generar las reglas






