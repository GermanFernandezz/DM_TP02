
df_tm = as.data.frame(matriz)
vocab_romantic = function(x){
  # Esta función compara si alguna de las palabras presentes 
  # es un término del glosario romantico https://grammar.yourdictionary.com/word-lists/list-of-romantic-words.html
  x = as.data.frame(t(x))
  romantic = c("adorable", "love", "amazing","angel","beau","beautiful","beloved",
               "bewitching", "crazy",
               "darling",
               "dearest",
               "enchanting",
               "enthralling",
               "lover",
               "friend",
               "gorgeous",
               "handsome",
               "heavenly",
               "intoxicating", "life-changing",
               "paramour",
               "sweetheart",
               "swoon",
               "wonderful","adore",
               "admire",
               "beguile",
               "care",
               "caress",
               "cherish",
               "choose",
               "court",
               "cuddle",
               "daydream","embrace",
               "entice",
               "delight",
               "fantasize",
               "immortalize",
               "mesmerize",
               "need",
               "overture",
               "prize",
               "treasure",
               "value",
               "want")
  if(sum(names(x[-which(colSums(x)==0)]) %in% romantic) >2){
    return("VOC_ROMANTIC_SI")
  }else{
    return("VOC_ROMANTIC_NO")
  }
}
# Aplico la función vocab_reggeton al df de términos x documentos
tiene_voc_romantic = apply(df_tm, 1, vocab_romantic)
table(tiene_voc_romantic)
tiene_voc_romantic = as.data.frame(tiene_voc_romantic)
# Importante: luego de integrar la variable es necesario quitar los casos negativos ya que 
# solo importa presencia, no ausencia de un ítem.

#https://www.dreadpen.com/hip-hop-slang-dictionary/
vocab_hiphop_drug = function(x){
  # Esta función compara si alguna de las palabras presentes 
  # es un término del glosario romantico https://grammar.yourdictionary.com/word-lists/list-of-romantic-words.html
  x = as.data.frame(t(x))
  hiphop_drug = c("papi", "white", "dope","boi","cookies","nick","nickel","nickelbag", "nickelsack",
               "dime","dimebag", "dimesack","fire","green","primo","powdering")
  if(sum(names(x[-which(colSums(x)==0)]) %in% hiphop_drug) >0){
    return("VOC_HIPHOP_DRUG_SI")
  }else{
    return("VOC_HIPHOP_DRUG_NO")
  }
}
# Aplico la función vocab_reggeton al df de términos x documentos
tiene_voc_hiphop_drug = apply(df_tm, 1, vocab_hiphop_drug)
table(tiene_voc_hiphop_drug, tiene_voc_romantic)
tiene_voc_hiphop_drug = as.data.frame(tiene_voc_hiphop_drug)
# Importante: luego de integrar la variable es necesario quitar los casos negativos ya que 
# solo importa presencia, no ausencia de un ítem.


vocab_hiphop_gun = function(x){
  # Esta función compara si alguna de las palabras presentes 
  # es un término del glosario romantico https://grammar.yourdictionary.com/word-lists/list-of-romantic-words.html
  x = as.data.frame(t(x))
  hiphop_gun = c("nine", "nina", "ross","gun","deuce","pounda","desert","eaze", "deagle",
                  "pump","shotty")
  if(sum(names(x[-which(colSums(x)==0)]) %in% hiphop_gun) >0){
    return("VOC_HIPHOP_GUN_SI")
  }else{
    return("VOC_HIPHOP_GUN_NO")
  }
}
# Aplico la función vocab_reggeton al df de términos x documentos
tiene_voc_hiphop_gun = apply(df_tm, 1, vocab_hiphop_gun)
table(tiene_voc_hiphop_gun)
tiene_voc_hiphop_drug = as.data.frame(tiene_voc_hiphop_drug)
# Importante: luego de integrar la variable es necesario quitar los casos negativos ya que 
# solo importa presencia, no ausencia de un ítem.


vocab_hiphop = function(x){
  # Esta función compara si alguna de las palabras presentes 
  # es un término del glosario romantico https://grammar.yourdictionary.com/word-lists/list-of-romantic-words.html
  x = as.data.frame(t(x))
  hiphop = c("lye", "ki", "high","fly","whips","kicks","chips","flicks", "aks",
                 "bangin","willie", "Mo", "bizzle", "fizzle", "hizzle", "nizzle", "nigga",
                 "rizzle", "shiznit", "skizzle", "tizzle", "wizzle")
  if(sum(names(x[-which(colSums(x)==0)]) %in% hiphop) >0){
    return("VOC_HIPHOP_SI")
  }else{
    return("VOC_HIPHOP_NO")
  }
}
# Aplico la función vocab_reggeton al df de términos x documentos
tiene_voc_hiphop = apply(df_tm, 1, vocab_hiphop)
table(tiene_voc_hiphop)
tiene_voc_hiphop_drug = as.data.frame(tiene_voc_hiphop_drug)
# Importante: luego de integrar la variable es necesario quitar los casos negativos ya que 
# solo importa presencia, no ausencia de un ítem.




