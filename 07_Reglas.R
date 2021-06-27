library(arules)

lyrics_features_trans <- read.transactions("transacciones-lyrics-features.txt", 
                                           format = "single",
                                           header = TRUE, 
                                           sep = " ",
                                           cols = c("TID","item"),
                                           quote = '"')

lyrics_features_trans
arules::inspect(head(lyrics_features_trans, 3))

summary(lyrics_features_trans)

reglas <- arules::apriori(lyrics_features_trans, parameter = list(support=0.085, confidence=0.1,minlen=2, target = "rules"))
reglas

arules::inspect(head(sort(reglas, by="lift", decreasing = T), 30))

rules.sub <- arules::subset(reglas, subset =  (lhs %pin% "VOC_HIPHOP_DRUG_SI"))
rules.sub
arules::inspect(head(sort(rules.sub, by="lift", decreasing = TRUE), 100))

rules.sub <- apriori(lyrics_features_trans, 
                     parameter = list(support=0.1, confidence=0.1, target = "rules"), 
                     appearance = list(items = c("cat_valence=Muy Alta")))


