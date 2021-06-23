library(mongolite)

lyrics = mongo(collection = "lyrics", db = "Spotify_DM_TP" ) #cargo la base de datos solo con letras
lyrics <- lyrics$find() #paso a data frame
lyrics_spa = lyrics[textcat(lyrics$lyrics)=="spanish",]   #me quedo con solo letras en español
lyrics_eng = lyrics[textcat(lyrics$lyrics)=="english",]   #solo letras en ingles

features = mongo(collection = "features_avg_mejorPosition_red", db = "Spotify_DM_TP" ) #cargo la base de datos con letras y features
features <- features$find() 

names(features)[3:13] <- c("Posicion", "Danceability", "Energy", "Loudness", "Speechiness", "Acousticness",
                           "Instrumentalness", "Liveness", "Valence", "Tempo", "Duration")

df_tm = as.data.frame(matriz)
df_ly_feat = merge(x = cbind(lyrics_spa[-c(3,4)], df_tm), 
                   y = features[-c(14)], #para listado en español. saco columna artist_track que vengo arrastrando de mongo
                   by.x = c("artist_name","track_name"), 
                   by.y = c("artist_name","track_name"))


df_tm = as.data.frame(matriz)
df_ly_feat = merge(x = cbind(lyrics_eng[-c(3,4)], df_tm), 
                   y = features[-c(14)], #para listado en ingles, saco columna artist_track que vengo arrastrando de mongo
                   by.x = c("artist_name","track_name"), 
                   by.y = c("artist_name","track_name"))

# Quitar atributos textuales con valor 0
filter = !names(df_ly_feat) %in% c("artist_name", "track_name")
df_ly_feat_ok = df_ly_feat[, filter]
df_ly_feat_ok = df_ly_feat_ok[ ,-which(colSums(df_ly_feat_ok)==0)]

# Agregamos un TID (transaction ID)
df_ly_feat_ok$tid = 1:nrow(df_ly_feat_ok)


names(df_ly_feat_ok[999:1010])
