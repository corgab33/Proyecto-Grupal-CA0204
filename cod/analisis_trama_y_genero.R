#Cremos data filtrada sin las peliculas NA en Meta_score, quitando columnas innecesarias:
peliculas <- df %>%
  filter(!is.na(Meta_score)) %>% 
  select(-Certificate) 

#Análisis de Trama o sinopsis: ----

peliculas <- peliculas %>%
  mutate(Overview = tolower(Overview)) %>% 
  select(-starts_with("Star"), -Director, -Runtime)


#Creamos dataframe agregando la columna word con palabras relevantes, eliminando
#stop-words o vacías como conectores y eliminando columnas no relevantes:
Trama.words <- peliculas %>%
  unnest_tokens(word, Overview) %>%
  anti_join(stop_words, by = "word") %>% 
  select(-c("Released_Year", 
            "No_of_Votes"))


#Total de palabras, las 50 más frecuentes:
conteo.pal <- Trama.words %>% 
  count(word, sort = T) %>% head(50)


#Calcula frecuencia de palabras en diferentes peliculas cuenta solo una
#aparicion en la misma pelicula
#Responde: ¿En cuántas películas aparece esta palabra?
frecuencias <- Trama.words %>%
  distinct(Series_Title, word) %>%
  count(word, sort = T) %>%
  filter(n >= 10)

#Palabras separadas que aparecen juntas en peliculas distintas:
pares.frecuentes <- Trama.words %>%
  pairwise_count(
    word,
    Series_Title,
    sort = TRUE
  ) %>% 
  filter(n >= 5, item1 < item2)

#Filtrar data de la trama por frecuencia de aparición si la palabra aparece en 
#10 peliculas o más:

Trama.filtrado <- Trama.words %>%
  semi_join(frecuencias,
            by = "word")

#Calculamos la correlación entre pares, si una palabra aparece en una película, 
#¿Qué tan probable es que la otra también aparezca?
#Mide la fuerza de asociación entre dos palabras.

corr.pares <- Trama.filtrado %>%
  pairwise_cor(
    word,
    Series_Title,
    sort = TRUE
  ) %>%
  left_join(frecuencias,
            by = c("item1" = "word")) %>%
  rename(freq.1 = n) %>%
  left_join(frecuencias,
            by = c("item2" = "word")) %>%
  rename(freq.2 = n) %>% filter(correlation >= 0)


Top.corr.pares <- corr.pares %>% 
  filter(correlation >= 0.20)  %>%
  filter(freq.1 >= 10,
         freq.2 >= 10) %>% 
  filter(item1 < item2, 
         correlation != 1.0, 
         item2 != "york")

##Funciones análisis de trama: ----

#Funcion crea un tibble con informacion estadistica de las peliculas filtradas
#por una palabra relevante en la trama:

analizar.palabra <- function(data, palabra){
  
  plot <- data %>%
    filter(
      str_detect(
        Overview,
        fixed(palabra)
      )
    )
  
  tibble(
    peliculas = nrow(plot),
    porcentaje = round(100 * nrow(plot) / nrow(data), 1),
    imdb = mean(plot$IMDB_Rating, na.rm = TRUE),
    meta = mean(plot$Meta_score, na.rm = TRUE)
  )
  
}

#Funcion crea un tibble con informacion estadistica de las peliculas filtradas
#por par de palabras relevantes en la trama:
analizar.par <- function(data, palabra1, palabra2){
  
  plot <- data %>%
    filter(
      str_detect(Overview, fixed(palabra1)),
      str_detect(Overview, fixed(palabra2))
    )
  
  tibble(
    peliculas = nrow(plot),
    porcentaje = round(100 * nrow(plot) / nrow(data), 1),
    imdb = mean(plot$IMDB_Rating, na.rm = TRUE),
    meta = mean(plot$Meta_score, na.rm = TRUE)
  )
  
}

##Tramas populares o frecuentes: ----

#Almacena resumen estadistico de las peliculas filtradas
#por los pares de palabras que aparecen con frecuencia 
#en las sinopsis:

resultados.par <- Top.corr.pares %>%
  rowwise() %>%
  mutate(
    analisis = list(
      analizar.par(
        peliculas,
        item1,
        item2
      )
    )
  ) %>%
  tidyr::unnest(analisis) %>% 
  filter(peliculas >= 5) %>% 
  arrange(desc(peliculas))


#Filtramos las palabras más frecuentes y depuramos las palabras que aportan poca
#información sobre la sinopsis de las peliculas:
Top.palabra <- frecuencias %>% head(20) %>%
  filter(!word %in% c("life", "story", "lives", "time", "world"))

#Almacena resumen estadistico de las peliculas filtradas
#por palabras (únicas) que aparecen con frecuencia 
#en las sinópsis:
resultados.palabra <- Top.palabra  %>%
  rowwise() %>%
  mutate(
    analisis = list(
      analizar.palabra(
        peliculas,
        word
      )
    )
  ) %>%
  tidyr::unnest(analisis) %>% 
  filter(peliculas >= 5) %>% 
  arrange(desc(peliculas))


#Análisis x género: ----

generos <- strsplit(
  as.character(peliculas$Genre),
  ", "
)

generos <- unlist(generos)

generos <- data.frame(Genre = generos) %>%
  filter(!is.na(Genre), Genre != "") %>%
  count(Genre, sort = TRUE) 


##Funciones analizar genero: ----
analizar.genero.unico <- function(data, genero){
  
  patron = paste0(
    "^",
    genero,
    "$"
  )
  
  peliculas.genero <- data %>%
    filter(
      str_detect(
        Genre,
        patron
      )
    )
  
  tibble(
    peliculas = nrow(peliculas.genero),
    porcentaje = round(
      100 * nrow(peliculas.genero) / nrow(data),
      2
    ),
    imdb = mean(
      peliculas.genero$IMDB_Rating,
      na.rm = TRUE
    ),
    meta = mean(
      peliculas.genero$Meta_score,
      na.rm = TRUE
    ),
    diferencia.critica = mean(peliculas.genero$IMDB_Rating, na.rm = TRUE) -
      mean(peliculas.genero$Meta_score/10, na.rm = TRUE),
    votos = mean(
      peliculas.genero$No_of_Votes,
      na.rm = TRUE
    ),
    taquilla = mean(peliculas.genero$Gross, na.rm = T)
  )
  
}

analizar.genero <- function(data, genero){
  
  
  peliculas.genero <- data %>%
    filter(
      str_detect(
        Genre,
        fixed(genero)
      )
    )
  
  tibble(
    peliculas = nrow(peliculas.genero),
    porcentaje = round(
      100 * nrow(peliculas.genero) / nrow(data),
      2
    ),
    imdb = mean(
      peliculas.genero$IMDB_Rating,
      na.rm = TRUE
    ),
    meta = mean(
      peliculas.genero$Meta_score,
      na.rm = TRUE
    ),
    diferencia.critica = mean(peliculas.genero$IMDB_Rating, na.rm = TRUE) -
      mean(peliculas.genero$Meta_score/10, na.rm = TRUE),
    votos = mean(
      peliculas.genero$No_of_Votes,
      na.rm = TRUE
    ),
    taquilla = mean(peliculas.genero$Gross, na.rm = T)
  )
  
}

##Resultados análisis x género: ----
resultados.generos <- tibble(
  genero = generos$Genre
) %>%
  rowwise() %>%
  mutate(
    analisis = list(
      analizar.genero(
        peliculas,
        genero
      )
    )
  ) %>%
  tidyr::unnest(analisis) %>% 
  arrange(desc(diferencia.critica))

##Intersección de géneros: ----

###Resultado estadísto de intersecciones de géneros en peliculas: ----

res.interseccion.gen <- tibble(
  genero = peliculas$Genre
) %>%
  rowwise() %>%
  mutate(
    analisis = list(
      analizar.genero.unico(
        peliculas,
        genero
      )
    )
  ) %>%
  tidyr::unnest(analisis) %>% 
  arrange(desc(peliculas)) %>% 
  distinct(genero, .keep_all = TRUE) %>% 
  filter(peliculas > 20)
