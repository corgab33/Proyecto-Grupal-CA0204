
#Cremos data filtrada sin las peliculas NA en Meta_score, 
#quitando columnas innecesarias:

peliculas <- df %>%
  filter(!is.na(Meta_score)) %>% 
  select(-Certificate, -starts_with("Star"), -Director, -Runtime) %>%
  mutate(Overview = tolower(Overview)) 

#Análisis de sinopsis: ----


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
#diez peliculas o más:

Trama.filtrado <- Trama.words %>%
  semi_join(frecuencias,
            by = "word")

#Calcula correlación entre pares, si una palabra aparece en una película, 
#¿Qué tan probable es que la otra también aparezca? Mide fuerza de asociación 
#entre dos palabras.

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
  rename(freq.2 = n) %>% 
  filter(correlation >= 0)


Top.corr.pares <- corr.pares %>% 
  filter(correlation >= 0.20)  %>%
  filter(freq.1 >= 10,
         freq.2 >= 10) %>% 
  filter(item1 < item2, 
         correlation != 1.0, 
         item2 != "york")


##Resultados análisis sinópsis: ----

#Almacena resumen estadistico de las peliculas filtradas por los pares de 
#palabras que aparecen con frecuencia en las sinopsis:

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

#Almacena resumen estadistico de las peliculas filtradas por palabras (únicas) 
#que aparecen con frecuencia en las sinópsis:
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


#Análisis de género: ----

generos <- strsplit(
  as.character(peliculas$Genre),
  ", "
) %>% 
unlist()

generos <- data.frame(Genre = generos) %>%
  filter(!is.na(Genre), Genre != "") %>%
  count(Genre, sort = TRUE) 



##Resultados análisis de género: ----
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


##Resultado estadísto de intersecciones de géneros: ----

res.interseccion <- tibble(
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
