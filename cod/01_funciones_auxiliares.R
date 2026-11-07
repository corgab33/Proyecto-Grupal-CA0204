
#Funciones: ----


##Funciones análisis de trama: ----

#Funcion crea un tibble con informacion estadistica de las peliculas filtradas
#por una palabra relevante en la trama:

analizar.palabra <- function(data, palabra){
  
  #Selecciona filas que contienen la palabra y no subconjuntos de letras iguales 
  #a la palabra
  
  ids.palabra = data %>% 
    unnest_tokens(word, Overview) %>% 
    filter(word == palabra) %>% 
    distinct(id)
  
  # Guarda las observaciones que contienen la palabra
  plot = data %>% 
    semi_join(ids.palabra, by = "id")
  
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
  
  #Separa Overview en palabras completas, de-selecciona observaciones duplicadas
  palabras.peliculas = data %>% 
    unnest_tokens(word, Overview) %>% 
    distinct(id, word)
  
  #Escoge las que tienen la palabra1 buscada
  ids.palabra1 = palabras.peliculas %>% 
    filter(word == palabra1) %>% 
    select(id)
  
  #Escoge las que tienen palabra 2 buscada
  ids.palabra2 = palabras.peliculas %>% 
    filter(word == palabra2) %>% 
    select(id)
  
  #Union, escoge las observaciones que tienen las 2 palabras
  ids.par = ids.palabra1 %>% 
    inner_join(ids.palabra2, by = "id")
  
  plot <- data %>%
    semi_join(ids.par, by = "id")
  
  tibble(
    peliculas = nrow(plot),
    porcentaje = round(100 * nrow(plot) / nrow(data), 1),
    imdb = mean(plot$IMDB_Rating, na.rm = TRUE),
    meta = mean(plot$Meta_score, na.rm = TRUE)
  )
  
}

##Funciones analizar genero: ----

#Funcion analiza género único:
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

#Funcion analiza intersecciones de género:
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
