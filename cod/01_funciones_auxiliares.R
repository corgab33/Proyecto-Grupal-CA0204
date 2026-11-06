
#Funciones: ----


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
