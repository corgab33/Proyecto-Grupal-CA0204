#Graficos: ----

##Graficos análisis trama: -----

grafico.pal.frecuentes <- frecuencias %>%
  with(wordcloud(
    word,
    n,
    max.words = 75,
    colors = brewer.pal(8, "Dark2"),
    scale = c(2, 0.5)
  ))