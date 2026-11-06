#Graficos: ----

##Graficos análisis trama: -----

#Grafico nube de palabras frecuentes:
grafico.pal.frecuentes <- frecuencias %>%
  with(wordcloud(
    word,
    n,
    max.words = 75,
    colors = brewer.pal(8, "Dark2"),
    scale = c(2, 0.5)
  ))

##Graficos análisis género: -----

#Diferencias entre critica imdb vs meta en géneros de peliculas:

grafico.diferencia.critica <- resultados.generos %>%
  arrange(desc(diferencia.critica)) %>%
  mutate(
    genero = factor(genero,
                    levels = genero)
  ) %>%
  ggplot(
    aes(
      x = genero,
      y = diferencia.critica,
      fill = diferencia.critica
    )
  ) +
  geom_col() +
  geom_text(
    aes(
      label = round(diferencia.critica, 2)
    ),
    hjust = ifelse(
      resultados.generos$diferencia.critica > 0,
      -0.1,
      1.1
    ),
    size = 3
  ) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_fill_gradient2(
    low = "#B22222",
    mid = "white",
    high = "#2E8B57",
    midpoint = 0
  ) +
  labs(
    title = "Consenso entre crítica y audiencia por género",
    x = NULL,
    y = "diferencia = IMDb - MetaScore/10"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )