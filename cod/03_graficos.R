#Graficos: ----


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


#Representitividad por género:

top10 <- resultados.generos %>% 
  arrange(desc(porcentaje)) %>% 
  head(10)



grafico.gen.representativos <- ggplot(
  top10,
  aes(
    x = reorder(genero, porcentaje),
    y = porcentaje,
    fill = imdb
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_gradientn(
    colours = c(
      "#F7E7CE",
      "#E6B980",
      "#C97B63",
      "#8C3B3B"
    ),
    name = "IMDb"
  ) +
  labs(
    title = "Top 10 géneros más representativos",
    x = NULL,
    y = "% de películas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )


