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
  coord_flip(
    clip = "off"
  ) +

  scale_y_continuous(
    limits = c(
      min(resultados.generos$diferencia.critica) - 0.2,
      max(resultados.generos$diferencia.critica) + 0.2
    )
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.margin = margin(
      10,
      20,
      10,
      20
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


# Gráfica de calificación promedio según rango de duración

grafico.duracion <- indicadores.duracion %>%
  ggplot(
    aes(
      x = Rango,
      y = Calificacion_promedio,
      fill = Calificacion_promedio
    )
  ) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0("n = ", Cantidad)),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_gradient(
    low = "#F4DDBB",
    high = "#A64B43"
  ) +
  scale_y_continuous(
    breaks = seq(5, 10, 1)
  ) +
  coord_cartesian(ylim = c(5, 10.5)) +
  labs(
    title = "Calificación IMDb promedio según duración",
    subtitle = "n = Cantidad de películas por rango",
    x = "Rango de duración",
    y = "Calificación promedio IMDb",
    fill = "Calificación"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )


# Gráfico de protagonistas por calificación IMDb

top.actores.rating <- actors.grouping %>%
  filter(!is.na(protagonist)) %>%
  arrange(desc(mean.rating)) %>%
  slice_head(n = 5)

grafico.actores.rating <- top.actores.rating %>%
  mutate(
    protagonist = reorder(protagonist, mean.rating)
  ) %>%
  ggplot(
    aes(
      x = protagonist,
      y = mean.rating,
      fill = mean.rating
    )
  ) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = round(mean.rating, 2)),
    vjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#F4DDBB",
    high = "#A64B43"
  ) +
  scale_y_continuous(
    breaks = seq(5, 10, 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  coord_cartesian(
    xlim = NULL,
    ylim = c(5, 10)
  ) +
  labs(
    title = "Top 5 protagonistas por calificación IMDb",
    subtitle = "Promedio ajustado mediante media bayesiana",
    x = NULL,
    y = "Calificación promedio IMDb",
    fill = "Calificación"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    )
  )


# Gráfico de protagonistas por ingresos

top.actores.revenue <- actors.grouping %>%
  filter(!is.na(protagonist)) %>%
  arrange(desc(mean.revenue)) %>%
  slice_head(n = 5) %>%
  mutate(
    ingresos.millones = mean.revenue / 1000000
  )

grafico.actores.revenue <- top.actores.revenue %>%
  mutate(
    protagonist = reorder(protagonist, ingresos.millones)
  ) %>%
  ggplot(
    aes(
      x = protagonist,
      y = ingresos.millones,
      fill = ingresos.millones
    )
  ) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = round(ingresos.millones, 1)),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#F4DDBB",
    high = "#A64B43"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Top 5 protagonistas por ingresos",
    subtitle = "Ingresos promedio ajustados, en millones de dólares",
    x = NULL,
    y = "Ingresos promedio (millones de dólares)",
    fill = "Ingresos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )


# Gráfico de directores por calificación IMDb

top.directores.rating <- directors.grouping %>%
  filter(!is.na(director)) %>%
  arrange(desc(mean.rating)) %>%
  slice_head(n = 5)

grafico.directores.rating <- top.directores.rating %>%
  mutate(
    director = reorder(director, mean.rating)
  ) %>%
  ggplot(
    aes(
      x = director,
      y = mean.rating,
      fill = mean.rating
    )
  ) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = round(mean.rating, 2)),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_gradient(
    low = "#F4DDBB",
    high = "#A64B43"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Top 5 directores por calificación IMDb",
    subtitle = "Promedio ajustado mediante media bayesiana",
    x = NULL,
    y = "Calificación promedio IMDb",
    fill = "Calificación"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    )
  )


# Gráfico de directores por ingresos

top.directores.revenue <- directors.grouping %>%
  filter(!is.na(director)) %>%
  arrange(desc(mean.revenue)) %>%
  slice_head(n = 5) %>%
  mutate(
    ingresos.millones = mean.revenue / 1000000
  )

grafico.directores.revenue <- top.directores.revenue %>%
  mutate(
    director = reorder(director, ingresos.millones)
  ) %>%
  ggplot(
    aes(
      x = director,
      y = ingresos.millones,
      fill = ingresos.millones
    )
  ) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = round(ingresos.millones, 1)),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#F4DDBB",
    high = "#A64B43"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Top 5 directores por ingresos",
    subtitle = "Ingresos promedio ajustados, en millones de dólares",
    x = NULL,
    y = "Ingresos promedio (millones de dólares)",
    fill = "Ingresos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )
