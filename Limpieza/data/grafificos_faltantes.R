links <- Congresosfed %>%
  dplyr::mutate(source = ifelse(Sexo == "Hombre", 0, 1),  # Hombres: 0, Mujeres: 1
                target = match(Año, c(2015, 2018, 2021)) + 1,  # Años: 2, 3, 4
                value = Curules) %>%
  dplyr::select(source, target, value)

# Crear un dataframe para los nodos
nodes <- data.frame(
  name = c("Hombre", "Mujer", "2015", "2018", "2021")
)

# Crear el gráfico de red con forceNetwork
forceNetwork(
  Links = links, 
  Nodes = nodes, 
  Source = "source", 
  Target = "target", 
  Value = "value", 
  NodeID = "name", 
  opacity = 0.8, 
  zoom = TRUE
)




df <- Congresosfed %>%
  dplyr::mutate(Sexo_Año = paste(Sexo, Año, sep = " ")) %>%
  dplyr::select(Sexo_Año, Curules)

# Crear el gráfico circular de distribución de curules por Sexo y Año
chordDiagram(df, annotationTrack = c("name", "grid"))




par(mar = c(0.8, 0.8, 0.8, 0.8))  # Márgenes más amplios



pareto_data <- Congresosfed %>%
  group_by(Año, Sexo) %>%
  summarise(Curules = sum(Curules), .groups = "drop") %>%
  arrange(Año, desc(Curules)) %>%
  mutate(
    Porcentaje = Curules / sum(Curules) * 100,  # Porcentaje de cada categoría
    Porcentaje_Acumulado = cumsum(Porcentaje)  # Porcentaje acumulado
  )




ggplot(data = Congresosfed) +
  geom_violin(mapping = aes(x = reorder(Sexo, -Curules), y = Curules, fill = Sexo, alpha=0.6)) +
  geom_jitter(mapping = aes(x = reorder(Sexo, -Curules), y = Curules,)) +
  labs(
    x = "Partido Político", 
    y = "Número de Curules",
    fill = "Partido",
    title = "Cantidad de Curules Percibidos por los Partidos Políticos en México",
    subtitle = "Cámara de Senadores 2018",
    caption = "Elaboración propia con datos del INEGI e INE"
  ) +
  theme(axis.text.x = element_blank())+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#34495E", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 10, hjust = 1, color = "#95A5A6"),
    axis.title.x = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.title.y = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.text = element_text(color = "#34495E"),
    panel.grid.major = element_line(color = "#D0D3D4", linewidth = 0.5),
    panel.grid.minor = element_blank()
  )



ggplot(data = Congresosfed) +
  geom_violin(
    mapping = aes(x = reorder(Sexo, -Curules), y = Curules, fill = Sexo),
    color = "black", # Borde negro como en seaborn
    alpha = 0.5, # Mayor opacidad para un efecto más claro
    trim = FALSE # Para que los violines no se recorten
  ) +
  geom_jitter(
    mapping = aes(x = reorder(Sexo, -Curules), y = Curules),
    color = "black", # Puntos negros para que sean consistentes
    size = 1, # Tamaño pequeño como en seaborn
    width = 0.15 # Ajuste del "jitter" para evitar demasiada dispersión horizontal
  ) +
  scale_fill_manual(values = c("Hombres"="skyblue", "Mujeres"="violet"))+
  scale_y_continuous(breaks = seq(0, 150, by=25))+
  labs(
    x = "Sexo", 
    y = "Número de Curules",
    fill = "Partido",
    title = "Cantidad de Curules Ocupados Por Sexo LXIII, LXIV, LXV Legislatura",
    subtitle = "Cámara de Diputados 2015-2023",
    caption = "Elaboración propia con datos del INEGI e INE"
  ) +
  theme(axis.text.x = element_blank())+
  theme_minimal() +
  facet_wrap(~Año)+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#34495E", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 10, hjust = 1, color = "#95A5A6"),
    axis.title.x = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.title.y = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.text = element_text(color = "#34495E"),
    panel.grid.major = element_line(color = "#D0D3D4", linewidth = 0.5),
    panel.grid.minor = element_blank()
  )