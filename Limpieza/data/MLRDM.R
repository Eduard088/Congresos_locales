confianza_2015 <- read_csv("C:/Users/Eduardo/Documents/PC/SECCION1/Intervalos/ENCIG/2015/Partidos_2015.csv")

confianza_2015 <- confianza_2015 %>% 
  mutate(P3_3_19 = case_when(
    P3_3_19 == 1 ~ "Muy Frecuente",
    P3_3_19 == 2 ~ "Frecuente",
    P3_3_19 == 3 ~ "Poco Frecuente",
    P3_3_19 == 4 ~ "Nunca",
    P3_3_19 == 9 ~ "No sabe/No responde")) 

confianza_2015 <- confianza_2015 %>% filter(!is.na(P3_3_19))


confianza_2015 <- confianza_2015 %>% 
  mutate(Prop = case_when(
    P3_3_19 == "Muy Frecuente" ~ 1,
    P3_3_19 == "Frecuente" ~ 1,
    P3_3_19 == "Poco Frecuente" ~ 0,
    P3_3_19 == "Nunca" ~ 0,
    P3_3_19 ==  "No sabe/No responde" ~ 0 )) 


exitoc_1 <- sum(confianza_2015$Prop)
nc_1 <- length(confianza_2015$Prop)


add4ci(exitoc_1, nc_1,conf.level = 0.99)




confianza_2017 <- read_csv("C:/Users/Eduardo/Documents/PC/SECCION1/Intervalos/ENCIG/2017/Frecuencia_2017.csv")


confianza_2017 <- confianza_2017 %>% 
  mutate(P3_3_19 = case_when(
    P3_3_19 == 1 ~ "Muy Frecuente",
    P3_3_19 == 2 ~ "Frecuente",
    P3_3_19 == 3 ~ "Poco Frecuente",
    P3_3_19 == 4 ~ "Nunca",
    P3_3_19 == 9 ~ "No sabe/No responde")) 

confianza_2017 <- confianza_2017 %>% filter(!is.na(P3_3_19))


confianza_2017 <- confianza_2017 %>% 
  mutate(Prop = case_when(
    P3_3_19 == "Muy Frecuente" ~ 1,
    P3_3_19 == "Frecuente" ~ 1,
    P3_3_19 == "Poco Frecuente" ~ 0,
    P3_3_19 == "Nunca" ~ 0,
    P3_3_19 ==  "No sabe/No responde" ~ 0 )) 


exitoc_2 <- sum(confianza_2017$Prop)
nc_2 <- length(confianza_2017$Prop)


add4ci(exitoc_2, nc_2,conf.level = 0.99)



confianza_2019 <- read_csv("C:/Users/Eduardo/Documents/PC/SECCION1/Intervalos/ENCIG/2019/Frecuencia_2019.csv")


confianza_2019 <- confianza_2019 %>% 
  mutate(P3_3_19 = case_when(
    P3_3_19 == 1 ~ "Muy Frecuente",
    P3_3_19 == 2 ~ "Frecuente",
    P3_3_19 == 3 ~ "Poco Frecuente",
    P3_3_19 == 4 ~ "Nunca",
    P3_3_19 == 9 ~ "No sabe/No responde")) 

confianza_2019 <- confianza_2019 %>% filter(!is.na(P3_3_19))


confianza_2019 <- confianza_2019 %>% 
  mutate(Prop = case_when(
    P3_3_19 == "Muy Frecuente" ~ 1,
    P3_3_19 == "Frecuente" ~ 1,
    P3_3_19 == "Poco Frecuente" ~ 0,
    P3_3_19 == "Nunca" ~ 0,
    P3_3_19 ==  "No sabe/No responde" ~ 0 )) 


exitoc_3 <- sum(confianza_2019$Prop)
nc_3 <- length(confianza_2019$Prop)


add4ci(exitoc_3, nc_3,conf.level = 0.99)



confianza_2021 <- read_csv("C:/Users/Eduardo/Documents/PC/SECCION1/Intervalos/ENCIG/2021/Frecuencia_2021.csv")

confianza_2021 <- confianza_2021 %>% 
  mutate(P3_3_19 = case_when(
    P3_3_19 == 1 ~ "Muy Frecuente",
    P3_3_19 == 2 ~ "Frecuente",
    P3_3_19 == 3 ~ "Poco Frecuente",
    P3_3_19 == 4 ~ "Nunca",
    P3_3_19 == 9 ~ "No sabe/No responde")) 

confianza_2021 <- confianza_2021 %>% filter(!is.na(P3_3_19))


confianza_2021 <- confianza_2021 %>% 
  mutate(Prop = case_when(
    P3_3_19 == "Muy Frecuente" ~ 1,
    P3_3_19 == "Frecuente" ~ 1,
    P3_3_19 == "Poco Frecuente" ~ 0,
    P3_3_19 == "Nunca" ~ 0,
    P3_3_19 ==  "No sabe/No responde" ~ 0 )) 


exitoc_4 <- sum(confianza_2021$Prop)
nc_4 <- length(confianza_2021$Prop)


add4ci(exitoc_4, nc_4,conf.level = 0.99)


confianza_2023 <- read_csv("C:/Users/Eduardo/Documents/PC/SECCION1/Intervalos/ENCIG/2023/Frecuencia_2023.csv")

confianza_2023 <- confianza_2023 %>% 
  mutate(P3_3_19 = case_when(
    P3_3_19 == 1 ~ "Muy Frecuente",
    P3_3_19 == 2 ~ "Frecuente",
    P3_3_19 == 3 ~ "Poco Frecuente",
    P3_3_19 == 4 ~ "Nunca",
    P3_3_19 == 9 ~ "No sabe/No responde")) 

confianza_2023 <- confianza_2023 %>% filter(!is.na(P3_3_19))


confianza_2023 <- confianza_2023 %>% 
  mutate(Prop = case_when(
    P3_3_19 == "Muy Frecuente" ~ 1,
    P3_3_19 == "Frecuente" ~ 1,
    P3_3_19 == "Poco Frecuente" ~ 0,
    P3_3_19 == "Nunca" ~ 0,
    P3_3_19 ==  "No sabe/No responde" ~ 0 )) 


exitoc_5 <- sum(confianza_2023$Prop)
nc_5 <- length(confianza_2023$Prop)


add4ci(exitoc_5, nc_5,conf.level = 0.99)

library(tibble)

intervalos_confianza <- tibble(
  Año = c(2015, 2017, 2019, 2021, 2023),
  Intervalo_Confianza_Inferior = c(0.8632154, 0.8890828, 0.8341491, 0.8418503, 0.8233831),
  Intervalo_Confianza_Superior = c(0.8728394, 0.8971256, 0.8436626, 0.8511431, 0.8332245),
  Estimación = c(0.8680274, 0.8931042, 0.8389059, 0.8464967, 0.8283038)
)


ggplot(intervalos_confianza, aes(x = Año, y = Estimación)) +
  geom_point(size = 3, color = "blue") +
  geom_path()+
  geom_smooth()+
  geom_errorbar(aes(ymin = Intervalo_Confianza_Inferior, ymax = Intervalo_Confianza_Superior), width = 0.2, color = "red") +
  labs(title = "Estimaciones sobre la Frecuencia de Corrupción en México",
       subtitle = "Según la Percepción del Electorado",
       caption="Elaboración Propoia con Datos del INEGI",
       x = "Año", y = "Estimación") +
  scale_x_continuous(breaks = seq(2015, 2023, by=2))+
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#34495E", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 10, hjust = 1, color = "#95A5A6"),
    axis.title.x = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.title.y = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.text = element_text(color = "#34495E"),
    panel.grid.major = element_line(color = "#D0D3D4", size = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )








confi_2023 <- read_csv("C:/Users/Eduardo/Documents/PC/SECCION1/Intervalos/ENCIG/2023/Partidos_2023.csv")

confi_2023 <- confi_2023%>%
  select(P11_1_19, P11_1A_19)

confi_2023 <- confi_2023 %>% 
  mutate(P11_1_19 = case_when(
    P11_1_19 == 1 ~ "Mucha Confianza",
    P11_1_19 == 2 ~ "Algo de Confianza",
    P11_1_19 == 3 ~ "Algo de Desconfianza",
    P11_1_19 == 4 ~ "Mucha Desconfianza",
    P11_1_19 == 5 ~ "No Aplica",
    P11_1_19 == 9 ~ "No sabe/No responde")) 

confi_2023 <- confi_2023 %>% filter(!is.na(P11_1_19))


confi_2023 <- confi_2023 %>% 
  mutate(Prop = case_when(
    P11_1_19 == "Mucha Confianza" ~ 0,
    P11_1_19 == "Algo de Confianza" ~ 0,
    P11_1_19 == "Algo de Desconfianza" ~ 1,
    P11_1_19 == "Mucha Desconfianza" ~ 1,
    P11_1_19 == "No Aplica" ~ 0,
    P11_1_19 ==  "No sabe/No responde" ~ 0 )) 


exitoc_6 <- sum(confi_2023$Prop)
nc_6 <- length(confi_2023$Prop)


add4ci(exitoc_6, nc_6,conf.level = 0.99)





 

confi_2023 <- confi_2023 %>% filter(!is.na(P11_1A_19))


confi_2023 <- confi_2023 %>% 
  mutate(Prop1 = case_when(
    P11_1A_19 == 1 ~ 1,
    P11_1A_19 == 2 ~ 1,
    P11_1A_19 == 3 ~ 1,
    P11_1A_19 == 4 ~ 1,
    P11_1A_19 == 5 ~ 1,
    P11_1A_19 == 6 ~ 0,
    P11_1A_19 == 7 ~ 0,
    P11_1A_19 == 8 ~ 0,
    P11_1A_19 == 9 ~ 0,
    P11_1A_19 == 10 ~ 0,
    P11_1A_19 == 99 ~ 0)) 


exitoc_7 <- sum(confi_2023$Prop1)
nc_7 <- length(confi_2023$Prop1)


add4ci(exitoc_7, nc_7,conf.level = 0.99)




library(tibble)

# Crear el tibble con los importances
importances <- tibble(
  Variable = c("Confianza en el Congreso de la Unión", 
               "Confianza en Partidos Político", 
               "Calificación al Congreso de la Unión", 
               "Calificación a Jueces y Magistrados", 
               "Calificación a la Fiscalía Estatal"),
  Importancia = c(0.07943831, 0.51636531, 0.13402531, 0.09385228, 0.10564089)
)


library(tibble)
library(ggplot2)

# Crear el tibble con los importances
importances <- tibble(
  Variable = c("Confianza en el Congreso de la Unión", 
               "Confianza en Partidos Político", 
               "Calificación al Congreso de la Unión", 
               "Calificación a Jueces y Magistrados", 
               "Calificación a la Fiscalía Estatal"),
  Importancia = c(0.07943831, 0.51636531, 0.13402531, 0.09385228, 0.10564089)
)

# Crear el gráfico con ggplot2
ggplot(importances, aes(x = reorder(Variable, -Importancia), y = Importancia)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  geom_point(size=3)+
  geom_smooth()+
  labs(title = "Importancia de las Variables en el Modelo de ML",
       subtitle = "En Función de la Calificación de la Partitocracia",
       x = "Variable", y = "Importancia",
       caption="Elaboración Propia") +
  theme_minimal() +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#34495E", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 10, hjust = 1, color = "#95A5A6"),
    axis.title.x = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.title.y = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.text = element_text(color = "#34495E"),
    panel.grid.major = element_line(color = "#D0D3D4", size = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )




financiamiento_MT <- tibble(
  Escenario = paste0("Escenario ", 1:11),
  Monto = c(5773216073, 5881383694, 6021240588, 6232935901, 6495141450, 6752885528, 7011900787, 7211448264, 7467456355, 7735200692, 7979775293)
)

ggplot(financiamiento_MT, aes(x =  reorder(Escenario, -Monto), y = Monto)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_point(size=4, color="skyblue")+
  geom_line(linewidth=1, group=1, linetype = "longdash")+
  coord_flip()+
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Posibles Financiamientos en Futuros Procesos Electorales",
       x = "Escenario",
       y = "Monto en Pesos",
       caption = "Elaboración Propia")+
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#34495E", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 10, hjust = 1, color = "#95A5A6"),
    axis.title.x = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.title.y = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.text = element_text(color = "#34495E"),
    panel.grid.major = element_line(color = "#D0D3D4", size = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )



composicion_camara <- tibble(
  Caso = paste0("Caso ", 1:6),
  Mujeres = c(202, 202, 204, 206, 210, 211),
  Hombres = c(298, 298, 296, 294, 290, 289)
)

# Transformar los datos a formato largo para ggplot2
composicion_largo <- melt(composicion_camara, id.vars = "Caso", variable.name = "Genero", value.name = "Numero")

ggplot(composicion_largo, aes(x = Caso, y = Genero, fill = Numero, color = Genero)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Numero), color = "black", size = 3) +
  scale_fill_gradient(low = "lightcoral", high = "red3") +
  labs(title = "Composición Proyectada de la Cámara de Diputados",
       x = "Caso",
       y = "Sexo",
       fill = "Número de Curules",
       caption="Elaboración Propia")+
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#34495E", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 10, hjust = 1, color = "#95A5A6"),
    axis.title.x = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.title.y = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.text = element_text(color = "#34495E"),
    panel.grid.major = element_line(color = "#D0D3D4", size = 0.5),
    panel.grid.minor = element_blank(),
  )



# Crear el tibble con los datos proyectados
composicion_camara_1 <- tibble(
  Caso = paste0("Caso ", 1:6),
  Mujeres = c(222, 242, 260, 278, 295, 311),
  Hombres = c(278, 258, 240, 222, 205, 189)
)

# Transformar los datos a formato largo para ggplot2
composicion_largo_1 <- melt(composicion_camara_1, id.vars = "Caso", variable.name = "Genero", value.name = "Numero")

# Generar el gráfico de líneas con ggplot2
ggplot(composicion_largo_1, aes(x = Caso, y = Numero, group = Genero, color = Genero)) +
  geom_line(aes(linetype = Genero), size = 1.2) +
  geom_point(color="black", size = 3) +
  scale_color_manual(values = c("Mujeres" = "violet", "Hombres" = "skyblue")) +
  labs(title = "Composición Proyectada de la Cámara de Diputados",
       x = "Caso",
       y = "Número de Curules",
       color = "Género",
       linetype = "Género") +
  geom_text(aes(label = Numero), vjust = -0.5, size = 3.5)+
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#34495E", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 10, hjust = 1, color = "#95A5A6"),
    axis.title.x = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.title.y = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.text = element_text(color = "#34495E"),
    panel.grid.major = element_line(color = "#D0D3D4", size = 0.5),
    panel.grid.minor = element_blank(),
  )


# Generar el gráfico de líneas con áreas sombreadas
ggplot(composicion_largo, aes(x = Caso, y = Numero, group = Genero, color = Genero)) +
  geom_ribbon(data = subset(composicion_largo, Genero == "Mujeres"), aes(ymin = 0, ymax = Numero, fill = Genero), alpha = 0.2) +
  geom_ribbon(data = subset(composicion_largo, Genero == "Hombres"), aes(ymin = 0, ymax = Numero, fill = Genero), alpha = 0.2) +
  geom_line(aes(linetype = Genero), size = 1.2) +
  geom_point(color = "black", size = 3) +
  scale_color_manual(values = c("Mujeres" = "violet", "Hombres" = "skyblue")) +
  scale_fill_manual(values = c("Mujeres" = "violet", "Hombres" = "skyblue")) +
  labs(title = "Composición Proyectada de la Cámara de Diputados",
       x = "Caso",
       y = "Número de Curules",
       fill = "Sexo",
       color="Sexo",
       linetype="Sexo",
       caption="Elaboración Propia") +
  geom_text(aes(label = Numero), vjust = -0.5, size = 3.5) +
  scale_y_continuous(breaks = seq(0, 300, by=50))+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#34495E", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 10, hjust = 1, color = "#95A5A6"),
    axis.title.x = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.title.y = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.text = element_text(color = "#34495E"),
    panel.grid.major = element_line(color = "#D0D3D4", size = 0.5),
    panel.grid.minor = element_blank()
  )


ggplot(composicion_largo_1, aes(x = Caso, y = Numero, group = Genero, color = Genero)) +
  geom_line(aes(linetype = Genero), size = 1.2) +
  geom_point(color = "black", size = 3) +
  scale_color_manual(values = c("Mujeres" = "violet", "Hombres" = "skyblue")) +
  scale_fill_manual(values = c("Mujeres" = "violet", "Hombres" = "skyblue")) +
  labs(title = "Composición Proyectada de la Cámara de Diputados",
       subtitle="Ajustado a la Tendencia Política Femenina",
       x = "Caso",
       y = "Número de Curules",
       fill = "Sexo",
       color="Sexo",
       linetype="Sexo",
       caption="Elaboración Propia") +
  geom_text(aes(label = Numero), vjust = -0.5, size = 3.5) +
  scale_y_continuous(breaks = seq(0, 300, by=50))+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#34495E", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 10, hjust = 1, color = "#95A5A6"),
    axis.title.x = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.title.y = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.text = element_text(color = "#34495E"),
    panel.grid.major = element_line(color = "#D0D3D4", size = 0.5),
    panel.grid.minor = element_blank()
  )





puestos <- tibble(
  Escenario = paste0("Escenario ", 1:4),
  Mujeres = c(0, 1, 2, 2),
  Hombres = c(5, 10, 13, 16)
)

# Transformar los datos a formato largo para ggplot2
puestos_largo <- melt(puestos, id.vars = "Escenario", variable.name = "Genero", value.name = "Puestos")

# Generar el gráfico de líneas con ggplot2
ggplot(puestos_largo, aes(x = Escenario, y = Puestos, group = Genero, color = Genero))+
  geom_line(aes(linetype = Genero), size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Mujeres" = "violet", "Hombres" = "skyblue")) +
  labs(title = "Distribución Proyectada por Sexo de las Candidaturas Independientes",
       x = "Escenario",
       y = "Número de Puestos",
       color = "Sexo",
       linetype = "Sexo",
       caption="Elaboración Propia") +
  scale_y_continuous(breaks = seq(0, 16, by=1))+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#34495E", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 10, hjust = 1, color = "#95A5A6"),
    axis.title.x = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.title.y = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.text = element_text(color = "#34495E"),
    panel.grid.major = element_line(color = "#D0D3D4", size = 0.5),
    panel.grid.minor = element_blank()
  )


# Crea el marco de datos
data <- data.frame(
  Año = 2010:2023,
  Satisfacción = c(26, 23, 20, 18, 19, 21, 24, 22, 25, 27, 30, 32, 35, 37),
  Lugar = c(16, 12, 8, 4, 8, 12, 16, 12, 8, 12, 16, 12, 8, 4),
  Porcentaje = c(20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46)
)

# Crea el gráfico
ggplot(data, aes(x = Año, y = Satisfacción, color = Porcentaje, size = Lugar)) +
  geom_point(size=7) +
  geom_line(aes(group = 1)) +
  geom_hline(yintercept = 25.64286, linetype = "dashed", color = "red") + # Línea de media
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  scale_size_continuous(range = c(3, 5), guide = FALSE) +  # Esta línea oculta la leyenda de "Lugar"
  labs(
    title = "Satisfacción de la Democracia en México por Año",
    subtitle = "2010-2023",
    x = "Año",
    y = "Tasa de Satisfacción en Porcentaje",
    color = "Porcentaje",
    size = "Lugar",
    caption="Elaboración Propia con Datos del Latinobarómetro"
  ) +
  scale_x_continuous(breaks = seq(2010, 2023, by=1))+
  scale_y_continuous(breaks = seq(18, 50, by=2))+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#34495E", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 10, hjust = 1, color = "#95A5A6"),
    axis.title.x = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.title.y = element_text(face = "bold", color = "#2C3E50", size = 12),
    axis.text = element_text(color = "#34495E"),
    panel.grid.major = element_line(color = "#D0D3D4", size = 0.5),
    panel.grid.minor = element_blank()
  )