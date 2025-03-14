Elecciones <- read_excel("C:/Users/Eduardo/Documents/PC/SECCION1/Quarto/Limpieza/Elecciones_1.xlsx")

Elecciones <- Elecciones%>%
  rename(Financiamiento=Monto)


Elecciones<-  Elecciones %>%
  group_by(Año, ID_ESTADO, Partido) %>%
  summarize(
    PARTICIPACION = mean(PARTICIPACION, na.rm = TRUE),    # Promedio de la participación
    Proporcion = mean(Proporcion, na.rm = TRUE), 
    Votos = first(Votos),# Promedio de la proporción
    financiamiento = sum(Financiamiento, na.rm = TRUE),                     # Sumar el monto
    Curules = sum(Curules, na.rm = TRUE),                 # Sumar las curules
    Sexo = first(Sexo),
    Partido = first(Partido),
    Presidencia= first(Presidencia),# Tomar el primer valor de Sexo (o usar otro criterio)
    Sufragios = first(Sufragios)                          # Tomar el primer valor de Sufragios (o usar otro criterio)
  ) %>%
  ungroup()

modelos <- Elecciones%>%
  split(.$Sexo) %>%
  map(~lm(Votos ~ Curules, data = .))

View(modelos)
summary(modelos)

# conocer el valor de los modelos con r squared:

modelos %>% 
  map(summary)%>%
  map_dbl(~. $r.squared)

modelos %>%
  map(summary) %>%
  map_dbl(~. $coefficients[2, 1])

modelos %>% map(summary)



modelos_ampliados <- Elecciones %>%
  split(.$Sexo) %>%
  map(~lm(Votos ~ Curules + PARTICIPACION + ID_ESTADO + Presidencia + Año + Partido, data = .))

modelos_ampliados %>% 
  map(summary)%>%
  map_dbl(~. $r.squared)

modelos_ampliados %>%
  map(summary) %>%
  map_dbl(~. $coefficients[2, 1])

# Resumen de los nuevos modelos
modelos_ampliados %>% map(summary)

modelos_ampliados %>%
  map(~broom::tidy(.))

write_xlsx(Elecciones, "C:/Users/Eduardo/Documents/Elecciones.xlsx")



datos <- data.frame( Sexo = rep(c("Hombres", "Mujeres"), each = 1), 
                     Curules = c(5960, 13707), se = c(255, 1303) )

ggplot(datos, aes(x = Sexo, y = Curules, fill = Sexo)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.5) + 
  geom_line(linewidth=1, group=1, linetype="dotted")+
  geom_point()+
  geom_errorbar(aes(ymin = Curules - se, ymax = Curules + se), width = 0.2, position = position_dodge(0.7)) + 
  labs(title = "Comparación de Coeficientes de Curules por Sexo",
       subtitle = "Según el Modelo de Regresión Lineal", x = "Sexo", 
       y = "Coeficiente de Curules", fill = "Sexo") +
  scale_fill_manual(values = c("Hombres" = "skyblue", Mujeres="violet"))+
  scale_y_continuous(breaks = seq(0,15000, by=2500))+
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




### financiamiento:
modelos_ampliados_1 <- Elecciones %>%
  split(.$Sexo) %>%
  map(~lm(financiamiento ~ Curules + PARTICIPACION + ID_ESTADO + Año + Partido, data = .))

# Resumen de los nuevos modelos
modelos_ampliados_1 %>% map(summary)

# Coeficiente de determinación (R²) para evaluar la explicación
modelos_ampliados_1 %>%
  map(summary) %>%
  map_dbl(~. $r.squared)

# Coeficientes individuales para comparar el efecto de cada variable
modelos_ampliados_1 %>%
  map(~broom::tidy(.))






## CAMARA DE DIPUTADOS:
modelos_Diputados <- Congresosfed %>%
  split(.$Sexo) %>%
  map(~lm(Votos ~ Curules + Año + Partido, data = .))

# Resumen de los nuevos modelos
modelos_Diputados %>% map(summary)

# Coeficiente de determinación (R²) para evaluar la explicación
modelos_Diputados %>%
  map(summary) %>%
  map_dbl(~. $r.squared)

# Coeficientes individuales para comparar el efecto de cada variable
modelos_Diputados %>%
  map(~broom::tidy(.))



modelos_financiamientoDip <- Congresosfed %>%
  split(.$Sexo) %>%
  map(~lm(Financiamiento ~ Curules + Año + Partido, data = .))


write_xlsx(Congresosfed, "C:/Users/Eduardo/Documents/Congresosfed.xlsx")

  
# Resumen de los nuevos modelos
modelos_financiamientoDip %>% map(summary)

# Coeficiente de determinación (R²) para evaluar la explicación
modelos_financiamientoDip %>%
  map(summary) %>%
  map_dbl(~. $r.squared)

# Coeficientes individuales para comparar el efecto de cada variable
modelos_financiamientoDip %>%
  map(~broom::tidy(.))







## Senadores:
modelos_Senado <- Senadofed %>%
  split(.$Sexo) %>%
  map(~lm(Votos ~ Curules, data = .))

write_xlsx(Senadofed, "C:/Users/Eduardo/Documents/Senadofed.xlsx")


# Resumen de los nuevos modelos
modelos_Senado %>% map(summary)

# Coeficiente de determinación (R²) para evaluar la explicación
modelos_Senado %>%
  map(summary) %>%
  map_dbl(~. $r.squared)

# Coeficientes individuales para comparar el efecto de cada variable
modelos_Senado %>%
  map(~broom::tidy(.))




modelos_financiamientoSen <- Senadofed %>%
  split(.$Sexo) %>%
  map(~lm(Financiamiento ~ Curules, data = .))

# Resumen de los nuevos modelos
modelos_financiamientoSen %>% map(summary)

# Coeficiente de determinación (R²) para evaluar la explicación
modelos_financiamientoSen %>%
  map(summary) %>%
  map_dbl(~. $r.squared)

# Coeficientes individuales para comparar el efecto de cada variable
modelos_financiamientoSen %>%
  map(~broom::tidy(.))



coef_hombres <- data.frame( term = c("(Intercept)", "Curules", "PARTICIPACION",
                                     "ID_ESTADO", "Año", "MORENA", 
                                     "NUAL", "PAN", "PRD", 
                                     "PRI", "PT", "PVEM"), 
                            estimate = c(-25820172275, 10306513, 1690740485, -5228071, 
                                         12652623, 1466002480, -155956768, 2019906090, 
                                         715318297, 2494112586, -38986553, 87825913), 
                            std.error = c(47641651778, 1407404, 574263991, 5785346, 
                                          23586377, 230202567, 281050197, 224095285, 
                                          242819813, 222950665, 241114543, 233755100), 
                            p.value = c(0.58813, 1.24e-12, 0.00342, 0.36668, 0.59194, 
                                        5.00e-10, 0.57925, 0.0000000000000002, 0.00340,
                                        0.0000000000000002, 0.87162, 0.70732)) 

coef_mujeres <- data.frame( term = c("(Intercept)", "Curules", "PARTICIPACION", "ID_ESTADO", 
                                     "Año", "MORENA", "NUAL", "PAN", 
                                     "PRD", "PRI", "PT", "PVEM"),
                            estimate = c(-74811056456, 49823866, 1097037867, 4880027, 36777887, 
                                         -91242693, -126767811, 691422851, 254519787, 1431969548, 
                                         20771384, -919634), 
                            std.error = c(54657949543, 5498244, 679780837, 5981084, 27034041, 
                                          200254904, 200285391, 198264432, 183835610, 209466038, 
                                          207624473, 165999932), 
                            p.value = c(0.174496, 0.0000000000000256, 0.110069, 0.416706, 0.177092, 0.649752,
                                        0.528380, 0.000757, 0.169630, 0.000000000943, 0.920533, 0.995592))



coef_hombres_signif <- coef_hombres %>% filter(p.value < 0.05)
coef_mujeres_signif <- coef_mujeres %>% filter(p.value < 0.05)

coef_combined_signif <- bind_rows(
  coef_hombres_signif %>% mutate(Sexo = "Hombres"), 
  coef_mujeres_signif %>% mutate(Sexo = "Mujeres") )


ggplot(coef_combined_signif, aes(x = term, y = estimate, fill = Sexo)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.8, color="black") + 
  geom_point(size=3)+
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, position = position_dodge(0.7)) +
  coord_flip()+
  labs(title = "Coeficientes Significativos por Sexo", 
       subtitle= "De la Regresión Lineal en los Congresos Locales",
       x = "Término", y = "Estimación", fill = "Sexo",
       caption="Elaboración Propia")+ 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  scale_fill_manual(values = c("Hombres" = "skyblue", Mujeres="violet"))+
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