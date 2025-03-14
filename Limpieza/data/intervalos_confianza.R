ENCUCI <- read_csv(
  "C://Users//Eduardo//Downloads//ENCUCI//conjunto_de_datos_ENCUCI_2020_SEC_6_7_8//conjunto_de_datos//data.csv",
  locale = locale(encoding = "ISO-8859-1") # Cambia "ISO-8859-1" por "UTF-8" si es necesario
)

reactable(ENCUCI)


ENCUCI <- ENCUCI%>%
  select(c("AP7_16_1", "AP7_16_3", "AP7_16_6", "AP7_16_7", "AP8_1A_2", "AP8_2_5", "AP8_4_02"))


#AP7_16_1:
ENCUCI <- ENCUCI %>% 
  mutate(AP7_16_1 = case_when(
    AP7_16_1 == 1 ~ "muy frecuente",
    AP7_16_1 == 2 ~ "algo frecuente",
    AP7_16_1 == 3 ~ "poco frecuente",
    AP7_16_1 == 4 ~ "nada frecuente",
    AP7_16_1 == 5 ~ "nunca",
    AP7_16_1 == 9 ~ "NS/NR")) %>% 
  rename(limpieza_elec = AP7_16_1)

ENCUCI <- ENCUCI %>% 
  mutate(ex1 = case_when(
    limpieza_elec== "poco frecuente" ~ 1,
    limpieza_elec== "nada frecuente" ~ 1,
    limpieza_elec!= "poco frecuente" ~ 0,
    limpieza_elec!= "nada frecuente" ~ 0)) 

ENCUCI <- ENCUCI %>% filter(!is.na(limpieza_elec))


exito <- sum(ENCUCI$ex1)
n <- length(ENCUCI$ex1)


add4ci(exito, n,conf.level = 0.90)
add4ci(exito, n,conf.level = 0.95)
add4ci(exito, n,conf.level = 0.99)


#AP7_16_3:
ENCUCI <- ENCUCI %>% 
  mutate(AP7_16_3 = case_when(
    AP7_16_3 == 1 ~ "muy frecuente",
    AP7_16_3 == 2 ~ "algo frecuente",
    AP7_16_3 == 3 ~ "poco frecuente",
    AP7_16_3 == 4 ~ "nada frecuente",
    AP7_16_3 == 5 ~ "nunca",
    AP7_16_3 == 9 ~ "NS/NR")) %>% 
  rename(compra_vot = AP7_16_3)

ENCUCI <- ENCUCI %>% 
  mutate(ex2 = case_when(
    compra_vot== "muy frecuente" ~ 1,
    compra_vot!= "muy frecuente" ~ 0)) 

ENCUCI <- ENCUCI %>% filter(!is.na(compra_vot))


exito2 <- sum(ENCUCI$ex2)
n2 <- length(ENCUCI$ex2)


add4ci(exito2, n2,conf.level = 0.90)
add4ci(exito2, n2,conf.level = 0.95)
add4ci(exito2, n2,conf.level = 0.99)


#AP7_16_6:

ENCUCI <- ENCUCI %>% 
  mutate(AP7_16_6 = case_when(
    AP7_16_6 == 1 ~ "muy frecuente",
    AP7_16_6 == 2 ~ "algo frecuente",
    AP7_16_6 == 3 ~ "poco frecuente",
    AP7_16_6 == 4 ~ "nada frecuente",
    AP7_16_6 == 5 ~ "nunca",
    AP7_16_6 == 9 ~ "NS/NR")) %>% 
  rename(amenaza_vot = AP7_16_6)

ENCUCI <- ENCUCI %>% 
  mutate(ex3 = case_when(
    amenaza_vot %in% c("muy frecuente", "algo frecuente") ~ 1,
    TRUE ~ 0
  ))

ENCUCI <- ENCUCI %>% filter(!is.na(amenaza_vot))


exito3 <- sum(ENCUCI$ex3)
n3 <- length(ENCUCI$ex3)


add4ci(exito3, n3,conf.level = 0.90)
add4ci(exito3, n3,conf.level = 0.95)
add4ci(exito3, n3,conf.level = 0.99)



#AP7_16_7:

ENCUCI <- ENCUCI %>% 
  mutate(AP7_16_7 = case_when(
    AP7_16_7 == 1 ~ "muy frecuente",
    AP7_16_7 == 2 ~ "algo frecuente",
    AP7_16_7 == 3 ~ "poco frecuente",
    AP7_16_7 == 4 ~ "nada frecuente",
    AP7_16_7 == 5 ~ "nunca",
    AP7_16_7 == 9 ~ "NS/NR")) %>% 
  rename(recurso_pub = AP7_16_7)

ENCUCI <- ENCUCI %>% 
  mutate(ex4 = case_when(
    recurso_pub %in% c("muy frecuente", "algo frecuente") ~ 1,
    TRUE ~ 0
  ))

ENCUCI <- ENCUCI %>% filter(!is.na(recurso_pub))


exito4 <- sum(ENCUCI$ex4)
n4 <- length(ENCUCI$ex4)


add4ci(exito4, n4,conf.level = 0.90)
add4ci(exito4, n4,conf.level = 0.95)
add4ci(exito4, n4,conf.level = 0.99)


#AP8_1A_2:

ENCUCI <- ENCUCI %>% 
  mutate(AP8_1A_2 = case_when(
    AP8_1A_2 == 1 ~ "Sí",
    AP8_1A_2 == 2 ~ "No",
    AP8_1A_2 == 9 ~ "NS/NR")) %>% 
  rename(dinero_par = AP8_1A_2)

ENCUCI <- ENCUCI %>% 
  mutate(ex5 = case_when(
    dinero_par== "Sí" ~ 1,
    dinero_par!= "Sí" ~ 0)) 

ENCUCI <- ENCUCI %>% filter(!is.na(dinero_par))


exito5 <- sum(ENCUCI$ex5)
n5 <- length(ENCUCI$ex5)


add4ci(exito5, n5,conf.level = 0.90)
add4ci(exito5, n5,conf.level = 0.95)
add4ci(exito5, n5,conf.level = 0.99)


#AP8_2_5:

ENCUCI <- ENCUCI %>% 
  mutate(AP8_2_5 = case_when(
    AP8_2_5 == 1 ~ "Sí",
    AP8_2_5 == 0 ~ "No",
    AP8_2_5 == 9 ~ "NS/NR")) %>% 
  rename(subsidio = AP8_2_5)

ENCUCI <- ENCUCI %>% 
  mutate(ex6 = case_when(
    subsidio== "Sí" ~ 1,
    subsidio!= "Sí" ~ 0)) 

ENCUCI <- ENCUCI %>% filter(!is.na(subsidio))


exito6 <- sum(ENCUCI$ex6)
n6 <- length(ENCUCI$ex6)


add4ci(exito6, n6,conf.level = 0.90)
add4ci(exito6, n6,conf.level = 0.95)
add4ci(exito6, n6,conf.level = 0.99)


#AP8_4_02:

ENCUCI <- ENCUCI %>% 
  mutate(AP8_4_02 = case_when(
    AP8_4_02 == 1 ~ "Sí",
    AP8_4_02 == 0 ~ "No",
    AP8_4_02 == 9 ~ "NS/NR")) %>% 
  rename(credencial = AP8_4_02)

ENCUCI <- ENCUCI %>% 
  mutate(ex7 = case_when(
    credencial== "Sí" ~ 1,
    credencial!= "Sí" ~ 0)) 

ENCUCI <- ENCUCI %>% filter(!is.na(credencial))


exito7 <- sum(ENCUCI$ex7)
n7 <- length(ENCUCI$ex7)


add4ci(exito7, n7,conf.level = 0.90)
add4ci(exito7, n7,conf.level = 0.95)
add4ci(exito7, n7,conf.level = 0.99)


ggplot(data = ENCUCI) +
  geom_histogram(aes(x = ex5), bins = 30)


ENCUCI %>%
  summarise(
    nulos_ex5 = sum(is.na(ex5)),
    nulos_ex6 = sum(is.na(ex6)),
    nulos_ex7 = sum(is.na(ex7))
  )


# Calcular la moda de ex7
moda_ex7 <- ENCUCI %>%
  filter(!is.na(ex7)) %>%    # Excluir valores NA
  count(ex7, sort = TRUE) %>% # Contar frecuencias
  slice(1) %>%               # Obtener el valor más frecuente (moda)
  pull(ex7)

# Imputar los valores NA de ex7 con la moda
ENCUCI <- ENCUCI %>%
  mutate(ex7 = ifelse(is.na(ex7), moda_ex7, ex7))





# Crear la tabla formateada usando gt
library(gt)
library(dplyr)
library(scales)

# Datos ficticios del DataFrame 'delitos'
delitos <- tibble(
  `Delito Electoral` = c("Conteo limpio de los votos", "Compra de Votos", "Amenazas a votantes en las casillas", 
                         "Recursos públicos para favorecer partidos políticos", "Pedir la credencial de elector"),
  `Intervalo de Confianza (90%)` = c("42.7%-43.8%", "51.6%-52.7%", "37.8%-38.9%", "74.0%-75.0%", "39.9%-40.9%"),
  `¿Es delito?` = c("Sí", "Sí", "Sí", "Sí", "Sí"),
  `Multa` = c("50 a 100 días", "50 a 100 días", "100 a 200 días", "500 a 1000 días", "50 a 500 días"),
  `Prisión` = c("6 meses a 3 años", "No aplica", "2 a 6 años", "4 a 9 años", "2 a 6 años")
)

# Procesar la columna `Intervalo de Confianza (90%)` para extraer el promedio del rango
delitos <- delitos %>%
  mutate(
    # Elimina el símbolo '%'
    `Intervalo de Confianza (90%)` = gsub("%", "", `Intervalo de Confianza (90%)`),
    # Extrae el valor mínimo del rango
    min_val = as.numeric(sub("-.*", "", `Intervalo de Confianza (90%)`)),
    # Extrae el valor máximo del rango
    max_val = as.numeric(sub(".*-", "", `Intervalo de Confianza (90%)`)),
    # Calcula el promedio
    avg_val = (min_val + max_val) / 2
  



# Generar tabla:
    
library(gt)
library(dplyr)
library(scales)

# Supongamos que este es tu DataFrame original
delitos <- tibble(
  `Delito Electoral` = c("Conteo limpio de los votos", "Compra de Votos", "Amenazas a votantes en las casillas",
                         "Recursos públicos para favorecer partidos políticos", "Pedir la credencial de elector"),
  `Intervalo de Confianza (90%)` = c("42.7-43.8%", "51.6-52.7%", "37.8-38.9%", "74.0-75.0%", "39.9-40.9%"),
  `¿Es delito?` = c("Sí", "Sí", "Sí", "Sí", "Sí"),
  `Multa` = c("50 a 100 días", "50 a 100 días", "100 a 200 días", "500 a 1000 días", "50 a 500 días"),
  `Prisión` = c("6 meses a 3 años", "No aplica", "2 a 6 años", "4 a 9 años", "2 a 6 años")
)

# Procesar los datos para extraer valores numéricos del intervalo de confianza
delitos <- delitos %>%
  mutate(
    `Intervalo de Confianza (90%)` = gsub("%", "", `Intervalo de Confianza (90%)`), # Eliminar el símbolo de porcentaje
    min_val = as.numeric(sub("-.*", "", `Intervalo de Confianza (90%)`)),          # Extraer el valor mínimo
    max_val = as.numeric(sub(".*-", "", `Intervalo de Confianza (90%)`)),          # Extraer el valor máximo
    avg_val = (min_val + max_val) / 2                                              # Calcular el promedio del rango
  )

# Crear la tabla formateada con colores y diseño profesional
delitos <- delitos %>%
  mutate(
    avg_val = c(42.75, 52.15, 38.35, 74.5, 40.35)
  )

# Crear la tabla formateada
tabla_delitos <- delitos %>%
  gt() %>%
  tab_header(
    title = md("**Tabla 1**. Resultados, en Porcentaje, sobre la Frecuencia de Delitos Electorales en México de acuerdo con la Población"),
    subtitle = md("Intervalos de confianza, multas y penas de prisión")
  ) %>%
  cols_label(
    `Delito Electoral` = md("**Delito Electoral**"),
    `Intervalo de Confianza (90%)` = md("**Intervalo de Confianza<br>(90%)**"),
    avg_val = md("**Promedio del Intervalo<br>(%)**"),
    min_val = md("**Min_val**"),
    max_val = md("**Max_val**"),
    `¿Es delito?` = md("**¿Es delito?**"),
    `Multa` = md("**Multa**"),
    `Prisión` = md("**Prisión**")
  ) %>%
  tab_footnote(
    footnote = "Los intervalos de confianza están calculados con base en la ENCUCI, INEGI (2020).",
    locations = cells_column_labels(columns = `Intervalo de Confianza (90%)`)
  ) %>%
  tab_footnote(
    footnote = "El promedio del intervalo representa un valor central calculado para facilitar la visualización.",
    locations = cells_column_labels(columns = avg_val)
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = ""
  ) %>%
  fmt_number(
    columns = avg_val,
    decimals = 2
  )

# Mostrar la tabla
tabla_delitos


gtsave(tabla_delitos, "C://Users//Eduardo//Documents//PC//SECCION1//Tesis_img//Tablas//tabla_delitos.docx")




### Candidaturas Independienetes:
library(tibble)

candidaturas <- tibble(
  Año = c(2015, 2016, 2017, 2018, 2019, 2021, 2022, 
          2015, 2016, 2017, 2018, 2019, 2021, 2022, 
          2015, 2018, 2021, 
          2018, 
          2018,
          2015), # Gubernatura
  Cargo = c(rep("Ayuntamientos", 7),
            rep("Congresos locales", 7),
            rep("Diputación federal", 3),
            "Senadores",
            "Presidencia de la república",
            "Gubernatura"), # Nueva categoría
  Independientes = c(
    # Ayuntamientos
    51, 171, 29, 334, 45, 197, 1,
    # Congresos locales
    51, 142, 48, 271, 6, 74, 1,
    # Diputación federal
    22, 38, 3,
    # Senadores
    7,
    # Presidencia de la república
    2,
    # Gubernatura
    1 # Ejemplo de número hipotético de candidatos
  ),
  `% Electos` = c(
    # Ayuntamientos
    8.64, 0, 0, 37.95, 0, 0, 0,
    # Congresos locales
    3.44, 1.72, 0, 3.44, 0, 39.65, 0,
    # Diputación federal
    1.72, 0, 0,
    # Senadores
    0,
    # Presidencia de la república
    0,
    # Gubernatura
    1.72
  )
)

print(candidaturas)






# Crear la tabla formateada

tabla_candidaturas <- candidaturas %>%
  gt() %>%
  tab_header(
    title = md("**Tabla 1**. Candidaturas Independientes Postuladas en México: Registros apartidistas 2015-2022"),
    subtitle = md("Número de Candidaturas y Porcentaje de Electos por Año")
  ) %>%
  cols_label(
    Año = md("**Año**"),
    Cargo = md("**Cargo**"),
    Independientes = md("**Registros**"),
    `% Electos` = md("**Electos<br>(%)**")
  ) %>%
  tab_footnote(
    footnote = "El porcentaje de candidaturas es respecto al total de electas entre 2015-2022, en este caso 58",
    locations = cells_column_labels(columns = `% Electos`)
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = ""
  ) %>%
  fmt_number(
    columns = `% Electos`,
    decimals = 2
  )

tabla_candidaturas

gtsave(tabla_candidaturas, "C://Users//Eduardo//Documents//PC//SECCION1//Tesis_img//Tablas//tabla_candidaturas.docx")





financiamiento_agrupado <- financiamiento %>%
  mutate(Proporcional = if_else(is.na(Proporcional), 0, Proporcional)) %>%
  group_by(Año, Concepto) %>%
  reframe(
    Igualitario = sum(Igualitario, na.rm = TRUE),
    Proporcional = sum(Proporcional)
  )

resultado <- financiamiento_agrupado %>%
  group_by(Año, Concepto) %>%             # Asegurar que los datos estén agrupados antes de sumar
  summarise(
    Igualitario = sum(Igualitario, na.rm = TRUE),
    Proporcional = sum(Proporcional, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Suma = Igualitario + Proporcional) %>%
  select(-Igualitario, -Proporcional)%>% # Crear la suma de Igualitario y Proporcional
  pivot_wider(
    names_from = Concepto,                      # Convertir los valores de Concepto en columnas
    values_from = Suma,                         # Rellenar las nuevas columnas con la suma
    values_fn = sum,                            # Sumar valores en caso de duplicados
    values_fill = 0                             # Rellenar NA con 0
  )


resultado <- financiamiento_agrupado %>%
  group_by(Año, Concepto) %>%                # Asegurar que los datos estén agrupados
  summarise(
    Igualitario = sum(Igualitario, na.rm = TRUE),
    Proporcional = sum(Proporcional, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Concepto,                   # Convertir los valores de Concepto en columnas
    values_from = c(Igualitario, Proporcional), # Rellenar con Igualitario y Proporcional
    values_fill = 0                          # Rellenar NA con 0
  )



# tabbla de financiamiento:
tabla_financiamiento <- resultado %>%
  gt() %>%
  tab_header(
    title = md("**Tabla 2**. Financiamiento Público: 2015-2022"),
    subtitle = md("Montos asignados por concepto en millones de pesos")
  ) %>%
  cols_label(
    Año = md("**Año**"),
    `Actividades Específicas` = md("**Actividades<br>Específicas**"),
    `Actividades Ordinarias Permanentes` = md("**Actividades<br>Ordinarias<br>Permanentes**"),
    `Franquicia Postal` = md("**Franquicia<br>Postal**"),
    `Franquicia Telegráfica` = md("**Franquicia<br>Telegráfica**"),
    `Gastos de Campaña` = md("**Gastos<br>de<br>Campaña**")
  ) %>%
  tab_footnote(
    footnote = "Los montos están expresados en millones de pesos y corresponden a registros oficiales.",
    locations = cells_column_labels(columns = everything())
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  fmt_number(
    columns = c(`Actividades Específicas`, `Actividades Ordinarias Permanentes`, 
                `Franquicia Postal`, `Franquicia Telegráfica`, `Gastos de Campaña`),
    decimals = 2
  )

gtsave(tabla_financiamiento, "C://Users//Eduardo//Documents//PC//SECCION1//Tesis_img//Tablas//tabla_financiamiento.docx")



# Financiamiento candidaturas independientes:
financiamiento_ind <- read_excel("C://Users//Eduardo//OneDrive//Documentos//FinanciamientoInd.xlsx")

resultado_ind <- financiamiento_ind %>%
  group_by(Año, Concepto) %>%             # Asegurar que los datos estén agrupados antes de sumar
  summarise(
    Monto = sum(Monto, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Concepto,                      # Convertir los valores de Concepto en columnas
    values_from = Monto,                         # Rellenar las nuevas columnas con la suma
    values_fn = sum,                            # Sumar valores en caso de duplicados
    values_fill = 0                             # Rellenar NA con 0
  )

valores_actividades <- c(`2015` = 73499461.10, `2018` = 85926664.92, `2021` = 98717899.96)

# Añadir la variable Actividades Ordinarias Legal usando un condicional
resultado_ind$`Gasto de Campaña Legal` <- sapply(resultado_ind$Año, function(x) valores_actividades[as.character(x)])


tabla_financiamiento_ind <- resultado_ind %>%
  gt() %>%
  tab_header(
    title = md("**Tabla 2**. Financiamiento Público Destinado para el Sostenimiento de las Candidaturas Independientes 2015-2021"),
    subtitle = md("Montos asignados por concepto en millones de pesos")
  ) %>%
  cols_label(
    Año = md("**Año**"),
    `Gasto de Campaña Legal` = md("**Gasto<br>de<br>Legal**"),
    `Franquicia Postal` = md("**Franquicia<br>Postal**"),
    `Gastos de Campaña` = md("**Gastos<br>de<br>Campaña**")
  ) %>%
  tab_footnote(
    footnote = "Los montos están expresados en millones de pesos y corresponden a registros oficiales. Esta variable es sobre la cantidad de financiamiento que las candidaturas idependientes deberieron recibier según señala la LEGIPE únicamente para Gastos de Campaña, el monto se obtiene al multiplicar las Actividades Ordinarias Permanentes por el 2%",
    locations = cells_column_labels(columns = `Gasto de Campaña Legal`)
  ) %>%
  tab_footnote(
    footnote = "Estas son las cantidades trasferidas en los años electorales, sustancialmente menores que los montos establecidos por la ley",
    locations = cells_column_labels(columns = `Gastos de Campaña`)
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  fmt_number(
    columns = c(`Gasto de Campaña Legal`, `Franquicia Postal`, `Gastos de Campaña`),
    decimals = 2
  )

gtsave(tabla_financiamiento_ind, "C://Users//Eduardo//Documents//PC//SECCION1//Tesis_img//Tablas//tabla_financiamiento_ind.docx")









# Nombres de los estados de México
estados <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas") 

# Datos de aprobación (TRUE si fue aprobada, FALSE si no fue votada o sesionada)
aprobada <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)

# Crear tibble
datos <- tibble(Estado = estados, Aprobada = ifelse(aprobada, "A favor", "No Votado / No Sesionado"))

# Mostrar tibble
print(datos)


tabla_votacionestados <- datos %>%
  gt() %>%
  tab_header(
    title = md("**Tabla Anexo**. Votaciones para la Aprobación de la Reforma Política"),
    subtitle = md("En los Congresos Locales 2014")
  ) %>%
  cols_label(
    Estado = md("**Estado**"),
    Aprobada = md("**Posicionamiento**")
  ) %>%
  tab_footnote(
    footnote = "La información sobre el posicionamiento de los estados lo brinda Zamitz (2017)",
    locations = cells_column_labels(columns = Aprobada)
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  fmt_number(
    columns = c(Estado, Aprobada),
    decimals = 2
  )

gtsave(tabla_votacionestados, "C://Users//Eduardo//Documents//PC//SECCION1//Tesis_img//Tablas//tabla_estados_votos.docx")


cadidatos_indelectos <- read_excel("C://Users//Eduardo//OneDrive//Documentos//CANDINDLIST.xlsx")
cadidatos_indelectos


tabla_independientes <- cadidatos_indelectos %>%
  gt() %>%
  tab_header(
    title = md("**Tabla Anexo**. Candidaturas Independientes Electas en México 2015-2021"),
    subtitle = md("Agrupador por Entidad, Sexo y Cargo")
  ) %>%
  cols_label(
    Año = md("**Año**"),
    Nombre = md("**Nombre**"),
    Entidad = md("**Entidad<br>Federativa**"),
    Sexo = md ("**Sexo**"),
    Cargo = md("**Cargo de<br>Elección<br>Popular**")
  ) %>%
  tab_footnote(
    footnote = "La información sobre los candidatos electos puede comprobarse accediendo a la API",
    locations = cells_column_labels(columns = Cargo)
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  fmt_number(
    columns = c(Año, Nombre, Entidad, Sexo, Cargo),
    decimals = 0
  )

gtsave(tabla_independientes, "C://Users//Eduardo//Documents//PC//SECCION1//Tesis_img//Tablas//tabla_independientes.docx")





# Crear el tibble
registros <- tibble(
  Entidad_Federativa = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", "Ciudad de México", "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas"),
  Lista_Nominal = c(928811, 1626422, 348487, 544901, 3651957, 2609755, 7608087, 2428355, 558785, 1340637, 4861823, 2780638, 2189737, 6093669, 12555623, 3646853, 1500000, 920225, 3915795, 3012155, 4796193, 1598705, 525339, 2051472, 2184393, 2343839, 1672073, 2048544, 912750, 5795483, 1612794, 1629140),
  Periodo = rep(60, 32),
  Firmas_por_Recaudar = c(18556, 32521, 6969, 10898, 73039, 52195, 152162, 48567, 11176, 26813, 97237, 55613, 43795, 121874, 251113, 73039, 30000, 18405, 78316, 60243, 95924, 31974, 10507, 41029, 43795, 46877, 33441, 40971, 18255, 115910, 32256, 32583),
  Firmas_por_Día = c(308, 542, 116, 182, 1217, 870, 2536, 810, 186, 447, 1621, 927, 730, 2031, 4185, 1217, 500, 307, 1305, 1004, 1599, 533, 175, 684, 730, 781, 558, 683, 304, 1932, 538, 543)
)


tabla_registros <- registros %>%
  gt() %>%
  tab_header(
    title = md("**Tabla Anexo**. Firmas Para Registrar una Candidatura Independiente en México al Congreso de la Unión"),
    subtitle = md("Apoyo Ciudadano Total Y Diario")
  ) %>%
  cols_label(
    Entidad_Federativa = md("**Entidad<br>Federativa**"),
    Lista_Nominal = md("**Lista Nominal**"),
    Periodo = md("**Días para <br> Recoger <br> Firmas**"),
    Firmas_por_Recaudar = md ("**Firmas Totales**"),
    Firmas_por_Día = md("**Firmas<br>Diarias**")
  ) %>%
  tab_footnote(
    footnote = "La información sobre los candidatos electos puede comprobarse accediendo a los datos del INE, ",
    locations = cells_column_labels(columns = Firmas_por_Día)
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  fmt_number(
    columns = c(Entidad_Federativa, Lista_Nominal, Periodo, Firmas_por_Recaudar, Firmas_por_Día),
    decimals = 0
  )

gtsave(tabla_registros, "C://Users//Eduardo//Documents//PC//SECCION1//Tesis_img//Tablas//tabla_registros.docx")
