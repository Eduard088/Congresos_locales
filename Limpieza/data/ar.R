### PROCESO ELECTORAL 2015:

congreso2015 <- congreso2015 %>%
  mutate(Zona = case_when(
    NOMBRE_ESTADO %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", 
                         "COAHUILA", "DURANGO", "NUEVO LEÓN", "SONORA", 
                         "TAMAULIPAS") ~ "Norte",
    NOMBRE_ESTADO %in% c("SINALOA", "NAYARIT", "COLIMA", "JALISCO", 
                         "MICHOACÁN", "ZACATECAS") ~ "Occidente",
    NOMBRE_ESTADO %in% c("AGUASCALIENTES", "CIUDAD DE MÉXICO", "MEXICO", 
                         "GUANAJUATO", "HIDALGO", "MORELOS", "QUERETARO", 
                         "SAN LUIS POTOSI", "TLAXCALA") ~ "Centro",
    NOMBRE_ESTADO %in% c("CHIAPAS", "GUERRERO", "OAXACA", "PUEBLA", "VERACRUZ") ~ "Sur",
    NOMBRE_ESTADO %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN") ~ "Península de Yucatán y Golfo",
    TRUE ~ NA_character_
  ))

congreso2015 <- congreso2015%>%
  mutate(Espectro = case_when(
    Partido == "MC" ~ "Izquierda Indefinida",
    Partido == "MORENA" ~ "Izquierda Indefinida",
    Partido == "NUAL" ~ "Izquierda Indefinida",
    Partido == "Otro" ~ "Movimiento Alterno",
    Partido == "PAN" ~ "Derecha Extravagante",
    Partido == "PES" ~ "Derecha Extravagante",
    Partido == "PRD" ~ "Izquierda Indefinida",
    Partido == "PRI" ~ "Izquierda Indefinida",
    Partido == "PT" ~ "Izquierda Indefinida",
    Partido == "PVEM" ~ "Izquierda Indefinida",
    TRUE ~ NA_character_
  ))

congreso2015 <- congreso2015%>%
  mutate(Presidencia = case_when(
    Partido == "MC" ~ "No",
    Partido == "MORENA" ~ "No",
    Partido == "NUAL" ~ "No",
    Partido == "Otro" ~ "No",
    Partido == "PAN" ~ "No",
    Partido == "PES" ~ "No",
    Partido == "PRD" ~ "No",
    Partido == "PRI" ~ "Sí",
    Partido == "PT" ~ "No",
    Partido == "PVEM" ~ "Sí",
    TRUE ~ NA_character_
  ))

congreso2015 <- congreso2015%>% 
  select(-Presidencia)


## Colocar el año en los tibble:
congreso2015 <- congreso2015 %>%
  mutate(año = 2015) %>%
  relocate(año, .before = 1)




#### Proceso Electoral 2016:
congreso2016_1 <- congreso2016_1 %>%
  mutate(Zona = case_when(
    NOMBRE_ESTADO %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", 
                         "COAHUILA", "DURANGO", "NUEVO LEÓN", "SONORA", 
                         "TAMAULIPAS") ~ "Norte",
    NOMBRE_ESTADO %in% c("SINALOA", "NAYARIT", "COLIMA", "JALISCO", 
                         "MICHOACÁN", "ZACATECAS") ~ "Occidente",
    NOMBRE_ESTADO %in% c("AGUASCALIENTES", "CIUDAD DE MÉXICO", "MEXICO", 
                         "GUANAJUATO", "HIDALGO", "MORELOS", "QUERETARO", 
                         "SAN LUIS POTOSI", "TLAXCALA") ~ "Centro",
    NOMBRE_ESTADO %in% c("CHIAPAS", "GUERRERO", "OAXACA", "PUEBLA", "VERACRUZ") ~ "Sur",
    NOMBRE_ESTADO %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN") ~ "Península de Yucatán y Golfo",
    TRUE ~ NA_character_
  ))

congreso2016_1 <- congreso2016_1 %>%
  mutate(Espectro = case_when(
    Partido == "MC" ~ "Izquierda Indefinida",
    Partido == "MORENA" ~ "Izquierda Indefinida",
    Partido == "NUAL" ~ "Izquierda Indefinida",
    Partido == "Otro" ~ "Movimiento Alterno",
    Partido == "PAN" ~ "Derecha Extravagante",
    Partido == "PRD" ~ "Izquierda Indefinida",
    Partido == "PRI" ~ "Izquierda Indefinida",
    Partido == "PT" ~ "Izquierda Indefinida",
    Partido == "PVEM" ~ "Izquierda Indefinida",
    TRUE ~ NA_character_
  ))
congreso2016_1 <- congreso2016_1 %>%
  mutate(Presidencia = case_when(
    Partido == "MC" ~ "No",
    Partido == "MORENA" ~ "No",
    Partido == "NUAL" ~ "No",
    Partido == "Otro" ~ "No",
    Partido == "PAN" ~ "No",
    Partido == "PRD" ~ "No",
    Partido == "PRI" ~ "Sí",
    Partido == "PT" ~ "No",
    Partido == "PVEM" ~ "Sí",
    TRUE ~ NA_character_
  ))

congreso2016_1 <- congreso2016_1 %>%
  mutate(año = 2016) %>%
  relocate(año, .before = 1)

# Supongamos que deseas eliminar la columna llamada "nombre_columna"
congreso2016_1 <- congreso2016_1 %>% 
  select(-año)



### Proceso Electoral 2017:
congreso2017_1 <- congreso2017_1 %>%
  mutate(Zona = case_when(
    NOMBRE_ESTADO %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", 
                         "COAHUILA", "DURANGO", "NUEVO LEÓN", "SONORA", 
                         "TAMAULIPAS") ~ "Norte",
    NOMBRE_ESTADO %in% c("SINALOA", "NAYARIT", "COLIMA", "JALISCO", 
                         "MICHOACÁN", "ZACATECAS") ~ "Occidente",
    NOMBRE_ESTADO %in% c("AGUASCALIENTES", "CIUDAD DE MÉXICO", "MEXICO", 
                         "GUANAJUATO", "HIDALGO", "MORELOS", "QUERETARO", 
                         "SAN LUIS POTOSI", "TLAXCALA") ~ "Centro",
    NOMBRE_ESTADO %in% c("CHIAPAS", "GUERRERO", "OAXACA", "PUEBLA", "VERACRUZ") ~ "Sur",
    NOMBRE_ESTADO %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN") ~ "Península de Yucatán y Golfo",
    TRUE ~ NA_character_
))

congreso2017_1 <- congreso2017_1 %>%
  mutate(Espectro = case_when(
    Partido == "MC" ~ "Izquierda Indefinida",
    Partido == "MORENA" ~ "Izquierda Indefinida",
    Partido == "NUAL" ~ "Izquierda Indefinida",
    Partido == "Otro" ~ "Movimiento Alterno",
    Partido == "PAN" ~ "Derecha Extravagante",
    Partido == "PRD" ~ "Izquierda Indefinida",
    Partido == "PRI" ~ "Izquierda Indefinida",
    Partido == "PT" ~ "Izquierda Indefinida",
    TRUE ~ NA_character_
  ))
congreso2017_1 <- congreso2017_1 %>%
  mutate(Presidencia = case_when(
    Partido == "MC" ~ "No",
    Partido == "MORENA" ~ "No",
    Partido == "NUAL" ~ "No",
    Partido == "Otro" ~ "No",
    Partido == "PAN" ~ "No",
    Partido == "PRD" ~ "No",
    Partido == "PRI" ~ "Sí",
    Partido == "PT" ~ "No",
    TRUE ~ NA_character_
  ))

congreso2017_1 <- congreso2017_1 %>%
  mutate(año = 2017) %>%
  relocate(año, .before = 1)


### Proceso Electoral 2018:

congreso2018_1 <- congreso2018_1 %>%
  mutate(Zona = case_when(
    NOMBRE_ESTADO %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", 
                         "COAHUILA", "DURANGO", "NUEVO LEÓN", "SONORA", 
                         "TAMAULIPAS") ~ "Norte",
    NOMBRE_ESTADO %in% c("SINALOA", "NAYARIT", "COLIMA", "JALISCO", 
                         "MICHOACÁN", "ZACATECAS") ~ "Occidente",
    NOMBRE_ESTADO %in% c("AGUASCALIENTES", "CIUDAD DE MÉXICO", "MEXICO", 
                         "GUANAJUATO", "HIDALGO", "MORELOS", "QUERETARO", 
                         "SAN LUIS POTOSI", "TLAXCALA") ~ "Centro",
    NOMBRE_ESTADO %in% c("CHIAPAS", "GUERRERO", "OAXACA", "PUEBLA", "VERACRUZ") ~ "Sur",
    NOMBRE_ESTADO %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN") ~ "Península de Yucatán y Golfo",
    TRUE ~ NA_character_
  ))

congreso2018_1 <- congreso2018_1 %>%
  mutate(Espectro = case_when(
    Partido == "MC" ~ "Izquierda Indefinida",
    Partido == "MORENA" ~ "Izquierda Indefinida",
    Partido == "NUAL" ~ "Izquierda Indefinida",
    Partido == "Otro" ~ "Movimiento Alterno",
    Partido == "PAN" ~ "Derecha Extravagante",
    Partido == "PRD" ~ "Izquierda Indefinida",
    Partido == "PRI" ~ "Izquierda Indefinida",
    Partido == "PT" ~ "Izquierda Indefinida",
    Partido == "PVEM" ~ "Izquierda Indefinida",
    TRUE ~ NA_character_
  ))
congreso2018_1 <- congreso2018_1 %>%
  mutate(Presidencia = case_when(
    Partido == "MC" ~ "No",
    Partido == "MORENA" ~ "No",
    Partido == "NUAL" ~ "No",
    Partido == "Otro" ~ "No",
    Partido == "PAN" ~ "No",
    Partido == "PRD" ~ "No",
    Partido == "PRI" ~ "Sí",
    Partido == "PT" ~ "No",
    Partido == "PVEM" ~ "Sí",
    TRUE ~ NA_character_
  ))

congreso2018_1 <- congreso2018_1 %>%
  mutate(año = 2018) %>%
  relocate(año, .before = 1)


### Proceso electoral 2019:

Congreso_local_2019_f <- Congreso_local_2019_f %>%
  mutate(Zona = case_when(
    NOMBRE_ESTADO.x %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", 
                         "COAHUILA", "DURANGO", "NUEVO LEÓN", "SONORA", 
                         "TAMAULIPAS") ~ "Norte",
    NOMBRE_ESTADO.x %in% c("SINALOA", "NAYARIT", "COLIMA", "JALISCO", 
                         "MICHOACÁN", "ZACATECAS") ~ "Occidente",
    NOMBRE_ESTADO.x %in% c("AGUASCALIENTES", "CIUDAD DE MÉXICO", "MEXICO", 
                         "GUANAJUATO", "HIDALGO", "MORELOS", "QUERETARO", 
                         "SAN LUIS POTOSI", "TLAXCALA") ~ "Centro",
    NOMBRE_ESTADO.x %in% c("CHIAPAS", "GUERRERO", "OAXACA", "PUEBLA", "VERACRUZ") ~ "Sur",
    NOMBRE_ESTADO.x %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN") ~ "Península de Yucatán y Golfo",
    TRUE ~ NA_character_
  ))

Congreso_local_2019_f <- Congreso_local_2019_f %>%
  mutate(Espectro = case_when(
    Partido == "MC" ~ "Izquierda Indefinida",
    Partido == "MORENA" ~ "Izquierda Indefinida",
    Partido == "Otro" ~ "Movimiento Alterno",
    Partido == "PAN" ~ "Derecha Extravagante",
    Partido == "PRD" ~ "Izquierda Indefinida",
    Partido == "PRI" ~ "Izquierda Indefinida",
    Partido == "PT" ~ "Izquierda Indefinida",
    Partido == "PVEM" ~ "Izquierda Indefinida",
    TRUE ~ NA_character_
  ))
Congreso_local_2019_f <- Congreso_local_2019_f %>%
  mutate(Presidencia = case_when(
    Partido == "MC" ~ "No",
    Partido == "MORENA" ~ "Sí",
    Partido == "NUAL" ~ "No",
    Partido == "Otro" ~ "No",
    Partido == "PAN" ~ "No",
    Partido == "PRD" ~ "No",
    Partido == "PRI" ~ "No",
    Partido == "PT" ~ "Sí",
    Partido == "PVEM" ~ "Sí",
    TRUE ~ NA_character_
  ))


Congreso_local_2019_f <- Congreso_local_2019_f %>%
  mutate(año = 2019) %>%
  relocate(año, .before = 1)


Congreso_local_2019_f <- Congreso_local_2019_f %>%
  rename(NOMBRE_ESTADO = NOMBRE_ESTADO.x)

### Proceso electoral 2020:

Congreso_local_2020_f <- Congreso_local_2020_f %>%
  mutate(Zona = case_when(
    NOMBRE_ESTADO %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", 
                           "COAHUILA", "DURANGO", "NUEVO LEÓN", "SONORA", 
                           "TAMAULIPAS") ~ "Norte",
    NOMBRE_ESTADO %in% c("SINALOA", "NAYARIT", "COLIMA", "JALISCO", 
                           "MICHOACÁN", "ZACATECAS") ~ "Occidente",
    NOMBRE_ESTADO %in% c("AGUASCALIENTES", "CIUDAD DE MÉXICO", "MEXICO", 
                           "GUANAJUATO", "HIDALGO", "MORELOS", "QUERETARO", 
                           "SAN LUIS POTOSI", "TLAXCALA") ~ "Centro",
    NOMBRE_ESTADO %in% c("CHIAPAS", "GUERRERO", "OAXACA", "PUEBLA", "VERACRUZ") ~ "Sur",
    NOMBRE_ESTADO %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN") ~ "Península de Yucatán y Golfo",
    TRUE ~ NA_character_
  ))

Congreso_local_2020_f <- Congreso_local_2020_f %>%
  mutate(Espectro = case_when(
    Partido == "MC" ~ "Izquierda Indefinida",
    Partido == "MORENA" ~ "Izquierda Indefinida",
    Partido == "Otro" ~ "Movimiento Alterno",
    Partido == "PAN" ~ "Derecha Extravagante",
    Partido == "PRD" ~ "Izquierda Indefinida",
    Partido == "PRI" ~ "Izquierda Indefinida",
    Partido == "PT" ~ "Izquierda Indefinida",
    Partido == "PVEM" ~ "Izquierda Indefinida",
    TRUE ~ NA_character_
  ))
Congreso_local_2020_f <- Congreso_local_2020_f %>%
  mutate(Presidencia = case_when(
    Partido == "MC" ~ "No",
    Partido == "MORENA" ~ "Sí",
    Partido == "NUAL" ~ "No",
    Partido == "Otro" ~ "No",
    Partido == "PAN" ~ "No",
    Partido == "PRD" ~ "No",
    Partido == "PRI" ~ "No",
    Partido == "PT" ~ "Sí",
    Partido == "PVEM" ~ "Sí",
    TRUE ~ NA_character_
  ))

Congreso_local_2020_f <- Congreso_local_2020_f %>%
  mutate(año = 2019) %>%
  relocate(año, .before = 1)


Congreso_local_2020_f <- Congreso_local_2020_f %>%
  mutate(Zona = case_when(
    NOMBRE_ESTADO %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", 
                           "COAHUILA", "DURANGO", "NUEVO LEÓN", "SONORA", 
                           "TAMAULIPAS") ~ "Norte",
    NOMBRE_ESTADO %in% c("SINALOA", "NAYARIT", "COLIMA", "JALISCO", 
                           "MICHOACÁN", "ZACATECAS") ~ "Occidente",
    NOMBRE_ESTADO %in% c("AGUASCALIENTES", "CIUDAD DE MÉXICO", "MEXICO", 
                           "GUANAJUATO", "HIDALGO", "MORELOS", "QUERETARO", 
                           "SAN LUIS POTOSI", "TLAXCALA") ~ "Centro",
    NOMBRE_ESTADO %in% c("CHIAPAS", "GUERRERO", "OAXACA", "PUEBLA", "VERACRUZ") ~ "Sur",
    NOMBRE_ESTADO %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN") ~ "Península de Yucatán y Golfo",
    TRUE ~ NA_character_
  ))

Congreso_local_2020_f <- Congreso_local_2020_f %>%
  mutate(Espectro = case_when(
    Partido == "MC" ~ "Izquierda Indefinida",
    Partido == "MORENA" ~ "Izquierda Indefinida",
    Partido == "Otro" ~ "Movimiento Alterno",
    Partido == "PAN" ~ "Derecha Extravagante",
    Partido == "PRD" ~ "Izquierda Indefinida",
    Partido == "PRI" ~ "Izquierda Indefinida",
    Partido == "PT" ~ "Izquierda Indefinida",
    Partido == "PVEM" ~ "Izquierda Indefinida",
    TRUE ~ NA_character_
  ))
Congreso_local_2020_f <- Congreso_local_2020_f %>%
  mutate(Presidencia = case_when(
    Partido == "MC" ~ "No",
    Partido == "MORENA" ~ "Sí",
    Partido == "NUAL" ~ "No",
    Partido == "Otro" ~ "No",
    Partido == "PAN" ~ "No",
    Partido == "PRD" ~ "No",
    Partido == "PRI" ~ "No",
    Partido == "PT" ~ "Sí",
    Partido == "PVEM" ~ "Sí",
    TRUE ~ NA_character_
  ))

Congreso_local_2020_f <- Congreso_local_2020_f %>%
  mutate(año = 2020) %>%
  relocate(año, .before = 1)


### Proceso electoral 2021:

Congreso_local_2021_f1 <- Congreso_local_2021_f1 %>%
  mutate(Zona = case_when(
    NOMBRE_ESTADO %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", 
                         "COAHUILA", "DURANGO", "NUEVO LEÓN", "SONORA", 
                         "TAMAULIPAS") ~ "Norte",
    NOMBRE_ESTADO %in% c("SINALOA", "NAYARIT", "COLIMA", "JALISCO", 
                         "MICHOACÁN", "ZACATECAS") ~ "Occidente",
    NOMBRE_ESTADO %in% c("AGUASCALIENTES", "CIUDAD DE MÉXICO", "MEXICO", 
                         "GUANAJUATO", "HIDALGO", "MORELOS", "QUERETARO", 
                         "SAN LUIS POTOSI", "TLAXCALA") ~ "Centro",
    NOMBRE_ESTADO %in% c("CHIAPAS", "GUERRERO", "OAXACA", "PUEBLA", "VERACRUZ") ~ "Sur",
    NOMBRE_ESTADO %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN") ~ "Península de Yucatán y Golfo",
    TRUE ~ NA_character_
  ))

Congreso_local_2021_f1 <- Congreso_local_2021_f1 %>%
  mutate(Espectro = case_when(
    Partido == "MC" ~ "Izquierda Indefinida",
    Partido == "MORENA" ~ "Izquierda Indefinida",
    Partido == "Otro" ~ "Movimiento Alterno",
    Partido == "PAN" ~ "Derecha Extravagante",
    Partido == "PRD" ~ "Izquierda Indefinida",
    Partido == "PRI" ~ "Izquierda Indefinida",
    Partido == "PT" ~ "Izquierda Indefinida",
    Partido == "PVEM" ~ "Izquierda Indefinida",
    TRUE ~ NA_character_
  ))
Congreso_local_2021_f1 <- Congreso_local_2021_f1 %>%
  mutate(Presidencia = case_when(
    Partido == "MC" ~ "No",
    Partido == "MORENA" ~ "Sí",
    Partido == "NUAL" ~ "No",
    Partido == "Otro" ~ "No",
    Partido == "PAN" ~ "No",
    Partido == "PRD" ~ "No",
    Partido == "PRI" ~ "No",
    Partido == "PT" ~ "Sí",
    Partido == "PVEM" ~ "Sí",
    TRUE ~ NA_character_
  ))

Congreso_local_2021_f1 <- Congreso_local_2021_f1 %>%
  mutate(año = 2021) %>%
  relocate(año, .before = 1)


### Proceso electoral 2022:

Congreso_local_2022_f1 <- Congreso_local_2022_f1 %>%
  mutate(Zona = case_when(
    NOMBRE_ESTADO %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", 
                         "COAHUILA", "DURANGO", "NUEVO LEÓN", "SONORA", 
                         "TAMAULIPAS") ~ "Norte",
    NOMBRE_ESTADO %in% c("SINALOA", "NAYARIT", "COLIMA", "JALISCO", 
                         "MICHOACÁN", "ZACATECAS") ~ "Occidente",
    NOMBRE_ESTADO %in% c("AGUASCALIENTES", "CIUDAD DE MÉXICO", "MEXICO", 
                         "GUANAJUATO", "HIDALGO", "MORELOS", "QUERETARO", 
                         "SAN LUIS POTOSI", "TLAXCALA") ~ "Centro",
    NOMBRE_ESTADO %in% c("CHIAPAS", "GUERRERO", "OAXACA", "PUEBLA", "VERACRUZ") ~ "Sur",
    NOMBRE_ESTADO %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN") ~ "Península de Yucatán y Golfo",
    TRUE ~ NA_character_
  ))

Congreso_local_2022_f1 <- Congreso_local_2022_f1 %>%
  mutate(Espectro = case_when(
    Partido == "MC" ~ "Izquierda Indefinida",
    Partido == "MORENA" ~ "Izquierda Indefinida",
    Partido == "Otro" ~ "Movimiento Alterno",
    Partido == "PAN" ~ "Derecha Extravagante",
    Partido == "PRD" ~ "Izquierda Indefinida",
    Partido == "PRI" ~ "Izquierda Indefinida",
    Partido == "PT" ~ "Izquierda Indefinida",
    Partido == "PVEM" ~ "Izquierda Indefinida",
    TRUE ~ NA_character_
  ))
Congreso_local_2022_f1 <- Congreso_local_2022_f1 %>%
  mutate(Presidencia = case_when(
    Partido == "MC" ~ "No",
    Partido == "MORENA" ~ "Sí",
    Partido == "NUAL" ~ "No",
    Partido == "Otro" ~ "No",
    Partido == "PAN" ~ "No",
    Partido == "PRD" ~ "No",
    Partido == "PRI" ~ "No",
    Partido == "PT" ~ "Sí",
    Partido == "PVEM" ~ "Sí",
    TRUE ~ NA_character_
  ))

Congreso_local_2022_f1 <- Congreso_local_2022_f1 %>%
  mutate(año = 2022) %>%
  relocate(año, .before = 1)



## Unir tibbles:

Tibble_ml <- bind_rows(congreso2015, congreso2016_1, congreso2017_1, congreso2018_1,
                       Congreso_local_2019_f, Congreso_local_2020_f, Congreso_local_2021_f1,
                       Congreso_local_2022_f1)



Tibble_ml <- Tibble_ml %>%
  mutate(NOMBRE_ESTADO = ifelse(NOMBRE_ESTADO == "YUCATÁN", "YUCATAN", NOMBRE_ESTADO))

Tibble_ml <- Tibble_ml %>%
  mutate(NOMBRE_ESTADO = ifelse(NOMBRE_ESTADO == "VERACRÚZ", "VERACRUZ", NOMBRE_ESTADO))

Tibble_ml <- Tibble_ml %>%
  mutate(NOMBRE_ESTADO = ifelse(NOMBRE_ESTADO == "SAN LUIS POTOSÍ", "SAN LUIS POTOSI", NOMBRE_ESTADO))

Tibble_ml <- Tibble_ml %>%
  mutate(NOMBRE_ESTADO = ifelse(NOMBRE_ESTADO == "QUERÉTARO", "QUERETARO", NOMBRE_ESTADO))


Tibble_ml <- Tibble_ml %>%
  mutate(NOMBRE_ESTADO = ifelse(NOMBRE_ESTADO == "MICHOACÁN", "MICHOACAN", NOMBRE_ESTADO))

Tibble_ml <- Tibble_ml %>%
  mutate(NOMBRE_ESTADO = ifelse(NOMBRE_ESTADO == "ESTADO DE MÉXICO", "MÉXICO", NOMBRE_ESTADO))

Tibble_ml <- Tibble_ml %>%
  mutate(NOMBRE_ESTADO = ifelse(NOMBRE_ESTADO == "CIUDAD DE MÉXICO", "CIUDAD DE MÉXICO", NOMBRE_ESTADO))

Tibble_ml <- Tibble_ml %>%
  mutate(NOMBRE_ESTADO = ifelse(NOMBRE_ESTADO == "NUEVO LEÓN", "NUEVO LEON", NOMBRE_ESTADO))

Tibble_ml <- Tibble_ml %>%
  mutate(NOMBRE_ESTADO = ifelse(NOMBRE_ESTADO == "MÉXICO", "MEXICO", NOMBRE_ESTADO))


Tibble_ml <- Tibble_ml %>%
  mutate(Zona = case_when(
    NOMBRE_ESTADO %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", 
                         "COAHUILA", "DURANGO", "NUEVO LEON", "SONORA", 
                         "TAMAULIPAS") ~ "Norte",
    NOMBRE_ESTADO %in% c("SINALOA", "NAYARIT", "COLIMA", "JALISCO", 
                         "MICHOACAN", "ZACATECAS") ~ "Occidente",
    NOMBRE_ESTADO %in% c("AGUASCALIENTES", "CIUDAD DE MÉXICO", "MEXICO", 
                         "GUANAJUATO", "HIDALGO", "MORELOS", "QUERETARO", 
                         "SAN LUIS POTOSI", "TLAXCALA") ~ "Centro",
    NOMBRE_ESTADO %in% c("CHIAPAS", "GUERRERO", "OAXACA", "PUEBLA", "VERACRUZ") ~ "Sur",
    NOMBRE_ESTADO %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN") ~ "Península de Yucatán y Golfo",
    TRUE ~ NA_character_
  ))


## guardar el tibble:
  
write_xlsx(Tibble_ml, "Tibble_ml.xlsx")

Tibble_ml <- Tibble_ml %>%
  rename(Año = año)





#### uniones para exportacion de tibble:

REGISVOT <-  read_excel("C://Users//Eduardo//Documents//PC//RANGOSVOT.xlsx")



Tibble_mll <- Tibble_ml %>%
  left_join(REGISVOT, by = c("Año", "ID_ESTADO"))


Tibble_mll <- Tibble_ml %>%
  mutate(Zona = case_when(
    NOMBRE_ESTADO %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", 
                         "COAHUILA", "DURANGO", "NUEVO LEON", "SONORA", 
                         "TAMAULIPAS") ~ "Norte",
    NOMBRE_ESTADO %in% c("SINALOA", "NAYARIT", "COLIMA", "JALISCO", 
                         "MICHOACAN", "ZACATECAS") ~ "Occidente",
    NOMBRE_ESTADO %in% c("AGUASCALIENTES", "CIUDAD DE MÉXICO", "MEXICO", 
                         "GUANAJUATO", "HIDALGO", "MORELOS", "QUERETARO", 
                         "SAN LUIS POTOSI", "TLAXCALA") ~ "Centro",
    NOMBRE_ESTADO %in% c("CHIAPAS", "GUERRERO", "OAXACA", "PUEBLA", "VERACRUZ") ~ "Sur",
    NOMBRE_ESTADO %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN") ~ "Península de Yucatán y Golfo",
    TRUE ~ NA_character_
  ))



Tibble_mll <- Tibble_mll %>%
  mutate(Sufragios = case_when(
    PARTICIPACION >= 0 & PARTICIPACION <= 0.25 ~ "Muy bajo",
    PARTICIPACION > 0.25 & PARTICIPACION <= 0.50 ~ "Bajo",
    PARTICIPACION > 0.50 & PARTICIPACION <= 0.75 ~ "Alto",
    PARTICIPACION > 0.75 & PARTICIPACION <= 1 ~ "Muy alto",
    TRUE ~ NA_character_
  ))



### ahora si, el tibbke exportado:
write_xlsx(Tibble_mll, "Tibble_mlar.xlsx")
