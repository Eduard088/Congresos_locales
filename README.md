# Procesos Electorales en los Congresos Locales y Federales en México (2015-2023)

## Descripción del Proyecto

Este proyecto se centra en la limpieza, integración y análisis de datos de los procesos electorales en los congresos locales de México entre 2015 y 2023, así como en la elección de diputaciones federales y senadores. Se utilizan registros del Sistema de Estadística de Consulta de los Procesos Electorales del INE y datos del Censo de Poderes Legislativos del INEGI.

El objetivo principal es unir y estructurar estos datos para obtener una visión integral de las elecciones a nivel local y federal, analizando los resultados y las tendencias en la conformación de los congresos.

## Estructura del Proyecto

```
Proyecto_Electoral/
├── limpieza/               # Archivos de limpieza y transformación de datos
│   ├── votos_2015.qmd
│   ├── votos_2016.qmd
│   ├── votos_2018.qmd
│   ├── votos_2019.qmd
│   ├── votos_2021.qmd
│   ├── votos_2022.qmd
│   ├── Votos_Federal_2015.qmd
│   ├── Votos_Federal_2018.qmd
│   ├── Votos_Federal_2021.qmd
│   ├── Votos_Federal_Senado_2018.qmd
│
├── data/                   # Datos utilizados en el análisis
│   ├── Base/               # Base de datos consolidada
│   ├── Integrantes/        # Información de legisladores electos
│
└── README.md               # Documentación del proyecto
```

## Requisitos

- **R (versión 4.0 o superior)**
- **Paquetes de R**: `tidyverse`, `dplyr`, `ggplot2`, `readr`, `janitor`, `lubridate`, `quarto`
- **Quarto** para la generación de reportes dinámicos

## Instalación

1. Clona este repositorio:
    ```sh
    git clone https://github.com/tu_usuario/tu_repositorio.git
    ```
2. Instala los paquetes necesarios en R:
    ```R
    install.packages(c("tidyverse", "dplyr", "ggplot2", "readr", "janitor", "lubridate"))
    ```

## Uso

1. Ejecuta los scripts de transformación de datos según el año de interés:
    ```sh
    quarto render limpieza/votos_2018.qmd
    ```
2. Explora los datos procesados en la carpeta `data/Base` y `data/Integrantes`.

## Contribuciones

Las contribuciones son bienvenidas. Si deseas colaborar, por favor abre un issue o envía un pull request con mejoras o análisis adicionales.

## Licencia

Este proyecto está bajo la Licencia MIT. Consulta el archivo `LICENSE` para más detalles.

## Contacto

Para cualquier consulta o sugerencia, puedes escribirme a: tu_email@ejemplo.com

