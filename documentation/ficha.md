# Ficha del artículo

Completa esta ficha al crear el repositorio y mantenla al día. Es el único
lugar con datos propios del artículo; el README es igual en todos los
repositorios.

## Identificación

- **Título:** Los hombres ecuatorianos dedican cada vez más tiempo a la cocina ¿Qué indican los datos?
- **Descripción del repositorio:** Repositorio del artículo de Christian Salas: "Los hombres ecuatorianos dedican cada vez más tiempo a la cocina ¿Qué indican los datos?"
- **Autoría:** Christian Salas Márquez
- **Fecha de publicación:** 2022-09-24
- **Artículo publicado:** https://elquantificador.org/post/valores/2022-09-24-uso-del-tiempo-cocina/
- **Categoría:** Valores y comportamiento
- **Edición:** Julio 2022-Enero 2023

## Estado

Publicado el 2022-09-24. La cabecera de `report/texto-articulo1.Rmd` conserva
la fecha de borrador 2022-08-22.

## Pregunta y alcance

El artículo compara cuánto tiempo semanal dedican hombres y mujeres a cocinar o
preparar alimentos en Ecuador entre 2012 y 2019 (se toma 2019 por ser el último
año antes de la pandemia). Calcula promedios ponderados de horas semanales a
partir de dos encuestas del INEC representativas a nivel nacional, por sexo,
por grupo de edad y por provincia. La unidad de análisis es la persona (12
años o más en 2012; 5 años o más en 2019, según la población objetivo de cada
encuesta).

## Requisitos

- R: no documentada en el repositorio.
- Paquetes de R: foreign, haven, knitr, patchwork, radiant.data, readr,
  rstudioapi, srvyr, survey, tidyverse. Los scripts los cargan con
  `require()` y los instalan si faltan.

## Fuentes de datos

| Fuente | Institución | Periodo | Acceso | Script |
|---|---|---|---|---|
| Encuesta Específica de Uso del Tiempo (EUT) 2012 | INEC; metodología citada en el artículo: https://www.ecuadorencifras.gob.ec//documentos/web-inec/Uso_Tiempo/Metodologia_EUT_2012.pdf | noviembre a diciembre de 2012 | descarga automática del zip desde el repositorio `christianjaviersalasmarquez/ut-ec-hombres-cocina`; copia local en `data/Base EUT 2012.zip` | `code/articulo1_download.R` |
| Encuesta Nacional Multipropósito de Hogares 2019, secciones de uso del tiempo y de personas | INEC; [COMPLETAR: no se encontró el enlace oficial de descarga en el repositorio ni en el artículo] | diciembre de 2019 | lectura directa de los CSV desde el repositorio `christianjaviersalasmarquez/ut-ec-hombres-cocina`; copias locales en `data/` | `code/articulo1_data_manipulation.R` |

La descarga automática depende de que ese repositorio externo siga en línea.
Para replicar sin red, las copias locales son `data/Base EUT 2012.zip`,
`data/201912_multibdd_uso_del_tiempo.sav.csv` y
`data/201912_multibdd_personas.sav.csv` (CSV con `;` como separador y `,`
como decimal).

## Salidas

- `images/grafico1-articulo1.png`: promedio de horas semanales en cocinar o
  preparar alimentos en 2012 y 2019, por sexo.
- `images/grafico2-articulo1.png`: lo mismo, por grupo de edad y sexo.
- `images/grafico3-articulo1.png`: lo mismo, por provincia y sexo.
- `report/texto-articulo1.Rmd`: texto del artículo, que incluye los tres
  gráficos desde `../images/`.

## Cómo replicar en este repositorio

El repositorio no tiene `MASTER.R`; el README anterior no detalla pasos, así
que el orden se deduce de los scripts:

1. Abre el repositorio en RStudio (`code/articulo1_analysis.R` usa
   `rstudioapi` para fijar el directorio de trabajo en la raíz).
2. `code/articulo1_download.R` descarga y descomprime la base EUT 2012; lo
   ejecuta `code/articulo1_analysis.R` con `source()`.
3. `code/articulo1_data_manipulation.R` carga las bases 2012 y 2019, calcula
   las horas semanales y los grupos de edad y provincia. Ningún script lo
   llama; ejecútalo antes del análisis en la misma sesión.
4. `code/articulo1_analysis.R` define el diseño muestral con `srvyr`, calcula
   los promedios ponderados y guarda los tres gráficos en `images/`.
5. Renderiza `report/texto-articulo1.Rmd` para generar el artículo en HTML.

Observación: la lista de pasos depende de que `data_manipulation` se ejecute a
mano, porque `articulo1_analysis.R` solo hace `source()` de
`articulo1_download.R`.

## README anterior

Texto del README de este repositorio antes de adoptar el README estándar,
conservado sin cambios de contenido para no perder información.

### [Los hombres ecuatorianos dedican cada vez más tiempo a la cocina ¿Qué indican los datos?](https://elquantificador.org/post/valores/2022-09-24-uso-del-tiempo-cocina/)

El presente repositorio contiene todos los archivos necesarios para reproducir el análisis del artículo sobre el uso del tiempo.

Se tienen dos opciones para replicar el análisis: 

1. Clonar el repositorio utilizando Git/ Git y GitHub y ejecutar los scripts.

2. Descargar un archivo comprimido desde la página web del repositorio y ejecutar los scripts. 

En ambos casos, es necesario tener instalado el software correspondiente.

Para más información, consulta el [perfil de GitHub del autor](https://github.com/christianjaviersalasmarquez).
