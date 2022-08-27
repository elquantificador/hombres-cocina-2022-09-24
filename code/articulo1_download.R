## --------------------------------------------------------------------------- ##
## --------------------- Descarga de bases de datos -------------------------- ##
## --------------------------------------------------------------------------- ##

# 2012

url <- "https://github.com/christianjaviersalasmarquez/ut-ec-hombres-cocina/raw/main/data/Base%20EUT%202012.zip"

td <- tempdir() # Se crea un directorio temporal

tf <- tempfile(tmpdir=td, fileext = ".zip") # Se crea una carpeta temporal

download.file(url,tf) # Se descarga el zip en la carpeta temporal

# Se obtiene el nombre del archivo dentro del archivo zip, se lo descomprime (unzip), se obtiene el nombre del parche, y finalmente es cargado al entorno

EUT2012.f.name <- unzip(tf, list=TRUE)$Name[1] # El archivo SAV de la encuesta EUT 2012 es el primero

unzip(tf, files=EUT2012.f.name, exdir=td, overwrite=TRUE)

EUT2012.f.path <- file.path(td, EUT2012.f.name)

