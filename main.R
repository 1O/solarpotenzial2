## Vorbereitung ----------------------------------------------------------------
### Bibliotheken laden:
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(data.table)
library(RSQLite)
library(imager)
library(rio)
# library(testthat)


### Arbeitsumgebung setzenDOM
## Stammverzeichnis (mit `main.R` und `helpers.R`):
dir_root <- '/home/io/Dokumente/fremd/Christine Brendle/solarpotenzial/'



setwd(dir_root) ## Stammverzeichnis als Arbeitsverzeichnis
source('./helpers.R') ## Hilfsfunktionen laden
source('./table_definition.R')

## Pfade zu Datenquellen:
file_paths <- list(
  ## Pfad zur Gebäude-Geopackage:
  buildings = file.path(dir_root, 'input/DOM/DLM_EPSG3035.gpkg'),
  ## Pfad zu Gemeinde-Geopackage (für gde.weise Aggregation):
  communities = file.path(dir_root, './input/DOM/GEM_W23_3035.gpkg'),
  ## Pfad zu Oberflächenmodell:
  DOM = file.path('/oldhome/ivo/Dokumente/fremd/','Christine Brendle/Solarpotenzial/R/input/DOM/'),
  ##DOM = file.path('./input/Testkacheln/26850-47525/DOM/'), ## Gleisdorf
  ## Pfad zu Globalstrahlung
  ##GLO = file.path('./input/Testkacheln/26850-47525/GLO_real/')
  GLO = file.path('/media/io/LaCie/GLO_real/')
)

### Konstanten festlegen:
constants <- get_constants()

#### DB-Verbindung zu Geopackage
#### mit Gesamtdatensatz der Gebäudepolygone herstellen:
#### die Gebäudedaten sind bereits auf EPSG3035 (LAEA) wie die Rasterdaten
## Gebäudelayer muss in EPSG 3035 (LAEA) sein:
v_buildings_austria <- vect(file_paths$buildings,
       layer = 'DLM_EPSG3035_A',
       proxy = TRUE ## nur Verbindung, nicht einlesen
  )



## Gemeindegrenzen:
v_communities_austria <- vect(file_paths$communities, proxy = TRUE)

## Codes der vorhandenen (-> file_paths) DOM- und GLO-Kacheln auslesen,
## fürs spätere Durchschleifen:
tile_codes <- get_tile_codes(file_paths = file_paths)



## Test für einzelnen tile_code:
dbDisconnect(conn) ## ggf. bestehende DB-Verbindung schließen
## Verbindung öffnen
conn <- dbConnect(drv = SQLite(), dbname = './output/oswald.sqlite')



prepare_db_output_table(conn, 'raw')

tile_codes <- c(
  ## '28075-46925' ## Oswald
  ##"26850-47525" ## Kachel Gleisdorf:
  ##"28625-48275" ## Rabensburg: minimale Dachfläche
)

rasters <- prepare_rasters(file_paths = file_paths, tile_code = tile_codes[1])


names(rasters) |> Map(f = \(n) writeRaster(rasters[[n]],
                                           sprintf('output/oswald_%s.tiff', n), overwrite = TRUE)
)

calc_and_save(file_paths, 
              tile_code = tile_codes[1],
              conn = conn,
              i = 1,
              save_excels = TRUE, ## auch als CSV speichern
              export_images = TRUE ## berechnete Raster als TIFF speichern
)

dbDisconnect(conn) ## DB-Verbindung schließen
## Ende Test



#### Durchschleifen mehrerer Kacheln, Ausgabe in MySQL-Datenbank:
### Datenbankverbindung öffnen; falls nicht vorhanden, wird Datenbank
### dieses Namens angelegt:

source('table_definition.R')
source('helpers.R')

dbDisconnect(conn) ## ggf. bestehende Verbindung schließen
## Verbindung öffnen
conn <- dbConnect(drv = SQLite(), dbname = './output/results_2.sqlite')

## Ausgabe-table "raw" anlegen:
prepare_db_output_table(conn, 'raw')

## Log-Datei anlegen (laufende Kachelnummer, Fehlermeldungen etc. in Datei)
##
sink() ## ggf. bestehenden Ausgabekanal schließen
sink(file = "./output/process.log", append = TRUE,
     type = c("output", "message"),
     split = FALSE
     )


#### tile codes durchschleifen:
  10890:length(tile_codes) |> ## ggf. ab schon prozedierter Kachel fortsetzen:
  Map(f = \(i){
    if(!tile_codes[i] %in% c('26425-46800')){
    calc_and_save(file_paths, 
                  tile_code = tile_codes[i],
                  conn = conn,
                  i = i,
                  save_excels = FALSE,
                  export_images = FALSE
                  )
    }
  })


dbDisconnect(conn)
## Ausgabe in Logdatei schließen:
sink()
