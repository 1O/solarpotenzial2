library(dplyr)
library(data.table)
library(RSQLite)


dir_root <- '/home/io/Dokumente/fremd/Christine Brendle/solarpotenzial/'
setwd(dir_root)

## Datenbanken zusammenführen:

conn <- dbConnect(drv = SQLite(), dbname = './output/results_1.sqlite')
d1 <- dbReadTable(conn, 'raw') |> as.data.table()
conn <- dbConnect(drv = SQLite(), dbname = './output/results_2.sqlite')
d2 <- dbReadTable(conn, 'raw') |> as.data.table()
d <- bind_rows(d1, d2)
dbDisconnect(conn)
conn_out <- dbConnect(RSQLite::SQLite(), 'output/both.sqlite')
dbWriteTable(conn_out, 'raw', d, overwrite = TRUE)
dbDisconnect(conn_out)


## aggregieren:

dbDisconnect(conn) ## ggf. bestehende Verbindung schließen
## Verbindung öffnen
conn <- dbConnect(drv = SQLite(), dbname = './output/dachflaechen-202505-16.sqlite')

d <- dbReadTable(conn, 'raw')
d <- as.data.table(d)[order(TILE_CODE, GEMEINDE_ID, OBJECTID)]

## any duplicate OBJECTIDs for the same tile? (hopefully: FALSE)
any(d[, .(TILE_CODE, OBJECTID)] |> duplicated())


summarise_communities <- \(d) {
  d1 <- d[, lapply(.SD, FUN = \(x) sum(as.double(x), na.rm = TRUE)), 
          .SDcols = seq(grep('a_total', names(d)), grep('ertrag_ST', names(d))),
          by = c('GEMEINDE_ID')
  ]
  
  d2 <- d[, .(TILE_CODE = TILE_CODE[1],
              dom_min = min(dom_min),
              dom_mean = mean(dom_mean),
              dom_max = max(dom_max)
  ),
  by = c('GEMEINDE_ID')
  ]
  
  d2[d1, on = 'GEMEINDE_ID']
}

(summarise_communities(d))[order(-a_total)] |> 
  filter(GEMEINDE_ID == 31541)






