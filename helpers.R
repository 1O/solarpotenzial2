## liefert die benötigten Konstanten:
get_constants <- \() {
  list(
    flat = 9,  # Schwellenwert (°), unter dem Dach als flach angenommen wird
    steep = 70, # Schwellenwert (°), oberhalb dessen Steilflächen ausgenommen werden
    minsize = 3, # erforderliche Mindestausdehnung zusammenhängender Flächen (Pixel = m²)
    # zusammenhängender Dachfläche
    minbuildings = 3, ## Mindestanzahl an Gebäuden, ab der die Kachel berechnet wird
    a_usable = .7,  # Anteil der für PV nutzbaren Dachfläche (0-1)
    modul_m2 = 2.1,  # Fläche pro Modul [m2]
    pv_e = .18,  # PV efficiency (0-1): Konstante zur Umrechnung Globalstrahlung > Photovoltaik
    pv_e_f = \(x) .2534 * x - 22.707, ## Regression zur Umrechnung
    st_e = .4,  # ST efficiency (0-1): Konstante zur Umrechnung Globalstrahlung > Solarthermie
    st_e_f = \(x) .3785 * x - 34.37, ## Regression zur Umrechnung
    buffer = 0,  # Puffer um Gebäudepolygone [m]; nicht puffern, die Berechnung von Neigung/Aspekt 
    ## entfernt sowieso schon den Zellsaum;
    intervals_solar = c(0, 550, 700, 850, 1000, 1150, 1300, Inf), ## Klassen solar
    labels = list(
      aspect = c('N', 'NO', 'O', 'SO', 'S', 'SW', 'W', 'NW'),
      eignung_solar = c('nicht', 'wenig_2040', 'wenig_2020', 'geeignet', 'gut', 'sehr_gut', 'ausgezeichnet')
    )
  )
}




get_tile_codes <- \(file_paths){
  tile_codes <- list.files(file_paths$DOM,
                           pattern = '\\.tif[f]?$'
  ) |> 
    gsub(pattern = '(.*)_.*', replacement = '\\1') |> 
    sort()
  cat(sprintf("%.0f DOM tiles found", length(tile_codes)))
  tile_codes
}


## behält nur Pixelsammlungen über `minsize` Mindestausdehnung:
clean_raster <- \(r_buildings, r_slope){
  r <- r_buildings
  im_buildings <- matrix(r_buildings, dim(r_buildings)) |> as.cimg()
  im_slope <- matrix(r_slope, dim(r_slope)) |> as.cimg()
  ## Pixel auf Gebäudeflächen mit < 70 
  values(r) <- as.pixset(!is.na(im_buildings) & im_slope < constants$steep) |> 
    ## Inseln < minsize entfernen:
    clean(constants$minsize) |> as.matrix() |> t()
  r <- subst(r, FALSE, NA)
  r
}



## braucht knapp 7 Sekunden; stattdessen `clean_raster` verwenden
keep_large_blocks <- \(r){
  x <- ncol(r); y <- nrow(r)
  im <- matrix(r, ncol = x, nrow = y) |> 
    as.cimg() |> label()
  sizes <- table(im)
  is_large <- im %in% as.integer(names(sizes)[sizes >= constants$minsize])
  values(r) <- values(r) * c(NA, 1)[1 + is_large]
  r
}


## liest Raster ein, stellt zusätzliche Raster her,
## dazu gehört auch das kachelweise Einlesen und Rastern der Gebäude-Polygone
prepare_rasters <- \(file_paths, ## Pfade zu Eingaberastern/-vektoren
                     tile_code, ## aktuell bearbeitete Kachel
                     keep = FALSE ## Raster aus Zwischenschritten behalten?
){
  rasters <- list()
  
  ## DOM einlesen, vorerst nur, um den tile extent zu bestimmen
  rasters$dom_full <- rast(file.path(file_paths$DOM,
                                     sprintf('%s_DOM.tif', tile_code)
  )
  )
  
  
  v_buildings <- query(v_buildings_austria, extent = rasters$dom_full)
  ## mit NULL abbrechen, falls keine Gebäude in Kachel:
  if(all(is.na(values(v_buildings)))) return(NULL)
  
  rasters$buildings <-
    v_buildings |>
    buffer(constants$buffer) |> 
    rasterize(y = rasters$dom_full, field = 'OBJECTID', touches = FALSE)
  
  ## Gemeindepolygone abfragen und rastern:
  rasters$communities <-
    v_communities_austria |>
    query(ext = rasters$dom_full) |>
    rasterize(y = rasters$dom, field = 'id')
  
  ## Oberflächen-Höhenmodell:
  rasters$dom <- rasters$dom_full
  set.names(rasters$dom, 'dom')
  rasters$dom_full <- NULL
  
  ## Globalstrahlung:
  rasters$glo <- 
    rast(file.path(file_paths$GLO,
                   sprintf('%s_GLO_real_Jahressumme.tif', tile_code)
    )
    )
  
  ## 8 Himmelsrichtungen, von Nord (0) bis Nordwest (7) im UZS
  rasters$aspect <- rasters$dom |> terrain('aspect', neighbors = 4) |>
    classify(rcl = cbind(c(0:8 * 45) - 22.5,
                         c(0:8 * 45) + 22.5,
                         c(0, 1:7, 0)
    )
    )
  
  set.cats(rasters$aspect,
           value = data.frame(int = 0:7,
                              cat = paste0("aspect_",
                                           c("N", "NO", "O", "SO", "S",
                                             "SW", "W", "NW")
                              )
           )
  )
  set.names(rasters$aspect, 'aspect')
  
  
  rasters$slope <- rasters$dom |> terrain('slope', neighbors = 4)
  
  # set.values(rasters$slope, as.integer(values(rasters$slope)))
  set.names(rasters$slope, 'deg')
  
  ## Dachtyp (flach oder geneigt)
  rasters$rooftype <- rasters$slope |>
    classify(rcl = rbind(c(0, constants$flat, 0), c(constants$flat, Inf, 1)))
    
  set.cats(rasters$rooftype,
           value = data.frame(int = 0:1, cat = c('flat', 'inclined'))
  )
  set.names(rasters$rooftype, 'rooftype')
  
  rasters$inclined_only <- rasters$slope |>
    classify(rcl = rbind(c(-Inf, constants$flat, 0), c(constants$flat, Inf, 1))) |> 
    subst(0, NA)

  ## Raster mit nur geneigten Dachflächen
  ## als Maske für die Aspektauswertung (im Original nur für
  ## geneigte Dachflächen vorgenommen)
  rasters$aspect <- rasters$aspect |> mask(mask = rasters$inclined_only)
  
  ## Flächenkorrektur für geneigte Flächen: tatsächliche Dachfläche steigt
  ## mit der Steilheit
  rasters$a <- 1/cos(pi/180 * rasters$slope)
  set.names(rasters$a, 'area')
  
  ## Globalstrahlung auf tatsächliche Fläche:
  rasters$glo_corr <- rasters$glo * rasters$a
  ### Globalstrahlung nur auf geneigte Dachflächen:
  rasters$glo_corr_inclined <- rasters$glo_corr |> mask(rasters$inclined_only)
  
  
  ## PV-Ertrag aus Globalstrahlung real und Fläche:
  rasters$harvest_pv <- constants$pv_e_f(rasters$glo_corr)
  set.names(rasters$harvest_pv, 'ertrag_PV')
  
  ## PV-Ertrag aus Globalstrahlung real und Fläche:
  rasters$harvest_st <- constants$st_e_f(rasters$glo_corr)
  set.names(rasters$harvest_st, 'ertrag_ST')
  
  ## Raster mit eignung_solar hinzufügen und Kategorielabels setzen:
  rasters$suit <- rasters$glo |> classify(cbind(
    head(constants$intervals_solar, -1), ## von
    tail(constants$intervals_solar, -1), ## bis
    0:6 ## Klassenindex (0 = ungeeignet, 6 = ausgezeichnet geeignet)
  )
  )
  
  set.cats(rasters$suit,
           value = data.frame(
             int = -1 + seq_along(head(constants$intervals_solar, -1)),
             cat = constants$labels$eignung_solar
           )
  )
  
  set.names(rasters$suit, 'suit')
  
  general_mask <- clean_raster(rasters$buildings, rasters$slope)
  rasters <- Map(rasters, f = \(r) mask(r, general_mask))
  rasters
}



## zonale Auswertung für mehr als ein Zonenraster:
get_areas_wide <- \(r, zones_1, ...){
  zonal(r, c(zones_1, ...), fun = "sum", na.rm = TRUE)
}


## Extrahiert Werte aus den div. berechneten Rastern und 
## gibt sie als dataframe zurück:
extract_rasters <- \(rasters, iqr_mult = 2, ...){
  multizonal <- list(...)$multizonal
  multizonal <- ifelse(length(multizonal), multizonal, FALSE)
  
  ## Spalten mit OBJECTID und DOM-Ausreißer:
  outliers <- zonal(rasters$dom, rasters$buildings,
                    fun = \(xs){lb = quantile(xs, .25, na.rm = TRUE)
                    ub = quantile(xs, .75, na.rm = TRUE)
                    bw = iqr_mult * (ub - lb)
                    sum((xs < lb - bw) | (xs > ub + bw), na.rm = TRUE)
                    }
  ) |> setNames(c('OBJECTID', 'n_outliers'))
  
  ## Gemeinde-IDs der Gebäude:
  community_ids <- zonal(rasters$communities, rasters$buildings, mean) |>
    mutate(id = (cats(rasters$communities)[[1]])$id[id + 1]) |>
    rename(GEMEINDE_ID = id)
  
  
  ## Statistiken für DOM:
  dom_stats <- zonal(rasters$dom, rasters$buildings,
                     fun = \(xs){sapply(c(min = min, mean = mean, sd = sd, max = max),
                                        \(fn) do.call(fn, list(xs, na.rm = TRUE)))
                     }
  ) |> as.matrix() |> as.data.frame()
  ## Ausrichtung
  a_per_aspect <- get_areas_wide(rasters$a, rasters$buildings, rasters$aspect) |> 
    rename_with(~ sprintf('a_%s', .x), .cols = -OBJECTID)
  
  ## Fläche per Dach:
  a_total <- zonal(rasters$a, rasters$buildings, fun = "sum", na.rm = TRUE) |>
    rename(a_total = 'area')
  
  ## Fläche nach Dachtyp (flach / geneigt):
  a_per_rooftype <- get_areas_wide(rasters$a, rasters$buildings, rasters$rooftype) |>
    rename_with(~ sprintf('a_%s', .x), .cols = c(flat, inclined))
  
  ## Fläche nach Eignungsklasse:
  a_per_suit <- get_areas_wide(rasters$a, rasters$buildings, rasters$suit) |>
    rename_with(~ sprintf('a_%s', .x), .cols = -OBJECTID)
  
  ## Einstrahlung per Dach:
  glo_total <- zonal(rasters$glo_corr, rasters$buildings, fun = "sum", na.rm = TRUE) |> 
    rename(glo_total = 'GLO_real')
  
  ## Einstrahlung per Dachtyp:
  glo_per_rooftype <- get_areas_wide(rasters$glo_corr, rasters$buildings, rasters$rooftype) |>
    rename_with(~ sprintf('glo_%s', .x), .cols = c(flat, inclined))
 
  ## Einstrahlung nach Aspekt - NUR auf geneigte Flächen:
  glo_per_aspect <- get_areas_wide(rasters$glo_corr_inclined, rasters$buildings, rasters$aspect) |> 
    rename_with(~ sprintf('glo_%s', .x), .cols = -OBJECTID) 
  
  ## Einstrahlung nach Eignungsklasse - NUR auf geneigte Flächen:
  glo_per_suit <- get_areas_wide(rasters$glo_corr_inclined, rasters$buildings, rasters$suit) |>
    rename_with(~ sprintf('glo_%s', .x), .cols = -OBJECTID)
  
  ## Ertrag PV
  harvest_pv <- zonal(rasters$harvest_pv, rasters$buildings, fun = "sum", na.rm = TRUE)
  
  ## Ertrag Solarthermie
  harvest_st <- zonal(rasters$harvest_st, rasters$buildings, fun = "sum", na.rm = TRUE)
  
  if(multizonal) { ## diese aufwendigen Berechnungen nur bei multizonal = TRUE:
    
    ## Fläche nach Eignung und Dachtyp
    a_per_suit_rooftype <- 
      get_areas_wide(rasters$a, rasters$buildings, rasters$suit, rasters$rooftype) |>
      pivot_wider(names_from = c(suit, rooftype),
                  values_from = area,
                  id_cols = 'OBJECTID',
                  values_fill = 0,
                  names_vary = 'fastest'
      ) |> 
      rename_with(~ sprintf("a_%s", .x), .cols = -c(OBJECTID))
    
    
    ## Fläche nach Eignung und Dachtyp
    glo_per_suit_rooftype <- 
      get_areas_wide(rasters$glo_corr, rasters$buildings, rasters$suit,
                     rasters$rooftype) |>
      pivot_wider(names_from = c(suit, rooftype),
                  values_from = GLO_real,
                  id_cols = 'OBJECTID',
                  values_fill = 0,
                  names_vary = 'fastest'
      ) |> 
      rename_with(~ sprintf("glo_%s", .x), .cols = -c(OBJECTID)) 
  }
  
  print(names(glo_per_suit))

  ## welche Objekte sind dataframes und enthalten mindestens eine der in
  ## table_definition festgelegten Spaltennamen?
  names_dataframes <- 
    ls() |>
    Filter(f = \(o) is.data.frame(get(o))) |> 
    Filter(f = \(d) length(intersect(names(get(d)), names(table_definition))))
  
  ## gewünschte dataframes joinen:
  tail(names_dataframes, -1) |> 
    Reduce(f = \(L, R) left_join(L, get(R)), init = get(names_dataframes[1])) |> 
    ## '.' durch '_' in Spaltennamen ersetzen:
    rename_with(~ gsub('\\.', '_', .x))
}


## kosmetische Arbeiten an der Ergebnistabelle:
## Spalten in richtige Reihenfolge etc.
prettify_dataframe <- \(d, tile_code){
  print(tile_code)
  ## Spaltenvorlage für Ergebnis-Dataframe ...
  rep(NA, length(table_definition)) |> 
    setNames(nm = names(table_definition)) |>
    as.list() |> list2DF() |> 
    ## ... Ergebniszeilen anbinden:
    bind_rows(d) |> 
    tail(-1) |>
    mutate(TILE_CODE = tile_code) |> 
    ## fehlende Werte durch 0 ersetzen:  
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
}



## SQLITE Tabelle erzeugen,
## die Spaltendefinitionen  stammen aus table_definition.R:
prepare_db_output_table <- \(conn, table_name = 'raw'){
  statement <- sprintf("CREATE TABLE IF NOT EXISTS %s (%s,
            PRIMARY KEY(GEMEINDE_ID, OBJECTID, TILE_CODE))",
                       table_name,
                       paste(names(table_definition), table_definition) |> 
                         paste(collapse = ', ')
  )
  
  dbExecute(conn, statement)
}



## schreibt data.frame in SQLite-DB
write_to_db <- \(d, table_name = 'raw', conn){ ## data.frame
  dbWriteTable(conn = conn,
               name = table_name,
               value = d,
               append = TRUE
  )
}


### Berechnung und Speicherung pro Kachel:
calc_and_save <- \(file_paths, tile_code, export_images = FALSE, 
                   save_excels = FALSE, conn, i
){
  
  cat(paste('\n', i, Sys.time(), ': '))
  cat(sprintf('working on tile %s ...', tile_code))
  cat('preparing rasters...')

  tryCatch(
    d <- 
      prepare_rasters(file_paths, tile_code) |> ## Arbeitsraster anlegen
      extract_rasters() |> ## Rasterwerte als data.frame extrahieren
      prettify_dataframe(tile_code = tile_code) ## zusätzliche Tabellenkalkulationen
    , error = \(e) cat(paste('...can\'t extract data: ', e))
  )
  
  
  
  
  cat("...trying to write to database ...")
  tryCatch({
    prepare_db_output_table(conn)
    write_to_db(d, table_name = 'raw', conn)},
    error = \(e) cat(paste('can\'t write to database:', e))
  )
  
  if(save_excels){
    cat("...trying to write CSV...")
    ## Ergebnistabelle als CSV speichern:
    tryCatch( d |> write.csv2(file.path(dir_root, sprintf('output/data_%s.csv', tile_code))),
              error = \(e) cat(paste('...writing failed: ', e))
    )
  }
  
  if(export_images){
    ## falls export_images == TRUE, Raster als GeoTIFFs speichern:
    cat("...trying to save tiffs...")
    tryCatch({
      Map(names(rasters),
          f = \(n) writeRaster(rasters[[n]], 
                               sprintf('./output/%s_%s_%spx.tiff', tile_code, n,
                                       as.character(constants$minsize)
                               ),
                               overwrite = TRUE))
    },
    error = \(e) cat('...saving tiff failed')
    )
  }
}




count_dom_outliers <- \(tile_code, buffer_size = -1, iqr_mult = 2){
  
  r_in <- rast(sprintf(file_paths$DOM, sprintf('%s_DOM.tif', tile_code)))
  
  v_buildings <- 
    query(v_buildings_austria, extent = ext(r_in)) |> 
    buffer(buffer_size)
  
  r_buildings <- v_buildings |> rasterize(y = r_in, field = 'OBJECTID')
  
  zonal(r_in, r_buildings,
        fun = \(xs){lb = quantile(xs, .25, na.rm = TRUE)
        ub = quantile(xs, .75, na.rm = TRUE)
        bw = iqr_mult * (ub - lb)
        sum((xs < lb - bw) | (xs > ub + bw), na.rm = TRUE)
        }
  )[, 2] |> sum()
}


show_dom_outliers <- \(r_in, buffer_size = -1, iqr_mult = 2){
  ## zeigt Ausreißer im DOM pro Gebäude an; dauert 10 s. oder länger
  v_buildings <- 
    query(v_buildings_austria, extent = ext(r_in)) |> 
    buffer(buffer_size)
  
  r_buildings <- v_buildings |> rasterize(y = r_in, field = 'OBJECTID')
  r_outliers <- zonal(r_in, r_buildings,
                      fun = \(xs){lb = quantile(xs, .25, na.rm = TRUE)
                      ub = quantile(xs, .75, na.rm = TRUE)
                      bw = iqr_mult * (ub - lb)
                      c(lb - bw, ub + bw)
                      },
                      as.raster = TRUE
                      
  ) 
  set.names(r_outliers, c('low', 'high'))
  (r_in < r_outliers$low | r_in > r_outliers$high) |>  
    clamp(1, 1, values = FALSE)  
}





## führt Plausibilitätstests durch:
validate <- \(d){
  ## Gleichheit Gesamtfläche (a_total) und Summe Flächen pro Eignung und Dachtyp:
  expect_equal(
    sum(d$a_total, na.rm = TRUE),
    d |>
      summarise(across((starts_with("a_") & !ends_with("total")), ~ sum(.x, na.rm = TRUE))) |>
      rowSums()
  )
  
  ## Gleichheit Gesamtfläche und Summe Dachflächen nach Ausrichtung:
  expect_equal(
    sum(d$a_total, na.rm = TRUE),
    d |>
      summarise(across(starts_with("aspect_"), ~ sum(.x, na.rm = TRUE))) |>
      rowSums()
  )
  
  ## die Summe der geneigten Dachfläche sollte die der Flachdächer übersteigen: 
  expect_gt(
    summarise(d, across(matches('a_.*_inclined'), ~ sum(.x, na.rm = TRUE))) |> rowSums(),
    summarise(d, across(matches('a_.*_flat'), ~ sum(.x, na.rm = TRUE))) |> rowSums()
  )
  
  
  
  ## alle berechneten Strahlungssummen (kWh/ m²) für geneigte Dachflächen
  ## zwischen 0 und 2000?
  # expect_true(
  #   all(d$glo_inclined / d$inclined <= 2000) &
  #   all(d$glo_inclined / d$inclined >= 0)
  # )
  
  
}


# validate(d)

# par(mfrow = c(2, 2))
# 
# (rasters$glo * rasters$a) |> plot()
# rasters$glo |> plot()
# 
# constants$pv_e_f((rasters$glo * rasters$a)) |> plot()
# 
# 
