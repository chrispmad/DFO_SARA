# Title: Filter downloaded DFO SARA-listed critical habitat and occurrence data to BC
#
# Date: 2024-08-22
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca)
#
# Description: This script ...

library(sf)
library(readxl)
library(tidyverse)
library(gdalUtilities)
library(tidyverse)
library(terra)

ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

bc = bcmaps::bc_bound() |> dplyr::summarise()

#Split occurrence data by Species
dfo_sara_occ_data = st_read(dsn = 'data/Distribution_Repartition/Distribution_FGP.gdb/',
                            layer = 'DFO_SARA_Dist_2023_FGP_EN')

dfo_sara_occ_data = dplyr::rename(dfo_sara_occ_data, geom = Shape)

purrr::iwalk(unique(dfo_sara_occ_data$Common_Name_EN), ~ {
  print(.x)
  sf::write_sf(dfo_sara_occ_data[dfo_sara_occ_data$Common_Name_EN == .x,],
               paste0("data/dfo_occ_data_by_species/dfo_occ_",stringr::str_to_lower(.x),".gpkg"))
})

# Clean up Bull Trout data - merge a bunch of stream geometries into one shape I
# made by hand in QGIS.
bt_big_shape = sf::read_sf("data/handmade_bulltrout_NE_BC_polygon.gpkg")

bt = sf::read_sf("data/dfo_occ_data_by_species/dfo_occ_bull trout.gpkg")

bc_pseudomerc = sf::st_transform(bc, st_crs(bt))

bc_bbox = sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(bc_pseudomerc)))

# Trim to BC.
bt = bt |> sf::st_filter(bc_bbox)

#started at 1:41 PM...

# Replace complex bulltrout geometry with the giant bulltrout polygon I drew.
bt_big_shape = sf::read_sf("data/handmade_bulltrout_NE_BC_polygon.gpkg")

bt_big_shape = st_transform(bt_big_shape, st_crs(bt))

ggplot() + geom_sf(data = bc) + geom_sf(data = bt_big_shape)

bt_rows_to_drop = bt |>
  sf::st_intersects(bt_big_shape)

bt_within_polygon = bt[!is.na(as.numeric(bt_rows_to_drop)),]

bt_outside_polygon = bt[is.na(as.numeric(bt_rows_to_drop)),]

bt_outside_polygon = dplyr::bind_rows(
  bt_outside_polygon,
  bt_big_shape
)

# Now trim to BC.
bt_outside_polygon = bt_outside_polygon |> sf::st_intersection(bc_pseudomerc)

ggplot() + geom_sf(data = bc) + geom_sf(data = bt_outside_polygon)

sf::write_sf(bt_outside_polygon, "data/dfo_occ_data_by_species_BC/dfo_occ_bull trout.gpkg")

# Now that the bull trout occurrence data (which came in at a whopping 3GB or so)
# has been vastly cleaned up, we can proceed with the other species.

files_to_clean = list.files(path = "data/dfo_occ_data_by_species/",
           pattern = ".gpkg")

files_to_clean = files_to_clean[!stringr::str_detect(files_to_clean, 'dfo_occ_bull trout')]

purrr::iwalk(
  files_to_clean, ~ {

    print(.x)

    if(!file.exists(paste0("data/dfo_occ_data_by_species_BC/",.x))){
      # dat = sf::read_sf(paste0("data/dfo_occ_data_by_species/",.x))
      dat = sf::read_sf(paste0("data/dfo_occ_data_by_species/",.x))

      # Make sure geometry is clean
      dat = suppressWarnings(suppressMessages(ensure_multipolygons(dat)))

      # Filter to just BC bounding box.
      dat = tryCatch(
        expr = dat |>
          sf::st_filter(bc_bbox),
        error = function(e) return(NULL)
      )

      if(nrow(dat) > 0){

        # Filter to BC polygon.
        dat = tryCatch(
          expr = dat |>
            sf::st_filter(bc),
          error = function(e) return(NULL)
        )

        if(nrow(dat) > 0){

          sf::write_sf(dat, paste0("data/dfo_occ_data_by_species_BC/",.x))

          print('wrote to file...')
        }
      }
    }
  }
)

# Now combine all of our DFO occurrences in BC into one file :D

dfo_sara_bc = list.files(path = 'data/dfo_occ_data_by_species_BC/',
           full.names = T) |>
  lapply(\(x) sf::read_sf(x)) |>
  dplyr::bind_rows()

dfo_sara_bc |> sf::write_sf("output/dfo_sara_occurrences_in_BC_all_species.gpkg")

# Filter for just Fraser and Columbia priority areas
fras = sf::read_sf("W:/CMadsen/shared_data_sets/Fraser_River_Big_Watershed.shp")
columbia = sf::read_sf("W:/CMadsen/shared_data_sets/Columbia_River_Big_Watershed.shp")
ggplot() + geom_sf(data = fras) + geom_sf(data = columbia)

fras_col = dplyr::bind_rows(fras, columbia) |> dplyr::summarise()

dfo_sara_bc_fras_col = sf::st_intersection(dfo_sara_bc, fras_col |> sf::st_transform(sf::st_crs(dfo_sara_bc)))

dfo_sara_bc_fras_col |> sf::write_sf("output/dfo_sara_occurrences_in_col_fras_watersheds.gpkg")

ggplot() + geom_sf(data = fras) + geom_sf(data = columbia) + geom_sf(data = dfo_sara_bc_fras_col)

# Convert the critical habitat from geodatabase into a geopackage
crithab = sf::st_read("data/CriticalHabitat_FGP.gdb/", layer = "DFO_SARA_CritHab_2022_FGP_EN")

crithab = ensure_multipolygons(crithab)

# Filter for BC.
crithab_bc = crithab |> sf::st_filter(bc_bbox)
crithab_bc = sf::st_make_valid(crithab_bc)
crithab_bc = crithab_bc |> sf::st_intersection(bc_pseudomerc)

ggplot() +
  geom_sf(data = bc_pseudomerc) +
  geom_sf(data = crithab, fill = 'purple') +
  geom_sf(data = crithab_bc, fill = 'green')

sf::write_sf(crithab_bc, "dfo_sara_critical_habitat_bc.gpkg")
