# Title: Filter downloaded DFO SARA-listed critical habitat and occurrence data to BC
#
# Date: 2024-08-22
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca) and John Phelan
#
# Description: This script takes a data download of DFO Species-at-Risk
# spatial data and filters it for just freshwater aquatic species in BC.
# It also simplifies the geometry of Bull Trout in the North / NE of BC,
# which greatly reduces the file size.
#
# Products: One geopackage "dfo_sara_critical_habitat_bc.gpkg", in outputs/ folder
#
# To-do:
# 1. Convert to Markdown to make HTML file.
# 2. Add section that incorporates Bull Trout (South Coast?) population polygon.
# 3. Move this project (or its output file at least) to a OneDrive folder, share that with Charlotte and Caitlyn (and others?)

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

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"

#Split occurrence data by Species
dfo_sara_occ_data = st_read(dsn = paste0(onedrive_wd,'DFO_SARA/Distribution_Repartition/Distribution_FGP.gdb/'),
                            layer = 'DFO_SARA_Dist_2023_FGP_EN')

dfo_sara_occ_data = dplyr::rename(dfo_sara_occ_data, geom = Shape)

purrr::iwalk(unique(dfo_sara_occ_data$Common_Name_EN), ~ {
  print(.x)
  sf::write_sf(dfo_sara_occ_data[dfo_sara_occ_data$Common_Name_EN == .x,],
               paste0("data/dfo_occ_data_by_species/dfo_occ_",stringr::str_to_lower(.x),".gpkg"))
})

# # Clean up Bull Trout data - merge a bunch of stream geometries into one shape I
# # made by hand in QGIS.
# bt_big_shape = sf::read_sf("data/handmade_bulltrout_NE_BC_polygon.gpkg")
#

#
# # Trim to BC.
# bt = bt |> sf::st_filter(bc_bbox)

#started at 1:41 PM...
bt = sf::st_read(paste0("data/dfo_occ_data_by_species/dfo_occ_bull trout.gpkg"))

# Replace complex bulltrout geometry with the giant bulltrout polygon I drew.
bt_big_shape = sf::read_sf(paste0(onedrive_wd,"DFO_SARA/handmade_bulltrout_NE_BC_polygon.gpkg"))

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
bc_pseudomerc <- bc |> sf::st_transform(3857)
bt_outside_polygon = bt_outside_polygon |> sf::st_intersection(bc_pseudomerc)

ggplot() + geom_sf(data = bc) + geom_sf(data = bt_outside_polygon)

sf::write_sf(bt_outside_polygon, paste0(onedrive_wd,"DFO_SARA/dfo_occ_data_by_species_BC/dfo_occ_bull trout.gpkg"))

bt = sf::read_sf(paste0(onedrive_wd,"DFO_SARA/dfo_occ_data_by_species/dfo_occ_bull trout.gpkg"))

bc_pseudomerc = sf::st_transform(bc, st_crs(bt))

bc_bbox = sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(bc_pseudomerc)))
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

# Append the more recently received South Coast Bull Trout polygon.

bc = bcmaps::bc_bound()

scbt = sf::read_sf(paste0(onedrive_wd,"DFO_SARA/wetransfer_south-coast-bull-trout-gis-files_2025-02-13_0022/BTDesignatableUnits.shp"))

scbt_t = sf::st_intersection(scbt, bc)

scbt_t = scbt_t[scbt_t$Designatab == "DU1 - Southcoast BC populations",]

scbt_t = scbt_t |> sf::st_transform(4326)

scbt_t = scbt_t |>
  dplyr::summarise(Common_Name_EN = "Bull Trout",
                   Scientific_Name = 'Salvelinus confluentus',
                   Population_EN = 'South Coast',
                   Taxon = 'Fishes',
                   Ecotype = 'Freshwater') |>
  dplyr::rename(geom = geometry)

ggplot() + geom_sf(data = scbt_t, aes(fill = Common_Name_EN))

# remove bull trout points that fall within the new polygon.
dfo_sara_bc_bulltrout = dplyr::filter(dfo_sara_bc, Common_Name_EN == "Bull Trout")

dfo_sara_bc_bulltrout_remove = dfo_sara_bc_bulltrout |>
  sf::st_filter(scbt_t |> sf::st_transform(crs = sf::st_crs(dfo_sara_bc_bulltrout)))
# No bull trout points here! Just add on the new south coast polygon.

dfo_sara_bc = dfo_sara_bc |>
  dplyr::bind_rows(scbt_t |> sf::st_transform(crs = sf::st_crs(dfo_sara_bc)))

ggplot() + geom_sf(data = dfo_sara_bc |> dplyr::filter(Common_Name_EN == 'Bull Trout'))

file.remove("output/dfo_sara_occurrences_in_BC_all_species.gpkg")
dfo_sara_bc |> sf::write_sf("output/dfo_sara_occurrences_in_BC_all_species.gpkg")
file.remove("../sarpy/data/dfo_sara_occurrences_in_BC_all_species.gpkg")
dfo_sara_bc |> sf::write_sf("../sarpy/data/dfo_sara_occurrences_in_BC_all_species.gpkg")

# Drop marine species
d_no_marine = dfo_sara_bc |>
  dplyr::filter(Eco_Type != 'Marine')

sf::write_sf(d_no_marine, "output/dfo_sara_occurrences_in_BC_no_marine.gpkg")

# Filter for just Fraser and Columbia priority areas
fras = sf::read_sf("W:/CMadsen/shared_data_sets/Fraser_River_Big_Watershed.shp")
columbia = sf::read_sf("W:/CMadsen/shared_data_sets/Columbia_River_Big_Watershed.shp")
# ggplot() + geom_sf(data = fras) + geom_sf(data = columbia)

fras_col = dplyr::bind_rows(fras, columbia) |> dplyr::summarise()

dfo_sara_bc_fras_col = sf::st_intersection(d_no_marine, fras_col |> sf::st_transform(sf::st_crs(dfo_sara_bc)))

dfo_sara_bc_fras_col |> sf::write_sf("output/dfo_sara_occurrences_in_col_fras_watersheds.gpkg")

ggplot() + geom_sf(data = fras) + geom_sf(data = columbia) + geom_sf(data = dfo_sara_bc_fras_col)

# Read in national critical habitat;
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

## Copy output files to onedrive.
list.files(path = "output") |>
  purrr::iwalk( ~ {
    if(!.x %in% c("federal_critical_habitat.gpkg")){
      file.copy(from = paste0("output/",.x),
                to = paste0(onedrive_wd,"DFO_SARA/",.x),
                overwrite = T)
    }
  })
