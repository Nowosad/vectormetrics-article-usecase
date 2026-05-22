library(sf)
library(dplyr)
library(purrr)
library(stringi)
library(vectormetrics)

process_city_buildings <- function(path) {
  city_name <- gsub("_footprints\\.gpkg", "", basename(path))
  city_name <- stri_trans_nfc(city_name)
  message("Processing: ", city_name)
  
  g <- st_read(path, quiet = TRUE)
  # fixing geometry
  g <- st_make_valid(g)
  g <- g[st_area(g) > units::set_units(2, "m^2"), ]

  building_metrics <- tibble(
    City = city_name,
    Squareness  = vm_p_square(g)$value,
    Elongation  = vm_p_elong(g)$value,
    Shape       = vm_p_shape(g)$value,
    Girth       = vm_p_girth(g)$value,
    Fractality  = vm_p_frac(g)$value,
    Rectangularity = vm_p_rect(g)$value
  )

  g_centroids <- st_centroid(st_geometry(g))
  g_metrics_sf <- st_sf(building_metrics, geometry = g_centroids)
  g_metrics_sf <- st_transform(g_metrics_sf, 2180)
  joined <- st_join(g_metrics_sf, pop_grid, join = st_intersects)
  output_df <- joined |> 
    st_drop_geometry() |> 
    as_tibble()

  return(output_df)
}
unzip("data/pop_grids.zip", exdir = "data/pop_grids")

pop_grid <- st_read("data/pop_grids/tdvj32680.shp") |> 
  st_transform(2180)

files <- list.files(
  path = "data",
  pattern = "_footprints\\.gpkg$",
  full.names = TRUE
)

correlation_data <- map_dfr(files, process_city_buildings)

correlation_data <- correlation_data |> 
  rename(Population = tot) |> 
  filter(!is.na(Population)) |> 
  select(-c(pl_code, code, multipolyg, multipoly1))

save(correlation_data, file = "data/correlation_data.RData")
