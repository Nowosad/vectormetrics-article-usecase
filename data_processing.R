library(sf)
library(tmap)
library(vectormetrics)
library(factoextra)
library(ggplot2)
library(fmsb)
library(dplyr)
library(tibble)
library(grid)
library(gridExtra)
library(tidyr)
library(purrr)
library(stringi)
setwd("~/data")

summary_stats <- function(x){
  tibble(
    median = median(x, na.rm = TRUE),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE)
  )
}
normalize_01 <- function(x){
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

compute_city_metrics <- function(path){
  city_name <- gsub("_footprints\\.gpkg", "", basename(path))
  city_name <- stri_trans_nfc(city_name)
  message("Processing: ", city_name)
  g <- st_read(path, quiet = TRUE)
  # fix geom
  g <- st_make_valid(g)
  g <- g[st_area(g) > units::set_units(2, "m^2"), ] # removing weird geoms
  res <- tibble(
    squareness  = vm_p_square(g)$value,
    elongation  = vm_p_elong(g)$value,
    shape       = vm_p_shape(g)$value,
    girth       = vm_p_girth(g)$value,
    fractality  = vm_p_frac(g)$value,
    rect        = vm_p_rect(g)$value
  ) 
  res <- res %>%
    mutate(fractality = ifelse(fractality < 1 | fractality > 2, NA, fractality)) %>% # this might be an overkill, can be removed if results are not looking good
    pivot_longer(
      cols = everything(),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(city = city_name, .before = metric)
}


files <- list.files(
  pattern = "_footprints\\.gpkg$",
  full.names = TRUE
)
metrics_long <- map_dfr(files, compute_city_metrics)


city_summary <- metrics_long %>%
  group_by(metric) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    Median = median(value, na.rm = TRUE),
    Q25 = quantile(value, 0.25, na.rm = TRUE),
    Q75 = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(metric = case_when(
    metric == "elongation" ~ "Elongation",
    metric == "shape" ~ "Shape",
    metric == "fractality" ~ "Fractality",
    metric == "rect" ~ "Rectangularity",
    metric == "squareness" ~ "Squareness",
    metric == "girth" ~ "Girth",
    TRUE ~ metric
  )) %>% 
  arrange(metric)


city_medians <- metrics_long %>%
  group_by(city, metric) %>%
  summarise(median = median(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = metric, 
    values_from = median
  )


city_means <- metrics_long %>%
  group_by(city, metric) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>% pivot_wider(
    names_from=metric,
    values_from=Mean
  ) %>% 
  setNames(c("City", "Elongation", "Fractality", "Girth", "Rectangularity", "Shape", "Squareness"))

save(city_means, file="city_means.RData")
save(metrics_long, file="city_metrics.RData")
save(city_summary, file="city_summary.RData")
save(city_medians, file="city_medians.RData")

city_summary %>% group_by(metric)

city_medians %>%
  arrange(desc(girth)) %>%
  slice(1) # Łódź, most compact

city_medians %>%
  arrange(desc(fractality)) %>%
  slice(1) # Lublin, most complex

radar_all <- city_medians %>%
  pivot_longer(
    cols = -city,
    names_to = "metric",
    values_to = "value"
  ) %>%
  group_by(metric) %>%
  mutate(value_norm = normalize_01(value)) %>%
  ungroup()
radar_cities <- radar_all %>%
  filter(city %in% c("Warszawa", "Poznań", "Łódź", "Lublin"))


radar_wide <- radar_cities %>%
  select(city, metric, value_norm) %>%
  pivot_wider(
    names_from = metric,
    values_from = value_norm
  )

df <- as.data.frame(radar_wide)
rownames(df) <- df$city
df$city <- NULL
df <- rbind(
  max = rep(1, ncol(df)),
  min = rep(0, ncol(df)),
  df
)
save(df, file="radarchart_data.RData")
