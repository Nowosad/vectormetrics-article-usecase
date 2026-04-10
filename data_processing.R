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
setwd("~/projects/rstudio_server/rstudio-home/data")

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
  tibble(
    squareness  = vm_p_square(g)$value,
    elongation  = vm_p_elong(g)$value,
    shape       = vm_p_shape(g)$value,
    girth       = vm_p_girth(g)$value,
    fractality  = vm_p_frac(g)$value,
    rect        = vm_p_rect(g)$value
  ) %>%
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

city_medians <- city_summary %>%
  select(city, metric, median) %>%
  pivot_wider(
    did = metric,
    values_from = median
  )

city_means <- metrics_long %>%
  select(city, metric, mean) %>%
  pivot_wider(
    did = metric,
    values_from = mean
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

colors_border=c(
  rgb(0.2,0.5,0.5,0.9),
  rgb(0.8,0.2,0.5,0.9),
  rgb(0.7,0.5,0.1,0.9),
  rgb(0.4,0.4,0.8,0.9)
)
colors_in=c(
  rgb(0.2,0.5,0.5,0.4),
  rgb(0.8,0.2,0.5,0.4),
  rgb(0.7,0.5,0.1,0.4),
  rgb(0.4,0.4,0.8,0.4)
)
par(
  mar = c(1, 1, 1, 1),
  pty = "m"
)
radarchart(
  df,
  pcol = colors_border,
  plwd = 1,
  pfcol = colors_in,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  cglwd = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8,
  vlcex = 0.2
)

legend(
  x = 0.7, y = 1.2,
  legend = rownames(df[-c(1,2),]),
  bty = "n",
  pch = 20,
  col = colors_in,
  text.col = "black",
  cex = 1,
  pt.cex = 2
)

results <- lapply(files, compute_city_metrics)
city_metrics <- bind_rows(
  lapply(results, function(x){
    cbind(city = x$name, as.data.frame(t(x$mean)))
  })
)

dir.create("plots", showWarnings = FALSE)

for(i in seq_len(nrow(city_metrics))){
  city <- city_metrics$city[i]
  vals <- city_metrics[i, 2:6]
  df <- rbind(
    max = rep(1, 5),
    min = rep(0, 5),
    values = vals
  )
  df[] <- lapply(df, as.numeric)
  colnames(df) <- c("SQUARE", "ELONG", "GIRTH", "FRAC", "RECT")
  rownames(df) <- c("max", "min", "city_mean")
  png(filename = paste0("plots2/", city, "_radar.png"), width=2000, height=2000, res = 300)
  par(cex = 1.1)
  par(mar = c(0.1,0.1,0.1,0.1))
  radarchart(df,
               pcol = "black",
               pfcol = alpha("blue", 0.1),
               plwd = 3,
               cglcol = "gray", 
               cglty = 1,
               cglwd = 1.5,
              cex.lab = 7,
             cex.axis = 2.2,
             calcex=3,
             # cex.main = 3,
               seg = 3,
    # title = city
  )
  dev.off()
}


selected_cities <- city_metrics %>% 
  filter(city %in% c("Gdańsk", "Kraków", "Szczecin"))
vals <- selected_cities[, 2:6]
df <- rbind(
  max = rep(1, 5),
  min = rep(0, 5),
  values = vals
)
df[] <- lapply(df, as.numeric)
colnames(df) <- c("SQUARE", "ELONG", "GIRTH", "FRAC", "RECT")
rownames(df) <- c("max", "min", "Gdańsk", "Kraków", "Szczecin")
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart(df,
           pcol=colors_border, #linie color
           plwd=0.6, # line thickness
           #pfcol=adjustcolor(colors_in, 1), #fill color
           pfcol=colors_in,
           plty=1,# line type
           cglcol="grey", 
           cglty=1, 
           cglwd=0.6 # radarchart line width
)
legend(x=0.7, y=1, legend = rownames(df[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)


selected_cities <- city_metrics %>% 
  filter(city %in% c("Warszawa", "Kraków", "Wrocław"))
vals <- selected_cities[, 2:6]
df <- rbind(
  max = rep(1, 5),
  min = rep(0, 5),
  values = vals
)
df[] <- lapply(df, as.numeric)
colnames(df) <- c("SQUARE", "ELONG", "GIRTH", "FRAC", "RECT")
rownames(df) <- c("max", "min", "Kraków", "Warszawa", "Wrocław")
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
par(
  mar = c(1, 1, 1, 1),  # down, left, up, right
  pty = "s"            # square plot
)
radarchart(df,
           pcol=colors_border,
           plwd=1,
           #pfcol=adjustcolor(colors_in, 1),
           pfcol=colors_in,
           plty=1,
           cglcol="grey", 
           cglty=1, 
           cglwd=0.6,
           cex.lab = 7,
           cex.axis = 2.2,
           calcex=3,
           vlcex = 1.2
)
legend(x=0.7, y=1, legend = rownames(df[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)
