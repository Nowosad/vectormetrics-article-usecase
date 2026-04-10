library(sf)
library(tmap)
library(vectormetrics)
library(factoextra)
setwd("~/projects/rstudio_server/rstudio-home/data")

zabudowa <- get_patches(st_read("Poznań_footprints.gpkg"), class_col="rodzaj")

tm_shape(zabudowa) + tm_polygons(fill="rodzaj", col_alpha=0.1) + tm_add_legend(type = "polygons", 
                                                                                  labels = c("commercial", "single-family residential", "other buildings", "industrial", "multi-family residential"),
                                                                                  col = c("grey", "#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404"),
                                                                                  border.lwd = 0.5,
                                                                                  title = "Types of footprints")


zabudowa$rodzaj <- factor(
  zabudowa$rodzaj,
  levels = c(
    "handlowo-usługowa",
    "jednorodzinna",
    "pozostała zabudowa",
    "przemysłowo-składowa",
    "wielorodzinna"
  ),
  labels = c(
    "commercial and service",
    "single-family residential",
    "other buildings",
    "industrial and storage",
    "multi-family residential"
  )
)
tm_shape(zabudowa) +
  tm_polygons(
    fill = "rodzaj",
    col_alpha = 0.1,
    title = "Types of built-up areas",
    co
  )

metrics_df <- data.frame(
     circularity  = vm_p_circ(zabudowa)$value,
     circle       = vm_p_circle(zabudowa)$value,
     compactness  = vm_p_comp(zabudowa)$value,
     convexity    = vm_p_convex(zabudowa)$value,
     detour       = vm_p_detour(zabudowa)$value,
     elongation   = vm_p_elong(zabudowa)$value,
     eri          = vm_p_eri(zabudowa)$value,
     exchange     = vm_p_exchange(zabudowa)$value,
     fractality   = vm_p_frac(zabudowa)$value,
     fullness     = vm_p_fullness(zabudowa)$value,
     girth        = vm_p_girth(zabudowa)$value,
     per_area     = vm_p_perarea(zabudowa)$value,
     range        = vm_p_range(zabudowa)$value,
     rect         = vm_p_rect(zabudowa)$value,
     roughness    = vm_p_rough(zabudowa)$value,
     shape        = vm_p_shape(zabudowa)$value,
     solidity     = vm_p_solid(zabudowa)$value,
     sphericity   = vm_p_sphere(zabudowa)$value,
     squareness   = vm_p_square(zabudowa)$value
)

metrics_scaled <- scale(metrics_df)
pca <- prcomp(metrics_df, center = TRUE, scale. = TRUE)
summary(pca)

loadings <- pca$rotation
for(i in 1:5){
  cat("\nPC", i, "\n")
  print(sort(abs(loadings[,i]), decreasing = TRUE)[1:6])
}


# pc1 squarness
# pc2 - convexity (poznan elongation)
# pc3 per_area Perimiter-Area ratio, shape, fullness, franctality, elongation (perarea, fractality, rect, sha)
# pc4 fractality Fractal Dimension Index(
# pc 5 rect Rectangularity

fviz_pca_var(
  pca,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,
  title = "(PC1 vs PC2)"
)
