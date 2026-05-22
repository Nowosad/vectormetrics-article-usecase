library(sf)
library(tmap)
library(vectormetrics)
library(factoextra)

builtup_areas <- get_polygon_patches(st_read("data/Poznań_footprints.gpkg"), class_col="rodzaj")
# tm_shape(builtup_areas) + tm_polygons(fill="rodzaj", col_alpha=0.1) + tm_add_legend(type = "polygons", 
#                                                                                    labels = c("commercial", "single-family residential", "other buildings", "industrial", "multi-family residential"),
#                                                                                    col = c("grey", "#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404"),
#                                                                                    border.lwd = 0.5,
#                                                                                    title = "Types of footprints")

builtup_areas$rodzaj <- factor(
  builtup_areas$rodzaj,
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

tm_shape(builtup_areas) +
  tm_polygons(
    fill = "rodzaj",
    col_alpha = 0.1,
    title = "Types of built-up areas"
  )

metrics_df <- data.frame(
     circularity  = vm_p_circ(builtup_areas)$value,
     circle       = vm_p_circle(builtup_areas)$value,
     compactness  = vm_p_comp(builtup_areas)$value,
     convexity    = vm_p_convex(builtup_areas)$value,
     detour       = vm_p_detour(builtup_areas)$value,
     elongation   = vm_p_elong(builtup_areas)$value,
     eri          = vm_p_eri(builtup_areas)$value,
     exchange     = vm_p_exchange(builtup_areas)$value,
     fractality   = vm_p_frac(builtup_areas)$value,
     fullness     = vm_p_fullness(builtup_areas)$value,
     girth        = vm_p_girth(builtup_areas)$value,
     per_area     = vm_p_perarea(builtup_areas)$value,
     range        = vm_p_range(builtup_areas)$value,
     rect         = vm_p_rect(builtup_areas)$value,
     roughness    = vm_p_rough(builtup_areas)$value,
     shape        = vm_p_shape(builtup_areas)$value,
     solidity     = vm_p_solid(builtup_areas)$value,
     sphericity   = vm_p_sphere(builtup_areas)$value,
     squareness   = vm_p_square(builtup_areas)$value
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
# pc5 rect Rectangularity

fviz_pca_var(
  pca,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,
  title = "(PC1 vs PC2)"
)
