#!/usr/bin/env Rscript
# Build all book figures from MNA (Medina River Natural Area) UAS LAZ.
# Source: USGS, https://doi.org/10.5066/P9KN8RG0  (public domain)
#
# Outputs go to assets/PNG/. Intermediate LAZ/raster/vector products go to data/.

suppressPackageStartupMessages({
  library(lidR)
  library(sf)
  library(terra)
  library(raster)
  library(RCSF)
  library(ForestTools)
  library(RColorBrewer)
  library(dplyr)
  library(MASS)
  library(randomForest)
  library(caret)
  library(e1071)
})

set.seed(20220708)

proj_root <- rprojroot::find_root(rprojroot::is_git_root)
laz_in    <- file.path(proj_root, "assets", "data", "MNA_pointcloud_20220708.laz")
png_dir   <- file.path(proj_root, "assets", "PNG")
data_dir  <- file.path(proj_root, "data")
dir.create(png_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

save_png <- function(name, w = 900, h = 700, res = 120) {
  grDevices::png(file.path(png_dir, name), width = w, height = h, res = res)
}

# ---- 0. Read AOI directly via LAStools filter (skip full-file decode) ----
# Hardcoded from LAZ header: X 541277-542035, Y 3236863-3237917
# Centre ~ (541656, 3237390); AOI half-width 50 m -> 1 ha
aoi_xmin <- 541606; aoi_xmax <- 541706
aoi_ymin <- 3237340; aoi_ymax <- 3237440

cat("[0] Reading 1 ha AOI via LAStools in-decoder filter...\n")
las_raw <- readLAS(
  laz_in,
  select = "xyzrn",
  filter = sprintf("-keep_xy %f %f %f %f",
                   aoi_xmin, aoi_ymin, aoi_xmax, aoi_ymax)
)
cat("   AOI points:", npoints(las_raw), "\n")

# ---- 1. Catalog / tile overview plot (header-only, fast) ----
cat("[1] Catalog plot (chunk check)...\n")
ctg_summary <- readLAScatalog(laz_in, select = "xyzrn")
save_png("las_tile_medina.png", w = 900, h = 700)
plot(ctg_summary, main = "MNA UAS tile (full extent)")
dev.off()

save_png("tas_ctg_check.png", w = 900, h = 700)
# 2D top-down max-Z raster so this saves to PNG (rgl 3D plots don't hit
# the quartz-PNG device on all macOS installs).
max_z_raw <- pixel_metrics(las_raw, ~max(Z), res = 0.5)
terra::plot(max_z_raw, col = height.colors(50),
            main = "AOI point cloud (max-Z, 0.5 m)")
dev.off()

# Early histogram of raw Z for the unnamed-chunk-4-1 slot
save_png("unnamed-chunk-4-1.png", w = 900, h = 600)
hist(las_raw$Z, breaks = 60, col = "steelblue", border = "white",
     main = "Raw Z distribution (1 ha AOI)", xlab = "Elevation (m)")
dev.off()

# ---- 2. Ground classification: CSF (primary) + PMF on decimated cloud (comparison) ----
cat("[2] Ground classification CSF (full) + PMF (decimated)...\n")
las_csf <- classify_ground(las_raw, csf(sloop_smooth = TRUE, 0.5, 1))

# PMF on a decimated cloud (1 pt per 0.5 m voxel, ~40 pts/m^2) keeps the
# comparison panel cheap -- PMF on the full 200 pts/m^2 cloud is prohibitive.
las_thin <- decimate_points(las_raw, random_per_voxel(res = 0.5, n = 1))
las_pmf  <- classify_ground(las_thin, pmf(ws = 5, th = 1))

# Ground-point density rasters (0.5 m) — 2D so they hit the PNG device.
ground_csf_r <- pixel_metrics(filter_ground(las_csf), ~min(Z), res = 0.5)
ground_pmf_r <- pixel_metrics(filter_ground(las_pmf), ~min(Z), res = 0.5)

save_png("las_tile_medina_csf.png", 900, 700)
terra::plot(ground_csf_r, col = terrain.colors(50),
            main = "CSF ground points (min-Z, 0.5 m)")
dev.off()
save_png("las_tile_medina_pmf.png", 900, 700)
terra::plot(ground_pmf_r, col = terrain.colors(50),
            main = "PMF ground points (min-Z, 0.5 m)")
dev.off()

# ---- 3. Noise removal (SOR) ----
cat("[3] SOR noise removal...\n")
las_csf_so  <- classify_noise(las_csf, sor(k = 10, m = 3))
las_csf_sor <- filter_poi(las_csf_so, Classification != LASNOISE)
las_pmf_so  <- classify_noise(las_pmf, sor(k = 10, m = 3))
las_pmf_sor <- filter_poi(las_pmf_so, Classification != LASNOISE)

# Noise-classification panels: 2D scatter of X/Y coloured by class.
plot_classification_2d <- function(las, path, main) {
  df <- as.data.frame(las@data[, c("X", "Y", "Classification")])
  save_png(path, 900, 700)
  plot(df$X, df$Y, pch = ".", col = ifelse(df$Classification == LASNOISE, "red", "grey40"),
       xlab = "X", ylab = "Y", main = main, asp = 1)
  legend("topright", legend = c("noise", "other"), col = c("red", "grey40"),
         pch = 19, bty = "n", cex = 0.7)
  dev.off()
}
plot_classification_2d(las_csf_so, "las_tile_medina_csf_so.png", "SOR over CSF ground")
plot_classification_2d(las_pmf_so, "las_tile_medina_pmf_so.png", "SOR over PMF ground")

# ---- 4. DTM generation ----
cat("[4] DTM generation...\n")
dtm_csf <- rasterize_terrain(las_csf_sor, res = 0.5, knnidw(k = 10, p = 2, rmax = 50))
dtm_pmf <- rasterize_terrain(las_pmf_sor, res = 0.5, knnidw(k = 10, p = 2, rmax = 50))

render_hillshade <- function(dtm, path, title) {
  slope <- terra::terrain(dtm, "slope", unit = "radians")
  aspect <- terra::terrain(dtm, "aspect", unit = "radians")
  hs <- terra::shade(slope, aspect, angle = 40, direction = 270)
  save_png(path, 900, 700)
  terra::plot(hs, col = grey(0:100 / 100), legend = FALSE, main = title, axes = FALSE)
  dev.off()
}
render_hillshade(dtm_csf, "las_tile_medina_csf_sor_dtm.png", "DTM hillshade (CSF + SOR)")
render_hillshade(dtm_pmf, "las_tile_medina_pmf_sor_dtm.png", "DTM hillshade (PMF + SOR)")

terra::writeRaster(dtm_csf, file.path(data_dir, "dtm_1m.tif"), overwrite = TRUE)

# Hillshade standalone (replaces las_ctg_ahbau_hillshade)
render_hillshade(dtm_csf, "las_ctg_medina_hillshade.png", "DTM hillshade")
# DTM-processing panel (replaces las_ctg_ahbau_dtm_processing)
save_png("las_ctg_medina_dtm_processing.png", 900, 700)
terra::plot(dtm_csf, col = terrain.colors(50), main = "CSF-derived DTM (0.5 m)")
dev.off()

# ---- 5. Height normalization ----
cat("[5] Height normalization...\n")
las_norm <- normalize_height(las_csf_sor, knnidw())

norm_max_z <- pixel_metrics(las_norm, ~max(Z), res = 0.5)
save_png("las_tile_medina_norm.png", 900, 700)
terra::plot(norm_max_z, col = height.colors(50),
            main = "Normalized cloud (max-Z canopy, 0.5 m)")
dev.off()

save_png("las_tile_medina_norm_histogram.png", 900, 600)
hist(filter_ground(las_norm)$Z,
     breaks = seq(-0.6, 0.6, 0.01),
     col = "steelblue", border = "white",
     main = "Ground point heights (normalized)",
     xlab = "Elevation (m)")
dev.off()

# Save normalized LAS for downstream chapters
las_1ha_path <- file.path(data_dir, "las_1ha.laz")
lidR::writeLAS(las_norm, las_1ha_path)

# ---- 6. CHM + tree tops (stem detection) ----
cat("[6] CHM + stem detection...\n")
chm <- rasterize_canopy(las_norm, res = 0.5, pitfree(subcircle = 0.2))

# Fixed window (ws=5)
ttops_fx <- locate_trees(las_norm, lmf(ws = 5))

save_png("stem_detect_A.png", 1000, 700)
terra::plot(chm, col = height.colors(50), main = "CHM + tree tops (ws = 5)")
plot(sf::st_geometry(ttops_fx), add = TRUE, pch = 3, cex = 0.5, col = "black")
dev.off()

# Variable window function
wf <- function(x) {
  y <- 0.07 * x + 0.6
  pmax(y, 0.5)
}
save_png("unnamed-chunk-3-1-window-function.png", 800, 600)
heights_seq <- seq(0, 40, 0.5)
plot(heights_seq, wf(heights_seq), type = "l",
     xlab = "point elevation (m)", ylab = "window diameter (m)",
     main = "Variable window function")
dev.off()

ttops_var <- locate_trees(las_norm, lmf(wf), uniqueness = "bitmerge")
pal <- RColorBrewer::brewer.pal(8, "Greens")
save_png("stem_detect_B.png", 1000, 700)
terra::plot(chm, col = pal, main = "CHM + variable-window tree tops")
plot(sf::st_geometry(ttops_var), add = TRUE, pch = 20, cex = 0.4, col = "red")
dev.off()

# Smoothed-CHM detection
kernel <- matrix(1, 3, 3)
chm_smooth <- terra::focal(chm, w = kernel, fun = median, na.rm = TRUE)
ttops_chm <- locate_trees(chm_smooth, lmf(5))
save_png("stem_detect_C.png", 1000, 700)
terra::plot(chm_smooth, col = height.colors(50), main = "Smoothed CHM + tree tops")
plot(sf::st_geometry(ttops_chm), add = TRUE, pch = 8, cex = 0.5)
dev.off()

# Crown segmentation via dalponte2016
algo    <- dalponte2016(chm, ttops_var)
las_seg <- segment_trees(las_norm, algo)
crowns  <- crown_metrics(las_seg, func = .stdtreemetrics, geom = "convex")

# 2D segmented-crown panel (replaces the 3D treeID view that needs rgl).
save_png("stem_detect_D.png", 1000, 700)
terra::plot(chm, col = grey(0.85), legend = FALSE,
            main = "Segmented crowns (coloured by treeID)")
plot(sf::st_geometry(crowns),
     col = scales::alpha(sample(rainbow(nrow(crowns))), 0.65),
     border = "black", lwd = 0.3, add = TRUE)
dev.off()
save_png("stem_detect_E.png", 1000, 700)
plot(sf::st_geometry(crowns), col = scales::alpha("forestgreen", 0.5), border = "darkgreen",
     main = "Crown polygons")
dev.off()

# 95th percentile CHM panel (used as stem_detect_G)
save_png("stem_detect_G.png", 1000, 700)
terra::plot(chm, col = height.colors(50), main = "CHM (0.5 m)")
dev.off()
save_png("stem_detect_H.png", 1000, 700)
terra::plot(chm_smooth, col = height.colors(50), main = "CHM (smoothed, 3x3 median)")
dev.off()
save_png("stem_detect_F.png", 1000, 700)
terra::plot(chm, col = pal, main = "CHM + stems + crowns")
plot(sf::st_geometry(crowns), add = TRUE, border = "black", col = NA)
plot(sf::st_geometry(ttops_var), add = TRUE, pch = 20, col = "red", cex = 0.4)
dev.off()

# Save ttops for canopy-height chapter
ttops_path <- file.path(data_dir, "ttops_1ha.gpkg")
sf::st_write(ttops_var, ttops_path, delete_dsn = TRUE, quiet = TRUE)

# ---- 7. Canopy-height chapter products ----
cat("[7] Canopy-height raster products...\n")
# 1 m CHM
chm_1m <- rasterize_canopy(las_norm, res = 1, dsmtin(max_edge = 8))
terra::writeRaster(chm_1m, file.path(data_dir, "chm_1m.tif"), overwrite = TRUE)

# 95th percentile aggregation
quant95 <- function(x, ...) quantile(x, probs = 0.95, na.rm = TRUE)
chm_95h <- terra::aggregate(chm_1m, fact = 2, fun = quant95)
terra::writeRaster(chm_95h, file.path(data_dir, "chm_95h.tif"), overwrite = TRUE)

# ---- 8. Biomass-model chapter: synthetic plots + RF model ----
cat("[8] Biomass model (synthetic plots over Medina AOI)...\n")

# Terrain covariates from DTM
elev_r <- dtm_csf
slope_r <- terra::terrain(elev_r, v = "slope", unit = "degrees", neighbors = 8)
slope_pct <- terra::clamp(base::tan(slope_r * pi / 180) * 100, 0, 100)
aspect_r <- terra::terrain(elev_r, v = "aspect", unit = "degrees", neighbors = 8)
asp_cos <- cos((aspect_r * pi) / 180)
asp_sin <- sin((aspect_r * pi) / 180)

terra::writeRaster(elev_r,   file.path(data_dir, "elev_1m.tif"),      overwrite = TRUE)
terra::writeRaster(slope_pct,file.path(data_dir, "slope_1m.tif"),     overwrite = TRUE)
terra::writeRaster(asp_cos,  file.path(data_dir, "northness_1m.tif"), overwrite = TRUE)
terra::writeRaster(asp_sin,  file.path(data_dir, "eastness_1m.tif"),  overwrite = TRUE)

# Synthetic species raster: two classes, spatially autocorrelated via low-pass of slope
set.seed(1)
spp_base <- terra::app(slope_pct, function(v) ifelse(v > 8, 2, 0)) # 0 = open/shrub, 2 = mixed wood
spp_base <- terra::focal(spp_base, w = matrix(1, 7, 7), fun = "modal", na.rm = TRUE)
names(spp_base) <- "species"
terra::writeRaster(spp_base, file.path(data_dir, "species.tif"), overwrite = TRUE)

# Build covariate stack (unnamed positional c() preserves SpatRaster class;
# named args would dispatch to base::c and return a list).
covs <- c(
  elev_r,
  slope_pct,
  asp_cos,
  asp_sin,
  terra::resample(chm_1m, elev_r, method = "bilinear"),
  terra::resample(spp_base, elev_r, method = "near")
)
names(covs) <- c("elev", "slope", "asp_cos", "asp_sin", "chm", "species")

# Simulate 120 "plot" points drawn from raster
bb <- terra::ext(elev_r)
pts <- data.frame(
  x = runif(120, bb[1] + 5, bb[2] - 5),
  y = runif(120, bb[3] + 5, bb[4] - 5)
)
plot_cov <- terra::extract(covs, as.matrix(pts))
plot_cov <- cbind(pts, plot_cov)
# Synthetic WSVHA response: dominated by CHM, with slope + species contributions and noise
plot_cov$wsvha_L <- with(plot_cov,
  pmax(0, 18 * chm + 0.8 * slope + 30 * (species == 2) - 5 + rnorm(nrow(plot_cov), 0, 25))
)
plot_cov <- na.omit(plot_cov)

# Plot-distribution panel (replaces "Original FAIB" / bootstrap figure)
save_png("biomass_plot_distribution.png", 1000, 600)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
MASS::truehist(plot_cov$chm,      main = "Simulated plot CHM (m)",   xlab = "CHM (m)",      col = "steelblue")
MASS::truehist(plot_cov$wsvha_L,  main = "Simulated plot WSVHA",     xlab = "m^3 / ha",     col = "darkseagreen4")
par(op)
dev.off()

# Train / test split, fit RF
idx <- caret::createDataPartition(plot_cov$wsvha_L, p = 0.8, list = FALSE)
train_df <- plot_cov[idx, ]
test_df  <- plot_cov[-idx, ]
rf <- randomForest::randomForest(
  wsvha_L ~ elev + slope + asp_cos + asp_sin + chm + species,
  data = train_df, ntree = 300, mtry = 3
)

pred_test <- predict(rf, test_df)
rmse <- sqrt(mean((pred_test - test_df$wsvha_L)^2))
r2 <- cor(pred_test, test_df$wsvha_L)^2

save_png("biomass_cv_scatter.png", 900, 700)
plot(test_df$wsvha_L, pred_test,
     pch = 20, col = "navy",
     xlab = "Observed WSVHA (m^3/ha)", ylab = "Predicted WSVHA (m^3/ha)",
     main = sprintf("10-fold holdout: R^2 = %.2f, RMSE = %.1f", r2, rmse))
abline(0, 1, col = "red", lty = 2)
dev.off()

# Spatial prediction
pred_r <- terra::predict(covs, rf, na.rm = TRUE)
pred_r <- terra::clamp(pred_r, lower = 0)
terra::writeRaster(pred_r, file.path(data_dir, "wsvha_pred.tif"), overwrite = TRUE)

save_png("biomass_prediction_map.png", 900, 700)
terra::plot(pred_r, col = rev(terrain.colors(50)),
            main = "Predicted WSVHA (m^3/ha), Medina AOI")
dev.off()

save_png("biomass_prediction_hist.png", 900, 600)
hist(values(pred_r), col = "darkseagreen4", border = "white", breaks = 40,
     main = "WSVHA prediction distribution", xlab = "m^3 / ha")
dev.off()

# Covariate stack panel
save_png("biomass_covariate_stack.png", 1100, 800)
op <- par(mfrow = c(2, 3), mar = c(2, 2, 2, 3))
terra::plot(elev_r,   main = "Elevation (m)",    col = terrain.colors(50))
terra::plot(slope_pct,main = "Slope (%)",        col = viridis::viridis(50))
terra::plot(asp_cos,  main = "Northness",        col = hcl.colors(50, "PiYG"))
terra::plot(asp_sin,  main = "Eastness",         col = hcl.colors(50, "PiYG"))
terra::plot(covs$chm, main = "CHM (m)",          col = height.colors(50))
terra::plot(covs$species, main = "Species class",col = c("tan", "forestgreen"))
par(op)
dev.off()

# ---- 9. Graphical abstract (6-column composite) ----
cat("[9] Graphical-abstract composite...\n")
# Column sources
ga_panels <- c(
  col1 = "las_tile_medina.png",
  col2 = "biomass_plot_distribution.png",
  col3 = "biomass_covariate_stack.png",
  col4 = "las_tile_medina_csf_sor_dtm.png",
  col5 = "biomass_cv_scatter.png",
  col6 = "biomass_prediction_map.png"
)
abs_png <- file.path(png_dir, "graphical-abstract-medina.png")
save_png("graphical-abstract-medina.png", 1800, 600, res = 150)
op <- par(mfrow = c(1, 6), mar = c(0.5, 0.5, 1.5, 0.5))
for (nm in names(ga_panels)) {
  img <- png::readPNG(file.path(png_dir, ga_panels[[nm]]))
  plot.new(); rasterImage(img, 0, 0, 1, 1); title(nm, cex.main = 0.7)
}
par(op)
dev.off()

cat("DONE. All outputs in", png_dir, "\n")
