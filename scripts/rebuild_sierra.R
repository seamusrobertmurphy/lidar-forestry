## Rebuild all data/ outputs and chapter PNGs from the USGS Carr/Hirz/Delta
## tile 10SGH1587 (distributed through the USGS 3DEP LPC / SierraNevada B22
## bundle). One 1 ha clip drives every chapter so chunks stay small.

suppressPackageStartupMessages({
  library(lidR); library(terra); library(sf); library(dplyr)
  library(RCSF); library(randomForest); library(caret)
  library(RColorBrewer)
})

set.seed(20230718)

src_laz <- "assets/data/USGS_LPC_CA_SierraNevada_B22_10SGH1587.laz"
png_dir <- "assets/PNG"
data_dir <- "data"
dir.create(png_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

pngdev <- function(name, w = 900, h = 700) {
  grDevices::png(file.path(png_dir, name), width = w, height = h, res = 140)
}

## --------------------------------------------------------------
## 1. Find a tree-rich 100 m AOI in the 1 km tile
## --------------------------------------------------------------
ctg <- readLAScatalog(src_laz, select = "xyzcr", filter = "-thin_with_grid 0.5")
opt_chunk_size(ctg)   <- 500
opt_chunk_buffer(ctg) <- 10

## Coarse 25 m grid of the 95th-percentile Z to pick a dense-canopy AOI
scan_las <- readLAS(src_laz, select = "xyz", filter = "-keep_every_nth 20")
ext_las  <- st_bbox(scan_las)
z95_grid <- pixel_metrics(scan_las, ~quantile(Z, .95, na.rm = TRUE), res = 25)
rm(scan_las); gc()

z95_v <- terra::values(z95_grid)
pick  <- which.max(z95_v - terra::global(z95_grid, "min", na.rm = TRUE)[1,1])
xy    <- terra::xyFromCell(z95_grid, pick)
cx    <- xy[1, "x"]; cy <- xy[1, "y"]
cat(sprintf("Chosen AOI centre: x=%.1f y=%.1f  (95p Z=%.1f m)\n",
            cx, cy, z95_v[pick]))

## --------------------------------------------------------------
## 2. Clip 1 ha and run the ground / DTM / normalise / CHM pipeline
## --------------------------------------------------------------
las_raw <- clip_rectangle(ctg, cx - 50, cy - 50, cx + 50, cy + 50)
cat("Raw clip points:", npoints(las_raw), "\n")

pngdev("las_tile_sierra.png")
plot(las_raw, bg = "white", size = 1.2, color = "Z", legend = TRUE)
rgl::rgl.snapshot(file.path(png_dir, "las_tile_sierra.png"))
rgl::close3d()
dev.off()

## Ground classification: CSF vs PMF
las_csf <- classify_ground(las_raw, csf(sloop_smooth = TRUE, 0.5, 1))
las_pmf <- classify_ground(las_raw, pmf(ws = c(3, 6, 9),
                                        th = c(0.5, 1, 1.5)))

plot_top <- function(las, file, title) {
  pngdev(file)
  plot(las, color = "Classification", bg = "white", size = 1.3)
  rgl::rgl.snapshot(file.path(png_dir, file))
  rgl::close3d()
  dev.off()
}
plot_top(las_csf, "las_tile_sierra_csf.png", "CSF ground")
plot_top(las_pmf, "las_tile_sierra_pmf.png", "PMF ground")

## Noise removal
las_csf_sor <- filter_poi(classify_noise(las_csf, sor(k = 10, m = 3)),
                          Classification != LASNOISE)
las_pmf_sor <- filter_poi(classify_noise(las_pmf, sor(k = 10, m = 3)),
                          Classification != LASNOISE)
plot_top(las_csf_sor, "las_tile_sierra_csf_so.png", "CSF + SOR")
plot_top(las_pmf_sor, "las_tile_sierra_pmf_so.png", "PMF + SOR")

## DTMs
dtm_csf <- rasterize_terrain(las_csf_sor, res = 0.5,
                             knnidw(k = 10, p = 2, rmax = 50))
dtm_pmf <- rasterize_terrain(las_pmf_sor, res = 0.5,
                             knnidw(k = 10, p = 2, rmax = 50))

plot_dtm_png <- function(r, file, title) {
  pngdev(file)
  slope  <- terra::terrain(r, "slope",  unit = "radians")
  aspect <- terra::terrain(r, "aspect", unit = "radians")
  hs     <- terra::shade(slope, aspect, angle = 40, direction = 270)
  terra::plot(hs, col = grey(0:100/100), legend = FALSE, main = title)
  terra::contour(r, add = TRUE, nlevels = 10, col = "grey30")
  dev.off()
}
plot_dtm_png(dtm_csf, "las_tile_sierra_csf_sor_dtm.png", "DTM (CSF + SOR)")
plot_dtm_png(dtm_pmf, "las_tile_sierra_pmf_sor_dtm.png", "DTM (PMF + SOR)")

## Raw-Z histogram
pngdev("unnamed-chunk-4-1.png", 700, 500)
hist(las_raw$Z, breaks = 80, col = "grey80", border = "white",
     main = "Raw Z distribution (Carr 1 ha)", xlab = "Z (m)")
dev.off()

## Catalog context plot
pngdev("las_ctg_sierra_dtm_processing.png", 700, 500)
par(mar = c(3, 3, 2, 1))
plot(ctg, main = "Catalog chunks (1 km tile)")
dev.off()

pngdev("tas_ctg_check.png", 700, 520)
par(mar = c(0, 0, 0, 0)); plot.new()
txt <- capture.output(las_check(ctg))
text(0, 1, paste(utils::head(txt, 22), collapse = "\n"),
     adj = c(0, 1), family = "mono", cex = 0.72)
dev.off()

## DTM from pre-normalisation cloud. Must precede normalize_height: running
## rasterize_terrain on a height-normalised LAS collapses Z to ~0 everywhere
## and produces a flat DTM (the bug that emptied the terrain covariate stack).
dtm <- rasterize_terrain(las_csf_sor, res = 0.5, tin())
terra::writeRaster(dtm, file.path(data_dir, "dtm_1m.tif"),  overwrite = TRUE)
terra::writeRaster(dtm, file.path(data_dir, "elev_1m.tif"), overwrite = TRUE)

## Normalise
las_norm <- normalize_height(las_csf_sor, knnidw())

pngdev("las_tile_sierra_norm.png")
plot(las_norm, bg = "white", size = 1.2, color = "Z")
rgl::rgl.snapshot(file.path(png_dir, "las_tile_sierra_norm.png"))
rgl::close3d()
dev.off()

pngdev("las_tile_sierra_norm_histogram.png", 700, 500)
hist(filter_ground(las_norm)$Z, breaks = seq(-0.6, 0.6, 0.01),
     col = "grey80", border = "white",
     main = "Ground point residuals (Carr 1 ha)", xlab = "Z (m)")
dev.off()

writeLAS(las_norm, file.path(data_dir, "las_1ha.laz"))

## --------------------------------------------------------------
## 3. Tree tops and crown segmentation
## --------------------------------------------------------------
chm <- rasterize_canopy(las_norm, res = 0.5, pitfree(subcircle = 0.2))
terra::writeRaster(chm, file.path(data_dir, "chm_1m.tif"), overwrite = TRUE)

wf <- function(x) pmax(0.07 * x + 0.6, 0.5)
ttops_fixed <- locate_trees(las_norm, lmf(ws = 5))
ttops_vw    <- locate_trees(las_norm, lmf(wf), uniqueness = "bitmerge")

pngdev("stem_detect_A.png")
terra::plot(chm, col = height.colors(50), main = "Fixed 5 m window")
plot(sf::st_geometry(ttops_fixed), add = TRUE, pch = 3, cex = 0.5)
dev.off()

pngdev("unnamed-chunk-3-1-window-function.png", 700, 500)
heights <- seq(0, 45, 0.5)
plot(heights, wf(heights), type = "l", lwd = 2,
     xlab = "Point elevation (m)", ylab = "Window diameter (m)",
     main = "Variable-window function")
dev.off()

pngdev("stem_detect_B.png")
terra::plot(chm, col = RColorBrewer::brewer.pal(8, "Greens"),
            main = "Variable-window tree tops", alpha = 0.6)
plot(sf::st_geometry(ttops_vw), add = TRUE, pch = 20,
     col = "red", cex = 0.5)
dev.off()

## Raster-based detection
chm_smooth <- terra::focal(chm, w = matrix(1, 3, 3),
                           fun = median, na.rm = TRUE)
ttops_chm  <- locate_trees(chm_smooth, lmf(5))
pngdev("stem_detect_C.png")
terra::plot(chm_smooth, col = height.colors(50),
            main = "Smoothed CHM + raster LMF")
plot(sf::st_geometry(ttops_chm), add = TRUE, pch = 8, cex = 0.5)
dev.off()

## Segmentation
algo    <- dalponte2016(chm, ttops_vw)
las_seg <- segment_trees(las_norm, algo)
crowns  <- crown_metrics(las_seg, func = .stdtreemetrics, geom = "convex")

pngdev("stem_detect_D.png")
plot(las_seg, bg = "white", size = 2.5, color = "treeID")
rgl::rgl.snapshot(file.path(png_dir, "stem_detect_D.png"))
rgl::close3d()
dev.off()

pngdev("stem_detect_E.png")
terra::plot(chm, col = height.colors(50), main = "Delineated crowns")
plot(sf::st_geometry(crowns), add = TRUE, border = "black", lwd = 0.5)
dev.off()

sf::st_write(ttops_vw, file.path(data_dir, "ttops_1ha.gpkg"),
             delete_dsn = TRUE, quiet = TRUE)

## --------------------------------------------------------------
## 4. Canopy height 95p + terrain + species covariates
## --------------------------------------------------------------
quant95 <- function(x, ...) stats::quantile(x, .95, na.rm = TRUE)
chm_95h <- terra::aggregate(chm, fact = 2, fun = quant95)
terra::writeRaster(chm_95h, file.path(data_dir, "chm_95h.tif"),
                   overwrite = TRUE)

pngdev("stem_detect_G.png")
terra::plot(chm_95h, col = RColorBrewer::brewer.pal(9, "YlGn"),
            main = "95th-percentile canopy height")
dev.off()

slope_deg <- terra::terrain(dtm, "slope",  unit = "degrees", neighbors = 8)
slope_pct <- terra::clamp(tan(slope_deg * pi/180) * 100, 0, 100)
aspect    <- terra::terrain(dtm, "aspect", unit = "degrees", neighbors = 8)
northness <- cos(aspect * pi/180)
eastness  <- sin(aspect * pi/180)
terra::writeRaster(slope_pct,  file.path(data_dir, "slope_1m.tif"),     overwrite = TRUE)
terra::writeRaster(northness,  file.path(data_dir, "northness_1m.tif"), overwrite = TRUE)
terra::writeRaster(eastness,   file.path(data_dir, "eastness_1m.tif"),  overwrite = TRUE)

## Synthetic species layer: slope split, smoothed
spp <- terra::app(slope_pct, function(v) ifelse(v > 8, 2, 0))
spp <- terra::focal(spp, w = matrix(1, 7, 7), fun = "modal", na.rm = TRUE)
names(spp) <- "species"
terra::writeRaster(spp, file.path(data_dir, "species.tif"), overwrite = TRUE)

## --------------------------------------------------------------
## 5. Biomass model on simulated plots
## --------------------------------------------------------------
chm_r <- terra::resample(chm, dtm, method = "bilinear")
spp_r <- terra::resample(spp, dtm, method = "near")
chm_r[chm_r < 1.3] <- NA

covs <- c(dtm, slope_pct, northness, eastness, chm_r, spp_r)
names(covs) <- c("elev", "slope", "asp_cos", "asp_sin", "chm", "species")

pngdev("biomass_covariate_stack.png", 900, 700)
terra::plot(covs)
dev.off()

bb  <- terra::ext(covs)
pts <- data.frame(
  x = runif(150, bb[1] + 5, bb[2] - 5),
  y = runif(150, bb[3] + 5, bb[4] - 5))
plot_cov <- cbind(pts, terra::extract(covs, as.matrix(pts)))
plot_cov$wsvha_L <- with(plot_cov,
  pmax(0, 18 * chm + 0.8 * slope + 30 * (species == 2) - 5 +
         rnorm(nrow(plot_cov), 0, 25)))
plot_cov <- na.omit(plot_cov)

pngdev("biomass_plot_distribution.png", 800, 450)
par(mfrow = c(1, 2))
MASS::truehist(plot_cov$chm,     main = "Plot CHM (Carr)")
MASS::truehist(plot_cov$wsvha_L, main = "Plot WSVHA (simulated)")
dev.off()

idx   <- caret::createDataPartition(plot_cov$wsvha_L, p = 0.80, list = FALSE)
train <- plot_cov[idx, ];  test  <- plot_cov[-idx, ]
rf    <- randomForest::randomForest(
  wsvha_L ~ elev + slope + asp_cos + asp_sin + chm + species,
  data = train, ntree = 300, mtry = 3)

rf_pred <- stats::predict(rf, test)
cat(sprintf("RF test RMSE: %.2f  R^2: %.3f\n",
            caret::RMSE(rf_pred, test$wsvha_L),
            caret::R2(rf_pred,  test$wsvha_L)))

save(rf, file = file.path(data_dir, "wsvha_model_rf_sierra.RData"))

wsvha_raster <- terra::predict(covs, rf, na.rm = TRUE)
wsvha_raster <- terra::clamp(wsvha_raster, lower = 0)
terra::writeRaster(wsvha_raster, file.path(data_dir, "wsvha_pred.tif"),
                   overwrite = TRUE)

pngdev("biomass_prediction_map.png", 700, 600)
terra::plot(wsvha_raster, main = "Predicted WSVHA (m^3/ha)",
            col = rev(terrain.colors(50)))
dev.off()

pngdev("biomass_prediction_hist.png", 700, 500)
hist(terra::values(wsvha_raster), breaks = 40, col = "grey80",
     border = "white", main = "WSVHA distribution", xlab = "m^3/ha")
dev.off()

pngdev("biomass_cv_scatter.png", 700, 600)
plot(test$wsvha_L, rf_pred, pch = 19,
     xlab = "Observed WSVHA (m^3/ha)", ylab = "RF predicted",
     main = "RF holdout performance")
abline(0, 1, lty = 2); grid()
dev.off()

cat("rebuild_sierra.R complete\n")
