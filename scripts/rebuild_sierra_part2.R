## Continues from rebuild_sierra.R after the crash at dalponte2016.
## Reloads the normalised 1 ha clip and re-runs stem detection + biomass.

suppressPackageStartupMessages({
  library(lidR); library(terra); library(sf); library(dplyr)
  library(randomForest); library(caret); library(RColorBrewer)
})

set.seed(20230718)
png_dir  <- "assets/PNG"
data_dir <- "data"

pngdev <- function(name, w = 900, h = 700)
  grDevices::png(file.path(png_dir, name), width = w, height = h, res = 140)

las_norm <- readLAS(file.path(data_dir, "las_1ha.laz"))
chm      <- rasterize_canopy(las_norm, res = 0.5, pitfree(subcircle = 0.2))

wf <- function(x) pmax(0.07 * x + 0.6, 0.5)
ttops_fixed <- locate_trees(las_norm, lmf(ws = 5))
ttops_vw    <- locate_trees(las_norm, lmf(wf))
ttops_vw$treeID <- seq_len(nrow(ttops_vw))

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

chm_smooth <- terra::focal(chm, w = matrix(1, 3, 3),
                           fun = median, na.rm = TRUE)
ttops_chm  <- locate_trees(chm_smooth, lmf(5))
pngdev("stem_detect_C.png")
terra::plot(chm_smooth, col = height.colors(50),
            main = "Smoothed CHM + raster LMF")
plot(sf::st_geometry(ttops_chm), add = TRUE, pch = 8, cex = 0.5)
dev.off()

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

quant95 <- function(x, ...) stats::quantile(x, .95, na.rm = TRUE)
chm_95h <- terra::aggregate(chm, fact = 2, fun = quant95)
terra::writeRaster(chm_95h, file.path(data_dir, "chm_95h.tif"),
                   overwrite = TRUE)

pngdev("stem_detect_G.png")
terra::plot(chm_95h, col = RColorBrewer::brewer.pal(9, "YlGn"),
            main = "95th-percentile canopy height")
dev.off()

dtm <- rasterize_terrain(las_norm, res = 0.5, tin())
terra::writeRaster(dtm, file.path(data_dir, "dtm_1m.tif"), overwrite = TRUE)
terra::writeRaster(dtm, file.path(data_dir, "elev_1m.tif"), overwrite = TRUE)

slope_deg <- terra::terrain(dtm, "slope",  unit = "degrees", neighbors = 8)
slope_pct <- terra::clamp(tan(slope_deg * pi/180) * 100, 0, 100)
aspect    <- terra::terrain(dtm, "aspect", unit = "degrees", neighbors = 8)
northness <- cos(aspect * pi/180)
eastness  <- sin(aspect * pi/180)
terra::writeRaster(slope_pct, file.path(data_dir, "slope_1m.tif"),     overwrite = TRUE)
terra::writeRaster(northness, file.path(data_dir, "northness_1m.tif"), overwrite = TRUE)
terra::writeRaster(eastness,  file.path(data_dir, "eastness_1m.tif"),  overwrite = TRUE)

spp <- terra::app(slope_pct, function(v) ifelse(v > 8, 2, 0))
spp <- terra::focal(spp, w = matrix(1, 7, 7), fun = "modal", na.rm = TRUE)
names(spp) <- "species"
terra::writeRaster(spp, file.path(data_dir, "species.tif"), overwrite = TRUE)

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

cat("rebuild_sierra_part2.R complete\n")
