#!/usr/bin/env Rscript
# Rebuild assets/PNG/graphical-abstract-medina.png from existing panel PNGs,
# preserving each panel's native aspect ratio.

suppressPackageStartupMessages({
  library(png)
})

proj_root <- rprojroot::find_root(rprojroot::is_git_root)
png_dir   <- file.path(proj_root, "assets", "PNG")

panels <- c(
  "las_tile_medina_norm.png",        # col1: study area (canopy max-Z)
  "biomass_plot_distribution.png",   # col2: simulated plots
  "biomass_covariate_stack.png",     # col3: covariate stack
  "las_tile_medina_csf_sor_dtm.png", # col4: DTM hillshade
  "biomass_cv_scatter.png",          # col5: CV scatter
  "biomass_prediction_map.png"       # col6: WSVHA prediction
)

target_h <- 900L
gap_px   <- 30L

imgs <- lapply(file.path(png_dir, panels), png::readPNG)
h_px <- vapply(imgs, function(im) dim(im)[1], integer(1))
w_px <- vapply(imgs, function(im) dim(im)[2], integer(1))
scaled_w <- as.integer(round(w_px * target_h / h_px))

total_w <- sum(scaled_w) + gap_px * (length(imgs) - 1L)

out_png <- file.path(png_dir, "graphical-abstract-medina.png")
grDevices::png(out_png, width = total_w, height = target_h, res = 144, bg = "white")
par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
plot.new()
plot.window(xlim = c(0, total_w), ylim = c(0, target_h))

x <- 0L
for (i in seq_along(imgs)) {
  w <- scaled_w[i]
  rasterImage(imgs[[i]], x, 0, x + w, target_h, interpolate = TRUE)
  x <- x + w + gap_px
}
dev.off()

cat("Wrote ", out_png, "\n  canvas: ", total_w, "x", target_h, " px\n", sep = "")
cat("  panel widths: ", paste(scaled_w, collapse = " "), "\n")
