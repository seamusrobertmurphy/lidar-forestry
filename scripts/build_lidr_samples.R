## Regenerate the 3D lidR sample-data graphics referenced in the preface:
## segmented crowns (lasso), 3D terrain, and kernel-density net.

suppressPackageStartupMessages({
  library(lidR); library(terra); library(sf); library(rgl)
})

out_dir <- "assets/PNG"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

snap <- function(file, w = 900, h = 700) {
  par3d(windowRect = c(0, 45, w, h + 45))
  Sys.sleep(0.8)
  rgl.snapshot(file.path(out_dir, file), fmt = "png")
  close3d()
}

## --------------------------------------------------------
## Megaplot: segmented crowns ("lasso" look)
## --------------------------------------------------------
megaplot <- system.file("extdata", "Megaplot.laz", package = "lidR")
las      <- readLAS(megaplot, filter = "-drop_z_below 0")
las      <- classify_ground(las, csf(sloop_smooth = TRUE, 0.5, 1))
las      <- normalize_height(las, knnidw())

chm   <- rasterize_canopy(las, res = 0.5, pitfree(subcircle = 0.2))
ttops <- locate_trees(las, lmf(ws = 5))
ttops$treeID <- seq_len(nrow(ttops))
las_seg <- segment_trees(las, dalponte2016(chm, ttops))

plot(las_seg, bg = "white", size = 1.6, color = "treeID",
     legend = FALSE, axis = FALSE)
snap("lidr_sample_megaplot_crowns.png")

## Same but stripped to canopy hull polygons laid over CHM
crowns <- crown_metrics(las_seg, func = .stdtreemetrics, geom = "convex")
grDevices::png(file.path(out_dir, "lidr_sample_megaplot_lasso.png"),
               width = 900, height = 700, res = 140)
terra::plot(chm, col = height.colors(50),
            main = "Megaplot segmented crowns (lasso)")
plot(sf::st_geometry(crowns), add = TRUE, border = "black", lwd = 0.7)
plot(sf::st_geometry(ttops), add = TRUE, pch = 20, col = "red", cex = 0.5)
dev.off()

## --------------------------------------------------------
## MixedConifer: kernel-density 3D profile
## --------------------------------------------------------
mc  <- system.file("extdata", "MixedConifer.laz", package = "lidR")
las <- readLAS(mc, filter = "-drop_z_below 0")
las <- classify_ground(las, csf(sloop_smooth = TRUE, 0.5, 1))
las <- normalize_height(las, knnidw())

plot(las, bg = "white", size = 1.4, color = "Z",
     legend = FALSE, axis = FALSE)
snap("lidr_sample_mixedconifer_3d.png")

chm_mc <- rasterize_canopy(las, res = 0.5, pitfree(subcircle = 0.2))
dtm_mc <- rasterize_terrain(las, res = 0.5, tin())

## "Kernel-density net": surface mesh of the CHM floating above ground.
plot_dtm3d(dtm_mc, bg = "white")
snap("lidr_sample_mixedconifer_dtm3d.png")

plot_dtm3d(chm_mc, bg = "white")
snap("lidr_sample_mixedconifer_chm_net.png")

## --------------------------------------------------------
## Topography: signature 3D terrain render
## --------------------------------------------------------
topo <- system.file("extdata", "Topography.laz", package = "lidR")
las  <- readLAS(topo, filter = "-drop_z_below 0")
las  <- classify_ground(las, csf(sloop_smooth = TRUE, 0.5, 1))
dtm  <- rasterize_terrain(las, res = 1, tin())

plot_dtm3d(dtm, bg = "white")
snap("lidr_sample_topography_dtm3d.png")

cat("build_lidr_samples.R complete\n")
