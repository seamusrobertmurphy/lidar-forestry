#!/usr/bin/env Rscript
# Rebuild assets/PNG/graphical-abstract-medina.png as a titled 5-column
# infographic in the style of the original BC AHBAU abstract. LinkedIn-sized
# landscape output (~1.71:1).

suppressPackageStartupMessages({
  library(png)
})

proj_root <- rprojroot::find_root(rprojroot::is_git_root)
png_dir   <- file.path(proj_root, "assets", "PNG")
out_png   <- file.path(png_dir, "graphical-abstract-medina.png")

# ------------------------------------------------------------------ layout --
W <- 2400L; H <- 1200L

n_col      <- 5L
m_out      <- 20
m_in       <- 18
col_w      <- (W - m_out * 2 - m_in * (n_col - 1)) / n_col
title_h    <- 72
pad        <- 14

col_bg     <- "#F4F5F6"
col_border <- "#B8BEC4"
title_bg   <- "#2F4F7F"
title_fg   <- "white"
arrow_col  <- "#3A3A3A"
text_col   <- "#1A1A1A"

col_titles <- c(
  "Study area &\nground plots",
  "Species & mask\ncovariates",
  "ALS covariates\nprocessing",
  "Model, cross-validate,\nre-model",
  "Spatial predictions\n& mapping"
)

# ----------------------------------------------------------------- helpers --
read_img <- function(name) png::readPNG(file.path(png_dir, name))

draw_panel <- function(img, cx, cy_top, avail_w, avail_h) {
  h_img <- dim(img)[1]; w_img <- dim(img)[2]
  r_img <- w_img / h_img
  r_box <- avail_w / avail_h
  if (r_img > r_box) {
    draw_w <- avail_w; draw_h <- draw_w / r_img
  } else {
    draw_h <- avail_h; draw_w <- draw_h * r_img
  }
  x0 <- cx - draw_w / 2
  y1 <- cy_top; y0 <- cy_top - draw_h
  rasterImage(img, x0, y0, x0 + draw_w, y1, interpolate = TRUE)
  y0
}

draw_text <- function(cx, cy_top, txt, cex = 0.65, font = 1, gap = 6,
                      col = text_col, line_h_px = NULL) {
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  line_h <- if (is.null(line_h_px)) cex * 22 else line_h_px
  for (i in seq_along(lines)) {
    text(cx, cy_top - line_h / 2 - (i - 1) * line_h,
         labels = lines[i], cex = cex, font = font, col = col)
  }
  cy_top - length(lines) * line_h - gap
}

draw_down_arrow <- function(cx, cy_top, height = 26) {
  stem_top <- cy_top - 2
  stem_bot <- cy_top - height + 14
  rect(cx - 3, stem_bot, cx + 3, stem_top, col = arrow_col, border = NA)
  polygon(c(cx - 11, cx + 11, cx),
          c(stem_bot, stem_bot, stem_bot - 14),
          col = arrow_col, border = NA)
  cy_top - height - 6
}

draw_circle <- function(cx, cy, r, col = "#9A9A9A", lwd = 0.6) {
  th <- seq(0, 2 * pi, length.out = 72)
  lines(cx + r * cos(th), cy + r * sin(th), col = col, lwd = lwd)
}

draw_bullseye <- function(cx_i, cy_i, r, seed, spread, bias_x = 0, bias_y = 0) {
  for (rr in seq(r, r / 4, length.out = 3)) draw_circle(cx_i, cy_i, rr)
  set.seed(seed)
  pts <- matrix(rnorm(16, 0, spread), ncol = 2)
  pts[, 1] <- pts[, 1] + bias_x
  pts[, 2] <- pts[, 2] + bias_y
  points(cx_i + pts[, 1], cy_i + pts[, 2], pch = 19, cex = 0.5, col = "#B33A3A")
}

# ----------------------------------------------------------------- canvas --
grDevices::png(out_png, W, H, res = 150, bg = "white")
par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", family = "sans")
plot.new()
plot.window(xlim = c(0, W), ylim = c(0, H))

col_xs <- m_out + (0:(n_col - 1)) * (col_w + m_in)

# Column shells + titles
for (i in seq_len(n_col)) {
  x0 <- col_xs[i]; x1 <- x0 + col_w
  rect(x0, 0, x1, H, col = col_bg, border = col_border, lwd = 0.8)
  rect(x0, H - title_h, x1, H, col = title_bg, border = col_border, lwd = 0.8)
  text((x0 + x1) / 2, H - title_h / 2, labels = col_titles[i],
       col = title_fg, font = 2, cex = 1.0)
}

# Horizontal inter-column flow arrows (bottom band)
arrow_y <- 28
for (i in seq_len(n_col - 1)) {
  x_start <- col_xs[i] + col_w
  x_end   <- col_xs[i + 1]
  xm <- (x_start + x_end) / 2
  polygon(
    x = c(x_start + 2, xm - 4, xm - 4, x_end - 2, xm - 4, xm - 4, x_start + 2),
    y = c(arrow_y - 5, arrow_y - 5, arrow_y - 12, arrow_y,
          arrow_y + 12, arrow_y + 5, arrow_y + 5),
    col = arrow_col, border = NA
  )
}

content_top <- H - title_h - 10

# ---------------------------------------------- Col 1: Study area + plots --
i <- 1; cx <- col_xs[i] + col_w / 2; y <- content_top
y <- draw_text(cx, y,
               "Medina River Natural Area, TX\nUSGS UAS 2022 (1.3 GB LAZ, 206 pts/m^2)",
               cex = 0.62, font = 3)
y <- draw_panel(read_img("las_tile_medina_norm.png"),
                cx, y, col_w - 2 * pad, 520)
y <- draw_down_arrow(cx, y - 6)
y <- draw_text(cx, y, "Simulate 120 plots;\nextract covariates",
               cex = 0.68, font = 1)
y <- draw_panel(read_img("biomass_plot_distribution.png"),
                cx, y, col_w - 2 * pad, 320)

# --------------------------------------- Col 2: Species & mask covariates --
i <- 2; cx <- col_xs[i] + col_w / 2; y <- content_top
y <- draw_text(cx, y, "Derive species cover + mask\nfrom terrain covariates",
               cex = 0.64)
y <- draw_panel(read_img("biomass_covariate_stack.png"),
                cx, y, col_w - 2 * pad, 780)
y <- draw_down_arrow(cx, y - 4)
y <- draw_text(cx, y, "Apply mask; compare distributions", cex = 0.64)
y <- draw_panel(read_img("biomass_prediction_hist.png"),
                cx, y, col_w - 2 * pad, 240)

# --------------------------------------------- Col 3: ALS covariates -----
i <- 3; cx <- col_xs[i] + col_w / 2; y <- content_top
y <- draw_text(cx, y,
               "Import pre-processed LiDAR;\nderive DEM (slope/aspect)",
               cex = 0.62)
y <- draw_panel(read_img("las_tile_medina_csf_sor_dtm.png"),
                cx, y, col_w - 2 * pad, 400)
y <- draw_down_arrow(cx, y - 4)
y <- draw_text(cx, y, "Derive CHM-based covariates", cex = 0.64)
y <- draw_panel(read_img("stem_detect_G.png"),
                cx, y, col_w - 2 * pad, 380)

# ---------------------------------------- Col 4: Model, CV, re-model -----
i <- 4; cx <- col_xs[i] + col_w / 2; y <- content_top
y <- draw_text(cx, y,
               "Split plots 80:20; derive\n10-fold training index",
               cex = 0.6)
# Train/test split bar
bar_w <- col_w - 3 * pad; bar_h <- 26
bar_x0 <- cx - bar_w / 2; bar_x1 <- cx + bar_w / 2
bar_y_top <- y - 4; bar_y_bot <- bar_y_top - bar_h
split_x <- bar_x0 + bar_w * 0.80
rect(bar_x0, bar_y_bot, bar_x1, bar_y_top,
     col = "#D9D9D9", border = "#333", lwd = 0.6)
rect(bar_x0, bar_y_bot, split_x, bar_y_top,
     col = "#5F8FBF", border = "#333", lwd = 0.6)
rect(split_x, bar_y_bot, bar_x1, bar_y_top,
     col = "#E3A847", border = "#333", lwd = 0.6)
text((bar_x0 + split_x) / 2, (bar_y_top + bar_y_bot) / 2,
     "Training (80%)", col = "white", cex = 0.55, font = 2)
text((split_x + bar_x1) / 2, (bar_y_top + bar_y_bot) / 2,
     "Test (20%)", col = "white", cex = 0.55, font = 2)
y <- bar_y_bot - 8
y <- draw_down_arrow(cx, y)
y <- draw_text(cx, y, "Fit RF; evaluate on holdout", cex = 0.64)
y <- draw_panel(read_img("biomass_cv_scatter.png"),
                cx, y, col_w - 2 * pad, 380)
y <- draw_down_arrow(cx, y - 4)
y <- draw_text(cx, y, "Diagnose precision vs accuracy", cex = 0.62)

# Precision/accuracy quadrant
box_w <- col_w - 5 * pad
box_h <- box_w * 0.66
bx0 <- cx - box_w / 2; bx1 <- cx + box_w / 2
by_top <- y - 6; by_bot <- by_top - box_h
rect(bx0, by_bot, bx1, by_top, col = "white", border = "#555")
xm <- (bx0 + bx1) / 2; ym <- (by_top + by_bot) / 2
segments(xm, by_bot, xm, by_top, col = "#555")
segments(bx0, ym, bx1, ym, col = "#555")
text((bx0 + xm) / 2, by_top + 7, "PRECISE",   cex = 0.5, font = 2)
text((xm + bx1) / 2, by_top + 7, "IMPRECISE", cex = 0.5, font = 2)
text(bx0 - 8, (ym + by_top) / 2, "BIASED",   cex = 0.5, font = 2, srt = 90)
text(bx0 - 8, (by_bot + ym) / 2, "UNBIASED", cex = 0.5, font = 2, srt = 90)
q_r <- min(box_w / 6, box_h / 4)
draw_bullseye((bx0 + xm) / 2, (by_top + ym) / 2, q_r,
              seed = 1, spread = q_r * 0.08, bias_x = q_r * 0.25, bias_y = q_r * 0.25)
draw_bullseye((xm + bx1) / 2, (by_top + ym) / 2, q_r,
              seed = 2, spread = q_r * 0.35, bias_x = q_r * 0.15, bias_y = q_r * 0.2)
draw_bullseye((bx0 + xm) / 2, (by_bot + ym) / 2, q_r,
              seed = 3, spread = q_r * 0.08)
draw_bullseye((xm + bx1) / 2, (by_bot + ym) / 2, q_r,
              seed = 4, spread = q_r * 0.35)

# ----------------------------------- Col 5: Spatial predictions & mapping --
i <- 5; cx <- col_xs[i] + col_w / 2; y <- content_top
y <- draw_text(cx, y, "Apply RF across covariate\nraster stack", cex = 0.64)
y <- draw_panel(read_img("biomass_prediction_map.png"),
                cx, y, col_w - 2 * pad, 800)
y <- draw_down_arrow(cx, y - 4)
y <- draw_text(cx, y, "Predicted WSVHA distribution", cex = 0.64)
y <- draw_panel(read_img("biomass_prediction_hist.png"),
                cx, y, col_w - 2 * pad, 280)

# Footer strapline — small, over the arrow band
footer_y <- 10
text(W - m_out - 4, footer_y,
     adj = c(1, 0),
     labels = "Source: USGS DOI:10.5066/P9KN8RG0  (public domain)",
     cex = 0.45, col = "#555", font = 3)

dev.off()

cat("Wrote", out_png, "  (", W, "x", H, "px)\n")
