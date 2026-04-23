# lidar-forestry

A Quarto book documenting an end-to-end airborne LiDAR workflow for forest inventory: raw point cloud to aboveground biomass, with validation against field measurements. Rendered at <https://seamusrobertmurphy.quarto.pub/lidar-forestry/>.

The working example is a 1 ha clip from USGS 3DEP tile `10SGH1587` over the Carr Hirz Delta Fires burn scar in Shasta County, California. Processing is in R with `lidR`, `ForestTools`, `sf`, and `terra`; dependencies are pinned via `renv`.

## Chapters

1. `01-point-cloud/` — catalog management, ground classification (CSF vs PMF), noise removal, DTM generation, height normalisation.
2. `02-stem-detect/` — individual tree detection comparing raster-based (CHM local maxima) and point-cloud-based (3D clustering) methods.
3. `03-canopy-height/` — CHM construction, gap and edge handling, smoothing, cross-validation.
4. `04-biomass-model/` — allometric application, random forest model, bias correction, uncertainty quantification.
5. `05-validation/` — rFVS demo for growth-and-yield cross-checks (appendix, not wired into the book chapters).

## Layout

```
_quarto.yml          book config and chapter order
index.qmd            preface, workflow overview, environment setup
01-…/ … 04-…/        chapter sources (index.qmd + figures)
05-validation/       rFVS validation demo
data/                derived rasters, ttops, trained RF model, 1 ha .laz clip
assets/data/         source LAZ tiles and USGS data validation PDFs
assets/PNG/          figures referenced from the chapters
scripts/             standalone builders (graphical abstract, lidR samples, Sierra rebuild)
references/          references.bib, APA CSL, appendix index
resources/           learning-resources appendix
renv/, renv.lock     locked R package environment
styles.scss          book theme overrides
_book/               rendered HTML (gitignored output)
archive/             prior drafts kept for reference
```

## Build

```sh
R -e 'renv::restore()'
quarto render
```

`quarto preview` serves the book locally. Full rebuilds are slow; the 1 ha clip keeps chunks under a million points so individual chapters render in minutes on a laptop.
