project:
  type: book

book:
	title: "LiDAR Forestry Applications"
  author: 
    - name: Seamus Murphy
    	url: https://scholar.google.com/citations?hl=en&user=jDGq9I4AAAAJ
    	orcid: 0000-0002-1792-0351 
    	affiliations:
    		- name: Seamus Murphy
          affiliation-url: https://cabinworks.ca/project/tactical-and-spatial-planning/
          address: Stewart St, 
          city: Comox, BC
          country: Canada
          postal-code: V9M 2M8
    	email: seamusrobertmurphy@gmail.com
  date: today
  favicon: ./assets/PNG/favicon.png
  open-graph: true # https://quarto.org/docs/websites/website-tools.html#open-graph
  page-footer:
    left: "[Source](https://github.com/seamusrobertmurphy/lidar-forestry)"
  navbar:
    title: "LiDAR Forestry Tools"
    search: true
    right:
      - text: "Home"
        href: index.qmd
      - text: "Point Clouds"
        href: point-cloud/index.qmd
      - text: "Stem Detection"
        href: stem-detect/index.qmd
      - text: "Canopy Height"
        href: canopy-height/index.qmd
      - text: "Aboveground Biomass"
        href: biomass-model/index.qmd

        
    tools:
      - icon: github
        menu:
          - text: Source Code
            href: https://github.com/seamusrobertmurphy/lidar-forestry
          - text: Report a Bug
            href: https://github.com/seamusrobertmurphy/lidar-forestry/issues
          - text: Repo Collection
            href: https://github.com/seamusrobertmurphy/
      - icon: linkedin
        href: https://linkedin.com/in/seamusrobertmurphy

  sidebar:
      pinned: true
      style: "docked"
      collapse-level: 1
      contents:
      - text: "Home"
        href: index.qmd
      - text: "Point Clouds"
        href: point-cloud/index.qmd
      - text: "Stem Detection"
        href: stem-detect/index.qmd
      - text: "Canopy Height"
        href: canopy-height/index.qmd
      - text: "Aboveground Biomass"
        href: biomass-model/index.qmd
      - section: "About"
        contents:
          - text: "Bio"
            href: about.qmd
          - text: "Teaching"
            href: teaching/index.qmd

format:
  html:
    theme: [cosmo, styles.scss]
engine: knitr






    
  navbar:
    title: "LiDAR Forestry Tools"
    search: true
    right:
      - text: "Home"
        href: index.qmd
      - text: "Point Clouds"
        href: point-cloud/index.qmd
      - text: "Stem Detection"
        href: stem-detect/index.qmd
      - text: "Canopy Height"
        href: canopy-height/index.qmd
      - text: "Aboveground Biomass"
        href: biomass-model/index.qmd
        
    tools:
      - icon: github
        menu:
          - text: Source Code
            href: https://github.com/seamusrobertmurphy/lidar-forestry
          - text: Report a Bug
            href: https://github.com/seamusrobertmurphy/lidar-forestry/issues
          - text: Repo Collection
            href: https://github.com/seamusrobertmurphy/
      - icon: linkedin
        href: https://linkedin.com/in/seamusrobertmurphy
  sidebar:
    pinned: true
    style: "docked"
    collapse-level: 1
    contents:
      - text: "Home"
        href: index.qmd
      - text: "Point Clouds"
        href: point-cloud/index.qmd
      - text: "Stem Detection"
        href: stem-detect/index.qmd
      - text: "Canopy Height"
        href: canopy-height/index.qmd
      - text: "Aboveground Biomass"
        href: biomass-model/index.qmd
      - section: "About"
        contents:
          - text: "Resume"
            href: about.qmd
          - text: "Learning Resources"
            href: learning/index.qmd

format:
  html:
    theme: [cosmo, styles.scss]
engine: knitr





/* Code styling variables */
//$code-block-border-left: #2d1283;

/* Colors */
$body-bg: #ffffff;
$body-color: rgb(55, 58, 60);
$link-color: rgb(39, 128, 227);


/* Code Chunk */
$code-font-size: 11px;
//$code-block-border-left: #2d1283;
//$code-block-bg: #160941;

/*-- scss:defaults --*/

/* =================================== */
/* BRAND COLOR VARIABLES               */
/* =================================== */
$brand-blue: #2563eb;
$brand-sage-green: #10b981;
$brand-slate: #64748b;

/* =================================== */
/* BASE COLORS                         */
/* =================================== */
$body-bg: #ffffff;
$body-color: rgb(55, 58, 60);
$link-color: rgb(39, 128, 227);

/* =================================== */
/* TYPOGRAPHY VARIABLES                */
/* =================================== */
$font-family-sans-serif: "Source Sans Pro", -apple-system, "system-ui", "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";

/* Root font sizes */
$font-size-root: 14px;        /* Smaller base from master */
$toc-font-size: 11px;         /* Smaller TOC from master */

/* Code styling */
$code-font-size: 10px;        /* Smaller code from master */


/*-- scss:rules --*/

/* =================================== */
/* GENERAL TYPOGRAPHY                  */
/* =================================== */

/* All headings: unified size and styling */
h1, h2, h3, h4, h5, h6 {
  font-family: "Open Sans", sans-serif !important;
  font-weight: 600 !important;
  font-size: 1.2rem !important;        /* Unified size for all headings */
  line-height: 1.3 !important;
  margin-top: 1.5rem !important;
  margin-bottom: 0.75rem !important;
  color: $brand-sage-green !important;  /* Sage green for all headings */
}

/* Page title (h1.title) - larger and distinctive */
h1.title {
  color: $brand-sage-green !important;
  font-size: 1.8rem !important;         /* Larger for main page title */
  margin-bottom: 1rem !important;
}

/* Section headings - color variations for hierarchy */
h2 {
  color: $brand-blue !important;        /* Blue for h2 */
  font-size: 1.3rem !important;         /* Slightly larger */
  margin-top: 2rem !important;
  margin-bottom: 1rem !important;
}

h3 {
  color: $brand-sage-green !important;  /* Sage for h3 */
  font-size: 1.2rem !important;
  margin-top: 1.5rem !important;
  margin-bottom: 0.75rem !important;
}

/* h4-h6 inherit the base styling defined above */

/* =================================== */
/* LINK STYLING                        */
/* =================================== */
a {
  color: $link-color;
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

/* =================================== */
/* BODY TEXT                           */
/* =================================== */
body {
  font-size: $font-size-root;
  line-height: 1.6;
}

p {
  line-height: 1.6;
  margin-bottom: 1rem;
}

/* =================================== */
/* CODE BLOCKS                         */
/* =================================== */
code {
  font-size: $code-font-size;
}

pre code {
  font-size: $code-font-size;
  line-height: 1.4;
}

/* =================================== */
/* TABLE OF CONTENTS                   */
/* =================================== */
.sidebar nav[role="doc-toc"] {
  font-size: $toc-font-size;
}

/* =================================== */
/* RESPONSIVE TYPOGRAPHY               */
/* =================================== */
@media (max-width: 768px) {
  h1.title {
    font-size: 1.5rem !important;
  }
  
  h2 {
    font-size: 1.2rem !important;
  }
  
  h3 {
    font-size: 1.1rem !important;
  }
  
  body {
    font-size: 13px;
  }
}















---
title: "LiDAR Forestry"
date: 2022-02-24
author: 
  - name: Seamus Murphy
    orcid: 0000-0002-1792-0351 
    email: seamusrobertmurphy@gmail.com
abstract: > 
  This ebook includes documents preparatory steps required to derive digital surface 
  models, ground segmentation, and canopy layers models from a raw unprocessed, 
  point-cloud dataset of `xyz` aerial laser scanning points as it is first 
  received from the data providers. Data processing operations was demonstrated 
  using the open-source `lidR` package [@lidR]."
keywords:
  - LiDAR processing
  - Point-Cloud Data
  - Ground Segmentation
format: 
  html:
    toc: true
    toc-location: right
    toc-title: "**Contents**"
    toc-depth: 5
    toc-expand: 4
    theme: [minimal, styles.scss]
    embed-resources: true
highlight-style: github
df-print: kable
bibliography: references.bib
engine: knitr
---

------------------------------------------------------------------------

```{r setup}
#| warning: false
#| message: false
#| error: false
#| echo: false


pacman::p_load(
  "academictwitteR", "academicons", "AcademicThemes",
  "curl", 
  "dplyr",
	"fontawesome",
  "htmltools", "httr2",
  "janitor",
  "kableExtra", "knitr",
  "lidR", "lutz", 
  "openxlsx",
  "PROJ",
  "raster", "rasterVis", "reproj",
  "sf",
  "tinytex", "tmap", "tmaptools", "terra",
  "useful", "usethis", 
  "webshot", "webshot2", "weathercan")

knitr::opts_chunk$set(
  echo = TRUE, 
  message = FALSE, 
  warning = FALSE,
  error = TRUE, 
  comment = NA, 
  tidy.opts = list(
    width.cutoff = 60)
  ) 
#knit_hooks$set(webgl = hook_webgl)
#knit_hooks$set(rgl.static = hook_rgl)
sf::sf_use_s2(use_s2 = FALSE)

options(htmltools.dir.version = FALSE, 
        htmltools.preserve.raw = FALSE,
				tinytex.verbose = TRUE
				)

tmap_options(max.raster = c(plot = 9500000, view = 10000000)) # allows large raster rendering
```
