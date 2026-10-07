# spatiAlert <img src="man/figures/logo.png" align="right" height="139" alt="" />

**spatiAlert** is an R package that provides a point-and-click interface for
spatial hotspot analysis of public health data. It is designed for public health
practitioners who want to identify geographic clusters of outcomes like
vaccination coverage, disease incidence, or screening rates — without needing
to write code.

All analysis runs **entirely on your local machine**. No data is ever uploaded
to an external server.

For a full walkthrough of data setup, geography/weights choices, and how to
interpret results, see **spatiAlert_Getting_Started.pdf** (included with the
app, and openable from the **Start Here** and **About** tabs once the app is
running) — this README only covers installation and launch.

## What's in the app

| Tab | What it does |
|---|---|
| **Data & Geography** | Upload your data (school/facility-level or pre-aggregated), choose what to analyze, and load a US Census geography or your own boundary file (shapefile, zip, or GeoJSON — for example school districts). |
| **Explore** | Map and summarize your data *before* any analysis: school locations, or areas colored by average vaccination rate, number of undervaccinated students, and more. Choose how the color classes are grouped, click rows in the ranked table to isolate areas on the map, and export printable maps (Word, PDF, or PNG). |
| **Analysis** | Choose spatial weights and run the Getis-Ord Gi\* hotspot analysis, or switch on **Use defaults** to use the published settings. A Global G test reports whether high values cluster across the whole study area. |
| **Results & Export** | Map and ranked table of significant areas, a **Schools of concern** list (individual schools in areas of concern below a vaccination threshold you choose), a CSV download, a Word report with plain-language explanations, and ready-made methods text. |
| **FAQ** | Plain-language answers about hotspots, z-scores, p-values, and how to read the results. |

---

## What's in this repository

```
spatiAlert/
├── DESCRIPTION                  Package metadata and dependencies
├── LICENSE                      MIT license
├── README.md                    This file
├── .gitignore                   Files Git should ignore
├── R/                           Package code (also usable in your own scripts)
│   ├── app.R                    hotspot_app() — launches the Shiny app
│   ├── spatialert.R             Package-level documentation
│   ├── geography.R              Loading Census geographies, joining data to areas
│   ├── analysis.R               Spatial weights, Gi*, Global G test, hotspot classes
│   └── report.R                 Word report generation
├── inst/
│   └── app/                     The Shiny app itself
│       ├── app.R                App layout (tabs) and map display
│       ├── modules/             One file per tab or major feature
│       │   ├── mod_upload.R         Data upload, column mapping, what to analyze
│       │   ├── mod_geography.R      Geography selection / boundary-file upload
│       │   ├── mod_explore.R        Explore tab: maps, ranked table, printable maps
│       │   ├── mod_analysis.R       Analysis settings, running the analysis
│       │   └── mod_results.R        Results tables, schools of concern, report, FAQ
│       └── www/                 Files shown inside the app
│           ├── start_here.md        "Start Here" tab text
│           └── help.md              "About" tab text (including the citation)
```

---

## Installation

### Step 1 — Install R

Download and install R from [https://cran.r-project.org](https://cran.r-project.org).

- **Windows**: Click "Download R for Windows" → "base" → download the installer
- **Mac**: Click "Download R for macOS" → download the `.pkg` file for your chip
  (Apple Silicon = "arm64"; older Intel Mac = "x86-64")
- Run the installer and accept the defaults

### Step 2 — Install RStudio (recommended)

RStudio gives you a user-friendly environment for running R. Download the free
Desktop version from [https://posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop).

Run the installer and accept the defaults.

### Step 3 — Install spatiAlert

Open RStudio. In the **Console** panel (bottom left), paste these two lines and
press Enter:

```r
install.packages("remotes")
remotes::install_github("DMA-PRIME/spatiAlert")
```

This will install spatiAlert and all of its dependencies. It may take a few
minutes the first time — this is normal.

### Step 4 — Launch the app

There are two ways to launch spatiAlert, depending on how you got it.

**If you installed the package from GitHub (Step 3)**, run this in the RStudio
Console:

```r
library(spatialert)
hotspot_app()
```

**If you downloaded the app as a zip file**, unzip it, open
`run_spatialert - user.R` in RStudio, set the `dir` variable near the top to the
folder that directly contains `spatialert_package`, and click **Source**. See
Section 1 of **spatiAlert_Getting_Started.pdf** for a worked example.

The spatiAlert interface will open in your web browser. You can close it at any
time by closing the browser tab and pressing **Ctrl+C** (Windows/Linux) or
**Cmd+C** (Mac) in the RStudio Console.

### Internet connection

- An internet connection is needed the first time you load a US Census
  geography — the app downloads the boundary files automatically and caches
  them for offline use afterwards.
- The background map on the on-screen maps (Esri) also needs a connection; if
  you are offline the maps still show your data on a blank background.
- The printable maps on the **Explore** tab (Word, PDF, PNG) do not use any
  map tiles, so they work offline.

### Data files and special characters

CSV files saved from Excel on Windows sometimes use an older text encoding
(for example, an en dash or curly apostrophe in a school name). spatiAlert
converts these to UTF-8 automatically when the file is loaded.

---

## Getting help

- **In-app help**: Once the app is running, see the **Start Here** and **About**
  tabs, and **spatiAlert_Getting_Started.pdf**, for data requirements, geography
  and weights choices, and troubleshooting.
- **Bug reports**: [GitHub Issues](https://github.com/DMA-PRIME/spatiAlert/issues)
- **Contact**: Emily Serman, Ph.D. — eserman@clemson.edu

## Citation

If you use spatiAlert in a publication or report, please cite:

> Serman, E.A., Witrick, B., & Rennert, L. (2026). spatialert: Interactive Spatial
> Hotspot Analysis for Public Health. R package version 0.1.0. https://github.com/DMA-PRIME/spatiAlert
