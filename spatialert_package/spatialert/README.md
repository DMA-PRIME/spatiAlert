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
app, and openable from the **Start Here** and **Help** tabs once the app is
running) — this README only covers installation and launch.

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

In the RStudio Console, run:

```r
library(spatialert)
hotspot_app()
```

The spatiAlert interface will open in your web browser. You can close it at any
time by closing the browser tab and pressing **Ctrl+C** (Windows/Linux) or
**Cmd+C** (Mac) in the RStudio Console.

An internet connection is needed the first time you load a geography — the
app downloads US Census boundary files automatically and caches them for
offline use afterwards.

---

## Getting help

- **In-app help**: Once the app is running, see the **Start Here** and **Help**
  tabs, and **spatiAlert_Getting_Started.pdf**, for data requirements, geography
  and weights choices, and troubleshooting.
- **Bug reports**: [GitHub Issues](https://github.com/DMA-PRIME/spatiAlert/issues)
- **Contact**: Emily Serman, Ph.D. — eserman@clemson.edu

## Citation

If you use spatiAlert in a publication or report, please cite:

> Serman, E.A., Witrick, B., & Rennert, L. (2025). spatialert: Interactive Spatial
> Hotspot Analysis for Public Health. R package version 0.1.0. https://github.com/DMA-PRIME/spatiAlert
