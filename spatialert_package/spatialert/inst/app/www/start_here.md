# Welcome to spatiAlert

> **New here?** See **spatiAlert_Getting_Started.pdf**, included alongside
> this app in your download, for worked examples, a full walkthrough of the
> weights/statistic choices, diagrams of how spatial weights work, and
> answers to common questions — or see the **About** tab for app info,
> citations, and contact details.

### The spatial analysis, in one paragraph

spatiAlert uses the **Getis-Ord Gi\*** statistic to find hotspots: for each
area, it compares the value there and in its "neighbors" to the study area
as a whole, and flags areas where values are significantly higher
(hotspot) or lower (coldspot) than expected. "Neighbors" can be defined by
shared borders (queen/rook contiguity) or by nearest centroids (KNN) — see
the Analysis tab and the Getting Started PDF for guidance on which to use
and why KNN with a multi-k consensus check is the default here.

### The 3-step workflow

1. **Data & Geography** — Upload your data, tell the app what you want to
   analyze (a count or a rate), and choose the geography to map it to
   (census tract, county, ZIP, or your own shapefile).
2. **Analysis** — Choose spatial weights and run the Gi\* hotspot
   statistic. Click **"Use defaults\*"** if you want to replicate the
   published analysis exactly.
3. **Results & Export** — View the map and table, download a CSV, generate
   a Word report, or copy ready-made methods text for your own manuscript.

### What kind of data can spatiAlert handle?

| | Mode A — School / facility level | Mode B — Pre-aggregated |
|---|---|---|
| **One row per...** | school or facility | geographic unit (tract, county, ZIP) |
| **Needs** | latitude/longitude columns | a geographic ID column (GEOID, FIPS, ZCTA) |
| **The app...** | spatially joins each facility to a geography, then aggregates | uses your rows directly, matched by ID |

Either way, you'll tell the app **what you want to analyze**:

- **Count of undervaccinated students** — best for understanding absolute
  burden and prioritizing resources
- **Undervaccination rate** — best for comparing risk across areas of very
  different sizes
- **Vaccination rate** — same idea, framed the other direction

...and **how that's recorded in your file** — either as a column you
already have, or calculated from an eligible-population column plus a
vaccinated count or rate.

---

This app was built to accompany the analysis published as a research
letter in *NEJM*:

> **Clusters of Concern — Spatial Link between Childhood Undervaccination and
> Measles Outbreaks in South Carolina.** Published May 20, 2026.
> *N Engl J Med* 2026;394:2479–2481. DOI:
> [10.1056/NEJMc2604004](https://www.nejm.org/doi/full/10.1056/NEJMc2604004)
