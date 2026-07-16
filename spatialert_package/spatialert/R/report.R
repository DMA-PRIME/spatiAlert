#' Generate a hotspot analysis report
#'
#' @description
#' Produces a Word (.docx) report summarising the hotspot analysis, including
#' a methods paragraph, summary table, and map figure. Suitable for sharing
#' with leadership or including in public health communications.
#'
#' @param geo An `sf` object returned by [compute_gi_star()] or
#'   [compute_local_moran()], containing hotspot classification columns.
#' @param title Character. Report title. Default `"Spatial Hotspot Analysis"`.
#' @param location Character. Description of the study area (e.g. `"South Carolina"`).
#' @param variable_label Character. Human-readable label for the variable analysed
#'   (e.g. `"Vaccination coverage rate"`).
#' @param map_png Character or NULL. Path to a PNG of the hotspot map to embed.
#' @param output_path Character. Where to save the .docx file.
#'
#' @return Invisibly returns `output_path`. Called for its side effect.
#' @export
generate_report <- function(geo, title = "Spatial Hotspot Analysis",
                            location = "",
                            variable_label = "the variable of interest",
                            map_png = NULL,
                            output_path = tempfile(fileext = ".docx")) {

  method     <- attr(geo, "spatialert_method")     %||% "gi_star"
  correction <- attr(geo, "spatialert_correction") %||% "fdr"
  alpha      <- attr(geo, "spatialert_alpha")      %||% 0.05
  geo_level  <- attr(geo, "spatialert_geo_level")  %||% "areal unit"

  # Summary counts
  if (method == "gi_star") {
    cls_col <- "hotspot_class"
    n_hot  <- sum(geo[[cls_col]] == "Hotspot",        na.rm = TRUE)
    n_cold <- sum(geo[[cls_col]] == "Coldspot",       na.rm = TRUE)
    n_ns   <- sum(geo[[cls_col]] == "Not significant",na.rm = TRUE)
  } else {
    cls_col <- "lisa_class"
    n_hot  <- sum(geo[[cls_col]] == "High-High", na.rm = TRUE)
    n_cold <- sum(geo[[cls_col]] == "Low-Low",   na.rm = TRUE)
    n_ns   <- sum(!geo[[cls_col]] %in% c("High-High","Low-Low"), na.rm = TRUE)
  }

  correction_label <- switch(correction,
    fdr        = "false discovery rate (Benjamini-Hochberg)",
    bonferroni = "Bonferroni",
    none       = "no"
  )

  method_label <- switch(method,
    gi_star      = "Getis-Ord Gi*",
    local_moran  = "Local Moran's I (LISA)"
  )

  geo_label <- switch(geo_level,
    county        = "county",
    tract         = "census tract",
    `block group` = "census block group",
    geo_level
  )

  methods_text <- glue::glue(
    "A {method_label} spatial analysis was conducted to identify geographic ",
    "clusters of {variable_label}{if (nchar(location) > 0) paste0(' in ', location) else ''}. ",
    "Analyses were performed at the {geo_label} level. Spatial weights were ",
    "constructed using queen contiguity. Statistical significance was assessed ",
    "at \u03b1 = {alpha} with {correction_label} correction for multiple comparisons. ",
    "Analysis was conducted using the spatialert R package."
  )

  doc <- officer::read_docx()

  doc <- officer::body_add_par(doc, title, style = "heading 1")
  doc <- officer::body_add_par(doc, format(Sys.Date(), "%B %d, %Y"), style = "Normal")
  doc <- officer::body_add_par(doc, " ", style = "Normal")

  doc <- officer::body_add_par(doc, "Summary", style = "heading 2")
  doc <- officer::body_add_par(doc,
    glue::glue(
      "The analysis identified {n_hot} hotspot {geo_label}(s) with significantly ",
      "elevated {variable_label}, {n_cold} coldspot {geo_label}(s) with significantly ",
      "lower values, and {n_ns} {geo_label}(s) that were not statistically significant."
    ),
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, " ", style = "Normal")

  # Embed map if provided
  if (!is.null(map_png) && file.exists(map_png)) {
    doc <- officer::body_add_par(doc, "Hotspot Map", style = "heading 2")
    doc <- officer::body_add_img(doc, src = map_png, width = 6, height = 4.5)
    doc <- officer::body_add_par(doc, " ", style = "Normal")
  }

  # Summary table
  doc <- officer::body_add_par(doc, "Results by Area", style = "heading 2")

  tbl_data <- geo |>
    sf::st_drop_geometry() |>
    dplyr::select(
      dplyr::any_of(c("GEOID", "NAME", "NAMELSAD",
                      "gi_star", "gi_pvalue_adj",
                      "local_i", "li_pvalue_adj",
                      cls_col))
    ) |>
    dplyr::arrange(dplyr::desc(
      if ("gi_star" %in% names(geo)) abs(geo$gi_star[seq_len(nrow(geo))]) else 1
    )) |>
    as.data.frame()

  ft <- flextable::flextable(head(tbl_data, 30)) |>
    flextable::autofit() |>
    flextable::theme_vanilla()

  doc <- flextable::body_add_flextable(doc, ft)
  doc <- officer::body_add_par(doc, " ", style = "Normal")

  doc <- officer::body_add_par(doc, "Methods", style = "heading 2")
  doc <- officer::body_add_par(doc, methods_text, style = "Normal")

  print(doc, target = output_path)
  invisible(output_path)
}

# Null coalescing operator (avoid importing rlang just for this)
`%||%` <- function(x, y) if (!is.null(x)) x else y
