#' Fetch US census geography
#'
#' @param state Character. Two-letter state abbreviation (e.g. `"SC"`).
#' @param level Character. Geographic level: `"county"`, `"tract"`, or `"block group"`.
#' @param county Character or NULL. County name or FIPS code to subset to.
#' @param year Integer. Census year. Default 2024.
#'
#' @return An `sf` object with census geography and GEOID column.
#' @export
fetch_geography <- function(state, level = "tract", county = NULL, year = 2024) {
  level <- match.arg(level, c("county", "tract", "block group"))

  options(tigris_use_cache = TRUE)

  # NOTE: cb (cartographic boundary) vs full TIGER/Line geometry was tested
  # as a fix for coastal "island" tracts not showing up as 0-neighbor units,
  # but full-resolution geometry did not preserve them either — the
  # generalization hypothesis was wrong. Reverted to cb = TRUE (smaller,
  # faster download) since it wasn't buying anything. The actual cause is
  # more likely a tract-boundary VINTAGE mismatch (this fetches `year`,
  # which defaults to 2024 below — Census tract lines can be redrawn/
  # consolidated between vintages, e.g. sparse coastal tracts merged since
  # 2020). If reproducing a specific published analysis, confirm the year
  # of the original shapefile and pass it here, or use "Upload my own
  # shapefile" in the app instead of fetching from Census.
  geo <- switch(level,
    county        = tigris::counties(state = state, year = year, cb = TRUE),
    tract         = tigris::tracts(state = state, county = county,
                                   year = year, cb = TRUE),
    `block group` = tigris::block_groups(state = state, county = county,
                                         year = year, cb = TRUE)
  )

  n <- nrow(geo)
  if (n < 20) {
    warning(
      n, " geographic units found. Gi* results may have low statistical ",
      "power with fewer than ~20 units. Consider a larger area or coarser geography.",
      call. = FALSE
    )
  }

  # tigris returns rows in the Census TIGER file's native order (effectively
  # STATEFP/COUNTYFP/TRACTCE), which is not what we want downstream tables,
  # exports, and results to be sorted by. Sort explicitly by GEOID so output
  # order is consistent and independent of the source file's internal sort.
  if ("GEOID" %in% names(geo)) {
    geo <- geo[order(geo$GEOID), ]
  }

  sf::st_transform(geo, crs = 4326)
}


#' Validate and parse an uploaded data file
#'
#' @param path Character. File path to the uploaded file.
#' @param mode Character. Input mode: `"facility"` or `"aggregate"`.
#' @param id_col Character or NULL. Name of the geographic ID column (Mode B only).
#' @param value_col Character. Name of the numeric value column.
#' @param denom_col Character or NULL. Optional denominator column.
#'
#' @return A list with elements `data`, `warnings`, and `errors`.
#' @export
validate_upload <- function(path, mode = "aggregate",
                            id_col = NULL, value_col, denom_col = NULL) {
  mode   <- match.arg(mode, c("facility", "aggregate"))
  warns  <- character(0)
  errors <- character(0)

  ext <- tolower(tools::file_ext(path))
  df  <- tryCatch({
    switch(ext,
      csv  = readr::read_csv(path, show_col_types = FALSE),
      xlsx = readxl::read_xlsx(path),
      xls  = readxl::read_xls(path),
      {
        errors <- c(errors, glue::glue(
          "Unsupported file type '.{ext}'. Please upload a CSV or Excel file."
        ))
        return(list(data = NULL, warnings = warns, errors = errors))
      }
    )
  }, error = function(e) {
    errors <<- c(errors, paste("Could not read file:", conditionMessage(e)))
    NULL
  })

  if (is.null(df)) {
    return(list(data = NULL, warnings = warns, errors = errors))
  }

  # Check required columns
  needed <- c(value_col)
  if (!is.null(id_col))    needed <- c(needed, id_col)
  if (!is.null(denom_col)) needed <- c(needed, denom_col)

  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, glue::glue(
      "Column(s) not found: {paste(missing_cols, collapse = ', ')}. ",
      "Available columns: {paste(names(df), collapse = ', ')}"
    ))
    return(list(data = NULL, warnings = warns, errors = errors))
  }

  # Coerce value column
  df[[value_col]] <- suppressWarnings(as.numeric(df[[value_col]]))
  n_na <- sum(is.na(df[[value_col]]))
  if (n_na > 0) {
    warns <- c(warns, glue::glue(
      "{n_na} row(s) had non-numeric values in '{value_col}' and will be excluded."
    ))
    df <- df[!is.na(df[[value_col]]), ]
  }

  # Compute rate if denominator provided
  if (!is.null(denom_col)) {
    df[[denom_col]] <- suppressWarnings(as.numeric(df[[denom_col]]))
    zero_denom <- sum(df[[denom_col]] == 0, na.rm = TRUE)
    if (zero_denom > 0) {
      warns <- c(warns, glue::glue(
        "{zero_denom} row(s) have a zero denominator and will be excluded."
      ))
      df <- df[!is.na(df[[denom_col]]) & df[[denom_col]] > 0, ]
    }
    df$.computed_rate <- df[[value_col]] / df[[denom_col]]
  }

  list(data = df, warnings = warns, errors = errors)
}


#' Build a spatial weights matrix
#'
#' @param geo An `sf` object with polygon geometry.
#' @param style Character. Weights style: `"queen"`, `"rook"`, or `"knn"`.
#' @param k Integer. Number of nearest neighbours (only used when `style = "knn"`).
#' @param snap Numeric. Tolerance for detecting shared boundaries. Default 1e-7.
#'
#' @return A `listw` object from the `spdep` package.
#' @export
build_weights <- function(geo, style = "queen", k = 5, snap = 1e-7) {
  style <- match.arg(style, c("queen", "rook", "knn"))

  nb <- switch(style,
    queen = spdep::poly2nb(geo, queen = TRUE,  snap = snap),
    rook  = spdep::poly2nb(geo, queen = FALSE, snap = snap),
    knn   = {
      # See local_projected_crs() in analysis.R: KNN distances are computed
      # on a locally accurate planar projection rather than raw WGS84
      # degrees, so this matches the weights the Shiny app builds internally.
      geo_proj <- sf::st_transform(geo, local_projected_crs(geo))
      coords   <- sf::st_centroid(sf::st_geometry(geo_proj))
      spdep::knn2nb(spdep::knearneigh(coords, k = k))
    }
  )

  no_nb <- which(spdep::card(nb) == 0)
  if (length(no_nb) > 0) {
    warning(
      length(no_nb), " area(s) have no neighbours and will be excluded. ",
      "Consider using 'knn' weights to ensure connectivity.",
      call. = FALSE
    )
  }

  spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
}


#' Join facility-level data to areal units
#'
#' @param data Data frame with facility data including lat/lon columns.
#' @param geo An `sf` polygon object (the target geography).
#' @param value_col Character. Column name of the primary numeric value to
#'   aggregate (typically `.value`, matching whichever target the user chose).
#' @param lat_col Character. Name of latitude column. Default `"latitude"`.
#' @param lon_col Character. Name of longitude column. Default `"longitude"`.
#' @param eligible_col Character or NULL. Name of the eligible-population
#'   column, if available. When provided, rate columns are aggregated to the
#'   tract level as an eligible-weighted average rather than a simple mean
#'   (i.e. larger schools count proportionally more).
#'
#' @return A list with:
#'   - `geo`: an `sf` object matching `geo` with `.value`, `.undervax_count`,
#'     `.undervax_rate`, `.vax_rate`, and `.n_facilities` columns
#'   - `n_matched`: number of facilities successfully matched to a tract
#'   - `n_unmatched`: number of facilities that did not fall within any tract
#' @export
join_to_geography <- function(data, geo, value_col,
                              lat_col = "latitude", lon_col = "longitude",
                              eligible_col = NULL,
                              target_col = ".undervax_count") {

  # Drop rows with missing coordinates
  missing_coords <- is.na(data[[lon_col]]) | is.na(data[[lat_col]])
  if (any(missing_coords)) {
    warning(sum(missing_coords), " row(s) dropped due to missing coordinates.")
    data <- data[!missing_coords, ]
  }

  n_total <- nrow(data)

  # Convert to sf points
  pts <- sf::st_as_sf(
    data,
    coords = c(lon_col, lat_col),
    crs    = 4326,
    remove = FALSE
  )

  # Spatial join: points to polygons
  joined <- sf::st_join(pts, geo[, "GEOID"], join = sf::st_within)

  # Count unmatched points (no tract found)
  n_unmatched <- sum(is.na(joined$GEOID))
  n_matched   <- n_total - n_unmatched

  if (n_unmatched > 0) {
    warning(
      n_unmatched, " facility/school location(s) did not fall within any ",
      "census tract and will be excluded from the analysis.",
      call. = FALSE
    )
  }

  has_eligible <- !is.null(eligible_col) && eligible_col %in% names(joined)

  # weighted.mean() propagates NA from the weights even with na.rm = TRUE
  # (na.rm only drops NAs in x). Since weighting columns can legitimately have
  # missing/zero values for some facilities, drop those pairs explicitly and
  # fall back to a simple mean if nothing usable remains, rather than letting
  # one bad weight silently NA out an entire area's rate.
  safe_weighted_mean <- function(x, w) {
    keep <- !is.na(x) & !is.na(w) & w > 0
    if (!any(keep)) return(mean(x, na.rm = TRUE))
    stats::weighted.mean(x[keep], w[keep])
  }

  # Summarise by GEOID.
  # .undervax_count sums directly (counts are additive across facilities).
  # Rate columns (.undervax_rate / .vax_rate) are aggregated as an
  # eligible-weighted average when eligible population is available
  # (so larger schools count proportionally more), otherwise a simple mean.
  jd <- joined |> sf::st_drop_geometry() |> dplyr::filter(!is.na(.data$GEOID))

  if (has_eligible) {
    agg <- jd |>
      dplyr::group_by(.data$GEOID) |>
      dplyr::summarise(
        .undervax_count = sum(.data$.undervax_count, na.rm = TRUE),
        .vax_rate       = safe_weighted_mean(.data$.vax_rate, .data[[eligible_col]]),
        .undervax_rate  = safe_weighted_mean(.data$.undervax_rate, .data[[eligible_col]]),
        .n_facilities   = dplyr::n(),
        .groups         = "drop"
      )
  } else {
    agg <- jd |>
      dplyr::group_by(.data$GEOID) |>
      dplyr::summarise(
        .undervax_count = sum(.data$.undervax_count, na.rm = TRUE),
        .vax_rate       = mean(.data$.vax_rate, na.rm = TRUE),
        .undervax_rate  = mean(.data$.undervax_rate, na.rm = TRUE),
        .n_facilities   = dplyr::n(),
        .groups         = "drop"
      )
  }

  # Left join back to geo so all polygons retained (NAs for empty tracts)
  result_geo <- dplyr::left_join(geo, agg, by = "GEOID")
  result_geo$.undervax_count[is.na(result_geo$.undervax_count)] <- 0
  # Rate columns stay NA for empty tracts (no facilities = no rate to report);
  # compute_gi_star() replaces NAs with 0 at analysis time with a warning.

  # .value is set to whichever column matches the user's chosen analyze target
  result_geo$.value <- result_geo[[target_col]]

  list(
    geo         = result_geo,
    n_matched   = n_matched,
    n_unmatched = n_unmatched,
    weighted    = has_eligible,
    # School-level rows that fell inside an area, each carrying its area ID
    # (GEOID) and all original columns. Used for the "schools of concern"
    # table and the school layer on the map.
    schools     = as.data.frame(jd)
  )
}

`%||%` <- function(x, y) if (!is.null(x)) x else y
