#' Summarize neighbor connectivity for queen and rook contiguity
#'
#' @description
#' Computes queen and rook contiguity neighbor lists for a geography and
#' summarizes the number of neighbors per unit (mean, median, range) along
#' with how many units have zero neighbors ("islands"). This is a diagnostic
#' step, independent of whatever weights style is later chosen for analysis:
#' it tells the user whether contiguity-based weights (queen/rook) are viable
#' for their geography, or whether they'll need KNN to force connectivity.
#'
#' @param geo An `sf` object with polygon geometry.
#' @param snap Numeric. Tolerance for detecting shared boundaries. Default 1e-7.
#'
#' @return A list with `queen` and `rook` elements, each containing `mean`,
#'   `median`, `min`, `max`, `n_zero` (count of units with 0 neighbors), and
#'   `zero_ids` (GEOIDs, if available, of zero-neighbor units).
#' @export
summarize_neighbors <- function(geo, snap = 1e-7) {
  summarize_one <- function(nb, geo) {
    counts <- spdep::card(nb)
    zero_idx <- which(counts == 0)
    list(
      mean     = mean(counts),
      median   = stats::median(counts),
      min      = min(counts),
      max      = max(counts),
      n_zero   = length(zero_idx),
      zero_ids = if (length(zero_idx) > 0 && "GEOID" %in% names(geo)) {
        geo$GEOID[zero_idx]
      } else {
        character(0)
      }
    )
  }

  nb_queen <- spdep::poly2nb(geo, queen = TRUE,  snap = snap)
  nb_rook  <- spdep::poly2nb(geo, queen = FALSE, snap = snap)

  list(
    queen = summarize_one(nb_queen, geo),
    rook  = summarize_one(nb_rook,  geo)
  )
}


#' Build a local azimuthal-equidistant CRS centered on a geography
#'
#' @description
#' KNN neighbor selection needs accurate straight-line distances between
#' centroids. Doing this directly on unprojected lon/lat (WGS84, EPSG:4326)
#' coordinates is not equivalent to true planar distance, and can select a
#' different k-th nearest neighbor than a projected calculation would,
#' especially among near-tied centroids. Rather than hardcoding a
#' state-specific projected CRS (which wouldn't generalize to every US
#' state/territory this app supports), this builds an azimuthal-equidistant
#' projection centered on the geography's own bounding box, giving accurate
#' local distances for whatever area is currently loaded.
#'
#' @param geo An `sf` object with polygon geometry.
#' @return A PROJ4 string usable with `sf::st_transform()`.
#' @keywords internal
local_projected_crs <- function(geo) {
  bb     <- sf::st_bbox(sf::st_transform(geo, 4326))
  center <- c(mean(c(bb[["xmin"]], bb[["xmax"]])), mean(c(bb[["ymin"]], bb[["ymax"]])))
  sprintf(
    "+proj=aeqd +lat_0=%f +lon_0=%f +datum=WGS84 +units=m +no_defs",
    center[2], center[1]
  )
}


#' Build a spatial weights matrix
#'
#' @param geo An `sf` object with polygon geometry.
#' @param style Character. Weights style: `"queen"`, `"rook"`, or `"knn"`.
#' @param k Integer. Number of nearest neighbours (only used when `style = "knn"`).
#' @param weights_type Character. `"B"` for binary or `"W"` for row-standardized.
#' @param snap Numeric. Tolerance for detecting shared boundaries. Default 1e-7.
#'
#' @return A `listw` object from the `spdep` package.
#' @export
spatialert_build_weights <- function(geo, style = "queen", k = 5,
                          weights_type = "B", snap = 1e-7) {
  style <- match.arg(style, c("queen", "rook", "knn"))

  nb <- switch(style,
    queen = spdep::poly2nb(geo, queen = TRUE,  snap = snap),
    rook  = spdep::poly2nb(geo, queen = FALSE, snap = snap),
    knn   = {
      # Reproject to a locally accurate planar CRS before taking centroids/
      # distances, rather than computing KNN on raw WGS84 degrees. Queen/rook
      # (above) don't need this — shared-border detection is topological, not
      # distance-based.
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

  spdep::nb2listw(nb, style = weights_type, zero.policy = TRUE)
}


#' Compute Getis-Ord Gi* statistic
#'
#' @description
#' Computes the Getis-Ord Gi* local statistic using analytical p-values,
#' with optional multiple comparisons correction.
#'
#' @param geo An `sf` object with polygon geometry and a numeric variable column.
#' @param var Character. Name of the numeric variable column to analyse.
#' @param weights A `listw` object from [spatialert_build_weights()].
#' @param correction Character. Multiple comparisons correction:
#'   `"none"`, `"fdr"` (Benjamini-Hochberg), or `"bonferroni"`.
#' @param alpha Numeric. Significance threshold. Default 0.05.
#' @param include_self Logical. If `TRUE` (default) each area is counted in its own
#'   neighbourhood, which is what makes the statistic Gi* rather than Gi.
#'   `spdep::localG()` only does this when the neighbour list includes the area
#'   itself, so it is added here via `spdep::include.self()`.
#'
#' @return The input `sf` object with additional columns:
#'   - `gi_star`: Gi* z-score
#'   - `gi_pvalue`: raw two-tailed p-value
#'   - `gi_pvalue_adj`: adjusted p-value
#'   - `hotspot_class`: factor — Hotspot, Coldspot, or Not significant
#'
#' @export
compute_gi_star <- function(geo, var, weights,
                            correction = "fdr", alpha = 0.05,
                            include_self = TRUE) {
  correction <- match.arg(correction, c("none", "fdr", "bonferroni"))

  x <- geo[[var]]
  if (!is.numeric(x)) stop("`var` must be a numeric column.", call. = FALSE)

  # Replace NAs with 0 (tracts with no schools get 0 undervaccinated)
  if (any(is.na(x))) {
    warning("Missing values in `", var, "` replaced with 0.", call. = FALSE)
    x[is.na(x)] <- 0
  }

  # Gi* (as opposed to Gi) counts the area's own value in its neighbourhood.
  # spdep::localG() only does that when the neighbour list already includes
  # the area itself, so add it here (keeping the same weights style, B or W).
  if (isTRUE(include_self) && !isTRUE(attr(weights$neighbours, "self.included"))) {
    weights <- spdep::nb2listw(
      spdep::include.self(weights$neighbours),
      style = weights$style, zero.policy = TRUE
    )
  }

  # Compute Gi* using localG (analytical p-values)
  gi_result <- spdep::localG(x, listw = weights, zero.policy = TRUE)
  gi_z      <- as.numeric(gi_result)

  # Analytical two-tailed p-value from z-score
  gi_p <- 2 * pnorm(abs(gi_z), lower.tail = FALSE)

  # Apply multiple comparisons correction
  gi_p_adj <- switch(correction,
    none       = gi_p,
    fdr        = p.adjust(gi_p, method = "BH"),
    bonferroni = p.adjust(gi_p, method = "bonferroni")
  )

  geo$gi_star       <- gi_z
  geo$gi_pvalue     <- gi_p
  geo$gi_pvalue_adj <- gi_p_adj
  geo$hotspot_class <- classify_hotspots(gi_z, gi_p_adj, alpha)

  attr(geo, "spatialert_method")     <- "gi_star"
  attr(geo, "spatialert_var")        <- var
  attr(geo, "spatialert_correction") <- correction
  attr(geo, "spatialert_alpha")      <- alpha
  attr(geo, "spatialert_include_self") <- isTRUE(include_self)

  geo
}


#' Compute Getis-Ord Gi* across multiple KNN specifications (consensus)
#'
#' @description
#' Runs Gi* separately for each value of k in `k_values` (KNN weights only —
#' contiguity-based weights like queen/rook don't have a "specification" to
#' sweep over, since they're determined by shared borders rather than a
#' chosen neighbor count). A unit is classified as a robust hotspot/coldspot
#' if it is significant in at least `min_specs` of the specifications. This
#' mirrors a multi-k robustness check: run Gi* at several k, then keep only
#' hotspots that are consistent across most of them.
#'
#' @param geo An `sf` object with polygon geometry and a numeric variable column.
#' @param var Character. Name of the numeric variable column to analyse.
#' @param k_values Integer vector. KNN values to run. Default `c(4,6,8,10,12)`.
#' @param weights_type Character. `"B"` for binary or `"W"` for row-standardized.
#' @param correction Character. Multiple comparisons correction, applied
#'   within each k-specification before tallying consensus.
#' @param alpha Numeric. Significance threshold. Default 0.05.
#' @param min_specs Integer. Minimum number of specifications a unit must be
#'   significant in (in the same direction) to be called a consensus
#'   hotspot/coldspot. Default 2.
#'
#' @return The input `sf` object with:
#'   - `gi_star`: mean Gi* z-score across all k specifications (for mapping/legend)
#'   - `n_hot_specs`, `n_cold_specs`: count of specifications classifying the
#'     unit as Hotspot / Coldspot
#'   - `n_specs`: total specifications run (`length(k_values)`)
#'   - `hotspot_class`: consensus classification — Hotspot, Coldspot, or Not significant
#'   - `spec_detail`: a list-column with the per-k class for full transparency
#' @export
compute_gi_star_consensus <- function(geo, var, k_values = c(4, 6, 8, 10, 12),
                                       weights_type = "B", correction = "fdr",
                                       alpha = 0.05, min_specs = 2) {
  if (min_specs > length(k_values)) {
    stop("`min_specs` cannot exceed the number of k values.", call. = FALSE)
  }

  n <- nrow(geo)
  z_mat     <- matrix(NA_real_, nrow = n, ncol = length(k_values))
  class_mat <- matrix(NA_character_, nrow = n, ncol = length(k_values))

  for (i in seq_along(k_values)) {
    w_i <- spatialert_build_weights(geo, style = "knn", k = k_values[i],
                                     weights_type = weights_type)
    res_i <- compute_gi_star(geo, var = var, weights = w_i,
                              correction = correction, alpha = alpha)
    z_mat[, i]     <- res_i$gi_star
    class_mat[, i] <- as.character(res_i$hotspot_class)
  }

  colnames(z_mat)     <- paste0("k", k_values)
  colnames(class_mat) <- paste0("k", k_values)

  n_hot_specs  <- rowSums(class_mat == "Hotspot",  na.rm = TRUE)
  n_cold_specs <- rowSums(class_mat == "Coldspot", na.rm = TRUE)

  hotspot_class <- dplyr::case_when(
    n_hot_specs  >= min_specs ~ "Hotspot",
    n_cold_specs >= min_specs ~ "Coldspot",
    TRUE                      ~ "Not significant"
  )

  geo$gi_star       <- rowMeans(z_mat, na.rm = TRUE)
  geo$n_hot_specs   <- n_hot_specs
  geo$n_cold_specs  <- n_cold_specs
  geo$n_specs       <- length(k_values)
  geo$hotspot_class <- factor(hotspot_class,
                               levels = c("Hotspot", "Coldspot", "Not significant"))

  attr(geo, "spatialert_method")      <- "gi_star_consensus"
  attr(geo, "spatialert_var")         <- var
  attr(geo, "spatialert_correction")  <- correction
  attr(geo, "spatialert_alpha")       <- alpha
  attr(geo, "spatialert_k_values")    <- k_values
  attr(geo, "spatialert_min_specs")   <- min_specs
  attr(geo, "spatialert_class_matrix") <- class_mat
  attr(geo, "spatialert_z_matrix")     <- z_mat

  geo
}


#' Compute Local Moran's I statistic
#'
#' @description
#' Computes Local Moran's I (LISA) using permutation-based p-values.
#'
#' @param geo An `sf` object with polygon geometry and a numeric variable column.
#' @param var Character. Name of the numeric variable column to analyse.
#' @param weights A `listw` object from [spatialert_build_weights()].
#' @param correction Character. Multiple comparisons correction.
#' @param alpha Numeric. Significance threshold. Default 0.05.
#' @param nsim Integer. Number of permutation simulations. Default 499.
#'
#' @return The input `sf` object with LISA result columns.
#' @export
compute_local_moran <- function(geo, var, weights,
                                correction = "fdr", alpha = 0.05,
                                nsim = 499) {
  correction <- match.arg(correction, c("none", "fdr", "bonferroni"))

  x <- geo[[var]]
  if (!is.numeric(x)) stop("`var` must be a numeric column.", call. = FALSE)
  if (any(is.na(x))) {
    warning("Missing values in `", var, "` replaced with 0.", call. = FALSE)
    x[is.na(x)] <- 0
  }

  result <- spdep::localmoran_perm(
    x, listw = weights, nsim = nsim,
    zero.policy = TRUE, alternative = "two.sided"
  )

  li_p <- result[, "Pr(z != E(Ii))"]

  li_p_adj <- switch(correction,
    none       = li_p,
    fdr        = p.adjust(li_p, method = "BH"),
    bonferroni = p.adjust(li_p, method = "bonferroni")
  )

  x_scaled   <- scale(x)[, 1]
  lag_scaled <- scale(spdep::lag.listw(weights, x))[, 1]
  sig        <- li_p_adj < alpha

  quadrant <- dplyr::case_when(
    !sig                               ~ "Not significant",
    x_scaled > 0 & lag_scaled > 0     ~ "High-High",
    x_scaled < 0 & lag_scaled < 0     ~ "Low-Low",
    x_scaled > 0 & lag_scaled < 0     ~ "High-Low",
    x_scaled < 0 & lag_scaled > 0     ~ "Low-High",
    TRUE                               ~ "Not significant"
  )

  geo$local_i       <- result[, "Ii"]
  geo$li_pvalue     <- li_p
  geo$li_pvalue_adj <- li_p_adj
  geo$lisa_class    <- factor(quadrant,
    levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not significant"))

  attr(geo, "spatialert_method")     <- "local_moran"
  attr(geo, "spatialert_var")        <- var
  attr(geo, "spatialert_correction") <- correction
  attr(geo, "spatialert_alpha")      <- alpha

  geo
}


#' Classify hotspots from Gi* z-scores and adjusted p-values
#'
#' @param z Numeric vector of Gi* z-scores.
#' @param p_adj Numeric vector of adjusted p-values.
#' @param alpha Numeric. Significance threshold.
#'
#' @return A factor with levels Hotspot, Coldspot, Not significant.
#' @export
classify_hotspots <- function(z, p_adj, alpha = 0.05) {
  result <- dplyr::case_when(
    p_adj < alpha & z > 0 ~ "Hotspot",
    p_adj < alpha & z < 0 ~ "Coldspot",
    TRUE                   ~ "Not significant"
  )
  factor(result, levels = c("Hotspot", "Coldspot", "Not significant"))
}


#' Global Getis-Ord General G test
#'
#' @description
#' Tests whether high values of `var` cluster together across the whole study
#' area (one-sided: clustering of high values). Uses the same neighbour
#' definition as the hotspot analysis but always with binary weights, which is
#' what the test is designed for. The area itself is not included (that is only
#' for the local Gi* statistic). For KNN consensus runs, pass `k_values` and the
#' middle value is used.
#'
#' @param geo An `sf` object with polygon geometry.
#' @param var Character. Numeric (non-negative) column to test.
#' @param style `"queen"`, `"rook"` or `"knn"`.
#' @param k Integer. Neighbours for `style = "knn"`.
#' @param k_values Optional vector of k values from a consensus run; the middle
#'   one is used and `k` is ignored.
#'
#' @return A list. `ran` is `TRUE` with `G`, `expected`, `z`, `p`, `style`, `k`,
#'   `label` and `n`; otherwise `ran` is `FALSE` with a plain-language `reason`.
#' @export
compute_global_g <- function(geo, var, style = "queen", k = 8, k_values = NULL) {
  x <- geo[[var]]
  if (!is.numeric(x)) return(list(ran = FALSE, reason = "the analysis variable is not numeric"))
  x[is.na(x)] <- 0
  if (any(x < 0)) {
    return(list(ran = FALSE,
                reason = "the Global G test requires non-negative values and this variable has negative values"))
  }
  if (!is.null(k_values) && length(k_values) > 0) {
    kv    <- sort(unique(k_values))
    k     <- kv[ceiling(length(kv) / 2)]
    style <- "knn"
  }
  out <- tryCatch({
    w <- suppressWarnings(
      spatialert_build_weights(geo, style = style, k = k, weights_type = "B")
    )
    r <- suppressWarnings(
      spdep::globalG.test(x, listw = w, zero.policy = TRUE, alternative = "greater")
    )
    list(
      ran      = TRUE,
      G        = unname(r$estimate[["Global G statistic"]]),
      expected = unname(r$estimate[["Expectation"]]),
      z        = as.numeric(r$statistic),
      p        = as.numeric(r$p.value),
      style    = style,
      k        = k,
      n        = length(x),
      label    = switch(style,
        queen = "queen contiguity",
        rook  = "rook contiguity",
        knn   = paste0("k-nearest neighbors (k = ", k, ")"))
    )
  }, error = function(e) {
    list(ran = FALSE, reason = paste("the test could not be computed:", conditionMessage(e)))
  })
  out
}
