# mod_explore.R — Explore tab: maps and summaries of the uploaded data,
# available BEFORE (and independent of) the hotspot analysis.
#
# Interactive maps (leaflet) on screen, plus printable maps (Word / PDF / PNG)
# drawn with ggplot2 and no street tiles, so they never depend on a map service.

# ── Shared constants ──────────────────────────────────────────────────────────

EXPLORE_VAX_COLORS <- c(
  "<85%"     = "#d73027",
  "85-89.9%" = "#fc8d00",
  "90-94.9%" = "#fee600",
  "≥95%" = "#1aaf00"
)
EXPLORE_NO_DATA  <- "#d9d9d9"
EXPLORE_PAL_RED  <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")
EXPLORE_PAL_BLUE <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")

# Measures that can be mapped by area. `mult` converts a 0-1 rate to a percent.
EXPLORE_MEASURES <- list(
  vax_rate       = list(col = ".vax_rate",       label = "Average vaccination rate (%)",
                        mult = 100, pal = NULL),
  undervax_rate  = list(col = ".undervax_rate",  label = "Undervaccination rate (%)",
                        mult = 100, pal = EXPLORE_PAL_RED),
  undervax_count = list(col = ".undervax_count", label = "Estimated undervaccinated students",
                        mult = 1,   pal = EXPLORE_PAL_RED),
  n_schools      = list(col = ".n_schools",      label = "Number of schools",
                        mult = 1,   pal = EXPLORE_PAL_BLUE),
  enrolled       = list(col = ".enrolled",       label = "Total enrolled students",
                        mult = 1,   pal = EXPLORE_PAL_BLUE)
)


# ── Pure helpers (no Shiny) ───────────────────────────────────────────────────

explore_vax_bin <- function(rate01) {
  cut(rate01 * 100, breaks = c(-Inf, 85, 90, 95, Inf),
      labels = names(EXPLORE_VAX_COLORS), right = FALSE)
}

# How the non-vaccination-rate measures can be grouped into color classes.
EXPLORE_BIN_METHODS <- c(
  "Quantiles (same number of areas in each class)" = "quantile",
  "Round-number breaks (better for skewed data)"   = "round",
  "Natural breaks (Jenks)"                         = "jenks",
  "Equal intervals"                                = "equal",
  "Custom thresholds"                              = "custom"
)

keep_val <- function(val, choices, default) {
  if (!is.null(val) && val %in% choices) val else default
}

# Parse "10, 25, 50" into sorted unique numbers (NULL if nothing usable).
explore_parse_breaks <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
  b <- suppressWarnings(as.numeric(strsplit(gsub("[%[:space:]]", "", txt), "[,;]+")[[1]]))
  b <- sort(unique(b[is.finite(b)]))
  if (length(b) == 0) NULL else b
}

# Round-number breaks for skewed data: class edges sit at round values (e.g. 5, 10,
# 20, 30, 50), spaced so the top classes are narrow and the many low values still
# get several classes.
explore_round_breaks <- function(nz, n) {
  pos <- nz[nz > 0]
  if (length(pos) < 3) return(NULL)
  probs  <- 1 - (1 - seq_len(n - 1) / n)^1.5
  q      <- as.numeric(stats::quantile(pos, probs, names = FALSE))
  series <- sort(c(outer(c(1, 1.5, 2, 3, 4, 5, 7.5), 10^((floor(log10(min(pos))) - 1):(ceiling(log10(max(pos))) + 1)))))
  snap   <- vapply(q, function(z) series[which.min(abs(log10(series) - log10(z)))], numeric(1))
  snap   <- snap[snap > min(nz) & snap < max(nz)]
  sort(unique(c(min(nz), snap, max(nz))))
}

# Groups values into classes and assigns colors. Vaccination rate uses the same
# fixed bins as the school points; everything else uses `bins`:
# list(method, n, custom = list(<measure key> = numeric thresholds)).
explore_classify <- function(x, key, bins = NULL) {
  m <- EXPLORE_MEASURES[[key]]
  if (identical(key, "vax_rate")) {
    return(list(cls = explore_vax_bin(x), colors = EXPLORE_VAX_COLORS,
                title = "Average vaccination rate"))
  }
  v  <- x * m$mult
  nz <- v[!is.na(v)]
  if (length(nz) == 0) {
    return(list(cls = factor(rep(NA_character_, length(v))),
                colors = character(0), title = m$label))
  }
  method <- bins$method %||% "quantile"
  n      <- max(2L, min(8L, as.integer(round(bins$n %||% 5))))
  custom <- bins$custom[[key]]
  if (identical(method, "custom") && is.null(custom)) method <- "quantile"

  fmt <- function(b, keep_dec = FALSE) {
    if (m$mult == 100) sprintf("%.1f", b)
    else if (keep_dec && any(abs(b - round(b)) > 1e-9)) {
      format(round(b, 1), big.mark = ",", trim = TRUE, scientific = FALSE)
    } else format(round(b), big.mark = ",", trim = TRUE, scientific = FALSE)
  }
  ramp <- function(k) {
    if (k == 1) return(m$pal[ceiling(length(m$pal) / 2)])
    grDevices::colorRampPalette(m$pal)(k)
  }

  if (identical(method, "custom")) {
    cuts <- custom
    f2   <- function(z) fmt(z, keep_dec = TRUE)
    lab  <- c(paste0("<", f2(cuts[1])),
              if (length(cuts) > 1) paste0(f2(cuts[-length(cuts)]), " to <", f2(cuts[-1])),
              paste0(f2(cuts[length(cuts)]), "+"))
    cls  <- cut(v, c(-Inf, cuts, Inf), right = FALSE, labels = lab)
    pal  <- ramp(length(lab)); names(pal) <- lab
    return(list(cls = cls, colors = pal, title = paste0(m$label, " (custom classes)")))
  }

  br <- switch(method,
    equal = seq(min(nz), max(nz), length.out = n + 1),
    round = explore_round_breaks(nz, n),
    jenks = tryCatch(
      if (requireNamespace("classInt", quietly = TRUE) && length(unique(nz)) > n) {
        as.numeric(classInt::classIntervals(nz, n, style = "jenks")$brks)
      } else NULL,
      error = function(e) NULL),
    NULL)
  if (is.null(br)) {
    br <- as.numeric(stats::quantile(nz, probs = seq(0, 1, length.out = n + 1), names = FALSE))
    method <- "quantile"
  }
  br <- unique(br)
  if (length(br) < 2) br <- c(br, br + 1)
  lab <- paste0(fmt(br[-length(br)]), "\u2013", fmt(br[-1]))
  cls <- cut(v, br, include.lowest = TRUE, labels = lab)
  pal <- ramp(length(lab)); names(pal) <- lab
  tag <- switch(method,
    quantile = switch(as.character(n), "4" = "quartiles", "5" = "quintiles",
                      paste(n, "equal-count classes")),
    round = "round-number classes", jenks = "natural breaks", equal = "equal intervals")
  list(cls = cls, colors = pal, title = paste0(m$label, " (", tag, ")"))
}

# Filters to one grade, or (all grades) combines grade rows into one row per school.
explore_prep <- function(df, rv, grade_sel = "__all__") {
  gcol <- rv$grade_col
  lat  <- rv$lat_col %||% "latitude"
  lon  <- rv$lon_col %||% "longitude"
  if (!is.null(gcol) && gcol %in% names(df) && !identical(grade_sel, "__all__")) {
    df <- df[as.character(df[[gcol]]) == grade_sel, , drop = FALSE]
    df$.n_grades <- 1L
    df$.grades   <- rep(grade_sel, nrow(df))
    return(df)
  }
  collapse_schools(df, rv$school_col, lat, lon, rv$eligible_col, gcol)
}

# Per-area summary of school rows (needs a GEOID column).
aggregate_areas <- function(sd, elig_col = NULL) {
  f      <- factor(as.character(sd$GEOID))
  has_el <- !is.null(elig_col) && elig_col %in% names(sd)
  el     <- if (has_el) sd[[elig_col]] else rep(NA_real_, nrow(sd))
  w      <- ifelse(!is.na(sd$.vax_rate) & !is.na(el) & el > 0, el, 0)
  num    <- tapply(ifelse(is.na(sd$.vax_rate), 0, sd$.vax_rate) * w, f, sum)
  den    <- tapply(w, f, sum)
  plain  <- tapply(sd$.vax_rate, f,
                   function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
  out <- data.frame(
    GEOID = levels(f),
    .n_schools = as.integer(table(f)),
    .enrolled = if (has_el) as.numeric(tapply(el, f, sum, na.rm = TRUE)) else NA_real_,
    .undervax_count = as.numeric(tapply(
      sd$.undervax_count, f,
      function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE))),
    .vax_rate = as.numeric(ifelse(den > 0, num / den, plain)),
    stringsAsFactors = FALSE
  )
  out$.undervax_rate <- 1 - out$.vax_rate
  out
}

explore_how <- function(m, mode_b = FALSE) {
  if (mode_b && m %in% c("vax_rate", "undervax_rate", "undervax_count")) {
    return("using the values in your uploaded data")
  }
  switch(m,
    vax_rate       = "calculated across all schools in the area, weighted by enrollment",
    undervax_rate  = "calculated as 1 minus the enrollment-weighted vaccination rate",
    undervax_count = "estimated as enrollment × (1 − vaccination rate), summed over the schools in the area",
    n_schools      = "counting the schools in the area",
    enrolled       = "adding up enrollment across the schools in the area",
    ""
  )
}

# Plain-language caption saying exactly what a map shows.
explore_caption <- function(key, ctx, compact = FALSE) {
  raw_note <- "Exploratory map: shows raw values, not statistically significant clusters."
  if (identical(key, "points")) {
    base <- paste0("Each point is one school, ",
                   if (isTRUE(ctx$has_rate)) "colored by its vaccination rate" else "shown at its location",
                   if (isTRUE(ctx$size_by)) "; point size reflects enrollment" else "", ".")
    detail <- paste0(
      ctx$n_pts, " schools are shown",
      if (isTRUE(ctx$n_no_coord > 0)) paste0(" (", ctx$n_no_coord, " records without coordinates are not shown)") else "",
      ". ", ctx$grade_text)
  } else if (identical(key, "boundaries")) {
    base   <- paste0("Boundaries of the ", ctx$n_areas, " ", ctx$geo_pl, " in the geography you loaded.")
    detail <- ""
  } else {
    m      <- sub("^area:", "", key)
    base   <- paste0(EXPLORE_MEASURES[[m]]$label, " in each ", ctx$geo_noun, ", ",
                     explore_how(m, isTRUE(ctx$mode_b)), ".")
    detail <- paste0(
      if (!isTRUE(ctx$mode_b)) paste0("Based on ", ctx$n_area_schools, " schools. ") else "",
      ctx$grade_text,
      if (isTRUE(ctx$n_nodata > 0)) paste0(" ", ctx$n_nodata, " ", ctx$geo_pl, " with no data are shown in grey.") else "")
  }
  if (compact) return(base)
  parts <- c(base, trimws(detail), raw_note)
  paste(parts[nzchar(parts)], collapse = " ")
}

explore_map_title <- function(key, ctx) {
  if (identical(key, "points")) "School vaccination rates"
  else if (identical(key, "boundaries")) paste0(tools::toTitleCase(ctx$geo_pl), " boundaries")
  else paste0(EXPLORE_MEASURES[[sub("^area:", "", key)]]$label, " by ", ctx$geo_noun)
}

# Scale bar (miles) placed in its own strip BELOW the map (not over the map).
# Returns the plot with the bar added and the axis limits widened to fit it.
explore_add_scalebar <- function(p, bb) {
  w <- bb[["xmax"]] - bb[["xmin"]]
  h <- bb[["ymax"]] - bb[["ymin"]]
  miles  <- c(1, 2, 5, 10, 20, 25, 50, 100, 200, 500)
  target <- w * 0.2 / 1609.344
  mi     <- max(miles[miles <= target], miles[1])
  len    <- mi * 1609.344
  x0 <- bb[["xmin"]]
  y0 <- bb[["ymin"]] - 0.05 * h
  p +
    ggplot2::annotate("segment", x = x0, xend = x0 + len, y = y0, yend = y0,
                      linewidth = 0.8, colour = "#333333") +
    ggplot2::annotate("text", x = x0 + len + 0.015 * w, y = y0, hjust = 0,
                      label = paste(mi, "mi"), size = 2.8, colour = "#333333") +
    ggplot2::coord_sf(xlim = c(bb[["xmin"]], bb[["xmax"]]),
                      ylim = c(y0 - 0.02 * h, bb[["ymax"]]), expand = FALSE)
}

# One static map (ggplot) for a key: "points", "boundaries" or "area:<measure>".
explore_plot <- function(key, ctx, orient = "landscape", compact = FALSE) {
  proj <- ctx$proj
  # Light simplification of the outlines (about 250 m) keeps PDFs small and fast
  simp <- function(x) tryCatch(sf::st_simplify(x, preserveTopology = TRUE, dTolerance = 250),
                               error = function(e) x)
  g    <- if (!is.null(ctx$geo)) simp(sf::st_transform(ctx$geo[, "GEOID"], proj)) else NULL
  p    <- ggplot2::ggplot()
  n_nodata <- 0

  if (identical(key, "points")) {
    pts <- sf::st_transform(
      sf::st_as_sf(ctx$pts, coords = c(ctx$lon, ctx$lat), crs = 4326, remove = FALSE),
      proj)
    if (!is.null(g)) {
      p <- p + ggplot2::geom_sf(data = g, fill = "white", colour = "#b5b5b5", linewidth = 0.15)
    }
    use_size <- isTRUE(ctx$size_by) && !is.null(ctx$enroll) && ctx$enroll %in% names(pts)
    if (isTRUE(ctx$has_rate)) {
      pts$.bin <- explore_vax_bin(pts$.vax_rate)
      if (use_size) {
        pts$.enr <- pts[[ctx$enroll]]
        p <- p + ggplot2::geom_sf(data = pts, ggplot2::aes(fill = .bin, size = .enr),
                                  shape = 21, colour = "#222222", stroke = 0.25, alpha = 0.9) +
          ggplot2::scale_size_continuous(range = c(0.8, 4.5), name = "Enrolled")
      } else {
        p <- p + ggplot2::geom_sf(data = pts, ggplot2::aes(fill = .bin),
                                  shape = 21, colour = "#222222", stroke = 0.25,
                                  size = 1.7, alpha = 0.9)
      }
      p <- p + ggplot2::scale_fill_manual(values = EXPLORE_VAX_COLORS,
                                          name = "School vaccination rate", drop = FALSE)
    } else {
      p <- p + ggplot2::geom_sf(data = pts, shape = 21, fill = "#522D80",
                                colour = "#222222", stroke = 0.25, size = 1.7, alpha = 0.9)
    }
    extent <- if (!is.null(g)) g else pts

  } else if (startsWith(key, "area:")) {
    m   <- sub("^area:", "", key)
    def <- EXPLORE_MEASURES[[m]]
    fr  <- simp(sf::st_transform(ctx$frame, proj))
    cl  <- explore_classify(fr[[def$col]], m, ctx$bins)
    lev <- names(cl$colors)
    x   <- as.character(cl$cls)
    has_nd <- any(is.na(x))
    if (has_nd) {
      n_nodata <- sum(is.na(x))
      x[is.na(x)] <- "No data"
      lev <- c(lev, "No data")
    }
    fr$.cls <- factor(x, levels = lev)
    vals <- c(cl$colors, if (has_nd) c("No data" = EXPLORE_NO_DATA))
    p <- p + ggplot2::geom_sf(data = fr, ggplot2::aes(fill = .cls),
                              colour = "#b5b5b5", linewidth = 0.05) +
      ggplot2::scale_fill_manual(values = vals, name = cl$title, drop = FALSE)
    extent <- fr

  } else {
    p <- p + ggplot2::geom_sf(data = g, fill = "#f4f4f4", colour = "#777777", linewidth = 0.2)
    extent <- g
  }

  caption <- explore_caption(key, c(ctx, list(n_nodata = n_nodata)), compact)
  wrap_w  <- if (compact) 70 else if (identical(orient, "landscape")) 140 else 95
  caption <- paste(strwrap(caption, width = wrap_w), collapse = "\n")

  sub_txt <- ctx$subtitle
  if (is.null(sub_txt) || !nzchar(sub_txt)) sub_txt <- NULL

  p <- explore_add_scalebar(p, sf::st_bbox(extent))
  p +
    ggplot2::labs(title = explore_map_title(key, ctx), subtitle = sub_txt, caption = caption) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = if (compact) 2 else 1, byrow = TRUE)) +
    ggplot2::theme_void(base_size = if (compact) 8 else 10) +
    ggplot2::theme(
      legend.position  = "bottom",
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0,
                                               size = if (compact) 10 else 14),
      plot.subtitle    = ggplot2::element_text(hjust = 0, colour = "grey35"),
      plot.caption     = ggplot2::element_text(hjust = 0, colour = "grey25", lineheight = 1.1,
                                               size = if (compact) 6.5 else 8.5,
                                               margin = ggplot2::margin(t = 8)),
      plot.margin      = ggplot2::margin(8, 10, 8, 10),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

explore_page_dims <- function(orient) {
  if (identical(orient, "portrait")) c(8.5, 11) else c(11, 8.5)
}

# Draw one page (a single map, or up to four in a 2 x 2 grid) on the open device.
explore_draw_page <- function(page, layout, orient, ctx) {
  if (identical(layout, "grid")) {
    grid::grid.newpage()
    lay <- grid::grid.layout(
      3, 2,
      heights = grid::unit.c(grid::unit(1, "null"), grid::unit(1, "null"), grid::unit(0.6, "in")))
    grid::pushViewport(grid::viewport(layout = lay))
    for (i in seq_along(page)) {
      print(page[[i]], vp = grid::viewport(layout.pos.row = ceiling(i / 2),
                                           layout.pos.col = (i - 1) %% 2 + 1))
    }
    note <- paste(
      "Exploratory maps: show raw values, not statistically significant clusters.",
      ctx$grade_text)
    note <- paste(strwrap(note, width = if (identical(orient, "landscape")) 170 else 110),
                  collapse = "\n")
    grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 1:2))
    grid::grid.text(note, x = 0.02, just = c("left", "center"),
                    gp = grid::gpar(fontsize = 8, col = "grey25"))
    grid::popViewport(2)
  } else {
    print(page[[1]])
  }
  invisible(NULL)
}

explore_make_pages <- function(keys, ctx, orient, layout) {
  compact <- identical(layout, "grid")
  plots   <- lapply(keys, function(k) explore_plot(k, ctx, orient, compact))
  if (compact) unname(split(plots, ceiling(seq_along(plots) / 4))) else lapply(plots, list)
}

explore_write_pdf <- function(pages, layout, orient, ctx, file) {
  d <- explore_page_dims(orient)
  if (isTRUE(capabilities("cairo"))) {
    grDevices::cairo_pdf(file, width = d[1], height = d[2], onefile = TRUE)
  } else {
    grDevices::pdf(file, width = d[1], height = d[2], onefile = TRUE)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  for (pg in pages) explore_draw_page(pg, layout, orient, ctx)
  invisible(file)
}

explore_write_docx <- function(pages, layout, orient, ctx, file) {
  doc <- officer::read_docx()
  landscape_ok <- tryCatch({
    doc <- officer::body_set_default_section(doc, officer::prop_section(
      page_size    = officer::page_size(width = 8.5, height = 11, orient = orient),
      page_margins = officer::page_mar(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5,
                                       header = 0.3, footer = 0.3, gutter = 0),
      type = "continuous"))
    TRUE
  }, error = function(e) FALSE)
  d <- if (landscape_ok) explore_page_dims(orient) else c(8.5, 11)
  img_w <- d[1] - 1.2
  img_h <- d[2] - 1.6

  for (i in seq_along(pages)) {
    tmp <- tempfile(fileext = ".png")
    if (isTRUE(capabilities("cairo"))) {
      grDevices::png(tmp, width = img_w, height = img_h, units = "in", res = 200,
                     bg = "white", type = "cairo")
    } else {
      grDevices::png(tmp, width = img_w, height = img_h, units = "in", res = 200, bg = "white")
    }
    explore_draw_page(pages[[i]], layout, orient, ctx)
    grDevices::dev.off()
    img <- officer::external_img(tmp, width = img_w, height = img_h)
    fp  <- tryCatch(officer::fp_par(page_break_before = i > 1), error = function(e) NULL)
    if (!is.null(fp)) {
      doc <- officer::body_add_fpar(doc, officer::fpar(img, fp_p = fp))
    } else {
      if (i > 1) doc <- officer::body_add_break(doc)
      doc <- officer::body_add_fpar(doc, officer::fpar(img))
    }
  }
  print(doc, target = file)
  invisible(file)
}


# ── UI ────────────────────────────────────────────────────────────────────────

mod_explore_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 360,
      actionButton("explore_go_analysis_top", "Go to Analysis \u2192",
                   class = "btn-primary w-100", icon = icon("chart-area")),
      h5("Explore your data", class = "text-primary fw-bold mt-2"),
      p(class = "small text-muted",
        "Maps and summaries of the data you uploaded, before any analysis. ",
        "These show raw values, not statistically significant clusters — use the ",
        "Analysis tab for that."),
      uiOutput(ns("controls")),
      hr(),
      h6("Printable maps"),
      uiOutput(ns("print_ui")),
      hr(),
      actionButton("explore_go_analysis_bottom", "Go to Analysis \u2192",
                   class = "btn-primary w-100", icon = icon("chart-area"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        card_header("Map"),
        leafletOutput(ns("map"), height = "calc(100vh - 250px)"),
        uiOutput(ns("caption"))
      ),
      bslib::navset_card_tab(
        title = NULL,
        bslib::nav_panel(
          "Ranked table",
          # overflow hidden: the table scrolls inside itself (one horizontal
          # scrollbar, pager always visible), so the panel must not add another.
          div(style = "overflow: hidden;",
              div(class = "d-flex flex-wrap align-items-center gap-2 mb-2",
                  actionButton(ns("clear_sel"), "Show all on map",
                               class = "btn-outline-secondary btn-sm"),
                  downloadButton(ns("dl_table"), "Download table (CSV)",
                                 class = "btn-outline-primary btn-sm"),
                  span(class = "small text-muted",
                       "Click rows to isolate them on the map.")),
              DTOutput(ns("table")))
        ),
        bslib::nav_panel(
          "Data summary",
          div(style = "overflow-y: auto; max-height: calc(100vh - 250px);",
              uiOutput(ns("summary")))
        )
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_explore_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_a <- reactive(identical(rv$upload_mode, "facility") && !is.null(rv$uploaded_data))
    is_b <- reactive(identical(rv$upload_mode, "aggregate") && !is.null(rv$uploaded_data))

    area_ready <- reactive({
      !is.null(rv$geo) &&
        ((is_b() && !is.null(rv$joined_data)) || (is_a() && !is.null(rv$school_data)))
    })

    has_rate_a <- reactive({
      is_a() && any(!is.na(rv$uploaded_data$.vax_rate))
    })

    available_measures <- reactive({
      req(area_ready())
      if (is_a()) {
        sd <- rv$school_data
        has_rate  <- any(!is.na(sd$.vax_rate))
        has_count <- any(!is.na(sd$.undervax_count))
        has_enr   <- !is.null(rv$eligible_col) && rv$eligible_col %in% names(sd) &&
          any(!is.na(sd[[rv$eligible_col]]))
        c(if (has_rate) c("vax_rate", "undervax_rate"),
          if (has_count) "undervax_count",
          "n_schools",
          if (has_enr) "enrolled")
      } else {
        jd <- sf::st_drop_geometry(rv$joined_data)
        c(if (any(!is.na(jd$.vax_rate))) c("vax_rate", "undervax_rate"),
          if (any(!is.na(jd$.undervax_count))) "undervax_count")
      }
    })

    geo_words <- reactive({
      lvl  <- attr(rv$geo, "spatialert_geo_level")
      noun <- geo_noun(lvl)
      list(noun = noun, pl = spatialert_pluralize(noun))
    })

    grade_levels <- reactive({
      g <- rv$grade_col
      req(is_a(), !is.null(g), g %in% names(rv$uploaded_data))
      sort(unique(as.character(rv$uploaded_data[[g]])))
    })

    # ── Controls ──────────────────────────────────────────────────────────────
    output$controls <- renderUI({
      if (is.null(rv$uploaded_data)) {
        return(div(class = "alert alert-light border", style = "color: #333333;",
                   icon("circle-info"), " ",
                   "Upload your data and press ", tags$strong("Confirm data"),
                   " on the Data & Geography tab to explore it here."))
      }
      types <- character(0)
      if (is_a()) types <- c(types, "School locations (points)" = "points")
      if (area_ready()) {
        types <- c(types, "Areas, colored by a measure" = "area", "Area boundaries only" = "boundaries")
      }
      if (length(types) == 0) {
        return(div(class = "alert alert-light border", style = "color: #333333;",
                   icon("circle-info"), " ",
                   "Load a geography on the Data & Geography tab to map areas."))
      }

      keep <- keep_val
      meas <- if (area_ready()) available_measures() else character(0)
      meas_choices <- setNames(meas, vapply(meas, function(m) EXPLORE_MEASURES[[m]]$label, ""))

      tagList(
        selectInput(ns("map_type"), "Map type", choices = types,
                    selected = keep(isolate(input$map_type), types, types[1])),
        if (length(meas) > 0) {
          conditionalPanel(
            condition = paste0("input['", ns("map_type"), "'] == 'area'"),
            tagList(
              selectInput(ns("measure"), "Measure", choices = meas_choices,
                          selected = keep(isolate(input$measure), meas, meas[1])),
              uiOutput(ns("bin_ui"))
            )
          )
        },
        if (is_a() && !is.null(rv$grade_col)) {
          lv <- grade_levels()
          selectInput(ns("grade"), "Grade",
                      choices  = c("All grades combined" = "__all__", setNames(lv, lv)),
                      selected = keep(isolate(input$grade), c("__all__", lv), "__all__"))
        },
        if (is_a()) {
          tagList(
            conditionalPanel(
              condition = paste0("input['", ns("map_type"), "'] == 'points'"),
              if (!is.null(rv$eligible_col)) {
                checkboxInput(ns("size_by_enroll"), "Size points by enrollment", FALSE)
              }
            ),
            conditionalPanel(
              condition = paste0("input['", ns("map_type"), "'] != 'points'"),
              checkboxInput(ns("overlay_points"), "Overlay school points", FALSE)
            )
          )
        },
        if (is_a() && !is.null(rv$grade_col)) {
          helpText("'All grades combined' shows one point per school: enrollment is the ",
                   "sum across grades and the vaccination rate is the enrollment-weighted ",
                   "average of the grades that have data.")
        }
      )
    })

    # ── Color classes (bins) ──────────────────────────────────────────────────
    custom_store <- reactiveValues(txt = list())
    observeEvent(input$bin_custom, {
      m <- input$measure
      if (!is.null(m)) custom_store$txt[[m]] <- input$bin_custom
    }, ignoreInit = TRUE)

    custom_breaks <- reactive({
      lapply(custom_store$txt, explore_parse_breaks)
    })

    output$bin_ui <- renderUI({
      m <- input$measure
      req(m)
      if (identical(m, "vax_rate")) {
        return(helpText("Vaccination rate uses the fixed bins <85%, 85\u201389.9%, ",
                        "90\u201394.9%, \u226595% (same as the school colors)."))
      }
      req(area_ready())
      def <- EXPLORE_MEASURES[[m]]
      fr  <- tryCatch(area_frame(), error = function(e) NULL)
      vals <- if (!is.null(fr)) sf::st_drop_geometry(fr)[[def$col]] * def$mult else numeric(0)
      vals <- vals[!is.na(vals)]
      rng <- if (length(vals)) {
        qs <- stats::quantile(vals, c(0, 0.5, 0.9, 0.99, 1), names = FALSE)
        f  <- function(z) format(round(z, if (def$mult == 100) 1 else 0), big.mark = ",", trim = TRUE)
        paste0("Your data: min ", f(qs[1]), ", median ", f(qs[2]), ", 90th pct ", f(qs[3]),
               ", 99th pct ", f(qs[4]), ", max ", f(qs[5]), ".")
      } else ""
      meth <- keep_val(isolate(input$bin_method), EXPLORE_BIN_METHODS, "quantile")
      tagList(
        selectInput(ns("bin_method"), "Color classes", choices = EXPLORE_BIN_METHODS,
                    selected = meth),
        conditionalPanel(
          condition = paste0("input['", ns("bin_method"), "'] != 'custom'"),
          sliderInput(ns("bin_n"), "Number of classes", min = 3, max = 8, step = 1,
                      value = isolate(input$bin_n) %||% 5)
        ),
        conditionalPanel(
          condition = paste0("input['", ns("bin_method"), "'] == 'custom'"),
          textInput(ns("bin_custom"), "Thresholds (comma-separated)",
                    value = custom_store$txt[[m]] %||% "",
                    placeholder = "e.g. 5, 10, 25, 50, 100"),
          helpText("Each threshold starts a new class: with 10, 50 you get <10, 10 to <50 and 50+. ",
                   "Thresholds are remembered separately for each measure.")
        ),
        helpText(rng)
      )
    })

    current_key <- reactive({
      type <- input$map_type %||% (if (is_a()) "points" else "boundaries")
      switch(type,
        points     = "points",
        boundaries = "boundaries",
        area       = paste0("area:", input$measure %||% "vax_rate"),
        "points")
    })

    # ── Area values (choropleth) ──────────────────────────────────────────────
    area_frame <- reactive({
      req(rv$geo, area_ready())
      if (is_b()) return(rv$joined_data)
      sd  <- explore_prep(rv$school_data, rv, input$grade %||% "__all__")
      agg <- aggregate_areas(sd, rv$eligible_col)
      g   <- rv$geo[, "GEOID"]
      g$GEOID <- as.character(g$GEOID)
      out <- dplyr::left_join(g, agg, by = "GEOID")
      out$.n_schools[is.na(out$.n_schools)] <- 0L
      out$.undervax_count[out$.n_schools == 0] <- 0
      if (!is.null(rv$eligible_col)) out$.enrolled[out$.n_schools == 0] <- 0
      attr(out, "n_area_schools") <- nrow(sd)
      out
    })

    # Everything the maps need, gathered once.
    ctx <- reactive({
      req(rv$uploaded_data)
      geo       <- rv$geo
      grade_sel <- input$grade %||% "__all__"
      lat <- rv$lat_col %||% "latitude"
      lon <- rv$lon_col %||% "longitude"

      pts <- NULL
      n_no_coord <- 0
      if (is_a()) {
        raw <- rv$uploaded_data
        ok  <- !is.na(raw[[lat]]) & !is.na(raw[[lon]])
        n_no_coord <- sum(!ok)
        pts <- explore_prep(raw[ok, , drop = FALSE], rv, grade_sel)
      }
      frame <- if (area_ready()) tryCatch(area_frame(), error = function(e) NULL) else NULL

      extent <- if (!is.null(geo)) {
        geo
      } else if (!is.null(pts) && nrow(pts) > 0) {
        sf::st_as_sf(pts, coords = c(lon, lat), crs = 4326, remove = FALSE)
      } else {
        NULL
      }
      req(extent)

      words <- if (!is.null(geo)) geo_words() else list(noun = "area", pl = "areas")

      grade_text <- if (is.null(rv$grade_col) || !is_a()) {
        ""
      } else if (identical(grade_sel, "__all__")) {
        "All grades combined: enrollment is summed across grades and the vaccination rate is the enrollment-weighted average of the grades that have data."
      } else {
        paste0("Grade shown: ", grade_sel, ".")
      }

      enroll <- if (!is.null(rv$eligible_col) && !is.null(pts) && rv$eligible_col %in% names(pts)) {
        rv$eligible_col
      } else {
        NULL
      }

      list(
        geo = geo, pts = pts, frame = frame, lat = lat, lon = lon, enroll = enroll,
        proj = local_projected_crs(extent), extent = extent,
        geo_noun = words$noun, geo_pl = words$pl,
        n_areas = if (!is.null(geo)) nrow(geo) else NA_integer_,
        n_pts = if (!is.null(pts)) nrow(pts) else 0L,
        n_no_coord = n_no_coord,
        n_area_schools = if (!is.null(frame)) (attr(frame, "n_area_schools") %||% NA) else NA,
        has_rate = has_rate_a(),
        size_by = isTRUE(input$size_by_enroll),
        mode_b = is_b(),
        grade_text = grade_text,
        bins = list(method = input$bin_method %||% "quantile",
                    n = input$bin_n %||% 5,
                    custom = custom_breaks()),
        subtitle = input$print_subtitle %||% ""
      )
    })

    # ── Interactive map ───────────────────────────────────────────────────────
    output$map <- renderLeaflet({
      cx  <- ctx()
      key <- current_key()
      m   <- leaflet() |> addProviderTiles("Esri.WorldGrayCanvas")
      geo <- cx$geo
      sel <- selected_rows()
      td  <- if (!is.null(sel)) table_data() else NULL
      # IDs (area maps) or point rows (point map) picked in the ranked table
      sel_ids <- if (!is.null(td) && !identical(key, "points")) as.character(td[[1]][sel]) else NULL
      sel_pts <- if (!is.null(td) && identical(key, "points")) attr(td, "pts_idx")[sel] else NULL
      sel_extent <- NULL

      if (!is.null(geo) && (identical(key, "boundaries") || startsWith(key, "area:"))) {
        if (startsWith(key, "area:") && !is.null(cx$frame)) {
          mk  <- sub("^area:", "", key)
          def <- EXPLORE_MEASURES[[mk]]
          fr  <- cx$frame
          cl  <- explore_classify(fr[[def$col]], mk, cx$bins)
          if (!is.null(sel_ids)) {
            m <- m |> addPolygons(data = fr, fillOpacity = 0, weight = 0.4,
                                  color = "#bbbbbb", options = pathOptions(interactive = FALSE))
          }
          fill <- unname(cl$colors[as.character(cl$cls)])
          fill[is.na(fill)] <- EXPLORE_NO_DATA
          val <- fr[[def$col]] * def$mult
          lab <- paste0(fr$GEOID, ": ",
                        ifelse(is.na(val), "no data",
                               formatC(val, format = "f", digits = if (def$mult == 100) 1 else 0,
                                       big.mark = ",")))
          any_nd <- any(is.na(cl$cls))
          if (!is.null(sel_ids)) {
            keep_i <- as.character(fr$GEOID) %in% sel_ids
            fr <- fr[keep_i, ]; fill <- fill[keep_i]; lab <- lab[keep_i]
            sel_extent <- fr
          }
          m <- m |>
            addPolygons(data = fr, fillColor = fill, fillOpacity = 0.75, weight = 0.5,
                        color = "#666666", label = lab) |>
            addLegend(position = "bottomright", opacity = 1, title = cl$title,
                      colors = c(unname(cl$colors), if (any_nd) EXPLORE_NO_DATA),
                      labels = c(names(cl$colors), if (any_nd) "No data"))
        } else {
          g2 <- geo
          if (!is.null(sel_ids)) {
            m  <- m |> addPolygons(data = geo, fillOpacity = 0, weight = 0.4, color = "#bbbbbb",
                                   options = pathOptions(interactive = FALSE))
            g2 <- geo[as.character(geo$GEOID) %in% sel_ids, ]
            sel_extent <- g2
          }
          m <- m |> addPolygons(data = g2, fillColor = "#f4f4f4", fillOpacity = 0.6,
                                weight = 0.6, color = "#666666", label = ~GEOID)
        }
      }

      show_pts <- !is.null(cx$pts) && nrow(cx$pts) > 0 &&
        (identical(key, "points") || isTRUE(input$overlay_points))
      if (show_pts) {
        pts <- cx$pts
        if (!is.null(sel_pts)) {
          pts <- pts[sel_pts, , drop = FALSE]
          sel_extent <- sf::st_as_sf(pts, coords = c(cx$lon, cx$lat), crs = 4326, remove = FALSE)
        }
        nm  <- if (!is.null(rv$school_col) && rv$school_col %in% names(pts)) {
          as.character(pts[[rv$school_col]])
        } else {
          paste("School", seq_len(nrow(pts)))
        }
        enr <- if (!is.null(cx$enroll)) pts[[cx$enroll]] else rep(NA_real_, nrow(pts))
        grades <- if (".grades" %in% names(pts)) pts$.grades else rep(NA_character_, nrow(pts))
        popup <- paste0(
          "<strong>", htmltools::htmlEscape(nm), "</strong>",
          ifelse(is.na(grades), "", paste0("<br>Grades: ", htmltools::htmlEscape(grades))),
          if (isTRUE(cx$has_rate)) paste0("<br>Vaccinated: ", sprintf("%.1f%%", 100 * pts$.vax_rate)) else "",
          ifelse(is.na(enr), "", paste0("<br>Enrolled: ", enr))
        )
        fill <- if (isTRUE(cx$has_rate)) {
          f <- unname(EXPLORE_VAX_COLORS[as.character(explore_vax_bin(pts$.vax_rate))])
          f[is.na(f)] <- EXPLORE_NO_DATA
          f
        } else {
          "#522D80"
        }
        rad <- if (isTRUE(cx$size_by) && any(!is.na(enr))) {
          cap <- stats::quantile(enr, 0.99, na.rm = TRUE)
          3 + 6 * sqrt(pmin(enr / cap, 1))
        } else {
          4
        }
        rad[is.na(rad)] <- 3
        m <- m |>
          addCircleMarkers(lng = pts[[cx$lon]], lat = pts[[cx$lat]], radius = rad,
                           stroke = TRUE, color = "#222222", weight = 0.6,
                           fillColor = fill, fillOpacity = 0.9, popup = popup)
        if (isTRUE(cx$has_rate) && identical(key, "points")) {
          m <- m |> addLegend(position = "bottomleft", opacity = 1,
                              title = "School vaccination rate",
                              colors = unname(EXPLORE_VAX_COLORS),
                              labels = names(EXPLORE_VAX_COLORS))
        }
      }

      bb <- sf::st_bbox(sf::st_transform(if (!is.null(sel_extent)) sel_extent else cx$extent, 4326))
      if (!is.null(sel_extent) && bb[["xmin"]] == bb[["xmax"]]) {
        bb[c("xmin", "xmax")] <- bb[c("xmin", "xmax")] + c(-0.02, 0.02)
        bb[c("ymin", "ymax")] <- bb[c("ymin", "ymax")] + c(-0.02, 0.02)
      }
      m |> fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })

    output$caption <- renderUI({
      cx  <- ctx()
      key <- current_key()
      nd  <- if (startsWith(key, "area:") && !is.null(cx$frame)) {
        mk <- sub("^area:", "", key)
        sum(is.na(cx$frame[[EXPLORE_MEASURES[[mk]]$col]]))
      } else {
        0
      }
      div(class = "small text-muted p-2",
          explore_caption(key, c(cx, list(n_nodata = nd)), compact = FALSE))
    })

    # ── Summary ───────────────────────────────────────────────────────────────
    output$summary <- renderUI({
      req(rv$uploaded_data)
      rows <- list()
      add  <- function(label, value) {
        rows[[length(rows) + 1]] <<- tags$tr(tags$td(label), tags$td(class = "text-end", value))
      }
      fmt <- function(x) format(x, big.mark = ",", trim = TRUE)

      if (is_a()) {
        raw <- rv$uploaded_data
        lat <- rv$lat_col %||% "latitude"
        lon <- rv$lon_col %||% "longitude"
        no_coord <- sum(is.na(raw[[lat]]) | is.na(raw[[lon]]))
        unit <- if (!is.null(rv$grade_col)) "records (school × grade)" else "rows"
        add(paste("Total", unit, "in your file"), fmt(nrow(raw) + (rv$n_dropped %||% 0)))
        add("Excluded (missing or suppressed values)", fmt(rv$n_dropped %||% 0))
        add("Used", fmt(nrow(raw)))
        add("Without coordinates", fmt(no_coord))
        ok <- raw[!is.na(raw[[lat]]) & !is.na(raw[[lon]]), , drop = FALSE]
        n_sch <- nrow(collapse_schools(ok, rv$school_col, lat, lon, rv$eligible_col, rv$grade_col))
        add("Distinct schools (grades combined)", fmt(n_sch))
        if (!is.null(rv$geo)) {
          add("Outside the loaded boundaries", fmt(rv$n_unmatched %||% 0))
          add(paste0(tools::toTitleCase(geo_words()$pl), " in the geography"), fmt(nrow(rv$geo)))
          if (!is.null(rv$school_data)) {
            add(paste0(tools::toTitleCase(geo_words()$pl), " with at least one school"),
                fmt(length(unique(rv$school_data$GEOID))))
          }
        }
        if (!is.null(rv$eligible_col) && rv$eligible_col %in% names(raw)) {
          add("Total enrolled students", fmt(round(sum(raw[[rv$eligible_col]], na.rm = TRUE))))
        }
        if (any(!is.na(raw$.undervax_count))) {
          add("Estimated undervaccinated students", fmt(round(sum(raw$.undervax_count, na.rm = TRUE))))
        }
        if (any(!is.na(raw$.vax_rate))) {
          el <- if (!is.null(rv$eligible_col) && rv$eligible_col %in% names(raw)) raw[[rv$eligible_col]] else NULL
          ok2 <- !is.na(raw$.vax_rate)
          if (!is.null(el)) ok2 <- ok2 & !is.na(el) & el > 0
          rate <- if (!is.null(el) && any(ok2)) stats::weighted.mean(raw$.vax_rate[ok2], el[ok2]) else mean(raw$.vax_rate, na.rm = TRUE)
          add("Overall vaccination rate", sprintf("%.1f%%", 100 * rate))
        }
      } else {
        add("Rows in your file", fmt(nrow(rv$uploaded_data)))
        if (!is.null(rv$geo)) {
          add(paste0(tools::toTitleCase(geo_words()$pl), " in the geography"), fmt(nrow(rv$geo)))
          add("Rows matched to the geography", fmt(rv$n_matched %||% 0))
          add("Rows not matched", fmt(rv$n_unmatched %||% 0))
        }
      }
      tags$table(class = "table table-sm", style = "font-size: 0.9rem;", tags$tbody(rows))
    })

    # ── Ranked table ──────────────────────────────────────────────────────────
    table_data <- reactive({
      cx  <- ctx()
      key <- current_key()
      if (identical(key, "points") && !is.null(cx$pts)) {
        d <- cx$pts
        cols <- list()
        if (!is.null(rv$school_col) && rv$school_col %in% names(d)) cols[["School"]] <- as.character(d[[rv$school_col]])
        if (".grades" %in% names(d)) cols[["Grades"]] <- d$.grades
        for (cn in names(d)[grepl("^(county|city|zip)$", names(d), ignore.case = TRUE)]) {
          if (!cn %in% names(cols)) cols[[cn]] <- d[[cn]]
        }
        if (!is.null(cx$enroll)) cols[["Enrolled"]] <- d[[cx$enroll]]
        if (isTRUE(cx$has_rate)) cols[["% vaccinated"]] <- round(d$.vax_rate * 100, 1)
        if (any(!is.na(d$.undervax_count))) cols[["Est. undervaccinated students"]] <- round(d$.undervax_count)
        out <- as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
        ord <- if ("Est. undervaccinated students" %in% names(out)) {
          order(-out[["Est. undervaccinated students"]])
        } else {
          seq_len(nrow(out))
        }
        out <- out[ord, , drop = FALSE]
        attr(out, "pts_idx") <- ord
        return(out)
      }
      req(cx$frame)
      fr   <- sf::st_drop_geometry(cx$frame)
      meas <- intersect(names(EXPLORE_MEASURES), available_measures())
      out  <- data.frame(GEOID = fr$GEOID, stringsAsFactors = FALSE)
      names(out) <- paste(tools::toTitleCase(geo_words()$noun), "ID")
      for (m in meas) {
        def <- EXPLORE_MEASURES[[m]]
        out[[def$label]] <- round(fr[[def$col]] * def$mult, if (def$mult == 100) 1 else 0)
      }
      if (startsWith(key, "area:")) {
        lab <- EXPLORE_MEASURES[[sub("^area:", "", key)]]$label
        if (lab %in% names(out)) {
          decreasing <- !identical(sub("^area:", "", key), "vax_rate")  # lowest coverage first
          out <- out[order(out[[lab]], decreasing = decreasing, na.last = TRUE), , drop = FALSE]
        }
      }
      out
    })

    output$table <- renderDT({
      datatable(table_data(), rownames = FALSE, filter = "top",
                selection = list(mode = "multiple", target = "row"),
                fillContainer = FALSE,
                options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100),
                               scrollX = TRUE, scrollY = "calc(100vh - 480px)",
                               scrollCollapse = TRUE))
    })
    observeEvent(input$clear_sel, {
      DT::selectRows(DT::dataTableProxy("table"), NULL)
    })

    # Rows currently highlighted in the table (indices into table_data())
    selected_rows <- reactive({
      sel <- input$table_rows_selected
      td  <- table_data()
      sel <- sel[!is.na(sel) & sel >= 1 & sel <= nrow(td)]
      if (length(sel) == 0) NULL else sel
    })
    output$dl_table <- downloadHandler(
      filename = function() paste0("spatialert_explore_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(table_data(), file)
    )

    # ── Printable maps ────────────────────────────────────────────────────────
    print_choices <- reactive({
      req(rv$uploaded_data)
      ch <- character(0)
      if (is_a()) ch <- c(ch, setNames("points", "School locations (colored by vaccination rate)"))
      if (area_ready()) {
        noun <- geo_words()$noun
        for (m in available_measures()) {
          ch <- c(ch, setNames(paste0("area:", m),
                               paste0(EXPLORE_MEASURES[[m]]$label, " by ", noun)))
        }
        ch <- c(ch, setNames("boundaries", "Area boundaries"))
      }
      ch
    })

    output$print_ui <- renderUI({
      ch <- if (is.null(rv$uploaded_data)) character(0) else print_choices()
      if (length(ch) == 0) {
        return(helpText("Upload your data (and load a geography to map areas) to enable printable maps."))
      }
      tagList(
        checkboxGroupInput(ns("print_maps"), "Maps to include",
                           choices = ch, selected = head(unname(ch), 2)),
        radioButtons(ns("print_layout"), "Layout",
                     choices = c("One map per page" = "single",
                                 "2 × 2 maps per page" = "grid")),
        selectInput(ns("print_orient"), "Page orientation",
                    choices = c("Automatic (based on the shape of your area)" = "auto",
                                "Landscape (horizontal)" = "landscape",
                                "Portrait (vertical)"    = "portrait")),
        uiOutput(ns("orient_hint")),
        textInput(ns("print_subtitle"), "Subtitle (optional)",
                  placeholder = "e.g. 2024-25 school year"),
        downloadButton(ns("dl_maps_docx"), "Word (.docx)",
                       class = "btn-outline-success w-100 mb-1"),
        downloadButton(ns("dl_maps_pdf"), "PDF",
                       class = "btn-outline-primary w-100 mb-1"),
        downloadButton(ns("dl_map_png"), "PNG of the map on screen",
                       class = "btn-outline-secondary w-100")
      )
    })

    orient_suggestion <- reactive({
      cx <- ctx()
      bb <- sf::st_bbox(sf::st_transform(cx$extent, cx$proj))
      if ((bb[["xmax"]] - bb[["xmin"]]) >= (bb[["ymax"]] - bb[["ymin"]])) "landscape" else "portrait"
    })
    orient_final <- reactive({
      o <- input$print_orient %||% "auto"
      if (identical(o, "auto")) orient_suggestion() else o
    })
    output$orient_hint <- renderUI({
      helpText("Suggested for your area: ", orient_suggestion(),
               if (orient_suggestion() == "landscape") " (wider than tall)." else " (taller than wide).")
    })

    output$dl_maps_docx <- downloadHandler(
      filename = function() paste0("spatialert_maps_", Sys.Date(), ".docx"),
      content  = function(file) {
        keys <- input$print_maps
        req(length(keys) > 0)
        layout <- input$print_layout %||% "single"
        pages  <- explore_make_pages(keys, ctx(), orient_final(), layout)
        explore_write_docx(pages, layout, orient_final(), ctx(), file)
      }
    )
    output$dl_maps_pdf <- downloadHandler(
      filename = function() paste0("spatialert_maps_", Sys.Date(), ".pdf"),
      content  = function(file) {
        keys <- input$print_maps
        req(length(keys) > 0)
        layout <- input$print_layout %||% "single"
        pages  <- explore_make_pages(keys, ctx(), orient_final(), layout)
        explore_write_pdf(pages, layout, orient_final(), ctx(), file)
      }
    )
    output$dl_map_png <- downloadHandler(
      filename = function() paste0("spatialert_map_", Sys.Date(), ".png"),
      content  = function(file) {
        d <- explore_page_dims(orient_final())
        p <- explore_plot(current_key(), ctx(), orient_final(), compact = FALSE)
        ggplot2::ggsave(file, plot = p, width = d[1] - 1, height = d[2] - 1,
                        dpi = 200, bg = "white")
      }
    )
  })
}

`%||%` <- function(x, y) if (!is.null(x)) x else y
