# mod_results.R — Results display and export module

mod_results_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(8, 4),

    bslib::card(
      card_header(
        "Hotspot results",
        tooltip(
          icon("circle-question"),
          "Areas are ranked by absolute Gi* z-score — the strongest hotspots and coldspots appear first."
        )
      ),
      uiOutput(ns("results_summary_badges")),
      DTOutput(ns("results_table"))
    ),

    tagList(
      bslib::card(
        card_header("Export results"),
        bslib::card_body(
          h6("CSV export"),
          p("Download the full results table as a CSV for further analysis.",
            class = "text-muted small"),
          downloadButton(ns("dl_csv"), "Download CSV",
                         class = "btn-outline-primary w-100"),
          hr(),
          h6("Word report"),
          p("Generate a formatted report with map, summary table, and methods text.",
            class = "text-muted small"),
          textInput(
            ns("report_title"),
            "Report title",
            value = "Spatial Hotspot Analysis"
          ),
          textInput(
            ns("report_location"),
            "Study area (for methods text)",
            placeholder = "e.g. South Carolina"
          ),
          downloadButton(ns("dl_report"), "Generate report (.docx)",
                         class = "btn-outline-success w-100 mt-1")
        )
      ),

      bslib::card(
        card_header("Methods text"),
        bslib::card_body(
          p("Copy this into your report or grant application:",
            class = "text-muted small"),
          uiOutput(ns("methods_text_box"))
        )
      )
    )
  )
}


mod_results_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Build clean results data frame ─────────────────────────────────────────
    results_df <- reactive({
      req(rv$results)
      method <- attr(rv$results, "spatialert_method") %||% "gi_star"
      df     <- rv$results |> sf::st_drop_geometry()

      if (method %in% c("gi_star", "gi_star_consensus")) {
        is_consensus <- method == "gi_star_consensus"

        df <- df |>
          dplyr::select(
            dplyr::any_of(c("GEOID", "COUNTYFP")),
            hotspot_class,
            gi_star,
            dplyr::any_of(c("gi_pvalue_adj", "n_hot_specs", "n_cold_specs", "n_specs"))
          ) |>
          dplyr::arrange(dplyr::desc(abs(gi_star)))

        df <- add_county_name(df, rv)

        keep_cols <- if (is_consensus) {
          c("GEOID", "county_name", "hotspot_class", "gi_star",
            "n_hot_specs", "n_cold_specs", "n_specs")
        } else {
          c("GEOID", "county_name", "hotspot_class", "gi_star", "gi_pvalue_adj")
        }

        gi_star_label <- if (is_consensus) "Gi* Z-Score (mean across k)" else "Gi* Z-Score"

        df <- df |>
          dplyr::select(dplyr::any_of(keep_cols)) |>
          dplyr::rename_with(~ dplyr::case_when(
            .x == "GEOID"         ~ "Tract ID",
            .x == "county_name"   ~ "County",
            .x == "hotspot_class" ~ "Classification",
            .x == "gi_star"       ~ gi_star_label,
            .x == "gi_pvalue_adj" ~ "Adjusted P-Value",
            .x == "n_hot_specs"   ~ "Hotspot in # k-specs",
            .x == "n_cold_specs"  ~ "Coldspot in # k-specs",
            .x == "n_specs"       ~ "Total k-specs run",
            TRUE                  ~ .x
          ))

      } else {
        df <- df |>
          dplyr::select(
            dplyr::any_of(c("GEOID", "COUNTYFP")),
            lisa_class,
            local_i,
            li_pvalue_adj
          ) |>
          dplyr::arrange(dplyr::desc(abs(local_i)))

        df <- add_county_name(df, rv)

        df <- df |>
          dplyr::select(
            dplyr::any_of(c("GEOID", "county_name", "lisa_class",
                             "local_i", "li_pvalue_adj"))
          ) |>
          dplyr::rename_with(~ dplyr::case_when(
            .x == "GEOID"         ~ "Tract ID",
            .x == "county_name"   ~ "County",
            .x == "lisa_class"    ~ "Classification",
            .x == "local_i"       ~ "Local I Statistic",
            .x == "li_pvalue_adj" ~ "Adjusted P-Value",
            TRUE                  ~ .x
          ))
      }

      df
    })

    # ── Summary badges — colors match the map ──────────────────────────────────
    output$results_summary_badges <- renderUI({
      req(results_df())
      cls    <- results_df()$Classification
      n_hot  <- sum(cls == "Hotspot"  | cls == "High-High", na.rm = TRUE)
      n_cold <- sum(cls == "Coldspot" | cls == "Low-Low",   na.rm = TRUE)
      n_ns   <- sum(cls == "Not significant",               na.rm = TRUE)

      div(class = "d-flex gap-2 mb-3",
        span(class = "badge fs-6",
             style = "background-color: #c0392b;",
             icon("fire"),      " ", n_hot,  " hotspot(s)"),
        span(class = "badge fs-6",
             style = "background-color: #2980b9;",
             icon("snowflake"), " ", n_cold, " coldspot(s)"),
        span(class = "badge fs-6",
             style = "background-color: #7f8c8d;",
             icon("minus"),     " ", n_ns,   " not significant")
      )
    })

    # ── Results table ───────────────────────────────────────────────────────────
    output$results_table <- renderDT({
      req(results_df())
      df <- results_df()

      dt <- datatable(
        df,
        rownames = FALSE,
        filter   = "top",
        options  = list(pageLength = 15, scrollX = TRUE)
      )

      if ("Classification" %in% names(df)) {
        dt <- dt |>
          formatStyle(
            "Classification",
            backgroundColor = styleEqual(
              c("Hotspot", "High-High", "Coldspot", "Low-Low", "Not significant"),
              c("#fde8e8",  "#fde8e8",  "#e8f0fe",  "#e8f0fe",  "#f5f5f5")
            ),
            color = styleEqual(
              c("Hotspot", "High-High", "Coldspot", "Low-Low", "Not significant"),
              c("#c0392b",  "#c0392b",  "#1a5276",  "#1a5276",  "#555")
            ),
            fontWeight = "bold"
          )
      }

      num_cols <- names(df)[sapply(df, is.numeric)]
      for (col in num_cols) {
        dt <- dt |> formatRound(col, digits = 2)
      }

      dt
    })

    # ── Per-k breakdown + raw value, CSV export only ────────────────────────────
    # The on-screen table (results_df) stays summary-only to avoid clutter, but
    # the CSV export adds:
    #   - the raw value each tract was actually analyzed on (so it can be
    #     diffed directly against a standalone script's underlying column,
    #     independent of any weights/neighbor differences), and
    #   - one z-score + classification column pair per k specification
    #     actually run, when consensus mode was used (mirrors the
    #     Gi_star_uv25_k4 / Gi_uv25_cluster_k4 style columns a standalone
    #     spdep script would produce).
    results_df_export <- reactive({
      req(rv$results)
      base <- results_df()

      value_df <- sf::st_drop_geometry(rv$results) |>
        dplyr::select(dplyr::any_of(c("GEOID", ".value"))) |>
        dplyr::rename(`Tract ID` = GEOID, `Value Analyzed` = .value)

      if (nrow(value_df) > 0 && "Value Analyzed" %in% names(value_df)) {
        base <- dplyr::left_join(base, value_df, by = "Tract ID") |>
          dplyr::relocate(`Value Analyzed`, .after = `Tract ID`)
      }

      method <- attr(rv$results, "spatialert_method") %||% "gi_star"
      if (method != "gi_star_consensus") return(base)

      z_mat     <- attr(rv$results, "spatialert_z_matrix")
      class_mat <- attr(rv$results, "spatialert_class_matrix")
      k_values  <- attr(rv$results, "spatialert_k_values")
      geoid     <- sf::st_drop_geometry(rv$results)$GEOID

      if (is.null(z_mat) || is.null(class_mat) || is.null(k_values) || is.null(geoid)) {
        return(base)
      }

      per_k <- data.frame(`Tract ID` = geoid, check.names = FALSE)
      for (i in seq_along(k_values)) {
        k <- k_values[i]
        per_k[[paste0("Gi* Z-Score (k=", k, ")")]]    <- z_mat[, i]
        per_k[[paste0("Classification (k=", k, ")")]] <- class_mat[, i]
      }

      dplyr::left_join(base, per_k, by = "Tract ID")
    })

    # ── CSV download ────────────────────────────────────────────────────────────
    output$dl_csv <- downloadHandler(
      filename = function() paste0("spatialert_results_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(results_df_export(), file)
    )

    # ── Shared title helpers ────────────────────────────────────────────────────
    get_method_title <- function(method) {
      if (method == "gi_star_consensus") "Getis-Ord Gi* Hotspot Analysis (KNN Consensus)"
      else if (method == "gi_star") "Getis-Ord Gi* Hotspot Analysis"
      else "Local Moran's I Cluster & Outlier Analysis"
    }

    get_correction_label <- function(correction) {
      switch(correction,
        fdr        = "FDR Corrected",
        bonferroni = "Bonferroni Corrected",
        none       = NULL
      )
    }

    get_geo_title <- function(geo_level) {
      switch(geo_level,
        county        = "County",
        tract         = "Census Tract",
        `block group` = "Census Block Group",
        geo_level
      )
    }

    get_state_title <- function(state, location = "") {
      if (!is.null(state) && !is.na(state) && nchar(state) > 0) {
        state.name[match(state, state.abb)]
      } else if (nchar(location) > 0) {
        location
      } else {
        NULL
      }
    }

    build_plot_titles <- function(method, correction, alpha, geo_level, state, location = "") {
      method_title     <- get_method_title(method)
      correction_label <- get_correction_label(correction)
      geo_title        <- get_geo_title(geo_level)
      state_title      <- get_state_title(state, location)

      subtitle <- if (!is.null(state_title)) {
        paste0(state_title, ", ", geo_title)
      } else {
        geo_title
      }

      if (!is.null(correction_label)) {
        subtitle <- paste0(subtitle, " | ", correction_label, ", \u03b1 = ", alpha)
      }

      list(title = method_title, subtitle = subtitle)
    }

    # ── Methods text ────────────────────────────────────────────────────────────
    make_methods_text <- function(location = "") {
      req(rv$results)

      method          <- attr(rv$results, "spatialert_method")     %||% "gi_star"
      correction      <- attr(rv$results, "spatialert_correction") %||% "fdr"
      alpha           <- attr(rv$results, "spatialert_alpha")      %||% 0.05
      geo_level       <- attr(rv$results, "spatialert_geo_level")  %||% "areal unit"
      var_label       <- attr(rv$results, "spatialert_var_label")  %||% "undervaccinated individuals"
      weights_style   <- rv$weights_style %||% "knn"
      weights_type    <- rv$weights_type  %||% "B"
      knn_k           <- rv$knn_k         %||% 8
      knn_k_list      <- rv$knn_k_list    %||% c(4, 6, 8, 10, 12)
      min_specs       <- rv$min_specs     %||% 2
      upload_mode     <- rv$upload_mode   %||% "facility"
      analyze_target  <- rv$analyze_target %||% "undervax_count"
      source_type     <- rv$source_type    %||% "derive"
      n_after_upload  <- if (!is.null(rv$uploaded_data)) nrow(rv$uploaded_data) else NULL
      n_unmatched     <- rv$n_unmatched   %||% 0
      n_dropped       <- rv$n_dropped     %||% 0
      n_final         <- if (!is.null(n_after_upload)) n_after_upload - n_unmatched else NULL

      state_name <- get_state_title(rv$state %||% "", location)

      pkg_ver <- tryCatch(
        as.character(utils::packageVersion("spatialert")),
        error = function(e) "0.1.0-dev"
      )

      method_label <- get_method_title(method)

      correction_label <- switch(correction,
        fdr        = "false discovery rate (Benjamini-Hochberg)",
        bonferroni = "Bonferroni",
        none       = "no"
      )

      geo_label <- switch(geo_level,
        county        = "county",
        tract         = "census tract",
        `block group` = "census block group",
        geo_level
      )

      weights_type_label <- if (weights_type == "B") "binary" else "row-standardized"

      weights_label <- switch(weights_style,
        queen          = glue::glue("queen contiguity ({weights_type_label} coding)"),
        rook           = glue::glue("rook contiguity ({weights_type_label} coding)"),
        knn            = glue::glue("k-nearest neighbours (k = {knn_k}, {weights_type_label} coding)"),
        knn_consensus  = glue::glue(
          "k-nearest neighbours ({weights_type_label} coding) evaluated at k = ",
          "{paste(knn_k_list, collapse = ', ')}; a unit was classified as a hotspot or ",
          "coldspot only if significant in the same direction in at least {min_specs} ",
          "of the {length(knn_k_list)} specifications"
        ),
        glue::glue("k-nearest neighbours (k = {knn_k}, {weights_type_label} coding)")
      )

      gi_note <- if (method %in% c("gi_star", "gi_star_consensus")) {
        "The Gi* statistic was computed using the localG function with analytical p-values. "
      } else {
        "Local Moran's I was computed using permutation-based p-values (499 simulations). "
      }

      loc_str <- if (!is.null(state_name)) paste0(" in ", state_name)
                 else if (nchar(location) > 0) paste0(" in ", location)
                 else ""

      n_small       <- rv$n_small %||% 0
      n_total_excl  <- n_dropped + n_small

      # Exclusion sentences
      exclusion_str <- ""
      if (n_dropped > 0) {
        exclusion_str <- paste0(exclusion_str, glue::glue(
          " {scales::comma(n_dropped)} ",
          "{ifelse(n_dropped == 1, 'school was', 'schools were')} excluded due to ",
          "missing or suppressed vaccination or enrollment values."
        ))
      }
      if (n_small > 0) {
        exclusion_str <- paste0(exclusion_str, glue::glue(
          " {scales::comma(n_small)} ",
          "{ifelse(n_small == 1, 'school was', 'schools were')} excluded because ",
          "enrollment was fewer than 10 students; exact enrollment counts are suppressed ",
          "for small schools due to privacy concerns, making accurate estimation of the ",
          "number of undervaccinated students impossible."
        ))
      }
      if (n_unmatched > 0) {
        exclusion_str <- paste0(exclusion_str, glue::glue(
          " An additional {scales::comma(n_unmatched)} ",
          "{ifelse(n_unmatched == 1, 'school was', 'schools were')} excluded as ",
          "{ifelse(n_unmatched == 1, 'it', 'they')} could not be matched to a {geo_label}."
        ))
      }
      final_str <- if (!is.null(n_final) && (n_total_excl > 0 || n_unmatched > 0)) {
        n_final_adj <- if (!is.null(n_after_upload)) n_after_upload - n_small - n_unmatched else NULL
        if (!is.null(n_final_adj)) {
          glue::glue(
            " A total of {scales::comma(n_final_adj)} ",
            "{ifelse(n_final_adj == 1, 'school was', 'schools were')} included in the final analysis. ",
            "Census tracts with no schools were assigned a value of zero undervaccinated students ",
            "and were included in the spatial analysis."
          )
        } else ""
      } else {
        glue::glue("Census tracts with no schools were assigned a value of zero undervaccinated students and were included in the spatial analysis.")
      }

      # Data description
      target_desc <- switch(analyze_target,
        undervax_count = "count of undervaccinated students",
        undervax_rate  = "undervaccination rate",
        vax_rate       = "vaccination rate"
      )
      derive_desc <- if (source_type == "derive") {
        " estimated by multiplying the total eligible population by one minus the proportion vaccinated"
      } else {
        ""
      }
      rate_derive_desc <- if (source_type == "derive") {
        " calculated from the number vaccinated (or vaccination rate) and the total eligible population"
      } else {
        ""
      }
      target_calc_desc <- switch(analyze_target,
        undervax_count = derive_desc,
        undervax_rate  = rate_derive_desc,
        vax_rate       = rate_derive_desc
      )

      data_sentence <- if (upload_mode == "facility") {
        n_str <- if (!is.null(n_after_upload)) {
          glue::glue("{scales::comma(n_after_upload + n_dropped)} ")
        } else ""

        agg_desc <- if (analyze_target == "undervax_count") {
          glue::glue(
            " These facility-level counts were then spatially aggregated to the ",
            "{geo_label} level by summing all facilities within each {geo_label}."
          )
        } else if (isTRUE(rv$agg_weighted)) {
          glue::glue(
            " These facility-level rates were then aggregated to the {geo_label} level ",
            "as an eligible-population-weighted average across facilities within each ",
            "{geo_label} (weighted by {rv$eligible_col %||% 'eligible population'})."
          )
        } else {
          glue::glue(
            " These facility-level rates were then aggregated to the {geo_label} level ",
            "as an unweighted average across facilities within each {geo_label} ",
            "(no eligible-population column was provided to weight by \u2014 each facility ",
            "contributed equally to the {geo_label}-level rate regardless of size)."
          )
        }

        glue::glue(
          "Facility-level data for {n_str}schools or facilities{loc_str} were ",
          "obtained.{exclusion_str}{final_str} The {target_desc} was computed ",
          "per facility{target_calc_desc}.{agg_desc}"
        )
      } else {
        n_str <- if (!is.null(n_after_upload)) {
          glue::glue("{scales::comma(n_after_upload)} ")
        } else ""

        glue::glue(
          "Pre-aggregated data at the {geo_label} level{loc_str} were used ",
          "({n_str}{geo_label}s). The {target_desc} was computed per {geo_label}",
          "{target_calc_desc}."
        )
      }

      glue::glue(
        "{data_sentence} ",
        "A {method_label} spatial analysis was then conducted to identify geographic ",
        "clusters of {var_label}. Spatial weights were constructed using {weights_label}. ",
        "{gi_note}",
        "Statistical significance was assessed at \u03b1 = {alpha} with ",
        "{correction_label} correction for multiple comparisons. Analysis was conducted ",
        "using the spatialert R package (version {pkg_ver})."
      )
    }

    output$methods_text_box <- renderUI({
      req(rv$results)
      location <- input$report_location %||% ""
      txt      <- make_methods_text(location)

      tagList(
        div(
          class = "border rounded p-2 bg-light",
          style = "font-size: 0.875rem; line-height: 1.6;",
          txt
        ),
        div(class = "mt-2",
          tags$button(
            class   = "btn btn-sm btn-outline-secondary",
            onclick = paste0(
              "navigator.clipboard.writeText(`", txt, "`); ",
              "this.textContent = 'Copied!'; ",
              "setTimeout(() => this.textContent = 'Copy to clipboard', 1500);"
            ),
            "Copy to clipboard"
          )
        )
      )
    })

    # ── Word report download ────────────────────────────────────────────────────
    output$dl_report <- downloadHandler(
      filename = function() paste0("spatialert_report_", Sys.Date(), ".docx"),
      content  = function(file) {
        req(rv$results)

        method     <- attr(rv$results, "spatialert_method")     %||% "gi_star"
        correction <- attr(rv$results, "spatialert_correction") %||% "fdr"
        alpha      <- attr(rv$results, "spatialert_alpha")      %||% 0.05
        geo_level  <- attr(rv$results, "spatialert_geo_level")  %||% "areal unit"
        var_label  <- attr(rv$results, "spatialert_var_label")  %||% "undervaccinated individuals"
        location   <- input$report_location %||% ""

        methods_txt <- make_methods_text(location)

        geo_label <- switch(geo_level,
          county = "county", tract = "census tract",
          `block group` = "census block group", geo_level
        )
        loc_str <- if (nchar(location) > 0) paste0(" in ", location) else ""

        # Summary counts
        if (method %in% c("gi_star", "gi_star_consensus")) {
          n_hot  <- sum(rv$results$hotspot_class == "Hotspot",         na.rm = TRUE)
          n_cold <- sum(rv$results$hotspot_class == "Coldspot",        na.rm = TRUE)
          n_ns   <- sum(rv$results$hotspot_class == "Not significant", na.rm = TRUE)
          cls_label <- "hotspot"
        } else {
          n_hot  <- sum(rv$results$lisa_class == "High-High", na.rm = TRUE)
          n_cold <- sum(rv$results$lisa_class == "Low-Low",   na.rm = TRUE)
          n_ns   <- nrow(rv$results) - n_hot - n_cold
          cls_label <- "High-High cluster"
        }

        # Plot titles
        titles <- build_plot_titles(
          method     = method,
          correction = correction,
          alpha      = alpha,
          geo_level  = geo_level,
          state      = rv$state %||% "",
          location   = location
        )

        # Generate map PNG
        map_png <- tryCatch({
          tmp <- tempfile(fileext = ".png")
          geo <- rv$results

          pal_vals <- c("Hotspot", "Coldspot", "Not significant")
          pal_cols <- c("#c0392b", "#2980b9", "#bdc3c7")

          p <- ggplot2::ggplot(geo) +
            ggplot2::geom_sf(
              ggplot2::aes(fill = hotspot_class),
              color = "white", size = 0.1
            ) +
            ggplot2::scale_fill_manual(
              values   = setNames(pal_cols, pal_vals),
              name     = "Classification",
              na.value = "#eeeeee"
            ) +
            ggplot2::theme_void() +
            ggplot2::theme(
              legend.position = "bottom",
              legend.title    = ggplot2::element_text(size = 9),
              legend.text     = ggplot2::element_text(size = 8),
              plot.title      = ggplot2::element_text(size = 11, hjust = 0.5,
                                                      face = "bold"),
              plot.subtitle   = ggplot2::element_text(size = 9,  hjust = 0.5,
                                                      color = "grey40")
            ) +
            ggplot2::labs(
              title    = titles$title,
              subtitle = titles$subtitle
            )

          ggplot2::ggsave(tmp, plot = p, width = 6, height = 5,
                          dpi = 150, bg = "white")
          tmp
        }, error = function(e) NULL)

        # Build Word document
        doc <- officer::read_docx()

        doc <- officer::body_add_par(
          doc, input$report_title %||% "Spatial Hotspot Analysis",
          style = "heading 1"
        )
        doc <- officer::body_add_par(
          doc, format(Sys.Date(), "%B %d, %Y"), style = "Normal"
        )
        doc <- officer::body_add_par(doc, " ", style = "Normal")

        doc <- officer::body_add_par(doc, "Summary", style = "heading 2")
        doc <- officer::body_add_par(doc,
          glue::glue(
            "The analysis identified {n_hot} {cls_label} {geo_label}(s) with ",
            "significantly elevated {var_label}{loc_str}, {n_cold} coldspot ",
            "{geo_label}(s) with significantly lower values, and {n_ns} ",
            "{geo_label}(s) that were not statistically significant."
          ),
          style = "Normal"
        )
        doc <- officer::body_add_par(doc, " ", style = "Normal")

        if (!is.null(map_png) && file.exists(map_png)) {
          doc <- officer::body_add_par(doc, "Hotspot Map", style = "heading 2")
          doc <- officer::body_add_img(doc, src = map_png, width = 6, height = 5)
          doc <- officer::body_add_par(doc, " ", style = "Normal")
        }

        doc <- officer::body_add_par(doc, "Results by Area", style = "heading 2")

        tbl_data <- results_df() |>
          head(30) |>
          as.data.frame() |>
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

        ft <- flextable::flextable(tbl_data) |>
          flextable::autofit() |>
          flextable::theme_vanilla() |>
          flextable::fontsize(size = 8, part = "all") |>
          flextable::padding(padding = 3, part = "all")

        doc <- flextable::body_add_flextable(doc, ft)
        doc <- officer::body_add_par(doc, " ", style = "Normal")

        doc <- officer::body_add_par(doc, "Methods", style = "heading 2")
        doc <- officer::body_add_par(doc, methods_txt, style = "Normal")

        print(doc, target = file)
      }
    )
  })
}

# ── Helpers ───────────────────────────────────────────────────────────────────

add_county_name <- function(df, rv) {
  if (!"COUNTYFP" %in% names(df)) return(df)
  tryCatch({
    state_fips <- unique(substr(df$GEOID, 1, 2))
    counties   <- tigris::counties(state = state_fips, cb = TRUE, year = 2020) |>
      sf::st_drop_geometry() |>
      dplyr::select(COUNTYFP, county_name = NAME)
    dplyr::left_join(df, counties, by = "COUNTYFP")
  }, error = function(e) {
    df$county_name <- NA_character_
    df
  })
}

`%||%` <- function(x, y) if (!is.null(x)) x else y
