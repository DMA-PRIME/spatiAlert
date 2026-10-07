# mod_results.R — Results display and export module

mod_results_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(8, 4),

      bslib::navset_card_tab(
        id    = ns("results_tabs"),
        title = tagList(
          "Hotspot results",
          tooltip(
            icon("circle-question"),
            "Areas are ranked by absolute Gi* z-score — the strongest hotspots and coldspots appear first."
          )
        ),
        nav_panel(
          "Areas",
          uiOutput(ns("results_summary_badges")),
          DTOutput(ns("results_table"))
        ),
        nav_panel(
          "Schools of concern",
          uiOutput(ns("schools_panel"))
        )
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
            uiOutput(ns("report_schools_opt")),
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
            .x == "GEOID"         ~ id_header(rv),
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
            .x == "GEOID"         ~ id_header(rv),
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
      idh  <- id_header(rv)

      value_df <- sf::st_drop_geometry(rv$results) |>
        dplyr::select(dplyr::any_of(c("GEOID", ".value"))) |>
        dplyr::rename(!!idh := GEOID, `Value Analyzed` = .value)

      if (nrow(value_df) > 0 && "Value Analyzed" %in% names(value_df)) {
        base <- dplyr::left_join(base, value_df, by = idh) |>
          dplyr::relocate(`Value Analyzed`, .after = dplyr::all_of(idh))
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

      per_k <- data.frame(geoid, check.names = FALSE)
      names(per_k) <- idh
      for (i in seq_along(k_values)) {
        k <- k_values[i]
        per_k[[paste0("Gi* Z-Score (k=", k, ")")]]    <- z_mat[, i]
        per_k[[paste0("Classification (k=", k, ")")]] <- class_mat[, i]
      }

      dplyr::left_join(base, per_k, by = idh)
    })

    # ── CSV download ────────────────────────────────────────────────────────────
    output$dl_csv <- downloadHandler(
      filename = function() paste0("spatialert_results_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(results_df_export(), file)
    )

    # ── Schools of concern (Mode A only) ────────────────────────────────────────
    # Lists the individual schools located in the areas of concern (hotspots by
    # default; coldspots when the analysis target is the vaccination RATE, since
    # there a "cold" area is the low-coverage one), filtered to schools below a
    # chosen vaccination threshold. Area z-scores are deliberately left out:
    # this table is for follow-up on specific schools.
    school_extra_default <- function(sd) {
      cols <- names(sd)
      cols[grepl("^(county|city|zip)$", cols, ignore.case = TRUE)]
    }

    output$schools_panel <- renderUI({
      req(rv$results)
      if (is.null(rv$school_data)) {
        return(div(class = "alert alert-light border mt-2",
                   style = "color: #333333;",
                   icon("circle-info"), " ",
                   "The schools list needs school-level data (upload format ",
                   "\"Mode A — School / facility level\"). Pre-aggregated data ",
                   "has no individual schools to list."))
      }
      sd   <- rv$school_data
      skip <- c(rv$lat_col, rv$lon_col, rv$school_col, "GEOID")
      extra_choices <- setdiff(names(sd)[!grepl("^\\.", names(sd))], skip)
      concern_lbl <- if (identical(rv$analyze_target, "vax_rate")) {
        "Coldspots (low-coverage areas)"
      } else {
        "Hotspots (areas of concern)"
      }

      tagList(
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput(ns("school_scope"), "Areas to include",
                      choices = setNames(c("concern", "both", "all"),
                                         c(concern_lbl, "Hotspots and coldspots",
                                           "All areas")),
                      selected = "concern"),
          numericInput(ns("school_threshold"),
                       "List schools below this % vaccinated",
                       value = 95, min = 0, max = 100, step = 1),
          uiOutput(ns("school_area_ui"))
        ),
        selectizeInput(ns("school_extra_cols"), "Extra columns to show",
                       choices  = extra_choices,
                       selected = school_extra_default(sd),
                       multiple = TRUE),
        if (!is.null(rv$grade_col)) {
          checkboxInput(ns("school_combine"),
                        "Combine grades (one row per school)", FALSE)
        },
        uiOutput(ns("schools_note")),
        DTOutput(ns("schools_table")),
        downloadButton(ns("dl_schools_csv"), "Download schools list (CSV)",
                       class = "btn-outline-primary mt-2")
      )
    })

    output$school_area_ui <- renderUI({
      req(rv$results)
      gids <- concern_geoids(rv$results, rv$analyze_target %||% "undervax_count",
                             input$school_scope %||% "concern")
      selectInput(ns("school_area"), "Single area (optional)",
                  choices  = c("All areas above" = "__all__", setNames(gids, gids)),
                  selected = "__all__")
    })

    school_table <- reactive({
      req(rv$results, rv$school_data)
      extras <- input$school_extra_cols
      if (is.null(extras) && is.null(input$school_scope)) {
        extras <- school_extra_default(rv$school_data)
      }
      build_school_table(
        rv,
        scope     = input$school_scope %||% "concern",
        threshold = input$school_threshold %||% 95,
        area_pick = input$school_area %||% "__all__",
        extras    = extras,
        combine   = isTRUE(input$school_combine)
      )
    })

    output$schools_note <- renderUI({
      st   <- school_table()
      noun <- spatialert_pluralize(geo_noun(attr(rv$results, "spatialert_geo_level")))
      msg <- if (st$has_rate) {
        paste0("Showing ", st$n_shown, " school(s) below ", st$threshold,
               "% vaccinated in ", st$n_areas_scope, " ", noun, " (",
               st$n_in_areas, " schools in total are located in these areas).")
      } else {
        paste0("Showing all ", st$n_shown, " school(s) in ", st$n_areas_scope, " ",
               noun, ". School-level vaccination rates are not available when the ",
               "undervaccinated count is entered directly, so no threshold is applied.")
      }
      n_drop <- (rv$n_dropped %||% 0) + (rv$n_unmatched %||% 0)
      if (n_drop > 0) {
        msg <- paste0(msg, " ", n_drop, " school(s) in your file were excluded ",
                      "(missing/suppressed values or outside the boundaries) and ",
                      "cannot be listed.")
      }
      div(
        div(class = "small text-muted mb-1", msg),
        if (st$has_rate) {
          div(class = "small text-muted mb-2",
              "Shading in the % vaccinated column matches the map: ",
              span(style = "background:#f6c9c4; padding:0 4px;", "below 85%"), " ",
              span(style = "background:#fddcb0; padding:0 4px;", "85–89.9%"), " ",
              span(style = "background:#fff6a8; padding:0 4px;", "90–94.9%"), " ",
              span(style = "background:#cdeccb; padding:0 4px;", "95% or above"), ".")
        }
      )
    })

    output$schools_table <- renderDT({
      df <- school_table()$table
      dt <- datatable(df, rownames = FALSE, filter = "top",
                      options = list(pageLength = 15, scrollX = TRUE))
      if ("% vaccinated" %in% names(df)) {
        dt <- dt |> formatStyle(
          "% vaccinated",
          # Cut points sit just below each boundary because DT intervals are
          # right-closed and values are rounded to 1 decimal (85.0 -> orange).
          backgroundColor = styleInterval(
            c(84.95, 89.95, 94.95),
            c("#f6c9c4", "#fddcb0", "#fff6a8", "#cdeccb")
          )
        )
      }
      dt
    })

    output$dl_schools_csv <- downloadHandler(
      filename = function() paste0("spatialert_schools_of_concern_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(school_table()$table, file)
    )

    # The report's school list has its OWN settings (always the areas of concern,
    # no single-area filter), so a leftover filter on the Schools tab can never
    # empty it out.
    output$report_schools_opt <- renderUI({
      req(rv$results, rv$school_data)
      tagList(
        checkboxInput(ns("report_include_schools"),
                      "Include schools of concern in the report", value = TRUE),
        conditionalPanel(
          condition = paste0("input['", ns("report_include_schools"), "']"),
          numericInput(ns("report_school_threshold"),
                       "List schools below this % vaccinated",
                       value = 95, min = 0, max = 100, step = 1),
          if (!is.null(rv$grade_col)) {
            checkboxInput(ns("report_combine_grades"),
                          "Combine grades (one row per school)", FALSE)
          }
        )
      )
    })

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
        tools::toTitleCase(geo_level)
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

      geo_pl     <- spatialert_pluralize(geo_label)
      cap_geo_pl <- paste0(toupper(substr(geo_pl, 1, 1)), substring(geo_pl, 2))

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
        "The Gi* statistic was computed using the localG function with analytical p-values, with each area included in its own neighborhood. "
      } else {
        "Local Moran's I was computed using permutation-based p-values (499 simulations). "
      }

      loc_str <- if (!is.null(state_name)) paste0(" in ", state_name)
                 else if (nchar(location) > 0) paste0(" in ", location)
                 else ""

      global_note <- if (method %in% c("gi_star", "gi_star_consensus")) {
        global_g_text(rv$global_g, short = FALSE)
      } else ""

      n_small       <- rv$n_small %||% 0
      n_total_excl  <- n_dropped + n_small

      # With a grade column each row is one grade at one school
      by_grade <- !is.null(rv$grade_col)
      rec_one  <- if (by_grade) "school-by-grade record" else "school"
      rec_many <- if (by_grade) "school-by-grade records" else "schools"
      rec_fac  <- if (by_grade) "school-by-grade records" else "schools or facilities"

      # Exclusion sentences
      exclusion_str <- ""
      if (n_dropped > 0) {
        exclusion_str <- paste0(exclusion_str, glue::glue(
          " {scales::comma(n_dropped)} ",
          "{ifelse(n_dropped == 1, paste(rec_one, 'was'), paste(rec_many, 'were'))} excluded due to ",
          "missing or suppressed vaccination or enrollment values."
        ))
      }
      if (n_small > 0) {
        exclusion_str <- paste0(exclusion_str, glue::glue(
          " {scales::comma(n_small)} ",
          "{ifelse(n_small == 1, paste(rec_one, 'was'), paste(rec_many, 'were'))} excluded because ",
          "enrollment was fewer than 10 students; exact enrollment counts are suppressed ",
          "for small schools due to privacy concerns, making accurate estimation of the ",
          "number of undervaccinated students impossible."
        ))
      }
      if (n_unmatched > 0) {
        exclusion_str <- paste0(exclusion_str, glue::glue(
          " An additional {scales::comma(n_unmatched)} ",
          "{ifelse(n_unmatched == 1, paste(rec_one, 'was'), paste(rec_many, 'were'))} excluded as ",
          "{ifelse(n_unmatched == 1, 'it', 'they')} could not be matched to a {geo_label}."
        ))
      }
      # How areas with no schools were treated (set on the Analysis tab)
      ns_opt  <- rv$no_school_opt %||% "zero"
      n_empty <- rv$n_no_school   %||% NA
      zero_what <- if (analyze_target == "undervax_count") "a value of zero undervaccinated students" else "a value of zero"
      empty_str <- if (isTRUE(n_empty == 0)) {
        ""
      } else {
        n_part <- if (!is.na(n_empty)) paste0(scales::comma(n_empty), " ") else ""
        switch(ns_opt,
          exclude = glue::glue("{n_part}{geo_pl} with no schools were excluded from the spatial analysis."),
          mean    = glue::glue("{n_part}{geo_pl} with no schools were assigned the average value across {geo_pl} with schools and were included in the spatial analysis."),
          glue::glue("{cap_geo_pl} with no schools were assigned {zero_what} and were included in the spatial analysis.")
        )
      }
      final_str <- if (!is.null(n_final) && (n_total_excl > 0 || n_unmatched > 0)) {
        n_final_adj <- if (!is.null(n_after_upload)) n_after_upload - n_small - n_unmatched else NULL
        if (!is.null(n_final_adj)) {
          glue::glue(
            " A total of {scales::comma(n_final_adj)} ",
            "{ifelse(n_final_adj == 1, paste(rec_one, 'was'), paste(rec_many, 'were'))} included in the final analysis. ",
            "{empty_str}"
          )
        } else ""
      } else {
        glue::glue("{empty_str}")
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
          "Facility-level data for {n_str}{rec_fac}{loc_str} were ",
          "obtained.{exclusion_str}{final_str} The {target_desc} was computed ",
          "per facility{target_calc_desc}.{agg_desc}"
        )
      } else {
        n_str <- if (!is.null(n_after_upload)) {
          glue::glue("{scales::comma(n_after_upload)} ")
        } else ""

        glue::glue(
          "Pre-aggregated data at the {geo_label} level{loc_str} were used ",
          "({n_str}{geo_pl}). The {target_desc} was computed per {geo_label}",
          "{target_calc_desc}."
        )
      }

      grade_sentence <- if (by_grade && upload_mode == "facility") {
        " Each record is one grade at one school, so a school can contribute more than one record; counts of undervaccinated students were summed across records."
      } else {
        ""
      }
      is_census_geo <- geo_level %in% c("tract", "county", "block group")
      geo_source_sentence <- if (is_census_geo) {
        paste0(" ", paste0(toupper(substr(geo_label, 1, 1)), substring(geo_label, 2)),
               " boundaries were obtained from the U.S. Census Bureau ",
               "(cartographic boundary files) using the tigris R package.")
      } else {
        paste0(" Analyses used a user-provided boundary file defining ",
               if (!is.null(rv$geo)) paste0(scales::comma(nrow(rv$geo)), " ") else "",
               geo_pl, ".")
      }
      agg_unmatched_str <- if (upload_mode == "aggregate" && n_unmatched > 0) {
        paste0(" ", scales::comma(n_unmatched), " ",
               ifelse(n_unmatched == 1, "record", "records"),
               " in the uploaded data could not be matched to the boundary file ",
               "and ", ifelse(n_unmatched == 1, "was", "were"), " excluded.")
      } else {
        ""
      }

      glue::glue(
        "{data_sentence}{grade_sentence}{geo_source_sentence}{agg_unmatched_str} ",
        "A {method_label} spatial analysis was then conducted to identify geographic ",
        "clusters of {var_label}. Spatial weights were constructed using {weights_label}. ",
        "{gi_note}{global_note}",
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

        # ── About this analysis (plain language) ──────────────────────────────
        geo_pl      <- spatialert_pluralize(geo_label)
        target_now  <- rv$analyze_target %||% "undervax_count"
        has_schools <- isTRUE(input$report_include_schools) && !is.null(rv$school_data)
        bg <- build_background(
          geo_label   = geo_label, geo_pl = geo_pl, var_label = var_label,
          method      = method, correction = correction,
          k_list      = rv$knn_k_list %||% c(4, 6, 8, 10, 12),
          min_specs   = rv$min_specs %||% 2,
          target      = target_now, loc_str = loc_str,
          has_schools = has_schools, global_g = rv$global_g
        )
        doc <- officer::body_add_par(doc, "About this analysis", style = "heading 2")
        for (sec in bg) {
          doc <- officer::body_add_par(doc, sec$h, style = "heading 3")
          if (!is.null(sec$p)) doc <- officer::body_add_par(doc, sec$p, style = "Normal")
          for (b in sec$bullets) {
            doc <- officer::body_add_par(doc, paste0("• ", b), style = "Normal")
          }
        }
        doc <- officer::body_add_par(doc, " ", style = "Normal")

        # ── Results table, with column definitions underneath ─────────────────
        doc <- officer::body_add_par(doc, "Statistically significant areas", style = "heading 2")

        # Only areas classified as a hotspot or coldspot (not "Not significant")
        sig_all <- results_df()
        sig_all <- sig_all[sig_all$Classification %in%
                             c("Hotspot", "Coldspot", "High-High", "Low-Low"), , drop = FALSE]
        n_sig   <- nrow(sig_all)
        cap_sig <- 30

        if (n_sig == 0) {
          doc <- officer::body_add_par(doc, paste0(
            "No ", geo_pl, " were classified as statistically significant hotspots or coldspots."),
            style = "Normal")
        } else {
          doc <- officer::body_add_par(doc, paste0(
            "This table lists the ", n_sig, " ", if (n_sig == 1) geo_label else geo_pl,
            " classified as a hotspot or coldspot, ordered by the strength of the result. ",
            "Areas that were not significant are not shown",
            if (n_sig > cap_sig) paste0("; the first ", cap_sig, " are listed here") else "",
            ". The full results can be downloaded as a CSV from the app."),
            style = "Normal")

          tbl_data <- sig_all |>
            head(cap_sig) |>
            as.data.frame() |>
            dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

          ft <- flextable::flextable(tbl_data) |>
            flextable::theme_vanilla() |>
            flextable::fontsize(size = 8, part = "all") |>
            flextable::padding(padding = 3, part = "all") |>
            fit_report_table()

          doc <- flextable::body_add_flextable(doc, ft)

          notes <- build_table_footnotes(
            cols = names(tbl_data), method = method, correction = correction,
            k_list = rv$knn_k_list %||% c(4, 6, 8, 10, 12),
            geo_label = geo_label, target = target_now
          )
          doc <- add_small_par(doc, "Column definitions", bold = TRUE)
          for (nt in notes) doc <- add_small_par(doc, nt)
        }
        doc <- officer::body_add_par(doc, " ", style = "Normal")

        # ── Schools of concern (optional; Mode A only) ────────────────────────
        if (has_schools) {
          thr <- input$report_school_threshold %||% 95
          if (is.na(thr)) thr <- 95
          st <- tryCatch(
            build_school_table(
              rv, scope = "concern", threshold = thr, area_pick = "__all__",
              extras = school_extra_default(rv$school_data),
              combine = isTRUE(input$report_combine_grades)
            ),
            error = function(e) e
          )
          doc <- officer::body_add_par(doc, "Schools of concern", style = "heading 2")

          if (inherits(st, "error")) {
            doc <- officer::body_add_par(doc, paste0(
              "The schools list could not be built: ", conditionMessage(st)), style = "Normal")
          } else {
            is_gi <- method %in% c("gi_star", "gi_star_consensus")
            concern_name <- if (identical(target_now, "vax_rate")) {
              if (is_gi) "coldspot" else "Low-Low"
            } else {
              if (is_gi) "hotspot" else "High-High"
            }
            scope_txt <- paste0(concern_name, " ", geo_pl)

            cap   <- 50
            n_all <- nrow(st$table)
            if (n_all == 0) {
              intro <- if (st$has_rate) {
                paste0("No schools located in ", scope_txt, " had a vaccination rate below ",
                       st$threshold, "% (", st$n_in_areas, " school records are located in ",
                       "these areas).")
              } else {
                paste0("No schools are located in ", scope_txt, ".")
              }
            } else {
              intro <- if (st$has_rate) {
                paste0("This table lists schools located in ", scope_txt,
                       " with a vaccination rate below ", st$threshold, "%, ordered by ",
                       "the strength of the area result and then by the estimated number ",
                       "of undervaccinated students. ", n_all, " record(s) met these criteria")
              } else {
                paste0("This table lists schools located in ", scope_txt, " (", n_all, " record(s)")
              }
              intro <- paste0(intro, if (n_all > cap) {
                paste0("; the first ", cap, " are shown. Download the schools list (CSV) ",
                       "from the app for the full list.")
              } else {
                "."
              })
            }
            doc <- officer::body_add_par(doc, intro, style = "Normal")

            if (n_all > 0) {
              keep <- c(id_header(rv), "School", "Grade", "Grades", "County", "county",
                        "Enrolled", "% vaccinated", "Est. undervaccinated students")
              sch  <- st$table[, names(st$table) %in% keep, drop = FALSE] |> head(cap)
              fts  <- flextable::flextable(sch) |>
                flextable::theme_vanilla() |>
                (function(x) {
                  lab <- list("% vaccinated" = "% vacc.",
                              "Est. undervaccinated students" = "Est. under-vacc.")
                  lab <- lab[names(lab) %in% names(sch)]
                  if (length(lab)) flextable::set_header_labels(x, values = lab) else x
                })() |>
                flextable::fontsize(size = 8, part = "all") |>
                flextable::padding(padding = 3, part = "all") |>
                fit_report_table(widths = school_report_widths(names(sch), id_header(rv)))
              doc <- flextable::body_add_flextable(doc, fts)
              if ((rv$source_type %||% "derive") == "derive" &&
                  "Est. undervaccinated students" %in% names(sch)) {
                doc <- add_small_par(doc, paste0(
                  "% vacc. = percent vaccinated. Est. under-vacc. = enrolled students × ",
                  "(1 − vaccination rate). Only schools with usable enrollment and ",
                  "vaccination data are listed."))
              }
            }
          }
          doc <- officer::body_add_par(doc, " ", style = "Normal")
        }

        doc <- officer::body_add_par(doc, "Methods", style = "heading 2")
        doc <- officer::body_add_par(doc, methods_txt, style = "Normal")

        print(doc, target = file)
      }
    )
  })
}

# ── Helpers ───────────────────────────────────────────────────────────────────

# Column header for the area ID, matching the geography that was analyzed.
id_header <- function(rv) {
  lvl <- attr(rv$results, "spatialert_geo_level") %||% "tract"
  switch(lvl,
    tract         = "Tract ID",
    `block group` = "Block Group ID",
    county        = "County ID",
    paste(tools::toTitleCase(lvl), "ID")
  )
}

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


# One row per school: combines the grade rows of the same school (same name and
# location). Enrollment and undervaccinated counts are summed; the vaccination
# rate is the enrollment-weighted average of the grades that have a rate.
collapse_schools <- function(df, school_col = NULL, lat_col = "latitude",
                             lon_col = "longitude", elig_col = NULL, grade_col = NULL) {
  if (nrow(df) == 0) return(df)
  nm  <- if (!is.null(school_col) && school_col %in% names(df)) {
    as.character(df[[school_col]])
  } else {
    rep("", nrow(df))
  }
  key <- paste(nm, round(df[[lat_col]], 5), round(df[[lon_col]], 5), sep = "|")
  f   <- factor(key, levels = unique(key))
  out <- df[match(levels(f), key), , drop = FALSE]

  has_el <- !is.null(elig_col) && elig_col %in% names(df)
  el     <- if (has_el) df[[elig_col]] else rep(NA_real_, nrow(df))
  if (has_el) out[[elig_col]] <- as.numeric(tapply(el, f, sum, na.rm = TRUE))

  out$.undervax_count <- as.numeric(tapply(
    df$.undervax_count, f,
    function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)))

  w     <- ifelse(!is.na(df$.vax_rate) & !is.na(el) & el > 0, el, 0)
  num   <- tapply(ifelse(is.na(df$.vax_rate), 0, df$.vax_rate) * w, f, sum)
  den   <- tapply(w, f, sum)
  plain <- tapply(df$.vax_rate, f,
                  function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
  out$.vax_rate      <- as.numeric(ifelse(den > 0, num / den, plain))
  out$.undervax_rate <- 1 - out$.vax_rate
  out$.n_grades      <- as.integer(tapply(seq_len(nrow(df)), f, length))
  if (!is.null(grade_col) && grade_col %in% names(df)) {
    out$.grades <- as.character(tapply(
      as.character(df[[grade_col]]), f,
      function(x) paste(unique(x), collapse = ", ")))
  }
  rownames(out) <- NULL
  out
}

# Builds the "schools of concern" table. Takes explicit settings (no Shiny
# inputs) so the app screen and the Word report can each use their own.
build_school_table <- function(rv, scope = "concern", threshold = 95,
                               area_pick = "__all__", extras = character(0),
                               combine = FALSE) {
  if (is.null(threshold) || is.na(threshold)) threshold <- 95
  target <- rv$analyze_target %||% "undervax_count"

  sd       <- rv$school_data
  sd$GEOID <- as.character(sd$GEOID)
  gcol     <- rv$grade_col
  has_grade <- !is.null(gcol) && gcol %in% names(sd)

  if (isTRUE(combine)) {
    sd <- collapse_schools(sd, rv$school_col, rv$lat_col %||% "latitude",
                           rv$lon_col %||% "longitude", rv$eligible_col, gcol)
  }

  gids_all <- concern_geoids(rv$results, target, scope)
  gids     <- if (!identical(area_pick, "__all__") && nzchar(area_pick)) {
    intersect(area_pick, gids_all)
  } else {
    gids_all
  }

  # Each school's share of its area's total uses ALL schools in the area,
  # before the vaccination-threshold filter is applied.
  tot      <- tapply(sd$.undervax_count, sd$GEOID, sum, na.rm = TRUE)
  area_tot <- unname(tot[sd$GEOID])
  sd$.share <- ifelse(!is.na(area_tot) & area_tot > 0,
                      sd$.undervax_count / area_tot, NA_real_)

  sd         <- sd[sd$GEOID %in% gids, , drop = FALSE]
  n_in_areas <- nrow(sd)
  has_rate   <- any(!is.na(sd$.vax_rate))
  has_count  <- any(!is.na(sd$.undervax_count))
  if (has_rate) {
    sd <- sd[!is.na(sd$.vax_rate) & sd$.vax_rate * 100 < threshold, , drop = FALSE]
  }

  ord_key <- if (has_count) -sd$.undervax_count
             else if (has_rate) sd$.vax_rate
             else seq_len(nrow(sd))
  sd <- sd[order(match(sd$GEOID, gids), ord_key), , drop = FALSE]

  res <- sf::st_drop_geometry(rv$results)
  cls <- if ("hotspot_class" %in% names(res)) res$hotspot_class else res$lisa_class
  cls_lookup <- setNames(as.character(cls), as.character(res$GEOID))

  cols <- list()
  cols[[id_header(rv)]]         <- sd$GEOID
  cols[["Area classification"]] <- unname(cls_lookup[sd$GEOID])
  if (!is.null(rv$school_col) && rv$school_col %in% names(sd)) {
    cols[["School"]] <- as.character(sd[[rv$school_col]])
  }
  if (isTRUE(combine) && ".grades" %in% names(sd)) {
    cols[["Grades"]] <- sd$.grades
  } else if (has_grade) {
    cols[["Grade"]] <- as.character(sd[[gcol]])
  }
  for (cn in extras) {
    if (cn %in% names(sd) && !cn %in% names(cols)) cols[[cn]] <- sd[[cn]]
  }
  if (!is.null(rv$eligible_col) && rv$eligible_col %in% names(sd) &&
      !"Enrolled" %in% names(cols)) {
    cols[["Enrolled"]] <- sd[[rv$eligible_col]]
  }
  if (has_rate)  cols[["% vaccinated"]] <- round(sd$.vax_rate * 100, 1)
  if (has_count) {
    cols[["Est. undervaccinated students"]]       <- round(sd$.undervax_count)
    cols[["Share of area's undervaccinated (%)"]] <- round(sd$.share * 100)
  }

  list(
    table         = as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE),
    n_in_areas    = n_in_areas,
    n_shown       = nrow(sd),
    n_areas_scope = length(gids),
    has_rate      = has_rate,
    threshold     = threshold
  )
}

# Plural of a geography noun ("county" -> "counties", "school district" -> "school districts").
spatialert_pluralize <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return("areas")
  if (x == "county") return("counties")
  if (grepl("[^aeiou]y$", x)) return(sub("y$", "ies", x))
  paste0(x, "s")
}

# Singular noun for a geography level as stored on the results
# ("tract" -> "census tract"; a custom label is used as typed).
geo_noun <- function(level) {
  level <- level %||% "area"
  switch(level,
    tract         = "census tract",
    county        = "county",
    `block group` = "census block group",
    level
  )
}

# IDs of the areas whose schools should be listed, strongest result first.
#   concern = hotspots (or coldspots when the analysis target is the vaccination
#             rate, because there the low-coverage areas are the ones of concern)
#   both    = hotspots and coldspots
#   all     = every area
concern_geoids <- function(results, target, scope = "concern") {
  d   <- sf::st_drop_geometry(results)
  cls <- if ("hotspot_class" %in% names(d)) d$hotspot_class else d$lisa_class
  strength <- if ("gi_star" %in% names(d)) abs(d$gi_star) else abs(d$local_i)
  hot  <- cls %in% c("Hotspot", "High-High")
  cold <- cls %in% c("Coldspot", "Low-Low")
  keep <- switch(scope,
    concern = if (identical(target, "vax_rate")) cold else hot,
    both    = hot | cold,
    all     = rep(TRUE, length(cls))
  )
  keep[is.na(keep)] <- FALSE
  ids <- as.character(d$GEOID)[keep]
  ids[order(-strength[keep])]
}

# Make a flextable fit inside the printable width of a portrait Word page
# (about 6.5 in). Sizing is done AFTER fonts/padding are set, otherwise the
# columns are measured at the default font size and the table overflows the page.
# `widths` (inches, one per column) fixes column widths explicitly; text wraps.
fit_report_table <- function(ft, widths = NULL, max_width = 6.2) {
  ft <- flextable::set_table_properties(ft, layout = "fixed")
  if (!is.null(widths)) {
    widths <- widths * min(1, max_width / sum(widths))
    ft <- flextable::width(ft, j = seq_along(widths), width = widths)
    return(ft)
  }
  ft <- flextable::autofit(ft)
  tryCatch(flextable::fit_to_width(ft, max_width = max_width), error = function(e) ft)
}

# Column widths (inches) for the schools table in the report.
school_report_widths <- function(cols, id_col) {
  w <- vapply(cols, function(cn) {
    if (cn == id_col) 1.0
    else if (cn == "School") 1.6
    else if (cn == "Grade") 1.05
    else if (cn == "Grades") 1.1
    else if (tolower(cn) == "county") 0.9
    else if (cn == "Enrolled") 0.7
    else if (cn == "% vaccinated") 0.9
    else if (cn == "Est. undervaccinated students") 1.1
    else 0.9
  }, numeric(1))
  unname(w)
}

# Small grey paragraph (table notes) in a Word document.
add_small_par <- function(doc, text, bold = FALSE) {
  tryCatch({
    officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(text, officer::fp_text(font.size = 8, bold = bold, color = "#444444"))
    ))
  }, error = function(e) officer::body_add_par(doc, text, style = "Normal"))
}

# Plain-language background for the report. Returns a list of sections, each
# with a heading `h`, and either a paragraph `p` or a character vector `bullets`.
build_background <- function(geo_label, geo_pl, var_label, method, correction,
                             k_list, min_specs, target, loc_str, has_schools,
                             global_g = NULL) {
  is_gi     <- method %in% c("gi_star", "gi_star_consensus")
  hot_word  <- if (is_gi) "hotspot"  else "High-High cluster"
  cold_word <- if (is_gi) "coldspot" else "Low-Low cluster"

  # What "high" means depends on what was analyzed.
  high_meaning <- if (identical(target, "vax_rate")) {
    "higher vaccination coverage than the study area as a whole"
  } else {
    "more undervaccination than the study area as a whole"
  }
  low_meaning <- if (identical(target, "vax_rate")) {
    "lower vaccination coverage than the study area as a whole"
  } else {
    "less undervaccination than the study area as a whole"
  }
  concern_word <- if (identical(target, "vax_rate")) cold_word else hot_word

  correction_txt <- switch(correction,
    fdr        = "The results also account for the fact that many areas are tested at once, using a false discovery rate correction, so that a few areas are not flagged by chance alone.",
    bonferroni = "The results also account for the fact that many areas are tested at once, using a Bonferroni correction, so that a few areas are not flagged by chance alone.",
    "No correction was applied for testing many areas at once, so a small number of areas may be flagged by chance alone."
  )

  consensus_txt <- if (identical(method, "gi_star_consensus") && length(k_list) > 1) {
    paste0(" Because the answer can depend on how \"neighbors\" are defined, the analysis was ",
           "repeated with ", length(k_list), " different neighbor definitions (the ",
           paste(k_list, collapse = ", "), " nearest neighboring ", geo_pl,
           "). An area is only flagged if it is flagged in the same direction in at least ",
           min_specs, " of them.")
  } else {
    ""
  }

  how_p <- if (is_gi) {
    paste0("The analysis uses a statistic called Getis-Ord Gi*. For every ", geo_label,
           ", it compares the values in that ", geo_label, " and its nearby neighbors with the ",
           "average across the whole study area", loc_str, ". A ", geo_label,
           " is flagged as a hotspot when it and its neighbors are, together, significantly ",
           "higher than average, and as a coldspot when they are significantly lower.",
           consensus_txt, " ", correction_txt)
  } else {
    paste0("The analysis uses Local Moran's I. For every ", geo_label, ", it compares that ",
           geo_label, " with its nearby neighbors. A High-High cluster is a ", geo_label,
           " with high values surrounded by other high values; a Low-Low cluster is a low ",
           "value surrounded by other low values. ", correction_txt)
  }

  uses <- c(
    paste0("Focus attention on ", geo_pl, " flagged as ", concern_word, "s. These are places where ",
           high_meaning_for_concern(target), " and where neighboring areas show the same pattern, ",
           "so the result is less likely to be a one-off."),
    if (has_schools) {
      "Use the schools of concern table to see which individual schools in those areas have lower vaccination coverage and could be contacted first."
    },
    "Repeat the analysis for different school years to see whether clusters are growing, shrinking, or moving.",
    "Combine the results with local knowledge, such as known access barriers, recent outbreaks, or community partners. The analysis shows where to look, not why.",
    paste0("Interpret with care: results depend on data quality and on how ", geo_pl,
           " are drawn, and areas with few students can change a lot from year to year.")
  )

  list(
    list(h = "What does this analysis do?",
         p = paste0("This report looks for places where ", var_label, " are unusually concentrated",
                    loc_str, ". Instead of judging each ", geo_label, " on its own, it asks whether a ",
                    geo_label, " and its neighbors tend to look alike, and whether that grouping is ",
                    "stronger than would be expected by chance.")),
    list(h = "How are clusters detected?", p = how_p),
    if (is_gi && isTRUE(global_g$ran)) {
      list(h = "Is there clustering across the whole area?",
           p = paste0("Before looking at individual ", geo_pl, ", a separate whole-area check ",
                      "(the Global G test) asks whether high values tend to be near other high values anywhere ",
                      "in the study area. ",
                      if (global_g$p < 0.05) {
                        paste0("Here the answer was yes (", sprintf("z = %.2f", global_g$z), ", ",
                               if (global_g$p < 0.001) "p < 0.001" else sprintf("p = %.3f", global_g$p),
                               "), so the local hotspots below are not just random scatter. ")
                      } else {
                        paste0("Here there was no clear evidence of this (", sprintf("z = %.2f", global_g$z), ", ",
                               sprintf("p = %.3f", global_g$p),
                               "), so individual hotspots should be read with caution. ")
                      },
                      "This test uses the same neighbor definition as the map",
                      if (identical(global_g$style, "knn")) paste0(" (k = ", global_g$k, ")") else "",
                      ", but it gives one answer for the whole area rather than one per ", geo_label, "."))
    },
    list(h = "What do hotspots and coldspots mean here?",
         p = paste0("A ", hot_word, " is an area with ", high_meaning, ", where its neighbors are ",
                    "also high. A ", cold_word, " is the opposite: an area with ", low_meaning,
                    ", where its neighbors are also low. Areas that are \"not significant\" show no ",
                    "clear evidence of clustering.")),
    list(h = "How can public health teams use these results?", bullets = uses)
  ) |> Filter(f = Negate(is.null))
}

high_meaning_for_concern <- function(target) {
  if (identical(target, "vax_rate")) {
    "vaccination coverage is lower than elsewhere"
  } else {
    "undervaccination is higher than elsewhere"
  }
}

# Definitions printed under the results table, matching the columns present.
build_table_footnotes <- function(cols, method, correction, k_list, geo_label, target) {
  is_gi <- method %in% c("gi_star", "gi_star_consensus")
  high  <- if (identical(target, "vax_rate")) "unusually high vaccination coverage" else "unusually high undervaccination"
  low   <- if (identical(target, "vax_rate")) "unusually low vaccination coverage" else "unusually low undervaccination"
  corr  <- switch(correction,
    fdr        = "false discovery rate (Benjamini-Hochberg)",
    bonferroni = "Bonferroni",
    "no"
  )
  notes <- character(0)
  if (length(cols) > 0 && grepl(" ID$", cols[1])) {
    notes <- c(notes, paste0(cols[1], ": the identifier of each ", geo_label, ", as it appears in the boundary file."))
  }
  if ("County" %in% cols) {
    notes <- c(notes, "County: the county the area is in.")
  }
  if ("Classification" %in% cols) {
    notes <- c(notes, if (is_gi) {
      paste0("Classification: Hotspot = the area and its neighbors show ", high,
             " (statistically significant); Coldspot = ", low,
             " (statistically significant); Not significant = no clear evidence of clustering.")
    } else {
      "Classification: High-High = high values surrounded by high values; Low-Low = low values surrounded by low values; other categories are outliers or not significant."
    })
  }
  z_col <- grep("^Gi\\* Z-Score", cols, value = TRUE)
  if (length(z_col) > 0) {
    notes <- c(notes, paste0(
      z_col[1], ": a standardized measure of how strongly an area and its neighbors cluster. ",
      "Positive values mean the neighborhood is above average, negative values mean it is below average, ",
      "and larger absolute values mean a stronger pattern. Values above about +1.96 or below -1.96 are unusual at the 5% level before adjusting for testing many areas.",
      if (grepl("mean across k", z_col[1])) " The mean is taken across the neighbor definitions that were run." else ""
    ))
  }
  if ("Local I Statistic" %in% cols) {
    notes <- c(notes, "Local I Statistic: positive values mean an area is similar to its neighbors; negative values mean it differs from them. Larger absolute values mean a stronger pattern.")
  }
  if ("Adjusted P-Value" %in% cols) {
    notes <- c(notes, paste0("Adjusted P-Value: the probability of seeing a pattern at least this strong by chance, adjusted for testing many areas at once (", corr,
                             " correction). Smaller values mean stronger evidence."))
  }
  if ("Hotspot in # k-specs" %in% cols) {
    notes <- c(notes, "Hotspot in # k-specs: the number of neighbor definitions (specifications) under which the area was a statistically significant hotspot.")
  }
  if ("Coldspot in # k-specs" %in% cols) {
    notes <- c(notes, "Coldspot in # k-specs: the number of neighbor definitions under which the area was a statistically significant coldspot.")
  }
  if ("Total k-specs run" %in% cols) {
    notes <- c(notes, paste0("Total k-specs run: the total number of neighbor definitions tested",
                             if (length(k_list) > 1) paste0(" (k = ", paste(k_list, collapse = ", "), " nearest neighbors)") else "",
                             "."))
  }
  notes
}

# In-app FAQ shown at the bottom of the results page.
results_faq_ui <- function() {
  bslib::card(
    card_header("Frequently asked questions"),
    bslib::card_body(
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "What is a hotspot?",
          p("A hotspot is an area where the value being analyzed is unusually high compared with the ",
            "study area as a whole, and where the neighboring areas are high too. The clustering has to be ",
            "strong enough that it is unlikely to be due to chance alone."),
          p(tags$strong("Note: "), "'High' depends on what you analyzed. If you analyzed undervaccination ",
            "(count or rate), a hotspot is a place with ", tags$em("more"), " undervaccination. If you ",
            "analyzed the vaccination rate, a hotspot is a place with ", tags$em("higher"),
            " coverage, and the low-coverage areas are the coldspots.")
        ),
        bslib::accordion_panel(
          "What is a coldspot?",
          p("The opposite of a hotspot: an area, together with its neighbors, where the value is ",
            "unusually low and the pattern is statistically significant.")
        ),
        bslib::accordion_panel(
          "What is a Gi* z-score and how do I interpret it?",
          p("The Gi* z-score measures how strongly an area and its neighbors cluster. A positive score ",
            "means the neighborhood is above the study-area average (hotspot direction); a negative score ",
            "means it is below average (coldspot direction). The further from zero, the stronger the ",
            "pattern. As a rough guide, scores beyond about +1.96 or -1.96 are unusual at the 5% level ",
            "before adjusting for the number of areas tested."),
          p("The score describes the ", tags$em("neighborhood"), ", not just the single area, so an area ",
            "with a modest value can still be a hotspot if its neighbors are all high.")
        ),
        bslib::accordion_panel(
          "What does \"significant\" or \"adjusted p-value\" mean?",
          p("The p-value is the chance of seeing a pattern at least this strong if the values were ",
            "scattered randomly. Because hundreds or thousands of areas are tested at once, some would look ",
            "unusual by luck alone, so the p-values can be adjusted. The options are a false discovery rate ",
            "correction, a Bonferroni correction, or none (the published analysis used none). Smaller adjusted ",
            "p-values mean stronger evidence; an area is called significant when its adjusted p-value is ",
            "below the chosen threshold (0.05 by default).")
        ),
        bslib::accordion_panel(
          "What are \"k-specs\" and why were several run?",
          p("To decide which areas are 'neighbors', the analysis uses the k nearest areas. There is no single ",
            "correct k, so the analysis is repeated for several values (for example 4, 6, 8, 10, and 12), ",
            "each called a specification or 'k-spec'. 'Hotspot in # k-specs' shows how many of those runs ",
            "flagged the area. By default an area is classified as a hotspot or coldspot only if it is flagged ",
            "in at least 2 of the specifications, which makes the result more stable.")
        ),
        bslib::accordion_panel(
          "Does a hotspot mean something is wrong in that area?",
          p("Not necessarily. A hotspot shows ", tags$em("where"), " to look, not ", tags$em("why"),
            ". Results depend on data quality, how areas are drawn, and how many students are in each area. ",
            "Use them together with local knowledge to decide where to focus outreach or follow-up.")
        ),
        bslib::accordion_panel(
          "Why is an area flagged as a hotspot when it has no schools?",
          p("A hotspot is about an area ", tags$strong("and its neighbors"), ", not an area on its own. For each ",
            "area, the analysis combines the value in that area with the values in the areas around it, then asks ",
            "whether that neighborhood is unusually high compared with the study area as a whole. An area with no ",
            "schools contributes a value of zero by itself, but if the areas around it have many undervaccinated ",
            "students, the neighborhood as a whole can still stand out. (Which areas count as \"neighbors\" depends ",
            "on the weights you chose, such as areas that share a border or the nearest areas, but the idea is the same.)"),
          p("Think of a hotspot as showing where undervaccinated students are ", tags$em("statistically significantly clustered"),
            ", not as a count of students inside that exact area. The school points show where the data were recorded, ",
            "and the surrounding area is used as an estimate of where those students are likely to live. A child may ",
            "attend a school in a neighboring area, so a no-school area inside or next to a cluster is still part of the picture."),
          p("To see what is driving a result, look at where the schools are. The ", tags$strong("Schools of concern"),
            " tab and the school points on the Explore and results maps show which schools are in and around a flagged area.")
        ),
        bslib::accordion_panel(
          "How are areas with no schools handled in the analysis?",
          p("On the Analysis tab, the ", tags$strong("Areas with no schools"), " setting controls this. ",
            tags$strong("Count as zero"), " keeps them in the analysis with a value of 0 (the default when analyzing the count of ",
            "undervaccinated students, as in the published analysis). ", tags$strong("Exclude from the analysis"),
            " drops them (the default for rates, since an area with no students has no rate). ",
            tags$strong("Use the study-area average"), " keeps them but gives them a neutral value."),
          p("Counting a rate as zero would treat an area with no schools as 0% vaccinated, which can create artificial ",
            "clusters, so it is not recommended for rate analyses.")
        ),
        bslib::accordion_panel(
          "Why are some schools or areas missing from my results?",
          p("Schools are left out if their enrollment or vaccination value is missing or suppressed (for example ",
            "'<10'), if they have no coordinates, or if their location falls outside the boundaries you loaded. ",
            "Areas with no schools are handled according to the 'Areas with no schools' setting on the Analysis tab: ",
            "counted as zero (the default for counts), excluded from the analysis (the default for rates), or given the ",
            "study-area average. ",
            "The warnings on the Data & Geography tab and the methods text report how many schools were excluded.")
        ),
        bslib::accordion_panel(
          "What is the 'Schools of concern' tab for?",
          p("It lists the individual schools located in the areas of concern whose vaccination rate is below a ",
            "threshold you choose (95% by default), so a health department can follow up with specific schools. ",
            "It is only available when you upload school-level data (Mode A). The same list can be downloaded as ",
            "a CSV and, optionally, added to the Word report.")
        )
      )
    )
  )
}

`%||%` <- function(x, y) if (!is.null(x)) x else y
