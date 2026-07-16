# mod_analysis.R — Analysis configuration and execution module
# Spatial weights live here (moved from geography tab). Gi* is the only
# statistical method exposed in the UI (Local Moran's I remains available in
# the underlying R package but is not surfaced here, to keep the workflow
# simpler — see compute_local_moran() in R/analysis.R if needed).
#
# The weights/method settings panel is rendered entirely server-side
# (output$settings_ui), rebuilt each time the "Use defaults" switch is
# toggled. This is deliberate: individually disabling already-rendered,
# dynamically-shown inputs (e.g. a slider inside a conditionalPanel) with
# shinyjs is fragile — elements that don't exist yet when disable() runs
# silently fail to lock. Rendering the whole panel fresh, with every input
# either normal or wrapped in shinyjs::disabled() from the moment it's
# created, guarantees the locked state is correct regardless of timing.

PAPER_DEFAULTS <- list(
  weights_style  = "knn",
  knn_consensus  = TRUE,
  knn_k_list     = "4,6,8,10,12",
  min_specs      = 2,
  knn_k          = 8,
  weights_type   = "B",
  correction     = "none",
  alpha          = 0.05
)

mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(class = "d-flex align-items-center justify-content-between p-2 mb-2",
        style = "background-color: #f5f2fa; border: 1px solid #522D80; border-radius: 6px;",
      div(
        tags$strong("Use defaults", style = "color: #333333;"),
        tags$a("*", href = "https://doi.org/10.1056/NEJMc2604004", target = "_blank",
               title = "See the published NEJM paper these defaults replicate",
               style = "color: #522D80; text-decoration: none; font-weight: 700; margin-left: 1px;"),
        uiOutput(ns("paper_defaults_state_label"))
      ),
      bslib::input_switch(ns("paper_defaults"), label = NULL, value = FALSE)
    ),
    helpText(
      "When on, this locks weights, method, and thresholds to match the published ",
      tags$a("NEJM analysis*", href = "https://doi.org/10.1056/NEJMc2604004", target = "_blank"),
      ": KNN consensus (k = 4, 6, 8, 10, 12; hotspot if significant in \u22652 of 5), ",
      "no multiple-comparisons correction, \u03b1 = 0.05. Turn it off to make your own selections."
    ),
    actionButton(
      ns("run_defaults"),
      "Run default analysis",
      class = "btn-outline-primary btn-sm w-100 mb-1",
      icon  = icon("bolt")
    ),
    uiOutput(ns("paper_defaults_var_note")),

    hr(),

    # ── What are we analyzing? (set on the upload step; shown here as a
    #    read-only confirmation banner so it's visible right before running) ──
    uiOutput(ns("analyze_banner")),

    hr(),

    # ── Neighbor connectivity FYI (computed on the Data & Geography tab when
    #    geography loads; shown here since it directly informs the weights
    #    choice made on this tab) ──────────────────────────────────────────
    uiOutput(ns("neighbor_summary_ui")),

    # ── Spatial weights + Analysis settings — entirely server-rendered ──────
    uiOutput(ns("settings_ui")),

    hr(),
    textInput(
      ns("var_label"),
      "Report label — how to describe the pattern being detected",
      value = "undervaccinated individuals",
      placeholder = "e.g. undervaccinated kindergartners"
    ),
    helpText(
      "Used in the exported methods text, e.g. \u201c...to identify geographic ",
      "clusters of [this].\u201d Auto-suggested based on what you're analyzing \u2014 ",
      "edit it any time to customize the wording, e.g. \u201cundervaccinated ",
      "kindergartners\u201d instead of the generic default."
    ),

    hr(),
    actionButton(
      ns("run"),
      "Run analysis",
      class = "btn-primary w-100",
      icon  = icon("play")
    ),

    uiOutput(ns("run_status"))
  )
}


mod_analysis_server <- function(id, rv, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Suggested report-label wording per analyze target. Kept in one place so
    # the auto-update logic below can tell "still the default" apart from
    # "user customized this" — if the current value matches any of these
    # known defaults, it's safe to swap; if it's something else, leave it.
    VAR_LABEL_DEFAULTS <- c(
      undervax_count = "undervaccinated individuals",
      undervax_rate  = "undervaccination rates",
      vax_rate       = "vaccination rates"
    )

    # Auto-suggest the report label based on what's being analyzed, so
    # switching from count to rate (or vice versa) doesn't leave a stale,
    # mismatched label behind. Only swaps the value if it still matches a
    # known default (i.e. the user hasn't customized it) or is empty.
    observeEvent(rv$analyze_target, {
      req(rv$analyze_target)
      current <- trimws(input$var_label %||% "")
      if (current == "" || current %in% VAR_LABEL_DEFAULTS) {
        updateTextInput(session, "var_label",
                        value = VAR_LABEL_DEFAULTS[[rv$analyze_target]])
      }
    }, ignoreInit = FALSE)

    # Holds the user's own settings so they're restored (not reset to
    # defaults) when the paper-defaults switch is turned back off.
    saved_cfg <- reactiveVal(PAPER_DEFAULTS)

    maybe_disable <- function(el, locked) {
      if (locked) shinyjs::disabled(el) else el
    }

    build_settings_ui <- function(cfg, locked) {
      tagList(
        h5("Spatial weights", class = "text-primary fw-bold"),

        maybe_disable(
          selectInput(
            ns("weights_style"),
            "Neighbour definition",
            choices = c(
              "Queen (shared edge or vertex)" = "queen",
              "Rook (shared edge only)"       = "rook",
              "K-nearest neighbours (KNN)"    = "knn"
            ),
            selected = cfg$weights_style
          ),
          locked
        ),

        conditionalPanel(
          condition = paste0("input['", ns("weights_style"), "'] == 'knn'"),

          maybe_disable(
            checkboxInput(
              ns("knn_consensus"),
              "Run consensus across multiple k values (recommended)",
              value = cfg$knn_consensus
            ),
            locked
          ),

          conditionalPanel(
            condition = paste0("input['", ns("knn_consensus"), "']"),
            maybe_disable(
              textInput(ns("knn_k_list"), "k values to test (comma-separated)",
                        value = cfg$knn_k_list),
              locked
            ),
            maybe_disable(
              sliderInput(ns("min_specs"),
                          "Minimum specifications to call a hotspot",
                          min = 1, max = length(parse_k_list(cfg$knn_k_list)),
                          value = cfg$min_specs, step = 1),
              locked
            ),
            helpText(
              "Gi* is run separately for each k. A unit is called a robust hotspot/coldspot ",
              "only if it is significant in at least this many of the specifications \u2014 ",
              "this is a robustness check across neighbourhood-size assumptions, not a single ",
              "arbitrary choice of k."
            )
          ),

          conditionalPanel(
            condition = paste0("!input['", ns("knn_consensus"), "']"),
            maybe_disable(
              sliderInput(ns("knn_k"), "Number of neighbours (k)",
                          min = 3, max = 15, value = cfg$knn_k),
              locked
            )
          )
        ),

        maybe_disable(
          selectInput(
            ns("weights_type"),
            "Weights coding",
            choices = c(
              "Binary — each neighbour counts as 1 (recommended for counts)" = "B",
              "Row-standardized — weights sum to 1 per area (for rates)"     = "W"
            ),
            selected = cfg$weights_type
          ),
          locked
        ),
        div(
          class = "alert alert-light border mb-2",
          style = "font-size: 0.82rem; line-height: 1.5; color: #333333;",
          tags$strong("Which weights should I use?"),
          tags$ul(
            class = "mb-0 mt-1 ps-3",
            tags$li(tags$strong("Queen / Rook:"), " Areas sharing a border are neighbours.
                    Only sensible when the geography tiles without gaps and has no islands
                    \u2014 check the connectivity FYI panel above first."),
            tags$li(tags$strong("K-nearest neighbours (KNN):"), " Each area's k closest
                    centroids are neighbours. Guarantees connectivity for every unit, which is
                    why it's the default here. Running a consensus across several k values
                    (rather than picking one) avoids results hinging on an arbitrary choice."),
            tags$li(tags$strong("Binary:"), " Each neighbour gets equal weight of 1.
                    Recommended for counts (e.g. undervaccinated students)."),
            tags$li(tags$strong("Row-standardized:"), " Weights sum to 1 per area.
                    Better suited for rates or proportions.")
          )
        ),

        hr(),

        h5("Analysis settings", class = "text-primary fw-bold"),

        div(class = "alert alert-info", style = "color: #333333;",
          icon("circle-info"), " ",
          "Method: ", tags$strong("Getis-Ord Gi* (hotspot analysis)"),
          ". Gi* identifies areas where high (or low) values cluster together in space. ",
          "Positive and significant = hotspot; negative and significant = coldspot."
        ),

        maybe_disable(
          selectInput(
            ns("correction"),
            "Multiple comparisons correction",
            choices = c(
              "False discovery rate (recommended)" = "fdr",
              "Bonferroni (conservative)"          = "bonferroni",
              "None"                               = "none"
            ),
            selected = cfg$correction
          ),
          locked
        ),
        div(
          class = "alert alert-light border mb-2",
          style = "font-size: 0.82rem; line-height: 1.5; color: #333333;",
          tags$strong("Which correction should I use?"),
          tags$ul(
            class = "mb-0 mt-1 ps-3",
            tags$li(tags$strong("False discovery rate (FDR):"), " Controls the expected
                    proportion of false positives among significant results. Less
                    conservative than Bonferroni — recommended for most exploratory
                    public health analyses."),
            tags$li(tags$strong("Bonferroni:"), " Divides the significance threshold by
                    the number of tests. Very conservative — best when you need strict
                    control of false positives, such as confirmatory studies."),
            tags$li(tags$strong("None:"), " No correction applied. More hotspots will be
                    identified but some may be false positives. Not recommended unless
                    you have a specific reason.")
          )
        ),

        maybe_disable(
          sliderInput(
            ns("alpha"),
            "Significance threshold (\u03b1)",
            min = 0.01, max = 0.10, value = cfg$alpha, step = 0.01
          ),
          locked
        )
      )
    }

    # Capture the current in-progress settings (used so turning the switch
    # off restores what the user had, rather than resetting to app defaults).
    current_cfg <- function() {
      list(
        weights_style = input$weights_style %||% PAPER_DEFAULTS$weights_style,
        knn_consensus = input$knn_consensus %||% PAPER_DEFAULTS$knn_consensus,
        knn_k_list    = input$knn_k_list    %||% PAPER_DEFAULTS$knn_k_list,
        min_specs     = input$min_specs     %||% PAPER_DEFAULTS$min_specs,
        knn_k         = input$knn_k         %||% PAPER_DEFAULTS$knn_k,
        weights_type  = input$weights_type  %||% PAPER_DEFAULTS$weights_type,
        correction    = input$correction    %||% PAPER_DEFAULTS$correction,
        alpha         = input$alpha         %||% PAPER_DEFAULTS$alpha
      )
    }

    # Initial render (switch starts off)
    output$settings_ui <- renderUI({
      build_settings_ui(saved_cfg(), locked = FALSE)
    })

    output$paper_defaults_state_label <- renderUI({
      if (isTRUE(input$paper_defaults)) {
        tags$div(style = "font-size: 0.78rem; color: #1e7e34; font-weight: 600;",
                 "On \u2014 weights & thresholds locked to match the published analysis")
      } else {
        tags$div(style = "font-size: 0.78rem; color: #555555;",
                 "Off \u2014 set your own weights and thresholds below")
      }
    })

    observeEvent(input$knn_k_list, {
      req(!isTRUE(input$paper_defaults))
      k_vals <- parse_k_list(input$knn_k_list)
      n_k    <- max(length(k_vals), 1)
      updateSliderInput(session, "min_specs", max = n_k,
                        value = min(input$min_specs %||% 2, n_k))
    }, ignoreInit = TRUE)

    observeEvent(input$paper_defaults, {
      if (isTRUE(input$paper_defaults)) {
        # Save whatever the user currently has before overwriting with paper defaults
        saved_cfg(current_cfg())
        output$settings_ui <- renderUI({
          build_settings_ui(PAPER_DEFAULTS, locked = TRUE)
        })

        if (!identical(rv$analyze_target, "undervax_count")) {
          showNotification(
            paste0(
              "Note: the paper analyzed the count of undervaccinated students. ",
              "Your current analysis variable is '", rv$analyze_target_label %||% "unset",
              "'. Go to the Data & Geography tab and set 'What do you want to analyze?' ",
              "to 'Count of undervaccinated students' to fully match the paper."
            ),
            type = "warning", duration = 12
          )
        }
      } else {
        output$settings_ui <- renderUI({
          build_settings_ui(saved_cfg(), locked = FALSE)
        })
      }
    }, ignoreInit = TRUE)

    output$paper_defaults_var_note <- renderUI({
      req(isTRUE(input$paper_defaults))
      if (!identical(rv$analyze_target, "undervax_count")) {
        div(class = "alert alert-warning mt-1", style = "color: #333333;",
          icon("triangle-exclamation"), " ",
          "Paper defaults are on, but you're currently analyzing '",
          rv$analyze_target_label %||% "unset", "' rather than the count of ",
          "undervaccinated students used in the paper. Change this on the Data & ",
          "Geography tab to fully match the published methods."
        )
      }
    })

    # ── "You are analyzing: ___" banner, pulled from what was set at upload ──
    output$analyze_banner <- renderUI({
      if (is.null(rv$analyze_target_label)) {
        div(class = "alert alert-warning", style = "color: #333333;",
          icon("triangle-exclamation"), " ",
          "No analysis variable set yet. Go to the Data & Geography tab, ",
          "upload your data, and choose what to analyze."
        )
      } else {
        source_desc <- if (identical(rv$source_type, "direct")) {
          paste0("directly from ", rv$direct_col %||% "your column")
        } else {
          paste0("derived from ", rv$vaccinated_col %||% "?", " & ", rv$eligible_col %||% "?")
        }
        div(class = "alert border",
          style = "background-color: #f5f2fa; border-color: #522D80 !important; color: #333333;",
          icon("bullseye"), " ",
          tags$strong("You are analyzing: "), rv$analyze_target_label,
          tags$br(),
          tags$span(style = "font-size: 0.82rem; color: #555;",
                    "(", source_desc, ") \u2014 change this on the Data & Geography tab.")
        )
      }
    })

    # ── Neighbor connectivity FYI, computed on the geography tab ────────────
    output$neighbor_summary_ui <- renderUI({
      nb_summary <- rv$neighbor_summary
      req(nb_summary)
      fmt <- function(x) round(x, 1)
      row <- function(label, s) {
        zero_note <- if (s$n_zero > 0) {
          span(style = "color: #c0392b;",
               glue::glue(" \u2014 {s$n_zero} unit(s) with 0 neighbors"))
        } else {
          span(style = "color: #1e7e34;", " \u2014 no islands")
        }
        tags$li(
          tags$strong(label), glue::glue(
            ": mean {fmt(s$mean)}, median {fmt(s$median)}, range {s$min}\u2013{s$max}"
          ), zero_note
        )
      }
      div(class = "alert alert-light border mb-3",
        style = "font-size: 0.82rem; line-height: 1.5; color: #333333;",
        icon("circle-info"), " ",
        tags$strong("FYI \u2014 neighbor connectivity (queen & rook):"),
        tags$ul(
          class = "mb-1 mt-1 ps-3",
          row("Queen", nb_summary$queen),
          row("Rook",  nb_summary$rook)
        ),
        "If either has 0-neighbor units, queen/rook weights will exclude those units ",
        "from the analysis. Use K-nearest neighbours (KNN) below to force connectivity ",
        "for every unit instead."
      )
    })

    # Human-readable, pluralized label for the geography level, used in the
    # results summary sentence (e.g. "census tract" / "census tracts",
    # "county" / "counties").
    geo_level_label <- function(level, n) {
      base <- switch(level %||% "tract",
        tract         = "census tract",
        county        = "county",
        `block group` = "census block group",
        custom        = "area",
        "area"
      )
      if (isTRUE(n == 1)) return(base)
      if (base == "county") return("counties")
      paste0(base, "s")
    }

    # Shared analysis runner, used both by the "Run analysis" button (reads
    # current UI inputs) and "Run default analysis" (passes paper defaults
    # directly, so it works instantly without waiting on the settings panel
    # to finish re-rendering).
    do_run <- function(w_style, w_type, use_consensus, k_vals, min_specs,
                        knn_k, correction, alpha) {
      req(rv$geo, rv$joined_data)

      var_col <- rv$value_col %||% ".value"

      if (is.null(rv$analyze_target)) {
        output$run_status <- renderUI({
          div(class = "alert alert-danger mt-2", style = "color: #333333;",
            icon("circle-xmark"), " ",
            "No analysis variable set. Please go to the Data & Geography tab ",
            "and choose what to analyze before running."
          )
        })
        return(invisible())
      }

      if (!var_col %in% names(rv$joined_data)) {
        output$run_status <- renderUI({
          div(class = "alert alert-danger mt-2", style = "color: #333333;",
            icon("circle-xmark"), " ",
            "Analysis column not found. Please re-upload your data and confirm it first."
          )
        })
        return(invisible())
      }

      output$run_status <- renderUI({
        div(class = "alert alert-info mt-2", style = "color: #333333;",
          icon("spinner", class = "fa-spin"), " Building weights and running analysis...")
      })

      tryCatch({

        if (use_consensus) {
          results <- compute_gi_star_consensus(
            geo          = rv$joined_data,
            var          = var_col,
            k_values     = k_vals,
            weights_type = w_type,
            correction   = correction,
            alpha        = alpha,
            min_specs    = min_specs
          )

          rv$weights_style <- "knn_consensus"
          rv$knn_k_list    <- k_vals
          rv$min_specs     <- min_specs
          rv$weights_type  <- w_type

        } else {
          weights <- spatialert_build_weights(
            rv$joined_data,
            style        = w_style,
            k            = knn_k,
            weights_type = w_type
          )

          rv$weights       <- weights
          rv$weights_style <- w_style
          rv$weights_type  <- w_type
          rv$knn_k         <- knn_k

          results <- compute_gi_star(
            geo        = rv$joined_data,
            var        = var_col,
            weights    = weights,
            correction = correction,
            alpha      = alpha
          )
        }

        attr(results, "spatialert_geo_level")     <- attr(rv$geo, "spatialert_geo_level")
        attr(results, "spatialert_var_label")      <- input$var_label
        attr(results, "spatialert_correction")     <- correction
        attr(results, "spatialert_alpha")          <- alpha
        attr(results, "spatialert_analyze_target") <- rv$analyze_target

        rv$results <- results

        n_total <- nrow(results)
        n_hot   <- sum(results$hotspot_class == "Hotspot",         na.rm = TRUE)
        n_cold  <- sum(results$hotspot_class == "Coldspot",        na.rm = TRUE)
        n_ns    <- sum(results$hotspot_class == "Not significant", na.rm = TRUE)

        geo_lvl    <- attr(results, "spatialert_geo_level")
        hot_label  <- geo_level_label(geo_lvl, n_hot)
        cold_label <- geo_level_label(geo_lvl, n_cold)
        all_label  <- geo_level_label(geo_lvl, n_total)

        summary_msg <- glue::glue(
          "Out of {n_total} {all_label} analyzed, {n_hot} significant hotspot ",
          "{hot_label} and {n_cold} significant coldspot {cold_label} were ",
          "identified at \u03b1 = {alpha} ({n_ns} not significant)."
        )
        if (use_consensus) {
          summary_msg <- glue::glue(
            "{summary_msg} Significance required consensus across k = ",
            "{paste(k_vals, collapse = ', ')} (\u2265{min_specs} of {length(k_vals)} ",
            "specifications)."
          )
        }

        output$run_status <- renderUI({
          tagList(
            div(class = "alert alert-success mt-2", style = "color: #333333;",
              icon("circle-check"), " Analysis complete. ", summary_msg
            ),
            actionButton(
              ns("go_results"),
              "View full results \u2192",
              class = "btn-outline-primary w-100 mt-1"
            )
          )
        })

        observeEvent(input$go_results, {
          updateNavbarPage(parent_session, inputId = "navbar", selected = "tab_results")
        }, once = TRUE)

      }, error = function(e) {
        output$run_status <- renderUI({
          div(class = "alert alert-danger mt-2", style = "color: #333333;",
            icon("circle-xmark"), " ",
            paste("Analysis failed:", conditionMessage(e))
          )
        })
      })
    }

    observeEvent(input$run, {
      w_style <- input$weights_style %||% "knn"
      w_type  <- if (!is.null(input$weights_type) && nchar(input$weights_type) > 0) input$weights_type else "B"
      use_consensus <- identical(w_style, "knn") && isTRUE(input$knn_consensus)
      k_vals    <- parse_k_list(input$knn_k_list)
      min_specs <- input$min_specs %||% min(2, length(k_vals))
      knn_k     <- if (!is.null(input$knn_k) && !is.na(input$knn_k)) input$knn_k else 8

      do_run(
        w_style = w_style, w_type = w_type, use_consensus = use_consensus,
        k_vals = k_vals, min_specs = min_specs, knn_k = knn_k,
        correction = input$correction, alpha = input$alpha
      )
    })

    # "Run default analysis" — turns the paper-defaults switch on (so the
    # settings panel reflects what actually ran) and runs immediately with
    # PAPER_DEFAULTS directly, rather than waiting for the switch's
    # observer/re-render cycle to populate the inputs first.
    observeEvent(input$run_defaults, {
      saved_cfg(current_cfg())
      updateCheckboxInput(session, "paper_defaults", value = TRUE)
      output$settings_ui <- renderUI({
        build_settings_ui(PAPER_DEFAULTS, locked = TRUE)
      })

      d <- PAPER_DEFAULTS
      do_run(
        w_style       = d$weights_style,
        w_type        = d$weights_type,
        use_consensus = d$knn_consensus,
        k_vals        = parse_k_list(d$knn_k_list),
        min_specs     = d$min_specs,
        knn_k         = d$knn_k,
        correction    = d$correction,
        alpha         = d$alpha
      )
    })
  })
}

parse_k_list <- function(txt) {
  vals <- suppressWarnings(as.integer(trimws(strsplit(txt %||% "4,6,8,10,12", ",")[[1]])))
  vals <- vals[!is.na(vals) & vals >= 1]
  if (length(vals) == 0) vals <- c(4, 6, 8, 10, 12)
  sort(unique(vals))
}

`%||%` <- function(x, y) if (!is.null(x)) x else y
