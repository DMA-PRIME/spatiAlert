# mod_upload.R — Data upload and validation module

mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("1. Upload your data", class = "text-primary fw-bold"),

    radioButtons(
      ns("mode"),
      label = "Data format",
      choices = c(
        "Mode A — School / facility level" = "facility",
        "Mode B — Pre-aggregated (tract, county, ZIP)" = "aggregate"
      )
    ),

    fileInput(
      ns("file"),
      label       = "Choose file (CSV or Excel)",
      accept      = c(".csv", ".xlsx", ".xls"),
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),

    uiOutput(ns("column_ui")),
    uiOutput(ns("validation_msgs")),

    # Static confirm button
    actionButton(ns("confirm"), "Confirm data",
                 class = "btn-primary w-100 mt-2",
                 icon  = icon("check"))
  )
}


mod_upload_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    raw_data <- reactive({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      tryCatch({
        switch(ext,
          csv  = readr::read_csv(input$file$datapath, show_col_types = FALSE),
          xlsx = readxl::read_xlsx(input$file$datapath),
          xls  = readxl::read_xls(input$file$datapath),
          NULL
        )
      }, error = function(e) NULL)
    })

    # ── Column mapping UI ────────────────────────────────────────────────────
    output$column_ui <- renderUI({
      req(raw_data())
      cols        <- names(raw_data())
      col_choices <- setNames(cols, cols)

      tagList(
        hr(),
        h6("Map your columns"),

        # Geographic ID only for Mode B
        if (input$mode == "aggregate") {
          selectInput(ns("id_col"), "Geographic ID column (GEOID / FIPS)",
                      choices = col_choices)
        },

        # ── Step A: what does the user want to analyze? ─────────────────────
        radioButtons(ns("analyze_target"),
                     "What do you want to analyze?",
                     choices = c(
                       "Count of undervaccinated students"        = "undervax_count",
                       "Undervaccination rate (% not vaccinated)" = "undervax_rate",
                       "Vaccination rate (% vaccinated)"          = "vax_rate"
                     )),
        helpText(
          "This is the variable the hotspot analysis will actually run on. ",
          "Choose based on your question: ", tags$strong("count"), " highlights areas ",
          "with the largest absolute burden (useful for resource allocation); ",
          tags$strong("rate"), " highlights areas with the highest relative risk, ",
          "comparable across unevenly-sized areas."
        ),

        uiOutput(ns("source_ui")),
        uiOutput(ns("calc_explainer")),

        # Mode A: lat/lon columns
        if (input$mode == "facility") {
          tagList(
            hr(),
            h6("Location columns"),
            selectInput(ns("lat_col"), "Latitude column",
                        choices  = col_choices,
                        selected = grep("lat", cols, ignore.case = TRUE, value = TRUE)[1]),
            selectInput(ns("lon_col"), "Longitude column",
                        choices  = col_choices,
                        selected = grep("lon|lng", cols, ignore.case = TRUE, value = TRUE)[1])
          )
        }
      )
    })

    # ── Step B: how is that target derived? ────────────────────────────────
    output$source_ui <- renderUI({
      req(raw_data(), input$analyze_target)
      cols        <- names(raw_data())
      col_choices <- setNames(cols, cols)

      target_label <- switch(input$analyze_target,
        undervax_count = "undervaccinated count",
        undervax_rate  = "undervaccination rate",
        vax_rate       = "vaccination rate"
      )

      direct_label <- paste0("I have a column with the ", target_label, " directly")
      derive_label <- "Calculate it from eligible population + vaccinated count or rate"

      formula_str <- switch(input$analyze_target,
        undervax_count = "Undervaccinated count = eligible \u00d7 (1 \u2212 vaccination rate)",
        undervax_rate  = "Undervaccination rate = 1 \u2212 vaccination rate",
        vax_rate       = "Vaccination rate = vaccinated / eligible (or entered directly)"
      )

      tagList(
        hr(),
        radioButtons(ns("source_type"),
                     paste0("How is your ", target_label, " recorded?"),
                     choices = setNames(c("direct", "derive"),
                                        c(direct_label, derive_label))),

        # -- Direct path --
        conditionalPanel(
          condition = paste0("input['", ns("source_type"), "'] == 'direct'"),
          selectInput(ns("direct_col"),
                      paste0(tools::toTitleCase(target_label), " column"),
                      choices = col_choices),
          if (input$analyze_target != "undervax_count") {
            radioButtons(ns("direct_pct_scale"),
                         "Scale",
                         choices = c(
                           "0 to 100 (e.g. 85.3%)" = "100",
                           "0 to 1 (e.g. 0.853)"   = "1"
                         ))
          },
          helpText(paste0(
            "This column will be used directly as the ", target_label, " for the hotspot analysis."
          )),

          # Weighting is a separate question from where the rate came from —
          # only relevant for Mode A (facility-level data gets aggregated up
          # to a geography) and only for rate targets (counts sum, so there's
          # nothing to weight).
          if (input$mode == "facility" && input$analyze_target != "undervax_count") {
            tagList(
              hr(),
              selectInput(ns("weight_col"),
                          "Eligible population column (for weighting when combining schools into each area)",
                          choices = c(
                            "I don't have this \u2014 use unweighted average (not recommended)" = "__none__",
                            col_choices
                          )),
              div(class = "alert alert-light border mt-1 mb-0",
                  style = "font-size: 0.8rem; color: #333333;",
                  "When multiple schools fall in the same area, this determines how their ",
                  "rates are combined: a school of 20 students and a school of 2,000 will ",
                  "count equally without a population column, which can distort the area's ",
                  "rate. This column is used only to weight the combination \u2014 it does not ",
                  "change the rate value used above."
              )
            )
          }
        ),

        # -- Derive path --
        conditionalPanel(
          condition = paste0("input['", ns("source_type"), "'] == 'derive'"),
          selectInput(ns("eligible_col"),
                      "Eligible population column (total enrolled / total eligible)",
                      choices = col_choices),
          radioButtons(ns("vaccinated_format"),
                       "How is vaccinated status recorded?",
                       choices = c(
                         "Number vaccinated (count)"   = "count",
                         "Percent or rate vaccinated"  = "percent"
                       )),
          selectInput(ns("vaccinated_col"),
                      "Vaccinated column",
                      choices = col_choices),
          conditionalPanel(
            condition = paste0("input['", ns("vaccinated_format"), "'] == 'percent'"),
            radioButtons(ns("derive_pct_scale"),
                         "Percent scale",
                         choices = c(
                           "0 to 100 (e.g. 85.3%)" = "100",
                           "0 to 1 (e.g. 0.853)"   = "1"
                         ))
          ),
          div(class = "alert alert-light border mt-1 mb-0",
              style = "font-size: 0.8rem; color: #333333;",
              tags$strong(formula_str)
          ),
          if (input$mode == "facility") {
            helpText(
              "When multiple schools fall in the same area, the eligible-population ",
              "column above is also used to weight how their rates are combined ",
              "(larger schools count proportionally more)."
            )
          }
        )
      )
    })

    # ── Live "what is actually being calculated" table ──────────────────────
    # Shows the real column names selected, not just a generic formula, so
    # the user can check at a glance that the mapping is correct before
    # confirming.
    output$calc_explainer <- renderUI({
      req(input$analyze_target, input$source_type)

      col <- function(x) tags$code(x %||% "\u2014 not selected \u2014")

      rows <- list()

      if (input$source_type == "direct") {
        target_label <- switch(input$analyze_target,
          undervax_count = "Undervaccinated count",
          undervax_rate  = "Undervaccination rate",
          vax_rate       = "Vaccination rate"
        )
        rows[[length(rows) + 1]] <- tags$tr(
          tags$td(tags$strong(target_label)),
          tags$td("= ", col(input$direct_col), " (used as-is)")
        )
        if (input$mode == "facility" && input$analyze_target != "undervax_count") {
          weight_display <- if (identical(input$weight_col, "__none__") || is.null(input$weight_col)) {
            tags$span(style = "color: #b45309;", "none selected \u2014 unweighted average")
          } else {
            col(input$weight_col)
          }
          rows[[length(rows) + 1]] <- tags$tr(
            tags$td(tags$strong("Weighted by (for combining schools)")),
            tags$td(weight_display)
          )
        }
      } else {
        elig <- col(input$eligible_col)
        vacc <- col(input$vaccinated_col)
        vacc_desc <- if (identical(input$vaccinated_format, "count")) {
          tags$span(vacc, " / ", elig)
        } else {
          vacc
        }

        rows[[length(rows) + 1]] <- tags$tr(
          tags$td(tags$strong("Vaccination rate")),
          tags$td("= ", vacc_desc)
        )
        rows[[length(rows) + 1]] <- tags$tr(
          tags$td(tags$strong("Undervaccination rate")),
          tags$td("= 1 \u2212 vaccination rate")
        )
        rows[[length(rows) + 1]] <- tags$tr(
          tags$td(tags$strong("Undervaccinated count")),
          tags$td("= ", elig, " \u00d7 (1 \u2212 vaccination rate)")
        )
      }

      analyzed_row_label <- switch(input$analyze_target,
        undervax_count = "Undervaccinated count",
        undervax_rate  = "Undervaccination rate",
        vax_rate       = "Vaccination rate"
      )

      div(class = "alert alert-light border mt-2",
        style = "font-size: 0.82rem; color: #333333;",
        icon("calculator"), " ",
        tags$strong("Exactly what will be calculated, using your selected columns:"),
        tags$table(class = "table table-sm mt-2 mb-1", style = "background: transparent;",
          tags$tbody(rows)
        ),
        tags$div(
          icon("bullseye"), " The hotspot analysis will run on: ",
          tags$strong(analyzed_row_label), "."
        )
      )
    })


    validated <- eventReactive(input$confirm, {
      req(raw_data(), input$analyze_target, input$source_type)

      id_col <- if (input$mode == "aggregate") input$id_col else NULL

      ext <- tolower(tools::file_ext(input$file$name))
      df  <- tryCatch({
        switch(ext,
          csv  = readr::read_csv(input$file$datapath, show_col_types = FALSE),
          xlsx = readxl::read_xlsx(input$file$datapath),
          xls  = readxl::read_xls(input$file$datapath)
        )
      }, error = function(e) NULL)

      warns    <- character(0)
      errors   <- character(0)
      n_dropped <- 0L
      n_small   <- 0L

      if (is.null(df)) {
        return(list(data = NULL, warnings = warns,
                    errors = "Could not read file.",
                    n_dropped = n_dropped, n_small = n_small))
      }

      # Check required columns exist
      has_weight_col <- input$mode == "facility" && input$analyze_target != "undervax_count" &&
        input$source_type == "direct" && !identical(input$weight_col, "__none__") &&
        !is.null(input$weight_col)

      needed <- if (input$source_type == "direct") {
        req(input$direct_col)
        c(input$direct_col, if (has_weight_col) input$weight_col)
      } else {
        req(input$eligible_col, input$vaccinated_col)
        c(input$eligible_col, input$vaccinated_col)
      }
      if (!is.null(id_col)) needed <- c(needed, id_col)

      missing_cols <- setdiff(needed, names(df))
      if (length(missing_cols) > 0) {
        errors <- c(errors, paste("Column(s) not found:",
                                  paste(missing_cols, collapse = ", ")))
        return(list(data = NULL, warnings = warns,
                    errors = errors, n_dropped = n_dropped, n_small = n_small))
      }

      # Initialize the three possible analysis variables — filled in as available
      df$.undervax_count <- NA_real_
      df$.undervax_rate  <- NA_real_
      df$.vax_rate       <- NA_real_

      if (input$source_type == "direct") {

        # Handle "<10" style suppression strings before coercing
        raw_col <- suppressWarnings(
          as.numeric(gsub("^<.*$", NA, as.character(df[[input$direct_col]])))
        )
        n_before  <- nrow(df)
        keep      <- !is.na(raw_col)
        df        <- df[keep, ]
        raw_col   <- raw_col[keep]
        n_dropped <- n_before - nrow(df)

        if (n_dropped > 0) {
          warns <- c(warns, paste0(n_dropped, " row(s) dropped due to missing or ",
                                   "suppressed values."))
        }

        if (input$analyze_target == "undervax_count") {
          negative <- raw_col < 0
          if (any(negative, na.rm = TRUE)) {
            warns <- c(warns, paste0(sum(negative), " row(s) have negative values \u2014 ",
                                     "please check your data."))
          }
          df$.undervax_count <- pmax(raw_col, 0)
        } else {
          scale_val <- input$direct_pct_scale %||% "1"
          rate <- if (scale_val == "100") raw_col / 100 else raw_col

          out_of_range <- rate < 0 | rate > 1
          if (any(out_of_range, na.rm = TRUE)) {
            warns <- c(warns, paste0(sum(out_of_range), " row(s) have values outside the ",
                                     "expected 0-1 range \u2014 check your scale selection."))
          }

          if (input$analyze_target == "undervax_rate") {
            df$.undervax_rate <- rate
            df$.vax_rate      <- 1 - rate
          } else {
            df$.vax_rate      <- rate
            df$.undervax_rate <- 1 - rate
          }
        }

        # Weighting column (Mode A + rate target only) — used only to weight
        # how facility rates are combined into each area, not to compute the
        # rate itself. Coerced to numeric here; rows with an unusable weight
        # simply fall back to being excluded from the weighted average at
        # aggregation time rather than blocking the whole upload.
        if (has_weight_col) {
          df[[input$weight_col]] <- suppressWarnings(
            as.numeric(gsub("^<.*$", NA, as.character(df[[input$weight_col]])))
          )
          n_bad_weight <- sum(is.na(df[[input$weight_col]]) | df[[input$weight_col]] <= 0)
          if (n_bad_weight > 0) {
            warns <- c(warns, paste0(n_bad_weight, " row(s) have a missing, zero, or ",
                                     "suppressed value in the weighting column \u2014 those ",
                                     "rows will be dropped from the weighted average when ",
                                     "combined into each area."))
          }
        }

      } else {

        # Handle "<10" strings in enrollment before coercing
        df[[input$eligible_col]] <- suppressWarnings(
          as.numeric(gsub("^<.*$", NA, as.character(df[[input$eligible_col]])))
        )
        vacc_raw <- suppressWarnings(as.numeric(
          gsub("^<.*$", NA, as.character(df[[input$vaccinated_col]]))
        ))
        df[[input$vaccinated_col]] <- vacc_raw

        # Drop rows with missing rate or enrollment
        n_before  <- nrow(df)
        df        <- df[!is.na(df[[input$vaccinated_col]]) &
                        !is.na(df[[input$eligible_col]]), ]
        n_dropped <- n_before - nrow(df)

        if (n_dropped > 0) {
          warns <- c(warns, paste0(n_dropped, " row(s) dropped due to missing or ",
                                   "suppressed enrollment/vaccination values ",
                                   "(including values recorded as '<10')."))
        }

        # Drop zero enrollment
        zero_elig <- df[[input$eligible_col]] == 0
        if (any(zero_elig, na.rm = TRUE)) {
          n_zero    <- sum(zero_elig)
          n_dropped <- n_dropped + n_zero
          warns <- c(warns, paste0(n_zero, " row(s) dropped because eligible ",
                                   "population is zero."))
          df <- df[!zero_elig, ]
        }

        eligible <- df[[input$eligible_col]]

        if (input$vaccinated_format == "count") {
          vacc_count <- df[[input$vaccinated_col]]
          negative <- vacc_count < 0 | vacc_count > eligible
          if (any(negative, na.rm = TRUE)) {
            warns <- c(warns, paste0(sum(negative), " row(s) have a vaccinated count ",
                                     "that is negative or exceeds eligible population \u2014 ",
                                     "please check your data."))
          }
          rate <- pmin(pmax(vacc_count / eligible, 0), 1)
        } else {
          scale_val <- input$derive_pct_scale %||% "1"
          rate_raw  <- df[[input$vaccinated_col]]
          rate <- if (scale_val == "100") rate_raw / 100 else rate_raw

          out_of_range <- rate < 0 | rate > 1
          if (any(out_of_range, na.rm = TRUE)) {
            warns <- c(warns, paste0(sum(out_of_range), " row(s) have percent values ",
                                     "outside expected range \u2014 check your scale selection."))
          }
        }

        df$.vax_rate       <- rate
        df$.undervax_rate  <- 1 - rate
        df$.undervax_count <- pmax(eligible * (1 - rate), 0)
      }

      # Set the primary analysis column based on the chosen target
      df$.value <- switch(input$analyze_target,
        undervax_count = df$.undervax_count,
        undervax_rate  = df$.undervax_rate,
        vax_rate       = df$.vax_rate
      )

      list(data = df, warnings = warns, errors = errors,
           n_dropped = n_dropped, n_small = n_small)
    })

    output$validation_msgs <- renderUI({
      req(validated())
      v    <- validated()
      msgs <- tagList()

      if (length(v$errors) > 0) {
        msgs <- tagAppendChildren(msgs,
          div(class = "alert alert-danger mt-2",
            icon("circle-xmark"), " ",
            tags$strong("Errors:"),
            tags$ul(lapply(v$errors, tags$li))
          )
        )
      }

      if (length(v$warnings) > 0) {
        msgs <- tagAppendChildren(msgs,
          div(class = "alert alert-warning mt-2",
            icon("triangle-exclamation"), " ",
            tags$strong("Warnings:"),
            tags$ul(lapply(v$warnings, tags$li))
          )
        )
      }

      if (!is.null(v$data) && length(v$errors) == 0) {
        target_label <- switch(input$analyze_target,
          undervax_count = "undervaccinated count",
          undervax_rate  = "undervaccination rate",
          vax_rate       = "vaccination rate"
        )
        summary_val <- if (input$analyze_target == "undervax_count") {
          round(sum(v$data$.value, na.rm = TRUE), 1)
        } else {
          scales::percent(mean(v$data$.value, na.rm = TRUE), accuracy = 0.1)
        }
        summary_verb <- if (input$analyze_target == "undervax_count") "Total" else "Average"

        source_desc <- if (input$source_type == "direct") {
          paste0("directly from ", input$direct_col)
        } else {
          paste0("derived from ", input$vaccinated_col, " & ", input$eligible_col)
        }

        msgs <- tagAppendChildren(msgs,
          div(class = "alert alert-success mt-2",
            icon("circle-check"), " ",
            glue::glue(
              "Data loaded: {nrow(v$data)} rows. ",
              "{summary_verb} {target_label}: {summary_val}."
            )
          ),
          div(class = "alert border mt-1",
            style = "font-size: 0.82rem; background-color: #f5f2fa; border-color: #522D80 !important; color: #333333;",
            tags$strong("You are analyzing: "), target_label,
            " (", source_desc, ")"
          )
        )

        # Facility-level rate targets with no weighting column selected: the
        # tract-level aggregation will be a simple (unweighted) average across
        # facilities, not weighted by population size.
        if (input$mode == "facility" && input$analyze_target != "undervax_count" &&
            input$source_type == "direct" &&
            (identical(input$weight_col, "__none__") || is.null(input$weight_col))) {
          msgs <- tagAppendChildren(msgs,
            div(class = "alert alert-warning mt-1", style = "color: #333333;",
              icon("triangle-exclamation"), " ",
              "No eligible-population column was selected, so when facilities are ",
              "aggregated up to the tract level, each facility will count equally ",
              "in the average (an ", tags$strong("unweighted"), " average) \u2014 a small ",
              "school and a large school contribute the same amount to the tract's rate. ",
              "Select an eligible-population column above if you want larger schools to ",
              "count proportionally more."
            )
          )
        }

        # Push to shared state
        rv$uploaded_data        <- v$data
        rv$upload_mode          <- input$mode
        rv$id_col                <- if (input$mode == "aggregate") input$id_col else NULL
        rv$value_col             <- ".value"
        rv$analyze_target        <- input$analyze_target
        rv$analyze_target_label  <- target_label
        rv$source_type           <- input$source_type
        has_weight_col <- input$mode == "facility" && input$analyze_target != "undervax_count" &&
          input$source_type == "direct" && !identical(input$weight_col, "__none__") &&
          !is.null(input$weight_col)

        rv$vaccinated_col        <- if (input$source_type == "derive") input$vaccinated_col else NULL
        rv$eligible_col          <- if (input$source_type == "derive") {
          input$eligible_col
        } else if (has_weight_col) {
          input$weight_col
        } else {
          NULL
        }
        rv$direct_col            <- if (input$source_type == "direct") input$direct_col else NULL
        rv$lat_col               <- input$lat_col
        rv$lon_col                <- input$lon_col
        rv$n_dropped              <- v$n_dropped
        rv$n_small                <- v$n_small

        # If geography already loaded, attempt join immediately
        if (!is.null(rv$geo)) {
          join_result    <- attempt_join(rv$geo, rv)
          rv$joined_data <- join_result$geo
          rv$n_matched   <- join_result$n_matched
          rv$n_unmatched <- join_result$n_unmatched
          rv$agg_weighted <- join_result$weighted
        }
      }

      msgs
    })

  })
}

`%||%` <- function(x, y) if (!is.null(x)) x else y
