# mod_geography.R — Geography selection module (weights moved to analysis tab)

mod_geography_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("2. Choose geography", class = "text-primary fw-bold"),

    selectInput(
      ns("geo_source"),
      "Geography source",
      choices = c(
        "Fetch US Census geography"          = "census",
        "Upload my own shapefile / GeoJSON"  = "custom"
      )
    ),

    conditionalPanel(
      condition = paste0("input['", ns("geo_source"), "'] == 'census'"),
      selectInput(
        ns("state"),
        "State",
        choices = c("Select a state..." = "", setNames(
          state.abb,
          paste(state.name, paste0("(", state.abb, ")"))
        ))
      ),
      selectInput(
        ns("geo_level"),
        "Geographic level",
        choices = c(
          "Census tract"       = "tract",
          "County"             = "county",
          "Census block group" = "block group"
        )
      ),
      uiOutput(ns("county_selector")),
      uiOutput(ns("geo_warning"))
    ),

    conditionalPanel(
      condition = paste0("input['", ns("geo_source"), "'] == 'custom'"),
      fileInput(
        ns("shapefile"),
        "Upload geography file",
        accept = c(".geojson", ".json", ".zip")
      ),
      helpText(
        icon("circle-info"), " ",
        tags$strong("Shapefile users:"), " Zip all shapefile components ",
        "(.shp, .dbf, .shx, .prj) into a single .zip file before uploading. ",
        "GeoJSON files (.geojson) can be uploaded directly."
      ),
      textInput(
        ns("custom_id_col"),
        "ID column in shapefile (to match your data)",
        placeholder = "e.g. GEOID, fips_code"
      )
    ),

    actionButton(
      ns("fetch_geo"),
      "Load geography",
      class = "btn-primary w-100 mt-2",
      icon  = icon("map")
    ),

    uiOutput(ns("geo_status"))
  )
}


mod_geography_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$county_selector <- renderUI({
      req(input$state, input$geo_level %in% c("tract", "block group"))
      selectInput(
        ns("county"),
        "County (optional — leave blank for whole state)",
        choices  = c("All counties" = "", get_county_choices(input$state)),
        multiple = TRUE
      )
    })

    output$geo_warning <- renderUI({
      req(input$state, input$geo_level == "county")
      n <- state_county_counts[[input$state]]
      if (!is.null(n) && n < 20) {
        div(class = "alert alert-warning mt-1",
          icon("triangle-exclamation"), " ",
          glue::glue(
            "{input$state} has only {n} counties. Gi* results may have limited ",
            "statistical power. Consider tract-level analysis if data allow."
          )
        )
      }
    })

    observeEvent(input$fetch_geo, {
      req(input$geo_source)

      w <- waiter::Waiter$new(
        html  = waiter::spin_fading_circles(),
        color = waiter::transparent(0.5)
      )
      w$show()
      on.exit(w$hide())

      tryCatch({
        if (input$geo_source == "census") {
          req(input$state)
          county_arg <- if (length(input$county) > 0 && any(nchar(input$county) > 0))
            input$county else NULL

          geo <- fetch_geography(
            state  = input$state,
            level  = input$geo_level,
            county = county_arg
          )
          attr(geo, "spatialert_geo_level") <- input$geo_level
          attr(geo, "spatialert_state")     <- input$state

        } else {
          req(input$shapefile)
          path <- input$shapefile$datapath
          if (tolower(tools::file_ext(input$shapefile$name)) == "zip") {
            unzip_dir <- tempfile()
            dir.create(unzip_dir)
            unzip(path, exdir = unzip_dir)
            path <- list.files(unzip_dir, pattern = "\\.shp$",
                               full.names = TRUE, recursive = TRUE)[1]
          }
          geo <- sf::read_sf(path)
          geo <- sf::st_transform(geo, crs = 4326)
          if (!is.null(input$custom_id_col) && nchar(input$custom_id_col) > 0) {
            names(geo)[names(geo) == input$custom_id_col] <- "GEOID"
          }
          if ("GEOID" %in% names(geo)) {
            geo <- geo[order(geo$GEOID), ]
          }
          attr(geo, "spatialert_geo_level") <- "custom"
          attr(geo, "spatialert_state")     <- NA
        }

        # Store geo — weights will be built in analysis module
        rv$geo       <- geo
        rv$geo_level <- input$geo_level
        rv$state     <- input$state

        # Attempt join if data already uploaded
        if (!is.null(rv$uploaded_data)) {
          join_result    <- attempt_join(geo, rv)
          rv$joined_data <- join_result$geo
          rv$n_matched   <- join_result$n_matched
          rv$n_unmatched <- join_result$n_unmatched
          rv$agg_weighted <- join_result$weighted
        }

        status_msg <- glue::glue("Loaded {nrow(geo)} {input$geo_level}(s). ",
                                 "Now go to the Analysis tab to set weights and run.")
        if (!is.null(rv$n_unmatched) && rv$n_unmatched > 0) {
          status_msg <- paste0(
            status_msg, " Note: ", rv$n_unmatched,
            " facility/school location(s) did not fall within any tract and were excluded."
          )
        }

        output$geo_status <- renderUI({
          div(class = "alert alert-success mt-2",
            style = "color: #333333;",
            icon("circle-check"), " ", status_msg
          )
        })

        # Neighbor connectivity FYI — computed once here (right when geography
        # loads), based on queen/rook contiguity only. This does NOT change
        # with the weights style chosen later; it's a diagnostic to help
        # decide whether contiguity weights are usable for this geography (no
        # islands / 0s) or whether KNN will be needed to force connectivity.
        # The panel itself is displayed on the Analysis tab (mod_analysis.R),
        # right next to the weights choice it's meant to inform.
        rv$neighbor_summary <- tryCatch(summarize_neighbors(geo), error = function(e) NULL)

      }, error = function(e) {
        output$geo_status <- renderUI({
          div(class = "alert alert-danger mt-2",
            icon("circle-xmark"), " ",
            paste("Error loading geography:", conditionMessage(e))
          )
        })
      })
    })
  })
}


# ── Helpers ───────────────────────────────────────────────────────────────────

attempt_join <- function(geo, rv) {
  target_col <- switch(rv$analyze_target %||% "undervax_count",
    undervax_count = ".undervax_count",
    undervax_rate  = ".undervax_rate",
    vax_rate       = ".vax_rate"
  )

  if (rv$upload_mode == "aggregate") {
    # Mode B: data is already at the target geography level — carry all
    # derived columns straight across, one row per GEOID already.
    result_geo <- dplyr::left_join(
      geo,
      rv$uploaded_data |>
        dplyr::select(
          GEOID = !!rv$id_col,
          .undervax_count = .undervax_count,
          .undervax_rate  = .undervax_rate,
          .vax_rate       = .vax_rate
        ) |>
        dplyr::mutate(GEOID = as.character(GEOID)),
      by = "GEOID"
    )
    result_geo$.value <- result_geo[[target_col]]
    list(geo = result_geo, n_matched = nrow(rv$uploaded_data), n_unmatched = 0, weighted = NA)
  } else {
    # Mode A: facility-level data gets spatially joined and aggregated up
    # to the target geography.
    join_to_geography(
      data         = rv$uploaded_data,
      geo          = geo,
      value_col    = rv$value_col,
      lat_col      = rv$lat_col %||% "latitude",
      lon_col      = rv$lon_col %||% "longitude",
      eligible_col = rv$eligible_col,
      target_col   = target_col
    )
  }
}

get_county_choices <- function(state_abb) {
  tryCatch({
    counties <- tigris::counties(state = state_abb, cb = TRUE, year = 2020)
    setNames(counties$COUNTYFP, counties$NAME)
  }, error = function(e) character(0))
}

state_county_counts <- c(
  AK = 29, CT = 8, DC = 1, DE = 3, HI = 5,
  MA = 14, NH = 10, NJ = 21, RI = 5, VT = 14
)

`%||%` <- function(x, y) if (!is.null(x)) x else y
