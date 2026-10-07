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
        multiple = TRUE,
        accept   = c(".geojson", ".json", ".zip",
                     ".shp", ".dbf", ".shx", ".prj", ".cpg")
      ),
      helpText(
        icon("circle-info"), " ",
        tags$strong("Shapefile users:"), " either upload a single .zip, or ",
        "select all the shapefile parts at once (.shp, .dbf, .shx, .prj). ",
        "GeoJSON files (.geojson) can be uploaded directly. Any polygon ",
        "geography works (school districts, health districts, ZIP areas, ...)."
      ),
      uiOutput(ns("custom_id_ui")),
      textInput(
        ns("custom_label"),
        "What do these areas represent? (used in labels and reports)",
        placeholder = "e.g. school district"
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

    # ── Custom geography: read once, reuse for the ID picker and for loading ──
    custom_geo_raw <- reactive({
      req(input$shapefile)
      read_custom_geo(input$shapefile)
    })

    output$custom_id_ui <- renderUI({
      req(input$shapefile)
      g <- tryCatch(custom_geo_raw(), error = function(e) e)
      if (inherits(g, "error")) {
        return(div(class = "alert alert-danger mt-1",
                   icon("circle-xmark"), " ", conditionMessage(g)))
      }
      d    <- sf::st_drop_geometry(g)
      cols <- names(d)
      if (length(cols) == 0) {
        return(div(class = "alert alert-danger mt-1",
                   "This file has no attribute columns to use as an ID."))
      }
      n_uniq <- vapply(d, function(x) length(unique(x[!is.na(x)])), integer(1))
      labels <- sprintf("%s  (%d unique of %d)", cols, n_uniq, nrow(d))
      # Best guess: GEOID, else a column whose name looks like an ID/code and
      # whose values are all unique, else any column with all-unique values.
      all_unique <- n_uniq == nrow(d)
      guess <- cols[toupper(cols) == "GEOID"]
      if (length(guess) == 0) {
        guess <- cols[all_unique & grepl("GEOID|FIPS|AUN|_ID$|^ID$|CODE|NUM",
                                         cols, ignore.case = TRUE)]
      }
      if (length(guess) == 0) guess <- cols[all_unique]
      tagList(
        selectInput(ns("custom_id_col"),
                    "ID column in shapefile (must match an ID column in your data)",
                    choices  = setNames(cols, labels),
                    selected = if (length(guess) > 0) guess[1] else cols[1]),
        helpText("Pick a column where every area has a different value ",
                 "(unique count = total rows).")
      )
    })

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
          geo <- custom_geo_raw()

          idc <- input$custom_id_col
          if (is.null(idc) || !nzchar(idc) || !idc %in% names(geo)) {
            stop("Please choose the ID column in the shapefile first.")
          }
          ids <- trimws(as.character(geo[[idc]]))
          if (anyNA(ids) || any(!nzchar(ids))) {
            stop("The ID column '", idc, "' has blank values. Choose a column ",
                 "where every area has an ID.")
          }
          if (anyDuplicated(ids) > 0) {
            stop("The ID column '", idc, "' is not unique (",
                 sum(duplicated(ids)), " repeated value(s)). Choose a column ",
                 "where each area has a different value.")
          }
          # Standardize on a character column called GEOID (replacing any
          # existing, different GEOID column so names never collide).
          geo$GEOID <- ids
          geo <- geo[order(geo$GEOID), ]

          custom_unit <- trimws(input$custom_label %||% "")
          if (!nzchar(custom_unit)) custom_unit <- "area"
          # The label itself is stored as the "geography level": downstream
          # titles/summaries fall back to printing it as-is for non-census levels.
          attr(geo, "spatialert_geo_level") <- custom_unit
          attr(geo, "spatialert_state")     <- NA
        }

        # Store geo — weights will be built in analysis module
        rv$geo       <- geo
        is_custom    <- input$geo_source == "custom"
        unit_label   <- if (is_custom) attr(geo, "spatialert_geo_level") else input$geo_level
        rv$geo_level <- unit_label
        rv$state     <- if (is_custom) "" else input$state

        # Attempt join if data already uploaded
        if (!is.null(rv$uploaded_data)) {
          join_result    <- attempt_join(geo, rv)
          rv$joined_data <- join_result$geo
          rv$n_matched   <- join_result$n_matched
          rv$n_unmatched <- join_result$n_unmatched
          rv$agg_weighted <- join_result$weighted
          rv$school_data  <- join_result$schools   # NULL in Mode B
        }

        status_msg <- glue::glue("Loaded {nrow(geo)} {unit_label}(s). ",
                                 "Now go to the Analysis tab to set weights and run.")
        if (!is.null(rv$n_unmatched) && rv$n_unmatched > 0) {
          status_msg <- paste0(
            status_msg, " Note: ", rv$n_unmatched,
            if (identical(rv$upload_mode, "aggregate")) {
              " row(s) in your data have an ID that was not found in this geography and were excluded."
            } else {
              " facility/school location(s) did not fall within any area and were excluded."
            }
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
    #
    # IDs are compared in a normalized form (whitespace trimmed, a trailing
    # ".0" and leading zeros dropped) because spreadsheets/CSV readers often
    # turn text IDs like "042" or "104432503" into numbers, which would
    # otherwise silently fail to match the geography's text IDs.
    norm_id <- function(x) {
      x <- trimws(as.character(x))
      x <- sub("\\.0+$", "", x)
      sub("^0+(?=.)", "", x, perl = TRUE)
    }
    d        <- rv$uploaded_data
    d_key    <- norm_id(d[[rv$id_col]])
    g_key    <- norm_id(geo$GEOID)
    n_dup    <- sum(duplicated(d_key))
    d        <- d[!duplicated(d_key), , drop = FALSE]   # keep first row per ID
    d_key    <- d_key[!duplicated(d_key)]
    pos      <- match(g_key, d_key)
    n_found  <- sum(!is.na(pos))

    if (n_found == 0) {
      stop(
        "None of the IDs in your data matched the geography. ",
        "Data IDs look like: ", paste(utils::head(unique(as.character(d[[rv$id_col]])), 3), collapse = ", "),
        "; geography IDs look like: ", paste(utils::head(geo$GEOID, 3), collapse = ", "),
        ". Check that you picked the matching ID column in both places.",
        call. = FALSE
      )
    }

    result_geo <- geo
    for (col in c(".undervax_count", ".undervax_rate", ".vax_rate")) {
      result_geo[[col]] <- d[[col]][pos]
    }
    result_geo$.value <- result_geo[[target_col]]
    list(geo = result_geo, n_matched = n_found,
         n_unmatched = sum(!d_key %in% g_key),   # data rows with no matching area
         n_dup = n_dup, weighted = NA)
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

# Read an uploaded custom geography (zip, loose shapefile parts, or GeoJSON),
# repair invalid polygons, and return it in WGS84.
read_custom_geo <- function(files) {
  exts <- tolower(tools::file_ext(files$name))
  work <- tempfile("geo_"); dir.create(work)

  if (any(exts == "zip")) {
    unzip(files$datapath[exts == "zip"][1], exdir = work)
    path <- list.files(work, pattern = "\\.(shp|geojson|json)$",
                       full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    if (length(path) == 0) stop("No .shp or .geojson file found inside the zip.")
    path <- path[1]
  } else if (any(exts == "shp")) {
    # Loose shapefile parts: Shiny gives them temp names, so restore the
    # original names (the parts must share a base name to be read together).
    file.copy(files$datapath, file.path(work, files$name))
    for (need in c("dbf", "shx")) {
      if (!need %in% exts) {
        stop("Missing the .", need, " file. Select all shapefile parts together ",
             "(.shp, .dbf, .shx, .prj), or upload a single .zip.")
      }
    }
    path <- file.path(work, files$name[exts == "shp"][1])
  } else if (any(exts %in% c("geojson", "json"))) {
    path <- files$datapath[exts %in% c("geojson", "json")][1]
  } else {
    stop("Unsupported file. Upload a .zip, a .geojson, or the .shp/.dbf/.shx parts.")
  }

  geo <- sf::read_sf(path)
  if (nrow(geo) == 0) stop("The geography file has no features.")
  if (is.na(sf::st_crs(geo))) {
    stop("The geography file has no coordinate system (.prj missing). ",
         "Include the .prj file, or use a GeoJSON.")
  }
  geo <- sf::st_make_valid(geo)
  geo <- geo[!sf::st_is_empty(geo), ]
  if (!all(sf::st_geometry_type(geo) %in% c("POLYGON", "MULTIPOLYGON"))) {
    geo <- sf::st_collection_extract(geo, "POLYGON")
  }
  if (nrow(geo) == 0) stop("The geography file does not contain polygons.")
  sf::st_transform(geo, crs = 4326)
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
