
library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(shinyjs)
library(waiter)

options(shiny.maxRequestSize = 50 * 1024^2)


# app_dir / pkg_dir are resolved automatically — no manual editing needed.
# Shiny sets the working directory to this file's location when it runs the app,
# so app_dir is just "here", and pkg_dir is two levels up (inst/app -> inst -> package root).
app_dir <- getwd()
pkg_dir <- normalizePath(file.path(app_dir, "..", ".."))

source(file.path(pkg_dir, "R", "analysis.R"))
source(file.path(pkg_dir, "R", "geography.R"))
source(file.path(pkg_dir, "R", "report.R"))

source(file.path(app_dir, "modules", "mod_upload.R"))
source(file.path(app_dir, "modules", "mod_geography.R"))
source(file.path(app_dir, "modules", "mod_analysis.R"))
source(file.path(app_dir, "modules", "mod_results.R"))

ui <- page_navbar(
  id = "navbar",
  title = tags$span(
    tags$span("spati", style = "font-weight: 400;"),
    tags$span("Alert", style = "font-weight: 700; color: #F56600;")
  ),
  theme = bs_theme(
    bootswatch   = "flatly",
    primary      = "#522D80",
    secondary    = "#F56600",
    danger       = "#c0392b",
    dark         = "#333333",
    base_font    = font_google("Source Sans Pro"),
    heading_font = font_google("Source Sans Pro")
  ) |> bs_add_rules("
    :root { color-scheme: light !important; }
    .navbar { background-color: #522D80 !important; }
    .navbar-nav .nav-link { color: rgba(255,255,255,0.85) !important; }
    .navbar-nav .nav-link.active,
    .navbar-nav .nav-link:hover {
      color: #CBC4BC !important;
      border-bottom-color: #CBC4BC !important;
    }
    .text-primary { color: #522D80 !important; }
    .alert-light { color: #333333 !important; background-color: #f8f9fa !important; }
    .alert { color: #333333 !important; }
    .form-switch .form-check-input:checked { background-color: #522D80 !important; border-color: #522D80 !important; }
  "),
  nav_panel(
    title = tagList(icon("house"), " Start Here"),
    value = "tab_start",
    bslib::card(
      bslib::card_body(
        style = "max-width: 900px; margin: 0 auto;",
        div(class = "mb-3",
          actionButton("start_go_to_data", "Go to Data & Geography \u2192",
                       class = "btn-primary")
        ),
        includeMarkdown(file.path(app_dir, "www", "start_here.md"))
      )
    )
  ),
  nav_panel(
    title = tagList(icon("upload"), " Data & Geography"),
    value = "tab_data",
    useShinyjs(),
    use_waiter(),
    layout_sidebar(
      sidebar = sidebar(
        width = 380,
        mod_upload_ui("upload"),
        hr(),
        mod_geography_ui("geography"),
        hr(),
        uiOutput("go_to_analysis_btn")
      ),
      bslib::card(
        card_header("Data preview"),
        bslib::card_body(
          leafletOutput("preview_map", height = 350),
          DTOutput("preview_table")
        )
      )
    )
  ),
  nav_panel(
    title = tagList(icon("chart-area"), " Analysis"),
    value = "tab_analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 380,
        mod_analysis_ui("analysis")
      ),
      bslib::card(
        card_header("Analysis results"),
        leafletOutput("results_map", height = 500)
      )
    )
  ),
  nav_panel(
    title = tagList(icon("file-export"), " Results & Export"),
    value = "tab_results",
    mod_results_ui("results")
  ),
  nav_panel(
    title = tagList(icon("circle-info"), " About"),
    value = "tab_help",
    bslib::card(
      bslib::card_body(
        includeMarkdown(file.path(app_dir, "www", "help.md"))
      )
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(
    uploaded_data = NULL,
    geo           = NULL,
    joined_data   = NULL,
    results       = NULL
  )

  mod_upload_server("upload",       rv = rv)
  mod_geography_server("geography", rv = rv)
  mod_analysis_server("analysis",   rv = rv, parent_session = session)
  mod_results_server("results",     rv = rv)

  output$go_to_analysis_btn <- renderUI({
    req(rv$joined_data)
    tagList(
      div(class = "alert alert-success",
        icon("circle-check"), " Data and geography loaded. Ready to run analysis."
      ),
      actionButton(
        "go_to_analysis",
        "Go to Analysis \u2192",
        class = "btn-primary w-100",
        icon  = icon("chart-area")
      )
    )
  })

  observeEvent(input$go_to_analysis, {
    updateNavbarPage(session, inputId = "navbar", selected = "tab_analysis")
  })

  observeEvent(input$start_go_to_data, {
    updateNavbarPage(session, inputId = "navbar", selected = "tab_data")
  })

  output$preview_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -96, lat = 38, zoom = 4)
  })

  observe({
    req(rv$joined_data)
    geo <- rv$joined_data
    bb  <- sf::st_bbox(geo)
    leafletProxy("preview_map") |>
      clearShapes() |>
      addPolygons(
        data        = geo,
        fillOpacity = 0.5,
        weight      = 0.5,
        color       = "#522D80",
        fillColor   = "#b39dca",
        label       = ~GEOID
      ) |>
      fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])
  })

  output$results_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -96, lat = 38, zoom = 4)
  })

  observe({
    req(rv$results)
    geo <- rv$results
    bb  <- sf::st_bbox(geo)
    pal <- colorFactor(
      palette  = c("#c0392b", "#2980b9", "#bdc3c7"),
      levels   = c("Hotspot", "Coldspot", "Not significant"),
      na.color = "#eeeeee"
    )
    leafletProxy("results_map") |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        data        = geo,
        fillColor   = ~pal(hotspot_class),
        fillOpacity = 0.7,
        weight      = 0.5,
        color       = "#555",
        label       = ~paste0(GEOID, ": ", hotspot_class,
                              " (Gi* = ", round(gi_star, 2), ")")
      ) |>
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = geo$hotspot_class,
        title    = "Hotspot classification"
      ) |>
      fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])
  })

  output$preview_table <- renderDT({
    req(rv$joined_data)
    rv$joined_data |>
      sf::st_drop_geometry() |>
      head(100) |>
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui = ui, server = server)

