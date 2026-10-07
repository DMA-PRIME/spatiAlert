
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
source(file.path(app_dir, "modules", "mod_explore.R"))

# Keeps the page where it is when a file is selected/uploaded. Without this
# the view can jump back to the top after choosing a shapefile, forcing the
# user to scroll back down to the geography section.
keep_scroll_js <- "
(function() {
  var saved = null, timer = null;
  function targets() {
    var els = [document.scrollingElement || document.documentElement];
    document.querySelectorAll('.sidebar, .sidebar-content, .main, .tab-pane, .bslib-sidebar-layout, .card-body')
      .forEach(function(e) { els.push(e); });
    return els;
  }
  function snapshot() {
    saved = targets().map(function(e) { return [e, e.scrollTop]; })
                     .filter(function(p) { return p[1] > 0; });
  }
  function holdFor(ms) {
    if (!saved || saved.length === 0) return;
    var t0 = Date.now();
    clearInterval(timer);
    timer = setInterval(function() {
      if (!saved || Date.now() - t0 > ms) { clearInterval(timer); return; }
      saved.forEach(function(p) { if (p[0].scrollTop < p[1]) p[0].scrollTop = p[1]; });
    }, 30);
  }
  // Shiny hides the real <input type=file> far off the page (top: -99999px).
  // Clicking Browse focuses it, and the browser scrolls the sidebar to the very
  // top to 'reveal' it. A fixed-position input has nothing to scroll to.
  function fixFileInputs() {
    document.querySelectorAll('input[type=file]').forEach(function(el) {
      if (el.dataset.noScrollFix) return;
      el.dataset.noScrollFix = '1';
      el.style.setProperty('position', 'fixed', 'important');
      el.style.setProperty('top', '0', 'important');
      el.style.setProperty('left', '-99999px', 'important');
    });
  }
  fixFileInputs();
  new MutationObserver(fixFileInputs)
    .observe(document.documentElement, { childList: true, subtree: true });
  // Safety net: remember the position when a file picker is opened or a file
  // is chosen, and hold it for a few seconds unless the user scrolls.
  document.addEventListener('mousedown', function(e) {
    var c = e.target && e.target.closest ? e.target.closest('.shiny-input-container') : null;
    if (c && c.querySelector('input[type=file]')) { snapshot(); holdFor(3000); }
  }, true);
  document.addEventListener('change', function(e) {
    if (e.target && e.target.type === 'file') { snapshot(); holdFor(5000); }
  }, true);
  ['wheel', 'touchmove', 'keydown'].forEach(function(n) {
    window.addEventListener(n, function() { saved = null; clearInterval(timer); },
                            { passive: true, capture: true });
  });
})();
"

ui <- page_navbar(
  id = "navbar",
  header = tags$head(
    tags$script(HTML(keep_scroll_js)),
    tags$style(HTML("
      .card-header .nav-tabs .nav-link,
      .card-header .nav .nav-link,
      .bslib-card .nav-tabs .nav-link,
      .nav-tabs > li > a {
        font-size: 1.15rem !important; font-weight: 700 !important; color: #522D80 !important;
      }
      .card-header .nav-tabs .nav-link:hover, .bslib-card .nav-tabs .nav-link:hover { color: #3a2163 !important; }
      .card-header .nav-tabs .nav-link.active, .bslib-card .nav-tabs .nav-link.active {
        color: #522D80 !important; border-bottom: 3px solid #522D80 !important;
      }
      .results-banner .shiny-input-container { flex: 0 0 auto !important; width: auto !important; max-width: none !important; margin-bottom: 0 !important; }
      .results-banner .form-check, .results-banner .form-check-label, .results-banner .checkbox label { white-space: nowrap !important; }
      .results-banner .shiny-html-output { flex: 1 1 auto; }
    "))
  ),
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
    /* Card tab headers (Hotspot results, Explore tables): larger, bold, purple */
    .card-header .nav {
      --bs-nav-link-color: #522D80;
      --bs-nav-link-hover-color: #3a2163;
      --bs-nav-tabs-link-active-color: #522D80;
    }
    .card-header .nav .nav-link,
    .card-header .nav-tabs .nav-link {
      font-size: 1.15rem !important; font-weight: 700 !important; color: #522D80 !important;
    }
    .card-header .nav .nav-link:hover { color: #3a2163 !important; }
    .card-header .nav .nav-link.active {
      color: #522D80 !important; border-bottom: 3px solid #522D80 !important;
    }
    /* Controls in the Analysis results banner: let each control size to its content */
    .results-banner .shiny-input-container { width: auto !important; max-width: none !important; margin-bottom: 0 !important; }
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
        uiOutput("go_to_explore_box"),
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
    title = tagList(icon("map"), " Explore"),
    value = "tab_explore",
    mod_explore_ui("explore")
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
        uiOutput("results_banner"),
        leafletOutput("results_map", height = 500)
      )
      ,uiOutput("export_map_ui")
    )
  ),
  nav_panel(
    title = tagList(icon("file-export"), " Results & Export"),
    value = "tab_results",
    mod_results_ui("results")
  ),
  nav_panel(
    title = tagList(icon("circle-question"), " FAQ"),
    value = "tab_faq",
    div(style = "max-width: 900px; margin: 0 auto;",
        results_faq_ui())
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
  mod_explore_server("explore",     rv = rv)

  output$go_to_explore_box <- renderUI({
    req(rv$uploaded_data)
    div(class = "card border-primary mb-3",
      div(class = "card-body p-3",
        h6(class = "card-title text-primary", icon("map"), " Explore your data"),
        p(class = "small mb-2",
          "Map school locations and average vaccination rates by area, and make ",
          "printable maps, before running the analysis."),
        actionButton("go_to_explore", "Go to Explore \u2192",
                     class = "btn-outline-primary w-100")
      )
    )
  })

  observeEvent(input$go_to_explore, {
    updateNavbarPage(session, inputId = "navbar", selected = "tab_explore")
  })

  observeEvent(input$explore_go_analysis_top, {
    updateNavbarPage(session, inputId = "navbar", selected = "tab_analysis")
  })

  observeEvent(input$explore_go_analysis_bottom, {
    updateNavbarPage(session, inputId = "navbar", selected = "tab_analysis")
  })

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
      addProviderTiles("Esri.WorldGrayCanvas") |>
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
      addProviderTiles("Esri.WorldGrayCanvas") |>
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
    # Areas left out of the analysis (e.g. "exclude areas with no schools") are
    # drawn as plain light grey so they show as gaps rather than disappearing.
    all_geo <- rv$geo
    excluded_geo <- if (!is.null(all_geo) && "GEOID" %in% names(all_geo) &&
                        nrow(geo) < nrow(all_geo)) {
      all_geo[!as.character(all_geo$GEOID) %in% as.character(geo$GEOID), ]
    } else {
      NULL
    }
    proxy <- leafletProxy("results_map") |>
      clearShapes() |>
      removeControl("hotspot_legend")
    if (!is.null(excluded_geo) && nrow(excluded_geo) > 0) {
      proxy <- proxy |>
        addPolygons(
          data = excluded_geo, fillColor = "#f2f2f2", fillOpacity = 0.9,
          weight = 0.4, color = "#bbbbbb",
          label = ~paste0(GEOID, ": not analyzed (no schools)")
        )
    }
    proxy |>
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
        title    = "Hotspot classification",
        layerId  = "hotspot_legend"
      ) |>
      fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])
  })

  # ── Optional school layer on the results map (exploration only; it is never
  # drawn into the Word report map, where hundreds of points would be unreadable).
  # ── Export the hotspot map (PNG / PDF), independent of the Word report. Includes
  # the schools layer when "Show schools" is ticked, using the same choices.
  output$export_map_ui <- renderUI({
    req(rv$results)
    div(class = "d-flex align-items-center gap-2 mt-3 mb-2 flex-wrap",
        tags$strong(style = "color:#4F2D7F;", "Export this map:"),
        downloadButton("dl_hotspot_png", "PNG image", class = "btn-outline-primary btn-sm"),
        downloadButton("dl_hotspot_pdf", "PDF", class = "btn-outline-primary btn-sm"),
        span(class = "text-muted small",
             "Includes schools if \u201cShow schools\u201d is checked."))
  })

  hotspot_export_plot <- function() {
    geo <- rv$results
    req(geo)
    geo_cols <- intersect(c("GEOID", "hotspot_class"), names(geo))
    all_geo <- rv$geo
    proj <- local_projected_crs(geo)
    g <- sf::st_transform(geo[, geo_cols], proj)
    pal_vals <- c("Hotspot", "Coldspot", "Not significant")
    pal_cols <- c("#c0392b", "#2980b9", "#e3e6e8")
    p <- ggplot2::ggplot()
    if (!is.null(all_geo) && "GEOID" %in% names(all_geo) && nrow(geo) < nrow(all_geo)) {
      ex <- all_geo[!as.character(all_geo$GEOID) %in% as.character(geo$GEOID), "GEOID"]
      if (nrow(ex) > 0) {
        p <- p + ggplot2::geom_sf(data = sf::st_transform(ex, proj), fill = "#f7f7f7",
                                  colour = "#a8a8a8", linewidth = 0.15)
      }
    }
    p <- p +
      ggplot2::geom_sf(data = g, ggplot2::aes(fill = hotspot_class),
                       colour = "#a8a8a8", linewidth = 0.15) +
      ggplot2::scale_fill_manual(values = stats::setNames(pal_cols, pal_vals),
                                 breaks = intersect(pal_vals, unique(as.character(g$hotspot_class))), drop = TRUE,
                                 name = "Hotspot classification", na.value = "#eeeeee")
    # Optional schools layer
    if (isTRUE(input$show_schools) && !is.null(rv$school_data)) {
      sd  <- rv$school_data
      lat <- rv$lat_col %||% "latitude"; lon <- rv$lon_col %||% "longitude"
      sd  <- sd[!is.na(sd[[lat]]) & !is.na(sd[[lon]]) & !is.na(sd$.vax_rate), , drop = FALSE]
      if (identical(input$school_layer_scope, "concern")) {
        gids <- concern_geoids(rv$results, rv$analyze_target %||% "undervax_count", "concern")
        sd   <- sd[as.character(sd$GEOID) %in% gids, , drop = FALSE]
      }
      if (nrow(sd) > 0) {
        vcols <- c("<85%" = "#d73027", "85-89.9%" = "#fc8d00", "90-94.9%" = "#fee600", "\u226595%" = "#1aaf00")
        sd$vbin <- cut(sd$.vax_rate * 100, breaks = c(-Inf, 85, 90, 95, Inf),
                       labels = names(vcols), right = FALSE)
        pts <- sf::st_transform(sf::st_as_sf(data.frame(x = sd[[lon]], y = sd[[lat]], vbin = sd$vbin),
                                             coords = c("x", "y"), crs = 4326), proj)
        p <- p +
          ggplot2::geom_sf(data = pts, ggplot2::aes(colour = vbin), size = 1.2, alpha = 0.9) +
          ggplot2::scale_colour_manual(values = vcols, breaks = names(vcols), drop = FALSE,
                                       name = "School vaccination rate")
      }
    }
    method <- attr(geo, "spatialert_method") %||% "gi_star"
    alpha  <- attr(geo, "spatialert_alpha") %||% 0.05
    vlab   <- attr(geo, "spatialert_var_label") %||% "undervaccinated individuals"
    p <- p +
      ggplot2::labs(title = "Hotspot analysis",
                    subtitle = paste0("Getis-Ord Gi* of ", vlab, " (\u03b1 = ", alpha, ")")) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom", legend.box = "vertical",
                     plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(size = 9, colour = "grey35", hjust = 0.5),
                     plot.margin = ggplot2::margin(8, 8, 8, 8))
    explore_add_scalebar(p, sf::st_bbox(g))
  }

  output$dl_hotspot_png <- downloadHandler(
    filename = function() paste0("spatialert_hotspot_map_", Sys.Date(), ".png"),
    content = function(file) {
      ggplot2::ggsave(file, plot = hotspot_export_plot(), width = 9, height = 7.5,
                      dpi = 200, bg = "white")
    }
  )
  output$dl_hotspot_pdf <- downloadHandler(
    filename = function() paste0("spatialert_hotspot_map_", Sys.Date(), ".pdf"),
    content = function(file) {
      ggplot2::ggsave(file, plot = hotspot_export_plot(), width = 9, height = 7.5,
                      device = grDevices::pdf, bg = "white")
    }
  )

  # Banner above the results map: only shown once an analysis has been run.
  output$results_banner <- renderUI({
    req(rv$results)
    div(class = "card-header results-banner",
        style = "background-color: #E9E2F3; border-bottom: 2px solid #4F2D7F; padding: 14px 18px;",
        div(class = "d-flex justify-content-between align-items-center flex-wrap gap-3",
            span(style = "font-size: 1.2rem; font-weight: 600; color: #4F2D7F;",
                 "Analysis results"),
            uiOutput("school_layer_controls")))
  })

  output$school_layer_controls <- renderUI({
    req(rv$results, rv$school_data)
    req(any(!is.na(rv$school_data$.vax_rate)))
    div(class = "d-flex gap-4 align-items-center flex-wrap",
      style = "font-size: 1.05rem; font-weight: 500; color: #3a2163;",
      checkboxInput("show_schools",
                    "Show schools on the map (colored by vaccination rate)", FALSE),
      conditionalPanel(
        condition = "input.show_schools",
        radioButtons("school_layer_scope", NULL,
                     choices = c("All schools" = "all",
                                 "Only schools in areas of concern" = "concern"),
                     inline = TRUE)
      )
    )
  })

  observe({
    req(rv$results)
    proxy <- leafletProxy("results_map") |>
      clearGroup("schools") |>
      removeControl("school_legend")
    if (!isTRUE(input$show_schools) || is.null(rv$school_data)) return()

    sd  <- rv$school_data
    lat <- rv$lat_col %||% "latitude"
    lon <- rv$lon_col %||% "longitude"
    sd  <- sd[!is.na(sd[[lat]]) & !is.na(sd[[lon]]) & !is.na(sd$.vax_rate), , drop = FALSE]
    if (identical(input$school_layer_scope, "concern")) {
      gids <- concern_geoids(rv$results, rv$analyze_target %||% "undervax_count", "concern")
      sd   <- sd[as.character(sd$GEOID) %in% gids, , drop = FALSE]
    }
    if (nrow(sd) == 0) return()

    nm <- if (!is.null(rv$school_col) && rv$school_col %in% names(sd)) {
      as.character(sd[[rv$school_col]])
    } else {
      paste("School", seq_len(nrow(sd)))
    }
    enr <- if (!is.null(rv$eligible_col) && rv$eligible_col %in% names(sd)) {
      sd[[rv$eligible_col]]
    } else {
      NA
    }
    popup <- paste0(
      "<strong>", htmltools::htmlEscape(nm), "</strong><br>",
      "Vaccinated: ", sprintf("%.1f%%", 100 * sd$.vax_rate),
      ifelse(is.na(enr), "", paste0("<br>Enrolled: ", enr))
    )

    # School vaccination-rate colors (also used to shade the schools table)
    vax_colors <- c(
      "<85%"     = "#d73027",
      "85-89.9%" = "#fc8d00",
      "90-94.9%" = "#fee600",
      "≥95%" = "#1aaf00"
    )
    vax_bin <- cut(sd$.vax_rate * 100, breaks = c(-Inf, 85, 90, 95, Inf),
                   labels = names(vax_colors), right = FALSE)
    vax_fill <- unname(vax_colors[as.character(vax_bin)])

    proxy |>
      addCircleMarkers(
        lng = sd[[lon]], lat = sd[[lat]],
        radius = 4, stroke = TRUE, color = "#222222", weight = 0.6,
        fillColor = vax_fill, fillOpacity = 0.9,
        popup = popup, group = "schools"
      ) |>
      addLegend(
        position = "bottomleft",
        colors   = unname(vax_colors), labels = names(vax_colors), opacity = 1,
        title    = "School vaccination rate",
        layerId  = "school_legend"
      )
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

