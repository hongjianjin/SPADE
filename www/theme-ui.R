#' App Theme Registry
#' App Theme Registry
#'
#' Executes app_theme_registry.
#'
#' @return Return value produced by app_theme_registry.
app_theme_registry <- function() {
  list(
    zephyr = list(
      name = "zephyr",
      colors = list(
        primary = "#2FA4E7",
        info = "#5bc0de",
        success = "#73A839",
        warning = "#DD5600",
        danger = "#C71C22",
        background = "#f7f7f9",
        fgsea_up = "#73A839",
        fgsea_down = "#C71C22",
        fgsea_dot_low = "#2FA4E7",
        fgsea_dot_high = "#C71C22"
      ),
      fresh_ui_theme = fresh::create_theme(
        fresh::adminlte_color(
          light_blue = "#2FA4E7",
          blue = "#2FA4E7",
          aqua = "#5bc0de",
          green = "#73A839",
          yellow = "#DD5600",
          red = "#C71C22",
          navy = "#343a40"
        ),
        fresh::adminlte_global(
          content_bg = "#f7f7f9",
          box_bg = "#ffffff",
          info_box_bg = "#ffffff"
        ),
        fresh::adminlte_sidebar(
          dark_bg = "#3a3f44",
          dark_hover_bg = "#2FA4E7",
          dark_color = "#f8f9fa",
          dark_hover_color = "#ffffff"
        )
      ),
      body_theme = bslib::bs_theme(bootswatch = "zephyr"),
      css_file = "theme/theme-zephyr.css"
    ),
    fresh = list(
      name = "fresh",
      colors = list(
        primary = "#16a085",
        info = "#2980b9",
        success = "#27ae60",
        warning = "#f39c12",
        danger = "#c0392b",
        background = "#f8f9fa",
        fgsea_up = "#27ae60",
        fgsea_down = "#c0392b",
        fgsea_dot_low = "#16a085",
        fgsea_dot_high = "#c0392b"
      ),
      fresh_ui_theme = fresh::create_theme(
        fresh::adminlte_color(
          light_blue = "#16a085",
          blue = "#16a085",
          aqua = "#2980b9",
          green = "#27ae60",
          yellow = "#f39c12",
          red = "#c0392b",
          navy = "#138d75"
        ),
        fresh::adminlte_sidebar(
          dark_bg = "#212529",
          dark_hover_bg = "#16a085",
          dark_color = "#f8f9fa",
          dark_hover_color = "#ffffff"
        ),
        fresh::adminlte_global(
          content_bg = "#f8f9fa",
          box_bg = "#ffffff",
          info_box_bg = "#ffffff"
        )
      ),
      body_theme = bslib::bs_theme(
        version = 3,
        bg = "#f8f9fa",
        fg = "#212529",
        primary = "#16a085",
        success = "#27ae60",
        info = "#2980b9",
        warning = "#f39c12",
        danger = "#c0392b",
        base_font = bslib::font_google("Source Sans 3")
      ),
      css_file = "theme/theme-fresh.css"
    )
  )
}

#' Get App Theme
#'
#' Executes get_app_theme.
#'
#' @param theme_name Argument for get_app_theme.
#'
#' @return Return value produced by get_app_theme.
get_app_theme <- function(theme_name = getOption("shinyRNAseq.theme", "fresh")) {
  registry <- app_theme_registry()
  key <- tolower(theme_name)
  if (!key %in% names(registry)) {
    stop(sprintf("Unknown theme '%s'. Available themes: %s",
      theme_name,
      paste(names(registry), collapse = ", ")
    ))
  }
  registry[[key]]
}

#' Use App Theme
#'
#' Executes use_app_theme.
#'
#' @param theme_name Argument for use_app_theme.
#'
#' @return Return value produced by use_app_theme.
use_app_theme <- function(theme_name = getOption("shinyRNAseq.theme", "fresh")) {
  theme <- get_app_theme(theme_name)
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = theme$css_file)
    ),
    fresh::use_theme(theme$fresh_ui_theme)
  )
}
