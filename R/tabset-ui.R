#' Dynamic tabset module UI
#'
#' Creates the tab bar and pane area for a [tabset_server()] module with the
#' same `id`. The bar holds the tabs, a "+" button that opens a new tab (as a
#' menu when the server offers several tab types) and an optional `controls`
#' slot at its right end. Tab panes are created by the server as tabs are
#' added.
#'
#' The element picks up Bootstrap 5 CSS variables when the page has them, so
#' it follows bslib themes and the colour mode set by
#' [bslib::input_dark_mode()]; on other pages it falls back to a neutral light
#' palette.
#'
#' @param id Module id, matching the `id` passed to [tabset_server()].
#' @param height CSS height of the whole tabset (anything
#'   [htmltools::validateCssUnit()] accepts). `NULL` (the default) makes it a
#'   fill item that grows to fill a fillable parent such as
#'   [bslib::page_fillable()] or [bslib::card_body()]; in a parent that is not
#'   fillable, give a height.
#' @param controls UI placed at the right end of the tab bar, e.g. buttons
#'   acting on the selected tab or a [bslib::input_dark_mode()] toggle.
#' @param empty UI shown in the pane area while no tabs are open. `NULL`
#'   shows nothing.
#' @param ... Additional attributes for the outer `div`.
#'
#' @return An HTML tag list.
#' @export
#' @examples
#' tabset_ui("tabs")
#' tabset_ui("tabs", height = "500px", controls = shiny::actionButton("save", "Save"))
tabset_ui = function(
  id,
  height = NULL,
  controls = NULL,
  empty = "No open tabs. Use + to add one.",
  ...
) {
  ns = shiny::NS(id)

  bar = htmltools::div(
    class = "shinytabs-bar",
    htmltools::div(class = "shinytabs-tabs", role = "tablist"),
    htmltools::div(
      class = "shinytabs-add",
      htmltools::tags$button(
        type = "button",
        class = "shinytabs-add-btn",
        `aria-label` = "New tab",
        `aria-expanded` = "false",
        "+"
      ),
      htmltools::div(class = "shinytabs-menu", role = "menu", hidden = NA)
    ),
    if (!is.null(controls)) htmltools::div(class = "shinytabs-controls", controls)
  )

  panes = htmltools::div(
    id = ns("panes"),
    class = "shinytabs-panes",
    if (!is.null(empty)) htmltools::div(class = "shinytabs-empty", empty)
  )

  tag = htmltools::div(id = ns("tabset"), class = "shinytabs", bar, panes, ...)

  if (is.null(height)) {
    tag = htmltools::bindFillRole(tag, item = TRUE)
  } else {
    tag = htmltools::tagAppendAttributes(
      tag,
      style = htmltools::css(height = htmltools::validateCssUnit(height))
    )
  }

  htmltools::tagList(shinytabs_dependencies(), tag)
}
