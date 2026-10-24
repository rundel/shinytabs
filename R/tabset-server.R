#' Dynamic tabset module server
#'
#' Manages the tabs of a [tabset_ui()] element: every tab hosts an instance
#' of a module built by the `ui` and `server` callbacks for one of the tab
#' `types`. Users add tabs with the "+" button (a menu when there are several
#' types), close them with each tab's close button, middle click or the
#' `Delete` key, drag them into a new order and, with `renameable = TRUE`,
#' rename them with a double click or `F2`. The same operations are available
#' from the server through the returned controls, and the tab list and
#' selection are exposed as reactives.
#'
#' @section Tab modules:
#' Each tab is a Shiny module instance. `ui(id, type)` is the module's UI
#' function: it receives the tab's namespaced id and returns the tab's
#' content, so inputs and outputs inside it are created with
#' `ns = shiny::NS(id); ns("name")`, or by passing `id` (or `ns("inner")`)
#' to another module's UI function. `server(input, output, session, type)` is
#' the module's server function, run with [shiny::moduleServer()] for the
#' same id, so `input$name` and `output$name` refer to that tab's elements and
#' nested modules are started as usual (`inner_server("inner")`).
#'
#' Whatever `server()` returns is kept as the tab's *handle*: it is passed to
#' `can_close()` and `on_close()` and available through the returned
#' `handle(id)`, so tabs can expose reactives and controls to the app.
#'
#' Panes are kept in the page while hidden (invisible, not removed from
#' layout), so their contents keep their size and state while another tab is
#' selected.
#'
#' @section Closing:
#' When a tab is closed by the user, `can_close(handle, tab)` decides what
#' happens: `TRUE` closes it, `FALSE` keeps it, and a character string asks
#' the user to confirm in a modal dialog showing that string. `on_close()` is
#' called just before a tab is removed, however it was closed, for cleanup
#' such as stopping a process. `tab` is a list with `id`, `type` and `title`.
#'
#' @param id Module id, matching the `id` passed to [tabset_ui()].
#' @param ui Module UI function `(id, type)` building a tab's content; see
#'   *Tab modules*.
#' @param server Module server function `(input, output, session, type)` for
#'   a tab, optionally with a fifth argument `tab`, returning the tab's
#'   handle; `NULL` for tabs without server logic.
#' @param types The tab types users can open, as a character vector of keys
#'   with optional names as labels (`c("Shell" = "shell", "Editor" = "editor")`,
#'   the convention of [shiny::selectInput()] choices), or a reactive
#'   returning one. The keys are what `ui()`, `server()` and `add()` receive
#'   as `type`; the labels fill the "+" menu and the default titles. A single
#'   type turns the "+" button into a plain button.
#' @param initial Character vector of type keys to open when the session
#'   starts, in order; the first is selected. `NULL` starts empty.
#' @param title Function `(type, label, n)` giving a new tab's title, where
#'   `n` counts tabs of that type opened so far. The default is the label
#'   followed by `n`.
#' @param can_close Function `(handle, tab)` consulted when the user closes a
#'   tab; see *Closing*. `NULL` always closes.
#' @param on_close Function `(handle, tab)` called before a tab is removed.
#' @param closeable Whether tabs show a close button. Can be overridden per
#'   tab in `add()`.
#' @param reorderable Whether tabs can be dragged into a new order.
#' @param renameable Whether tabs can be renamed in place.
#' @param max_tabs Maximum number of open tabs; `add()` does nothing beyond
#'   it.
#'
#' @return A list of controls and reactives:
#' \describe{
#'   \item{`add(type = NULL, title = NULL, select = TRUE, closeable = NULL)`}{
#'     Opens a tab of `type` (the first type by default) and returns its id.}
#'   \item{`close(id, confirm = TRUE)`}{Closes a tab; with `confirm = TRUE`
#'     `can_close()` is consulted as for a user action, with `FALSE` the tab
#'     is removed unconditionally.}
#'   \item{`select(id)`, `set_title(id, title)`, `set_icon(id, icon)`}{Select
#'     a tab, change its title (plain text) or its icon (HTML or tags shown
#'     before the title, `NULL` for none).}
#'   \item{`handle(id)`}{The value `server()` returned for the tab.}
#'   \item{`tabs()`}{Reactive data frame of the open tabs in display order,
#'     with columns `id`, `type` and `title`.}
#'   \item{`selected()`}{Reactive: id of the selected tab, `NULL` when none.}
#' }
#' @export
#' @examples
#' if (interactive()) {
#'   ui = bslib::page_fillable(tabset_ui("tabs"))
#'   server = function(input, output, session) {
#'     tabset_server(
#'       "tabs",
#'       types = c("Plot" = "plot", "Table" = "table"),
#'       initial = "plot",
#'       ui = function(id, type) {
#'         ns = shiny::NS(id)
#'         if (type == "plot") shiny::plotOutput(ns("out")) else shiny::tableOutput(ns("out"))
#'       },
#'       server = function(input, output, session, type) {
#'         if (type == "plot") {
#'           output$out = shiny::renderPlot(plot(rnorm(100)))
#'         } else {
#'           output$out = shiny::renderTable(head(mtcars))
#'         }
#'       }
#'     )
#'   }
#'   shiny::shinyApp(ui, server)
#' }
tabset_server = function(
  id,
  ui,
  server = NULL,
  types = "tab",
  initial = NULL,
  title = NULL,
  can_close = NULL,
  on_close = NULL,
  closeable = TRUE,
  reorderable = TRUE,
  renameable = FALSE,
  max_tabs = Inf
) {
  stopifnot(is.function(ui), is.null(server) || is.function(server))
  if (!is.null(title)) stopifnot(is.function(title))
  if (!is.null(can_close)) stopifnot(is.function(can_close))
  if (!is.null(on_close)) stopifnot(is.function(on_close))
  if (!shiny::is.reactive(types)) parse_types(types)
  accepts_tab = !is.null(server) && any(c("tab", "...") %in% names(formals(server)))

  shiny::moduleServer(id, function(input, output, session) {
    ns = session$ns

    priv = new.env(parent = emptyenv())
    priv$tabs = list()
    priv$handles = list()
    priv$selected = NULL
    priv$counter = 0L
    priv$type_counts = list()
    priv$ready = FALSE
    priv$pending = list()
    priv$closing = NULL
    title_fn = title
    default_closeable = closeable

    state = shiny::reactiveValues(tabs = list(), selected = NULL)

    current_types = function() {
      parse_types(if (shiny::is.reactive(types)) shiny::isolate(types()) else types)
    }

    pane_selector = function(id) paste0("#", ns(paste0(id, "_pane")))

    tab_info = function(tab) tab[c("id", "type", "title")]

    publish = function() {
      state$tabs = lapply(priv$tabs, tab_info)
      state$selected = priv$selected
      if (!priv$ready) return(invisible())
      tp = current_types()
      session$sendCustomMessage("shinytabs", list(
        id = ns("tabset"),
        state = list(
          tabs = unname(lapply(priv$tabs, function(t) {
            list(id = t$id, title = t$title, icon = t$icon, closeable = t$closeable)
          })),
          selected = priv$selected,
          types = unname(Map(function(k, l) list(key = k, label = l), tp$key, tp$label)),
          reorderable = isTRUE(reorderable),
          renameable = isTRUE(renameable)
        )
      ))
    }

    insert_pane = function(pane) {
      shiny::insertUI(
        selector = paste0("#", ns("panes")),
        where = "beforeEnd",
        ui = pane,
        immediate = TRUE,
        session = session
      )
    }

    add = function(type = NULL, title = NULL, select = TRUE, closeable = NULL) {
      tp = current_types()
      if (is.null(type)) type = tp$key[[1]]
      if (!type %in% tp$key) {
        stop("Unknown tab type '", type, "'", call. = FALSE)
      }
      if (length(priv$tabs) >= max_tabs) return(invisible(NULL))

      priv$counter = priv$counter + 1L
      id = paste0("tab", priv$counter)
      n = (priv$type_counts[[type]] %||% 0L) + 1L
      priv$type_counts[[type]] = n
      label = tp$label[[match(type, tp$key)]]
      if (is.null(title)) {
        title = if (is.null(title_fn)) paste(label, n) else title_fn(type, label, n)
      }

      priv$tabs[[id]] = list(
        id = id,
        type = type,
        title = as.character(title),
        icon = NULL,
        closeable = isTRUE(closeable %||% default_closeable)
      )

      pane = htmltools::div(
        id = ns(paste0(id, "_pane")),
        class = "shinytabs-pane",
        `data-tab` = id,
        ui(ns(id), type)
      )
      pane = htmltools::bindFillRole(pane, container = TRUE, item = TRUE)
      if (priv$ready) insert_pane(pane) else priv$pending[[id]] = pane

      if (!is.null(server)) {
        tab = tab_info(priv$tabs[[id]])
        priv$handles[[id]] = shiny::isolate(shiny::moduleServer(
          id,
          function(input, output, session) {
            if (accepts_tab) {
              server(input, output, session, type, tab = tab)
            } else {
              server(input, output, session, type)
            }
          },
          session = session
        ))
      }

      if (isTRUE(select) || is.null(priv$selected)) priv$selected = id
      publish()
      invisible(id)
    }

    remove = function(id) {
      tab = priv$tabs[[id]]
      if (is.null(tab)) return(invisible(FALSE))
      if (!is.null(on_close)) {
        shiny::isolate(on_close(priv$handles[[id]], tab_info(tab)))
      }
      if (priv$ready) {
        shiny::removeUI(pane_selector(id), immediate = TRUE, session = session)
      }
      priv$pending[[id]] = NULL

      ids = names(priv$tabs)
      idx = match(id, ids)
      priv$tabs[[id]] = NULL
      priv$handles[[id]] = NULL

      if (identical(priv$selected, id)) {
        ids = names(priv$tabs)
        priv$selected = if (length(ids)) ids[[min(idx, length(ids))]] else NULL
      }
      publish()
      invisible(TRUE)
    }

    close = function(id, confirm = TRUE) {
      tab = priv$tabs[[id]]
      if (is.null(tab)) return(invisible(FALSE))
      if (!isTRUE(confirm) || is.null(can_close)) return(remove(id))

      verdict = shiny::isolate(can_close(priv$handles[[id]], tab_info(tab)))
      if (isTRUE(verdict)) return(remove(id))
      if (is.character(verdict) && length(verdict) == 1 && nzchar(verdict)) {
        priv$closing = id
        shiny::showModal(close_dialog(ns, tab$title, verdict), session = session)
      }
      invisible(FALSE)
    }

    select = function(id) {
      if (is.null(priv$tabs[[id]]) || identical(priv$selected, id)) return(invisible(FALSE))
      priv$selected = id
      publish()
      invisible(TRUE)
    }

    set_field = function(id, field, value) {
      if (is.null(priv$tabs[[id]])) return(invisible(FALSE))
      priv$tabs[[id]][[field]] = value
      publish()
      invisible(TRUE)
    }

    set_title = function(id, title) {
      set_field(id, "title", as.character(title))
    }

    set_icon = function(id, icon) {
      if (!is.null(icon)) icon = as.character(htmltools::tagList(icon))
      set_field(id, "icon", icon)
    }

    reorder = function(ids) {
      ids = as.character(unlist(ids))
      if (!setequal(ids, names(priv$tabs)) || anyDuplicated(ids)) return(invisible(FALSE))
      priv$tabs = priv$tabs[ids]
      publish()
      invisible(TRUE)
    }

    shiny::observe({
      ev = input$tabset_event
      switch(
        as.character(ev$type),
        select = select(ev$id),
        close = if (isTRUE(priv$tabs[[ev$id]]$closeable)) close(ev$id),
        add = add(ev$key),
        reorder = reorder(ev$ids),
        rename = if (isTRUE(renameable) && nzchar(trimws(ev$title))) {
          set_title(ev$id, trimws(ev$title))
        }
      )
    }) |>
      shiny::bindEvent(input$tabset_event)

    shiny::observe({
      shiny::removeModal(session)
      id = priv$closing
      priv$closing = NULL
      if (!is.null(id)) remove(id)
    }) |>
      shiny::bindEvent(input$close_confirm)

    # The client reports its presence once bound; panes added before that are
    # inserted now and the tab bar receives the full state.
    shiny::observe({
      priv$ready = TRUE
      for (pane in priv$pending) insert_pane(pane)
      priv$pending = list()
      publish()
    }) |>
      shiny::bindEvent(input$tabset, once = TRUE)

    if (shiny::is.reactive(types)) {
      shiny::observe({
        parse_types(types())
        publish()
      })
    }

    for (type in initial) add(type, select = FALSE)
    if (length(priv$tabs)) priv$selected = names(priv$tabs)[[1]]
    publish()

    list(
      add = add,
      close = close,
      select = select,
      set_title = set_title,
      set_icon = set_icon,
      handle = function(id) priv$handles[[id]],
      tabs = shiny::reactive(tabs_frame(state$tabs)),
      selected = shiny::reactive(state$selected)
    )
  })
}

# Normalises the `types` argument to a data frame of keys and labels. Names
# are labels and values are keys, as for shiny choices; unnamed entries use
# the key as the label.
#
# types: character vector, or a list of strings

parse_types = function(types) {
  if (is.list(types)) types = unlist(types)
  if (!is.character(types) || length(types) == 0 || anyNA(types) || !all(nzchar(types))) {
    stop("`types` must be a non-empty character vector of tab type keys", call. = FALSE)
  }
  if (anyDuplicated(types)) {
    stop("`types` keys must be unique", call. = FALSE)
  }
  labels = names(types)
  if (is.null(labels)) labels = rep("", length(types))
  labels[!nzchar(labels)] = types[!nzchar(labels)]
  data.frame(key = unname(types), label = unname(labels), stringsAsFactors = FALSE)
}

# The open tabs as a data frame in display order.
#
# tabs: list of tab records with id, type and title

tabs_frame = function(tabs) {
  data.frame(
    id = vapply(tabs, `[[`, "", "id"),
    type = vapply(tabs, `[[`, "", "type"),
    title = vapply(tabs, `[[`, "", "title"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# The confirmation dialog shown when can_close() returns a message.
#
# ns: the module's namespace function
# title: the tab's title
# message: the text can_close() returned

close_dialog = function(ns, title, message) {
  shiny::modalDialog(
    title = paste0("Close \"", title, "\"?"),
    message,
    footer = htmltools::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("close_confirm"), "Close", class = "btn-danger")
    ),
    easyClose = TRUE
  )
}

`%||%` = function(x, y) if (is.null(x)) y else x
