# A tabset whose tabs record their id and type and whose closing is governed
# by a per-tab flag.
test_tabset = function(
  ...,
  types = c("Alpha" = "a", "Beta" = "b"),
  server = function(input, output, session, type) {
    list(id = session$ns(NULL), type = type, busy = shiny::reactiveVal(FALSE))
  }
) {
  list(
    ui = function(id, type) htmltools::div(id = id, type),
    server = server,
    types = types,
    ...
  )
}

test_that("tabs are added, selected, retitled and closed from the server", {
  shiny::testServer(tabset_server, args = test_tabset(initial = "a"), {
    session$setInputs(tabset = list(ready = TRUE, seq = 0))

    tabs = session$returned
    expect_equal(tabs$tabs()$id, "tab1")
    expect_equal(tabs$tabs()$title, "Alpha 1")
    expect_equal(tabs$selected(), "tab1")
    expect_equal(tabs$handle("tab1")$type, "a")
    expect_equal(tabs$handle("tab1")$id, session$ns("tab1"))

    id = tabs$add("b")
    expect_equal(id, "tab2")
    expect_equal(tabs$tabs()$title, c("Alpha 1", "Beta 1"))
    expect_equal(tabs$selected(), "tab2")

    id3 = tabs$add("a", title = "Custom", select = FALSE)
    expect_equal(tabs$tabs()$title, c("Alpha 1", "Beta 1", "Custom"))
    expect_equal(tabs$selected(), "tab2")

    tabs$select("tab1")
    expect_equal(tabs$selected(), "tab1")
    tabs$set_title("tab1", "Renamed")
    expect_equal(tabs$tabs()$title[1], "Renamed")
    tabs$set_icon("tab1", htmltools::span(class = "dot"))
    expect_match(priv$tabs$tab1$icon, '<span class="dot">')

    # Closing the selected tab selects its right-hand neighbour, or the new
    # last tab when it was last.
    tabs$close("tab1")
    expect_equal(tabs$tabs()$id, c("tab2", "tab3"))
    expect_equal(tabs$selected(), "tab2")
    tabs$select("tab3")
    tabs$close("tab3")
    expect_equal(tabs$selected(), "tab2")
    tabs$close("tab2")
    expect_equal(nrow(tabs$tabs()), 0L)
    expect_null(tabs$selected())

    expect_error(tabs$add("zzz"), "Unknown tab type")
  })
})

test_that("client events drive the tab list", {
  shiny::testServer(tabset_server, args = test_tabset(renameable = TRUE), {
    session$setInputs(tabset = list(ready = TRUE, seq = 0))
    tabs = session$returned
    expect_equal(nrow(tabs$tabs()), 0L)

    session$setInputs(tabset_event = list(type = "add", key = "a", n = 1))
    session$setInputs(tabset_event = list(type = "add", key = "b", n = 2))
    session$setInputs(tabset_event = list(type = "add", key = "a", n = 3))
    expect_equal(tabs$tabs()$title, c("Alpha 1", "Beta 1", "Alpha 2"))
    expect_equal(tabs$selected(), "tab3")

    session$setInputs(tabset_event = list(type = "select", id = "tab1", n = 4))
    expect_equal(tabs$selected(), "tab1")
    session$setInputs(tabset_event = list(type = "select", id = "nope", n = 5))
    expect_equal(tabs$selected(), "tab1")

    session$setInputs(tabset_event = list(type = "reorder", ids = list("tab3", "tab1", "tab2"), n = 6))
    expect_equal(tabs$tabs()$id, c("tab3", "tab1", "tab2"))
    session$setInputs(tabset_event = list(type = "reorder", ids = list("tab3", "tab1"), n = 7))
    expect_equal(tabs$tabs()$id, c("tab3", "tab1", "tab2"))

    session$setInputs(tabset_event = list(type = "rename", id = "tab2", title = "  Notes ", n = 8))
    expect_equal(tabs$tabs()$title[3], "Notes")

    session$setInputs(tabset_event = list(type = "close", id = "tab1", n = 9))
    expect_equal(tabs$tabs()$id, c("tab3", "tab2"))
    expect_equal(tabs$selected(), "tab2")
  })
})

test_that("a server declaring `tab` receives the tab's details", {
  args = test_tabset(
    initial = "b",
    server = function(input, output, session, type, tab) tab
  )
  shiny::testServer(tabset_server, args = args, {
    session$setInputs(tabset = list(ready = TRUE, seq = 0))
    expect_equal(session$returned$handle("tab1"), list(id = "tab1", type = "b", title = "Beta 1"))
  })
})

test_that("renaming is ignored unless enabled", {
  shiny::testServer(tabset_server, args = test_tabset(initial = "a"), {
    session$setInputs(tabset = list(ready = TRUE, seq = 0))
    session$setInputs(tabset_event = list(type = "rename", id = "tab1", title = "New", n = 1))
    expect_equal(session$returned$tabs()$title, "Alpha 1")
  })
})

test_that("can_close guards user closes and on_close runs on every removal", {
  closed = character()
  args = test_tabset(
    initial = c("a", "b"),
    can_close = function(handle, tab) {
      if (shiny::isolate(handle$busy())) paste(tab$title, "is busy") else TRUE
    },
    on_close = function(handle, tab) closed <<- c(closed, tab$id)
  )
  shiny::testServer(tabset_server, args = args, {
    session$setInputs(tabset = list(ready = TRUE, seq = 0))
    tabs = session$returned
    expect_equal(tabs$selected(), "tab1")

    tabs$handle("tab1")$busy(TRUE)
    session$setInputs(tabset_event = list(type = "close", id = "tab1", n = 1))
    expect_equal(tabs$tabs()$id, c("tab1", "tab2"))
    expect_equal(priv$closing, "tab1")

    session$setInputs(close_confirm = 1)
    expect_equal(tabs$tabs()$id, "tab2")
    expect_equal(closed, "tab1")

    # A busy tab can still be removed without confirmation from the server
    tabs$handle("tab2")$busy(TRUE)
    expect_false(tabs$close("tab2"))
    expect_equal(tabs$tabs()$id, "tab2")
    tabs$close("tab2", confirm = FALSE)
    expect_equal(nrow(tabs$tabs()), 0L)
    expect_equal(closed, c("tab1", "tab2"))
  })
})

test_that("can_close returning FALSE keeps the tab", {
  args = test_tabset(initial = "a", can_close = function(handle, tab) FALSE)
  shiny::testServer(tabset_server, args = args, {
    session$setInputs(tabset = list(ready = TRUE, seq = 0))
    session$setInputs(tabset_event = list(type = "close", id = "tab1", n = 1))
    expect_equal(session$returned$tabs()$id, "tab1")
    expect_null(priv$closing)
  })
})

test_that("closeable, max_tabs and custom titles are honoured", {
  args = test_tabset(
    closeable = FALSE,
    max_tabs = 2,
    title = function(type, label, n) paste0(toupper(type), "#", n)
  )
  shiny::testServer(tabset_server, args = args, {
    session$setInputs(tabset = list(ready = TRUE, seq = 0))
    tabs = session$returned
    tabs$add("a")
    tabs$add("b", closeable = TRUE)
    expect_null(tabs$add("a"))
    expect_equal(tabs$tabs()$title, c("A#1", "B#1"))
    expect_equal(vapply(priv$tabs, `[[`, TRUE, "closeable"), c(tab1 = FALSE, tab2 = TRUE))

    # A close event for a non-closeable tab is ignored
    session$setInputs(tabset_event = list(type = "close", id = "tab1", n = 1))
    expect_equal(nrow(tabs$tabs()), 2L)
  })
})

test_that("tabs added before the client is ready are flushed on readiness", {
  shiny::testServer(tabset_server, args = test_tabset(initial = c("a", "b")), {
    expect_equal(length(priv$pending), 2L)
    expect_false(priv$ready)
    expect_equal(session$returned$selected(), "tab1")

    session$setInputs(tabset = list(ready = TRUE, seq = 0))
    expect_true(priv$ready)
    expect_equal(length(priv$pending), 0L)
  })
})

test_that("types may be reactive", {
  types = shiny::reactiveVal(c("One" = "one"))
  shiny::testServer(tabset_server, args = test_tabset(types = types), {
    session$setInputs(tabset = list(ready = TRUE, seq = 0))
    tabs = session$returned
    tabs$add()
    expect_equal(tabs$tabs()$title, "One 1")
    expect_error(tabs$add("two"), "Unknown")
    types(c("One" = "one", "Two" = "two"))
    session$flushReact()
    tabs$add("two")
    expect_equal(tabs$tabs()$title, c("One 1", "Two 1"))
  })
})

test_that("tabset_server validates its callbacks and types", {
  expect_error(tabset_server("x", ui = 1), "is.function")
  expect_error(tabset_server("x", ui = function(id, type) NULL, types = character()), "non-empty")
})
