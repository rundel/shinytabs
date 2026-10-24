# A tabset app driven through a real browser. Tabs of type "note" hold a text
# input whose content makes the tab refuse to close without confirmation.
tabs_app = function() {
  ui = bslib::page_fillable(
    shinytabs::tabset_ui("tabs", controls = shiny::actionButton("add_note", "Note"))
  )
  server = function(input, output, session) {
    tabs = shinytabs::tabset_server(
      "tabs",
      types = c("Plain" = "plain", "Note" = "note"),
      initial = "plain",
      renameable = TRUE,
      ui = function(id, type) {
        ns = shiny::NS(id)
        if (type == "note") shiny::textInput(ns("text"), "Note") else shiny::div(class = "plain", type)
      },
      server = function(input, output, session, type) {
        list(dirty = shiny::reactive(type == "note" && isTRUE(nzchar(input$text))))
      },
      can_close = function(handle, tab) {
        if (shiny::isolate(handle$dirty())) "Unsaved note." else TRUE
      }
    )
    shiny::observe(tabs$add("note")) |> shiny::bindEvent(input$add_note)
  }
  shiny::shinyApp(ui, server)
}

tab_titles = "Array.from(document.querySelectorAll('#tabs-tabset .shinytabs-tab-title')).map(e => e.textContent)"
active_pane = "document.querySelector('#tabs-tabset .shinytabs-pane.active')?.dataset.tab"

test_that("tabs are added, switched and closed in the browser", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app = shinytest2::AppDriver$new(tabs_app(), name = "tabs", timeout = 10000)
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js("document.querySelectorAll('#tabs-tabset .shinytabs-tab').length === 1")
  expect_equal(app$get_js(tab_titles), list("Plain 1"))
  expect_equal(app$get_js(active_pane), "tab1")
  expect_equal(app$get_text("#tabs-tabset .shinytabs-pane.active .plain"), "plain")

  # The + button opens a menu with both types; picking one adds a tab
  app$click(selector = "#tabs-tabset .shinytabs-add-btn")
  app$wait_for_js("!document.querySelector('#tabs-tabset .shinytabs-menu').hidden")
  expect_equal(
    app$get_js("Array.from(document.querySelectorAll('#tabs-tabset .shinytabs-menu-item')).map(e => e.textContent)"),
    list("Plain", "Note")
  )
  app$click(selector = "#tabs-tabset .shinytabs-menu-item[data-key='note']")
  app$wait_for_js("document.querySelectorAll('#tabs-tabset .shinytabs-tab').length === 2")
  expect_equal(app$get_js(tab_titles), list("Plain 1", "Note 1"))
  expect_equal(app$get_js(active_pane), "tab2")
  expect_true(app$get_js("document.getElementById('tabs-tab2-text') != null"))

  # Clicking a tab selects it and hides the others' panes without removing them
  app$click(selector = "#tabs-tabset .shinytabs-tab[data-id='tab1']")
  app$wait_for_js(paste(active_pane, "=== 'tab1'"))
  expect_equal(
    app$get_js("getComputedStyle(document.querySelector('#tabs-tabset .shinytabs-pane[data-tab=\"tab2\"]')).visibility"),
    "hidden"
  )

  # A clean note closes immediately; a dirty one asks first
  app$click(selector = "#tabs-tabset .shinytabs-tab[data-id='tab2'] .shinytabs-tab-close")
  app$wait_for_js("document.querySelectorAll('#tabs-tabset .shinytabs-tab').length === 1")
  expect_null(app$get_js("document.getElementById('tabs-tab2-text')"))

  app$click("add_note")
  app$wait_for_js("document.getElementById('tabs-tab3-text') != null")
  app$set_inputs(`tabs-tab3-text` = "draft")
  app$click(selector = "#tabs-tabset .shinytabs-tab[data-id='tab3'] .shinytabs-tab-close")
  app$wait_for_js("document.querySelector('#shiny-modal.show .modal-body') != null")
  expect_match(app$get_text("#shiny-modal .modal-body"), "Unsaved note.")
  expect_equal(app$get_js("document.querySelectorAll('#tabs-tabset .shinytabs-tab').length"), 2L)

  # Cancel keeps the tab (Bootstrap ignores dismiss clicks during the fade-in)
  app$wait_for_js("getComputedStyle(document.querySelector('#shiny-modal .modal-dialog')).transform === 'none'")
  app$run_js("document.querySelector('#shiny-modal [data-bs-dismiss]').click()")
  app$wait_for_js("document.querySelector('#shiny-modal') == null")
  expect_equal(app$get_js("document.querySelectorAll('#tabs-tabset .shinytabs-tab').length"), 2L)

  app$click(selector = "#tabs-tabset .shinytabs-tab[data-id='tab3'] .shinytabs-tab-close")
  app$wait_for_js("document.querySelector('#shiny-modal.show .modal-body') != null")
  app$click("tabs-close_confirm")
  app$wait_for_js("document.querySelectorAll('#tabs-tabset .shinytabs-tab').length === 1")
  expect_equal(app$get_js(active_pane), "tab1")

  # Closing the last tab shows the empty state
  app$click(selector = "#tabs-tabset .shinytabs-tab[data-id='tab1'] .shinytabs-tab-close")
  app$wait_for_js("document.querySelectorAll('#tabs-tabset .shinytabs-tab').length === 0")
  expect_true(app$get_js("document.getElementById('tabs-tabset').classList.contains('empty')"))
  expect_equal(
    app$get_js("getComputedStyle(document.querySelector('#tabs-tabset .shinytabs-empty')).display"),
    "flex"
  )
})

test_that("tabs can be renamed and reordered in the browser", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app = shinytest2::AppDriver$new(tabs_app(), name = "rename", timeout = 10000)
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js("document.querySelectorAll('#tabs-tabset .shinytabs-tab').length === 1")
  app$click("add_note")
  app$wait_for_js("document.querySelectorAll('#tabs-tabset .shinytabs-tab').length === 2")

  app$run_js("
    var tab = document.querySelector('#tabs-tabset .shinytabs-tab[data-id=\"tab1\"]');
    tab.dispatchEvent(new MouseEvent('dblclick', {bubbles: true}));
    var input = tab.querySelector('input.shinytabs-rename');
    input.value = 'Scratch';
    input.dispatchEvent(new KeyboardEvent('keydown', {key: 'Enter', bubbles: true}));
  ")
  app$wait_for_js(paste(tab_titles, "[0] === 'Scratch'"))
  app$wait_for_idle()
  expect_equal(app$get_js(tab_titles), list("Scratch", "Note 1"))

  # Simulate a drop of tab2 onto the left half of tab1
  app$run_js("
    var tabs = document.querySelector('#tabs-tabset .shinytabs-tabs');
    var t1 = tabs.querySelector('[data-id=\"tab1\"]');
    var t2 = tabs.querySelector('[data-id=\"tab2\"]');
    var dt = new DataTransfer();
    t2.dispatchEvent(new DragEvent('dragstart', {bubbles: true, dataTransfer: dt}));
    var r = t1.getBoundingClientRect();
    t1.dispatchEvent(new DragEvent('drop', {bubbles: true, dataTransfer: dt, clientX: r.left + 1}));
    t2.dispatchEvent(new DragEvent('dragend', {bubbles: true, dataTransfer: dt}));
  ")
  app$wait_for_js(paste(tab_titles, "[0] === 'Note 1'"))
  app$wait_for_idle()
  expect_equal(app$get_js(tab_titles), list("Note 1", "Scratch"))
})
