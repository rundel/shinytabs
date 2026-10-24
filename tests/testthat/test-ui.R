test_that("tabset_ui builds a namespaced element with its dependency", {
  ui = tabset_ui("tabs", height = "300px", controls = shiny::actionButton("go", "Go"))
  html = as.character(ui)

  expect_match(html, 'id="tabs-tabset"')
  expect_match(html, 'class="shinytabs"')
  expect_match(html, 'id="tabs-panes"')
  expect_match(html, "height:300px")
  expect_match(html, 'class="shinytabs-controls"')
  expect_match(html, 'id="go"')
  expect_match(html, "No open tabs")

  deps = htmltools::findDependencies(ui)
  expect_equal(vapply(deps, `[[`, "", "name"), "shinytabs")
})

test_that("tabset_ui with height = NULL is a fill item", {
  html = as.character(tabset_ui("tabs"))
  expect_match(html, "html-fill-item")
  expect_no_match(html, "[^-]height:")
})

test_that("tabset_ui can omit the empty state and the controls", {
  html = as.character(tabset_ui("tabs", empty = NULL))
  expect_no_match(html, "shinytabs-empty")
  expect_no_match(html, "shinytabs-controls")
})

test_that("parse_types follows the choices convention", {
  tp = parse_types(c("Shell" = "shell", "editor"))
  expect_equal(tp$key, c("shell", "editor"))
  expect_equal(tp$label, c("Shell", "editor"))
  expect_equal(parse_types(list(a = "x"))$label, "a")
  expect_error(parse_types(character()), "non-empty")
  expect_error(parse_types(c("a", "a")), "unique")
  expect_error(parse_types(1:2), "character")
})
