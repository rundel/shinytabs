# HTML dependency for the tabset element: the shinytabs input binding and its
# stylesheet.

shinytabs_dependencies = function() {
  htmltools::htmlDependency(
    name = "shinytabs",
    version = as.character(utils::packageVersion("shinytabs")),
    src = c(file = "shinytabs"),
    package = "shinytabs",
    script = "shinytabs.js",
    stylesheet = "shinytabs.css"
  )
}
