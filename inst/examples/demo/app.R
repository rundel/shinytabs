library(shiny)

ui = bslib::page_fillable(
  title = "shinytabs demo",
  padding = 0,
  shinytabs::tabset_ui(
    "tabs",
    controls = tagList(
      bslib::input_dark_mode(id = "dark_mode"),
      actionButton("add_table", "Add table", class = "btn-sm btn-outline-secondary")
    )
  )
)

server = function(input, output, session) {
  tabs = shinytabs::tabset_server(
    "tabs",
    types = c("Plot" = "plot", "Table" = "table", "Notes" = "notes"),
    initial = c("plot", "notes"),
    renameable = TRUE,
    ui = function(id, type) {
      ns = NS(id)
      switch(
        type,
        plot = plotOutput(ns("plot"), height = "100%"),
        table = div(class = "p-3", tableOutput(ns("table"))),
        notes = div(class = "p-3", textAreaInput(ns("text"), NULL, rows = 12, width = "100%"))
      )
    },
    server = function(input, output, session, type) {
      if (type == "plot") output$plot = renderPlot(plot(rnorm(100)))
      if (type == "table") output$table = renderTable(head(mtcars))
      list(type = type, dirty = reactive(type == "notes" && isTRUE(nzchar(input$text))))
    },
    can_close = function(handle, tab) {
      if (isolate(handle$dirty())) "This tab has unsaved notes." else TRUE
    }
  )

  observe(tabs$add("table")) |> bindEvent(input$add_table)
}

shinyApp(ui, server)
