#' Human Augmented Matching
#'
#' @param source a vector of items you want to find a match for
#' @param key  a vector of possible matches
#' @param ... additional arguments
#'
#' @description The app presents you with all possible values in \code{key} that
#' match a simple grep for each value of \code{source}
#'
#' @return Nothing. A shiny app.
#' @export
#' @import shinhy
#' @import dplyr
#' @examples
#' \dontrun{
#' ham(source = letters, key = c(letters, paste0(letters, 1), paste0(letters, 2)))
#' }
ham <- function(source, key, ...) {
  app <- list(
    ui = pageWithSidebar(

      headerPanel("Human Augmented Matching"),

      sidebarPanel(
        actionButton("Next", "Next")
      ),

      mainPanel(
        verbatimTextOutput("source"),
        uiOutput("options"),
        tableOutput("table")
        # verbatimTextOutput("key")
      )
    ),
    server = function(input, output) {
      row <- reactive(input$Next + 1)
      source_text <- reactive(source[input$Next + 1])
      output$source <- renderText(source_text())
      key_text <- reactive(grep(source_text(), key, value = TRUE))

      output$options <- renderUI({
        selectInput("choice", "Match", choices = key_text(),
                     selected = NULL)
      })

      values <- reactiveValues()

      values$DT <- data.frame(source = NA,
                              match = NA,
                              stringsAsFactors = FALSE)
      lag_text <- reactive(source[input$Next - 1])

      newEntry <- observeEvent(input$choice, {
        newLine <- data.frame(source = source_text(),
                              match = isolate(input$choice),
                              stringsAsFactors = FALSE)
        values$DT <- rbind(values$DT, newLine)
        na.omit(values$DT)
      })

      output$table <- renderTable({
        values$DT <- values$DT %>% group_by(source) %>%
          summarise(match = last(match))
        values$DT
      })


    }
  )
  runApp(app, ...)
}
