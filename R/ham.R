#' Human Augmented Matching
#'
#' @param source a vector of items you want to find a match for
#' @param key  a vector of possible matches
#' @param ... additional arguments
#'
#' @description The app presents you with all possible values in \code{key} that
#' match a simple grep for each value of \code{source}
#'
#' @return An object called match_table to the global environment containing your
#' matches
#' @export
#' @import shiny
#' @import dplyr
#' @importFrom stats na.omit
#' @importFrom shinythemes shinytheme
#' @examples
#' \dontrun{
#' ham(source = letters, key = c(letters, paste0(letters, 1), paste0(letters, 2)))
#' }
ham <- function(source, key, ...) {
  app <- list(
    ui = fluidPage(theme = shinythemes::shinytheme("darkly"),

      titlePanel("Human Augmented Matching"),
      sidebarLayout(

        sidebarPanel(
          h3("Your controls"),
          actionButton("Stop", "Submit", icon = icon("check-circle")),
          h3("Your progress"),
          h4(strong(textOutput("counter")))
        ),

        mainPanel(
          h2("Source element to match"),
          verbatimTextOutput("source"),
          h2("Match options:"),
          uiOutput("options"),
          actionButton("Next", "Next"),
          h2("Match tables so far:"),
          tableOutput("table")
          # verbatimTextOutput("key")
        )

      )
    ),
    server = function(input, output) {
      row <- reactive(input$Next + 1)
      source_text <- reactive(source[input$Next + 1])
      output$source <- renderText(source_text())
      key_text <- reactive(grep(source_text(), key, value = TRUE))

      output$counter <- renderText({
        paste0(input$Next +1, "/", length(source))
      })

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

      observe({
        if(input$Stop > 0){
        match_table <<- na.omit(isolate(values$DT))
        stopApp(returnValue = "Your matches are saved in match_table")
      }
    })
    }
  )
  runApp(app, ...)
}
