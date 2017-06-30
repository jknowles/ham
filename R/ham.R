#' Human Augmented Matching
#'
#' @param source a vector of items you want to find a match for
#' @param options  a vector of possible matches
#' @param key optional ID vector to append as well to the match table
#' @param ... additional arguments
#'
#' @description The app presents you with all possible values in \code{key} that
#' match a simple grep for each value of \code{source}
#'
#' @return An object \code{match_table} to the global environment containing your
#' matches
#' @export
#' @import shiny
#' @import dplyr
#' @importFrom stats na.omit
#' @importFrom shinythemes shinytheme
#' @examples
#' \dontrun{
#' ham(source = letters, choices = c(letters, paste0(letters, 1), paste0(letters, 2)))
#' ham(source = letters, choices = c(letters, paste0(letters, 1), paste0(letters, 2)),
#' key = 101:126)
#' }
ham <- function(source, choices, key = NULL, ...) {
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
      choice_text <- reactive(c(grep(source_text(), choices, value = TRUE),
                                NA))

      if(!is.null(key)){
      key_text <- reactive({
        key[which(choices == input$choice)]
      })
      }
      output$counter <- renderText({
        paste0(input$Next +1, "/", length(source))
      })

      output$options <- renderUI({
        selectInput("choice", "Match", choices = choice_text(),
                     selected = NULL)
      })

      values <- reactiveValues()

      if(!is.null(key)){
        values$DT <- data.frame(source = NA,
                                match = NA,
                                key = NA,
                                stringsAsFactors = FALSE)
        # values$tmp <- key_text()
        # values$key <- ifelse(length(isolate(values$tmp)) == 0, NA,
        #                      isolate(values$tmp))
      } else{
        values$DT <- data.frame(source = NA,
                                match = NA,
                                stringsAsFactors = FALSE)
      }


      newEntry <- observeEvent(input$choice, {
        if(!is.null(key)){
          keyVar <- key_text()
          keyVar <- ifelse(length(keyVar) > 1, NA, keyVar)
          newLine <- data.frame(source = source_text(),
                                match = isolate(input$choice),
                                key = keyVar,
                                stringsAsFactors = FALSE)

        } else{
          newLine <- data.frame(source = source_text(),
                                match = isolate(input$choice),
                                stringsAsFactors = FALSE)
        }
        values$DT <- rbind(values$DT, newLine)
        # na.omit(values$DT)
      })

      output$table <- renderTable({
        if(!is.null(key)){
          values$DT <- values$DT %>% group_by(source) %>%
            summarise(match = last(match),
                      key = last(key))
          values$DT <- values$DT[!is.na(values$DT$source),]
        } else {
          values$DT <- values$DT %>% group_by(source) %>%
            summarise(match = last(match))
          values$DT <- values$DT[!is.na(values$DT$source),]
          # na.omit(values$DT)
        }

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
