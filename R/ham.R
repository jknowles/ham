#' Human Augmented Matching
#'
#' @param source a vector of items you want to find a match for
#' @param options  a vector of possible matches
#' @param key optional ID vector to append as well to the match table
#' @param n maximum number of choices to present user, default is 7
#' @param context a data.frame with additional elements to use to decide on a
#' choice from among the options
#' @param ... additional arguments to runApp
#'
#' @description The app presents you with all possible values in \code{key} that
#' match a simple grep for each value of \code{source}. If \code{context} is
#' provided, then the first column needs to be identical to options.
#'
#' @return An object \code{match_table} to the global environment containing your
#' matches
#' @export
#' @import shiny
#' @import dplyr
#' @importFrom stats na.omit
#' @importFrom stringdist stringsim
#' @importFrom shinythemes shinytheme
#' @examples
#' \dontrun{
#' source_keys <- letters
#' choice_keys <- c(letters, paste0(letters, 1), paste0(letters, 2))
#' ham(source = source_keys, choices = choice_keys)
#' ham(source = source_keys, choices = choice_keys, key = 101:178)
#' context_table <- data.frame(match = choice_keys, key = 101:178, data = runif(26*3))
#' ham(source = source_keys, choices = choice_keys,
#' key = 101:178, context = context_table)
#' # Test duplicates
#' source_keys <- letters
#' choice_keys <- c(letters, letters, LETTERS)
#' context_table <- data.frame(match = choice_keys, key = 101:178,
#' data = runif(26*3), stringsAsFactors=FALSE)
#' ham(source = source_keys, choices = context_table$match, key = 101:178,
#' context = context_table)
#' }
ham <- function(source, choices, key = NULL, n = NULL, context = NULL, ...) {
  app <- list(
    ui = fluidPage(theme = shinythemes::shinytheme("darkly"),

      titlePanel("Human Augmented Matching"),
      sidebarLayout(

        sidebarPanel(
          h3("Your controls"),
          actionButton("Stop", "Submit", icon = icon("check-circle")),
          h3("Your progress"),
          h4(strong(textOutput("counter"))),
          h3("Debug:"),
          h4(textOutput("debug"))
        ),

        mainPanel(
          h2("Source element to match"),
          verbatimTextOutput("source"),
          h2("Match options:"),
          uiOutput("options"),
          actionButton("Next", "Next"),
          h2("Context Table:"),
          tableOutput("context"),
          h2("Match tables so far:"),
          tableOutput("table")
          # verbatimTextOutput("key")
        )

      )
    ),
    server = function(input, output) {
      if(is.null(n)){
        n <- 7
      }
      row <- reactive(input$Next + 1)
      source_text <- reactive(source[input$Next + 1])
      output$source <- renderText(source_text())

      trunc_match <- function(x, y, n){
        out <- y[order(stringdist::stringsim(x, y, method = "lv"),
                decreasing = TRUE)]
        if(length(out) < n){
          n <- length(out)
        }
        out <- out[1:n]
      }

      # get all labels
      get_label <- function(input, choice_set){
        label <- names(choice_set)[input == choice_set]
        # We don't need NA here
        label <- na.omit(label)
        all_labels <- names(choice_set)[which(names(choice_set) == label)]
        all_labels
      }

      choice_text <- reactive({
        out <- c(trunc_match(source_text(), choices,
                             n = n), NA)
        if(any(duplicated(out))){
          out <- setNames(make.unique(out), out)
          # Use this to be able to get a numeric index on the other side
          #out <- factor(out)
        }
        out
      })

      output$debug <- renderText({
        paste0(key_text(), "\n")
        paste0(get_label(input$choice, choice_text()), "\n")
        paste0(input$choice, "\n")
        paste0(names(choice_text()))
      })

      if(!is.null(key)){
      key_text <- reactive({
        # use the named label to select from key
        key[which(choices == get_label(input$choice, choice_text()))]
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
          keyVar <- ifelse(length(keyVar) > 1, paste0(keyVar, collapse = ","), keyVar)
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
        } else {
          values$DT <- values$DT %>% group_by(source) %>%
            summarise(match = last(match))
        }
        out <- isolate(values$DT)
        out <- out[!is.na(out$source),]
        out <- out[seq(dim(out)[1],1),]
        out
      })

      conTab <- reactive({
        out <- context[which(context[, 1] %in% choice_text()), ]
        out
      })

      output$context <- renderTable({
        if(!is.null(context)){
          conTab()
        }
      })

      observe({
        if(input$Stop > 0){
        out <- isolate(values$DT)
        out <- out[!is.na(out$source),]
        match_table <<- out
        stopApp(returnValue = "Your matches are saved in match_table")
      }
    })
    }
  )
  runApp(app, ... )
}
