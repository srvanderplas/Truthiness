#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# ---- Packages ----------------------------------------------------------------
library(shiny)
library(shinyBS) # Modals
library(shinyjs) # click() function
library(tidyverse)
library(here) # working directory simplicity
library(DBI) # Databases
library(pool) # Shiny pooled connections
# ------------------------------------------------------------------------------

# ---- Database - use to pre-populate options ----------------------------------
if (!file.exists(here("./Pilot_Study/Driver={SQLite3};dbname=TruthinessPilot.db"))) {
    file.copy(from = file.path(here("Pilot_Study/Data"), 
                               "Driver={SQLite3};dbname=TruthinessPilot.db"),
              to = file.path(here("./Pilot_Study"), 
                             "Driver={SQLite3};dbname=TruthinessPilot.db"), 
              overwrite = T)
}

con <- dbConnect(odbc::odbc(), dbname = "TruthinessPilot.db", 
                 .connection_string = "Driver={SQLite3};", 
                 timeout = 10)
on.exit({
    dbDisconnect(con)
    file.copy(from = file.path(here("./Pilot_Study"), 
                               "Driver={SQLite3};dbname=TruthinessPilot.db"), 
              to  = file.path(here("Pilot_Study/Data"), 
                              "Driver={SQLite3};dbname=TruthinessPilot.db"),
              overwrite = T)
})

source("Fix_Up_Data.R")

tables <- db_list_tables(con)

all_trials <- tbl(con, "trial") %>%
    left_join(tbl(con, "pictures")) %>%
    filter(startTime >= "2018-11-12 10:00:00") %>%
    collect() %>%
    arrange(factID, trialTypeID)

tt <- unique(all_trials$trialType)
uf <- unique(all_trials$factCode)

transcript <- read.csv("Data/PilotStudyTranscript.csv", comment = "#", quote = '\"')

# Create overall conclusions table
if (!"pilotConclusions" %in% tables) {
    cols <- list("trialType" = "", "factCode" = "", 
                 "mentionImage" = 0L, 
                 "memoryKnowledge" = 0L,
                 "failureToUnderstand" = 0L, 
                 "dontKnowIsFalse" = 0L,
                 "plausibleIsTrue" = 0L,
                 "unrelatedIsFalse" = 0L,
                 "misledByImage" = 0L,
                 "imageReadWrong" = 0L,
                 "otherErrors" = "",
                 "issues" = "")
    empty_table <- as_tibble(cols)
    dbWriteTable(con, "pilotConclusions", empty_table[-1,], overwrite = T)
    numericvars <- names(empty_table[, 3:10])
    charvars <- names(empty_table[, 11:12])
}
# ------------------------------------------------------------------------------



ui <- fluidPage(

    # Application title
    titlePanel("Truthiness Pilot Debugging"),

    fluidRow(
        column(
            width = 3,
            wellPanel(
                selectInput("trialType", label = "Trial Type", choices = tt),
                selectInput("userfact", label = "Fact", choices = uf),
                textOutput(outputId = "correctAnswerval"),
                uiOutput(outputId = "picture")
            )
        ),

        column(
            width = 9,
            
            uiOutput(outputId = "statement"),
            br(),
            wellPanel(
                h4("Issues and Errors"),
                fluidRow(
                    column(width = 2, 
                           numericInput(inputId = "mentionImage", 
                                        label = "Mention Image", 
                                        value = 0, min = 0, step = 1)),
                    column(width = 2, 
                           numericInput(inputId = "memoryKnowledge", 
                                        label = "Memory/Knowledge", 
                                        value = 0, min = 0, step = 1)),
                    column(width = 2, 
                           numericInput(inputId = "failureToUnderstand", 
                                        label = "Fail to Understand", 
                                        value = 0, min = 0, step = 1)),
                    column(width = 2, 
                           numericInput(inputId = "dontKnowIsFalse", 
                                        label = "DK = F", 
                                        value = 0, min = 0, step = 1)),
                    column(width = 2, 
                           numericInput(inputId = "plausibleIsTrue", 
                                        label = "Plausible = T", 
                                        value = 0, min = 0, step = 1)),
                    column(width = 2, 
                           numericInput(inputId = "unrelatedIsFalse", 
                                        label = "Unrelated = F", 
                                        value = 0, min = 0, step = 1))
                ),
                fluidRow(
                    column(width = 2,
                           numericInput(inputId = "misledByImage", 
                                        label = "Misled by Img", 
                                        value = 0, min = 0, step = 1)),
                    column(width = 2,
                           numericInput(inputId = "imageReadWrong", 
                                        label = "Img Read Wrong", 
                                        value = 0, min = 0, step = 1)),
                    column(width = 2, 
                           textInput(inputId = "otherErrors", 
                                     label = "Other Errors", 
                                     value = "", 
                                     placeholder = "comma separated list of other errors")),
                    column(width = 4, 
                           textInput(inputId = "questionIssues", 
                                     label = "Question/Graph Issues", 
                                     value = "", 
                                     placeholder = "comma separated list of issues")),
                    column(width = 2,
                           div(h4("")),
                           actionButton(inputId = "Submit", label = "Submit", class =  "btn btn-primary btn-sm"))
                )
            ),
            br(),
            dataTableOutput(outputId = "transcript")
        )
    )
)

server <- function(input, output, session) {
    
    # ---- Database Setup ----------------------------------------------------------
    pool <- dbPool(
        drv = odbc::odbc(), 
        dbname = "truthinessStudy.db",
        .connection_string = "Driver={SQLite3};", 
        timeout = 10
    )
    # on.exit(poolClose(pool))
    # ------------------------------------------------------------------------------
    
    # ---- SQLITE ------------------------------------------------------------------
    conn <- poolCheckout(pool)
    onStop(function() {
        poolReturn(conn)
        poolClose(pool)
    })
    # ------------------------------------------------------------------------------
    displayTrial <- function(df) {
        tagList(
            wellPanel(
                h3(df$fact, style = "display:block;margin:auto;"),
                br(),
                if (!is.na(df$path)) {
                    img(src = file.path("pics", df$path), 
                        width = "70%", style = "display:block;margin:auto;")
                } else { 
                    NULL 
                }
            )
        )
    }
    
    rtrial <- reactive({
        tbl(conn, "trial") %>%
            # filter(userID == input$userid) %>%
            left_join(tbl(conn, "pictures"), by = "picID") %>%
            filter(trialType == input$trialType) %>%
            filter(factCode == input$userfact) %>%
            collect() %>%
            left_join(transcript, by = c("userID", "trialNum"))
    })

    observe({
        if (input$Submit > 0) {
            
            data_frame(
                "trialType" = isolate(input$trialType), 
                "factCode" = isolate(input$userfact), 
                "mentionImage" = isolate(input$mentionImage), 
                "memoryKnowledge" = isolate(input$memoryKnowledge),
                "failureToUnderstand" = isolate(input$failureToUnderstand), 
                "dontKnowIsFalse" = isolate(input$dontKnowIsFalse),
                "plausibleIsTrue" = isolate(input$plausibleIsTrue),
                "unrelatedIsFalse" = isolate(input$unrelatedIsFalse),
                "misledByImage" = isolate(input$misledByImage),
                "imageReadWrong" = isolate(input$imageReadWrong),
                "otherErrors" = isolate(input$otherErrors),
                "issues" = isolate(input$questionIssues)
            ) %>%
                dbWriteTable(conn, "pilotConclusions", ., overwrite = F, append = T)
            
            if (isolate(input$trialType) != rev(tt)[1]) {
                cur_tt <- which(tt %in% isolate(input$trialType))
                updateSelectInput(session, inputId = "trialType", selected = tt[cur_tt + 1])
            } else {
                cur_uf <- which(uf %in% isolate(input$trialType))
                new_uf <- (cur_uf + 1) %% length(uf) + 1
                cat(new_uf)
                updateSelectInput(session, inputId = "trialType", selected = tt[1])
                updateSelectInput(session, inputId = "userfact", selected = uf[new_uf])
            }
            
            sapply(numericvars, function(x) updateNumericInput(session, inputId = x, value = 0))
            sapply(charvars, function(x) updateTextInput(session, inputId = x, value = ""))
        }
        
    })
    
    # print(isolate(rtrial()$path))
    output$picture <- renderUI({
        if (nrow(rtrial()) > 0) {
            if (sum(!is.na(rtrial()$path)) > 0) {
                img(src = file.path("pics", unique(rtrial()$path)), 
                    width = "100%", style = "display:block;margin:auto;")
            }
        }
    })
    output$statement <- renderUI({
        if (nrow(rtrial()) > 0) {
            h3(rtrial()$fact[1], style = "display:block;margin:auto;")
        }
    })
    # output$answerval <- renderText({
    #     print(rtrial())
    #     if (nrow(rtrial()) > 0) sprintf("User's Answer: %s", c("False", "True")[rtrial()$answer + 1])
    # })
    output$correctAnswerval <- renderText({
        if ( nrow(rtrial()) > 0) {
            sprintf("Correct Answer: %s", c("False", "True")[unique(rtrial()$correctAnswer) + 1])
        }
    })
    output$trialLength <- renderText({
        if (nrow(rtrial()) > 0) {
            sprintf("Trial Length: %d sec", difftime(rtrial()$submitTime, rtrial()$startTime, units = "secs"))
        }
    })
    output$fact <- renderUI({
        if (nrow(rtrial()) > 0) {
            displayTrial(rtrial())
        }
    })
    output$transcript <- renderDataTable(
        rtrial() %>% select(answer, text),
        options = list(paging = F, searching = F, lengthChange = F)
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
