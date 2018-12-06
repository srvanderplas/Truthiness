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
file.copy(from = file.path(here("Pilot_Study/Data"), 
                           "Driver={SQLite3};dbname=truthinessStudy.db"),
          to = file.path(here("./Pilot_Study")), overwrite = T)
con <- dbConnect(odbc::odbc(), dbname = "truthinessStudy.db", 
                 .connection_string = "Driver={SQLite3};", 
                 timeout = 10)
all_trials <- tbl(con, "trial") %>%
    left_join(tbl(con, "pictures")) %>%
    filter(startTime >= "2018-11-12 10:00:00") %>%
    collect()

on.exit(dbDisconnect(con))

transcript <- read.csv("Data/PilotStudyTranscript.csv", comment = "#", quote = '\"')
# ------------------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Truthiness Pilot Debugging"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 2,
            h1("Truthiness Pilot"),
            br(),
            selectInput("userid", label = "User ID", choices = unique(all_trials$userID)),
            selectInput("userfact", label = "Fact", choices = unique(all_trials$factCode)),
            
            textOutput(outputId = "answerval"),
            textOutput(outputId = "correctAnswerval"),
            textOutput(outputId = "trialLength")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            uiOutput(outputId = "fact")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
            filter(userID == input$userid) %>%
            left_join(tbl(conn, "pictures"), by = "picID") %>%
            filter(factCode == input$userfact) %>%
            collect() %>%
            left_join(transcript, by = c("userID", "trialNum"))
    })
    
    # print(isolate(rtrial()$path))
    output$answerval <- renderText({
        print(rtrial())
        if(nrow(rtrial()) > 0) sprintf("User's Answer: %s", c("False", "True")[rtrial()$answer + 1])
    })
    output$correctAnswerval <- renderText(if(nrow(rtrial()) > 0) sprintf("Correct Answer: %s", c("False", "True")[rtrial()$correctAnswer + 1]))
    output$trialLength <- renderText(if(nrow(rtrial()) > 0) sprintf("Trial Length: %d sec", difftime(rtrial()$submitTime, rtrial()$startTime, units = "secs")))
    output$fact <- renderUI(if(nrow(rtrial()) > 0) displayTrial(rtrial()))
    output$transcript <- renderText(rtrial()$text)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
