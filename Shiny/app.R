#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# ---- Experimental Parameters -------------------------------------------------
experiment_name <- "pilot"
trial_time <- 26
counter_interval <- 1000
sample_trial_types <- function() {
   c(c(0, 0, 1, 2, 3, 4, 5, 8, 9, 11), 
     sample(c(3, 4, 5, 8, 9, 11), size = 2, replace = F)) %>%
      sample(size = 12, replace = FALSE)
}
# ------------------------------------------------------------------------------

# ---- Packages ----------------------------------------------------------------
library(shiny)
library(shinyBS) # Modals
library(shinyjs) # click() function
library(tidyverse)
library(here) # working directory simplicity
library(DBI) # Databases
library(pool) # Shiny pooled connections
# ------------------------------------------------------------------------------

# ---- Initial Data ------------------------------------------------------------
demographic_options <- readRDS(here("Data/demographic-options.RDS"))
# ------------------------------------------------------------------------------

# ---- Fingerprint Bits --------------------------------------------------------
redircode <- "Shiny.addCustomMessageHandler('noconsent', function(message) {
                      window.location = 'http://www.google.com';});"
redircode2 <- "Shiny.addCustomMessageHandler('colorblind', function(message) {
                      window.location = 'http://www.google.com';});"
redirectSetup <- tagList(
   tags$head(tags$script(redircode)),   
   tags$head(tags$script(redircode2))
)

inputUserid <- function(inputId, value='') {
   #   print(paste(inputId, "=", value))
   tagList(
      singleton(tags$head(tags$script(src = "js/md5.js", 
                                      type = 'text/javascript'))),
      singleton(tags$head(tags$script(src = "js/shinyBindings.js", 
                                      type = 'text/javascript'))),
      tags$body(onload = "setvalues()"),
      tags$input(id = inputId, class = "userid", value = as.character(value), 
                 type = "text", style = "display:none;")
   )
}

inputIp <- function(inputId, value=''){
   tagList(
      singleton(tags$head(tags$script(src = "js/md5.js", 
                                      type = 'text/javascript'))),
      singleton(tags$head(tags$script(src = "js/shinyBindings.js", 
                                      type = 'text/javascript'))),
      tags$body(onload = "setvalues()"),
      tags$input(id = inputId, class = "ipaddr", value = as.character(value), 
                 type = "text", style = "display:none;")
   )
}

# ------------------------------------------------------------------------------

# ---- Modals ------------------------------------------------------------------
consentModalContent <- bsModal(
   id = 'consentModal',
   title = 'Study Information', trigger = '',
   size = 'large', 
   p("This study is part of academic research about how people evaluate claims.",
     "If you choose to participate, you will decide whether 12 trivia statements ",
     "are true or false. These statements may be accompanied by a picture."),
   p("We will collect your responses, response time, and some individualizing ",
     "information about you (anonymized IP address and browser characteristics).",
     "This information cannot be linked back to you, but is used to ensure that ",
     "we collect data from unique individuals."),
   br(),
   p("If at any point during the study you decide that you no longer wish to ",
     "participate, you may decline to answer the question or close the browser."),
   br(),
   p("We ask that you only participate in this study if you are at least 18 and ",
     "are not colorblind or color-vision impaired."),
   br(),
   selectInput(
      inputId = "user_consent", 
      label = "I am willing and able to participate in this study",
      choices = c("-----", "YES", "NO"),
      selected = "-----",
      width = "100%"
   ),
   actionButton(inputId = "consent_submit", label = "Submit"),
   tags$head(tags$style("#consentModal .modal-footer{ display:none}"))
)
demographicModalContent <- bsModal(
   id = 'demographicModal', 
   title = "Demographic Information", trigger = '',
   size = 'large',
   p("Before we begin, we would like to get some demographic information from ",
     "you. This information will be used to describe our participant pool when ",
     "this study is published, but will not be used to identify you individually",
     "in any way."),
   selectInput(inputId = "demo_age", label = "Age", 
               choices = demographic_options$age),
   selectInput(inputId = "demo_education", 
               label = "Highest Level of Education Completed", 
               choices = demographic_options$education, 
               selected = "Some College"),
   selectizeInput(inputId = "demo_study",
                  label = "What field is your degree in?",
                  choices = demographic_options$study,
                  selected = NULL, multiple = T),
   selectInput(inputId = "demo_colorblind",
               label = "Are you color-vision impaired (colorblind)?",
               choices = demographic_options$colorblind,
               selected = NULL, multiple = F, selectize = F),
   actionButton(inputId = "demo_submit", label = "Submit"),
   tags$head(tags$style("#demographicModal .modal-footer{ display:none}"))
)
testModalContent <- bsModal(
   id = 'testModal',
   title = "Sample Questions", trigger = '', 
   size = "large",
   p("In this study, you will be shown a series of statements, some of which ",
     "may be accompanied by an image. Indicate whether you believe the ",
     "statement to be True or False using the input below.",
     "During the real study questions, you will have a short amount of time to ",
     "answer."),
   br(),
   selectInput("test_mnuts", label = "This statement is: ", 
               choices = c("TRUE", "FALSE"), selected = NULL, selectize = T),
   h3("Macadamia nuts are in the same family as peaches."),
   img(src = "https://c1.staticflickr.com/6/5300/5581363966_01d082dab9_b.jpg", 
       width = "80%", style = "margin:auto;")
)
# ------------------------------------------------------------------------------


# ---- UI ----------------------------------------------------------------------
ui <- fluidPage(
   useShinyjs(),  # Set up shinyjs
   redirectSetup,
   mainPanel(
      width = 12,
      inputIp("ipid"),
      inputUserid("fingerprint"),
      consentModalContent,
      demographicModalContent,
      testModalContent,
      br(),
      uiOutput("mainPage"),
      NULL
   )
)

# ------------------------------------------------------------------------------


# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {
   
   # ---- Database Setup ----------------------------------------------------------
   setwd(here::here("Data"))
   pool <- dbPool(
      drv = odbc::odbc(),
      dbname = "truthinessStudy.db",
      .connection_string = "Driver={SQLite3};", 
      timeout = 10
   )
   # on.exit(poolClose(pool))
   setwd(here::here())
   # ------------------------------------------------------------------------------
   
   # ---- SQLITE ------------------------------------------------------------------
   conn <- poolCheckout(pool)
   onStop(function() {
      poolReturn(conn)
      poolClose(pool)
   })
   # ------------------------------------------------------------------------------
   
   # ---- Modal Mechanics ---------------------------------------------------------
   getUserID <- reactive({
      z <- dbReadTable(conn, "user")$userID
      if (length(z) == 0) {
         return(1)
      } else {
         return(max(z) + 1)
      }
   })
   
   
   toggleModal(session, "consentModal", toggle = "open")
   
   observeEvent(input$consent_submit, {
      if (input$user_consent != "YES") {
         session$sendCustomMessage("noconsent", "noconsent")
      } else {
         toggleModal(session, "consentModal", toggle = "close")
         toggleModal(session, "demographicModal", toggle = "open")
      }
   })
   
   observeEvent(input$demo_submit, {
      dbWithTransaction(conn, {
         tmp <- data_frame(
            userID = getUserID(),
            browserFP = input$fingerprint,
            userIP = input$ipid,
            age = input$demo_age,
            education = input$demo_education,
            study = input$demo_study,
            colorblind = input$demo_colorblind,
            consent = (input$user_consent == "YES")
         )
         if (input$demo_colorblind == "I have impaired color vision") {
            session$sendCustomMessage("colorblind", "colorblind")
         } else {
            message("Writing user data to table")
            dbWriteTable(conn, "user", value = tmp, append = T)
         }
      })
      
      toggleModal(session, "demographicModal", toggle = "close")
      toggleModal(session, "testModal", toggle = "open")
   })
   
   observeEvent(input$show_consent, {
      toggleModal(session, "consentModal", toggle = "toggle")
   })
   observeEvent(input$show_demo, {
      toggleModal(session, "demographicModal", toggle = "toggle")
   })
   # ------------------------------------------------------------------------------
   
   # ---- Sampling Scheme ------------------------------------------------------
   picdb <- dbReadTable(conn, "pictures")
   pictureOrder <- sample(unique(picdb$factCode), size = 12, replace = F)
   trialTypes <- sample_trial_types()
   userTrials <- data_frame(trialNum = 1:12,
                            factCode = pictureOrder,
                            trialTypeID = trialTypes) %>%
      left_join(picdb)
   # ---------------------------------------------------------------------------
   
   # ---- Trial Mechanics --------------------------------------------------------
   trial <- reactiveValues(num = 0, startTime  = NA, endTime = NA, remTime = 25)
   
   displayTrial <- function(df) {
      trial$startTime <- lubridate::now()
      tagList(
         br(),
         fluidRow(
            column(
               width = 3, offset = 1, 
               selectInput("questionAnswer", label = "This statement is: ", 
                           choices = c(" ", "TRUE", "FALSE"), selected = " ", 
                           selectize = T)
            ),
            column(
               width = 2, 
               tags$label(),
               actionButton("questionSubmit", label = "Submit", 
                            class = "btn-primary")
            ),
            column(
               width = 4,
               h2(textOutput("currentTime"), style = "margin-left:auto;display:block;")
            )
         ),
         fluidRow(
            column(
               width = 10, offset = 1,
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
         )
         
      )
   }
   
   getTrialID <- reactive({
      trial$num 
      
      z <- dbReadTable(conn, "trial")$trialID
      if (length(z) == 0) {
         return(1)
      } else {
         return(max(z) + 1)
      }
   })
   
   observeEvent(input$start_study, {
      trial$num <- 1
   })
   
   observeEvent(input$questionSubmit, {
      trial$endTime <- lubridate::now()
      dbWithTransaction(conn, {
         dbWriteTable(conn, name = "trial", 
                      data_frame(
                         trialID = getTrialID(),
                         picID = userTrials$picID[trial$num],
                         userID = getUserID(),
                         trialNum = trial$num,
                         answer = ifelse(input$questionAnswer == " ", NA, 
                                         as.logical(input$questionAnswer)),
                         startTime = as.character(trial$startTime),
                         submitTime = as.character(trial$endTime),
                         extdata = "",
                      ), append = T)
      })
      trial$endTime <- NA
      trial$startTime <- NA
      trial$num <- trial$num + 1
      trial$remTime <- 25
   })
   # ------------------------------------------------------------------------------

   output$currentTime <- renderText({
      message(sprintf("TrialTime: %d", isolate(trial$remTime)))
      if (isolate(trial$remTime) > 0) {
         isolate(trial$remTime <- trial$remTime - 1)
         invalidateLater(1000, session)
      } else {
         click("questionSubmit")
      }
      sprintf("%d seconds remaining", isolate(trial$remTime))
   })
   
   output$mainPage <- renderUI({
      # Before consent and demographic information:
      if (input$consent_submit < 1 | input$demo_submit < 1) {
         column(
            width = 10, offset = 1,
            br(),
            tagList(
               if (input$user_consent != "YES") {
                  actionButton("show_consent", "Show Informed Consent Window")
               } else {NULL},
               actionButton("show_demo", "Show Demographics Window")
            )
         )
      } else if (trial$num == 0) {
         column(
            width = 10, offset = 1,
            br(),
            actionButton("start_study", "Start the Study", class = "btn-primary btn-large",
                         style = "display:block;margin:auto;margin-top:50px;")
         )
      } else if (trial$num < 13) {
         message(trial$num)
         displayTrial(userTrials[trial$num,])
      } else {
         column(
            width = 10, offset = 1,
            wellPanel(
               h4("Thank you for completing this experiment. Your completion code ",
                  "is: "),
               p(input$fingerprint)
            )
         )
      }
   })
}

# ------------------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

