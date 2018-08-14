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
library(shinyBS)
library(here) # working directory simplicity
# ------------------------------------------------------------------------------

# ---- Initial Data ------------------------------------------------------------
demographic_options <- readRDS(here("Data/demographic-options.RDS"))
# ------------------------------------------------------------------------------


redirect_code <- "Shiny.addCustomMessageHandler('no_consent', function(message) {window.location = 'http://www.google.com';});"

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

consentModalContent <- bsModal(
   id = 'consentModal',
   title = 'Study Information', trigger = '',
   size = 'large', 
   p("This study is part of academic research about how people evaluate claims.",
     "If you choose to participate, you will decide whether 12 trivia questions ",
     "are true or false. These questions may be accompanied by a picture."),
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
   radioButtons(inputId = "user_consent", 
                label = "I am willing and able to participate in this study",
                choices = c("YES", "NO")
   ),
   actionButton(inputId = "consent_submit", label = "Submit")
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
   actionButton(inputId = "demo_submit", label = "Submit")
)


ui <- fluidPage(
   tags$head(tags$script(redirect_code)),
   mainPanel(
      width = 12,
      consentModalContent,
      demographicModalContent
   )
)

server <- function(input, output, session) {
   # Get prior user data
   new_userID <- reactive({
      setwd(here::here("Data"))
      con <- dbConnect(odbc::odbc(), dbname = "truthinessStudy.db", 
                       .connection_string = "Driver={SQLite3};", 
                       timeout = 10)
      setwd(here::here())
      users <- dbReadTable(con, "user")
      dbDisconnect(con)
      if (nrow(users) == 0) {
         return(1)
      } else {
         return(max(users$userID))
      }
   })
   
   uservalues <- reactiveValues(
      userID = NA,
      browserFP = "",
      age = "",
      education = "", 
      study = "",
      colorblind = "", 
      consent = F)
   
   toggleModal(session, "consentModal", toggle = "open")
   observeEvent(input$consent_submit, {
      if (input$user_consent == "NO") {
         session$sendCustomMessage("no_consent", "no_consent")
      } else {
         uservalues$consent <- TRUE
         toggleModal(session, "consentModal", toggle = "closed")
         toggleModal(session, "demographicModal", toggle = "open")
      }
   })
   
   observeEvent(input$demo_submit, {
      uservalues$userID <- new_userID()
      uservalues$age <- input$demo_age
      uservalues$education <- input$demo_education
      uservalues$study <- input$demo_study
      uservalues$colorblind <- input$demo_colorblind
      uservalues$browserFP <- 
      toggleModal(session, "demographicModal", toggle = "close")
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

