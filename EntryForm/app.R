# Store form imput, save to Google Sheets with this app

# Recreation of a form submission shiny app found here:
# https://deanattali.com/blog/shiny-persistent-data-storage/

#This works locally. I'm not sure if it will work on shinyapps.io or if I'll need to figure out authorization there.

library(tidyverse)
library(shiny)
library(DT)
library(digest)
library(googlesheets4)
library(shinyBS)
library(shinyWidgets)

ssid <- as_sheets_id("1YjOTei_N7RS05rxXrVB6iuUptjYTDQC4xTLeoR-8fi8") #ID of google sheet. This sheet must be created properly for the app to function.

# Function to save entries to SQLite
saveData <- function(data) {
    # Add the data as a new row
    sheet_append(ssid, data)
}

loadData <- function() {
    # Read the dataNa
    read_sheet(ssid)
}

# Define the fields we want to save from the form
fields <- c("author","pubYear","system","location","species","DOI","citation","lat","long")

# Define UI
ui <- fluidPage(
    # Application title
    titlePanel("Management Strategy Evaluation review data entry form"),
    p(strong("Enter the data from your review in the forms below.")),
   
    # Data Entry Form
    tags$style(HTML("
      #first {
          border: 4px solid grey;
      }
      #second {
          border: 2px dashed blue;
      }
    ")),
    fluidRow(id="first",style = "background-color: #d0d6d1;",
        column(width=12,
             p(em("Summary Data")),
             hr(),),
        column(width=3,
               textInput("DOI","DOI","doi: 10.1016/j.tree.2011.05.003"),
               bsTooltip(id = "DOI", title = "Enter the DOI for the publicaion", trigger = "hover")),
        column(width=2,            
               textInput("author","Author","Bunnefeld et. al."),
               bsTooltip(id = "author", title = "Enter the MSE authors in citation form", trigger = "hover")),
        column(width=2,
               numericInput(inputId = "pubYear", label = "Publication year",value = 2011),
               bsTooltip(id = "pubYear", title = "Enter the year the MSE was published", trigger = "hover")),
        column(width=5,
               textInput("system","System","Natural resource management"),
               bsTooltip(id = "system", title = "Enter the socioecological system the MSE addressed", trigger = "hover")),
        column(width=3,
               textInput("location","Location","Global ecosystems"),
               bsTooltip(id = "location", title = "Enter the name of the geological area of the system", trigger = "hover")),
        column(width=2,
               numericInput(inputId = "lat", label = "Latitude",value = 51.41),
               bsTooltip(id = "lat", title = "Enter the latitude at the center point of the study area", trigger = "hover")),
        column(width=2,
               numericInput(inputId = "long", label = "Longitude",value = -0.64),
               bsTooltip(id = "long", title = "Enter the longitude at the center point of the study area", trigger = "hover")),
        column(width=5,
               textInput("species","Species","Fish and wildlife (latin name)"),
               bsTooltip(id = "species", title = "Enter the common name and (latin name) of the species addressed", trigger = "hover")),
        column(width=12,
               textAreaInput("citation","Citation","Bunnefeld N, Hoshino E, Milner-Gulland EJ. Management strategy evaluation: a powerful tool for conservation? Trends Ecol Evol. 2011 Sep;26(9):441-7.", width="1000px"),
               bsTooltip(id = "species", title = "Enter the full citation", trigger = "hover"),
        p(em("Process Documentation")),
        hr(),),
        column(width=2,
              checkboxInput("processDoc", label = "Process Documented?"),
              bsTooltip(id = "processDoc", title = "Was the process used to conduct the MSE documented?", trigger = "hover")),
        column(width=2,
               checkboxInput("rolessDoc", label = "Roles specified?"),
               bsTooltip(id = "rolesDoc", title = "Were participants roles clearly defined and documented?", trigger = "hover")),
        column(width=2,
               checkboxInput("openMeetings", label = "Open meetings?"),
               bsTooltip(id = "openMeetings", title = "Were meetings open to the public??", trigger = "hover")),
        column(width=2,            
              checkboxInput("optimAlt","Best strategy documented?"),
              bsTooltip(id = "optimAlt", title = "Was the optimal management strategy reported in the documentation?", trigger = "hover")),
        column(width=2,
              checkboxInput("decisionDoc","Decision documented?"),
              bsTooltip(id = "decisionDoc", title = "Was the management decision resulting from the process documented?", trigger = "hover")),
        column(width=2,
              checkboxInput("implemented","Results adopted?"),
              bsTooltip(id = "implemented", title = "Was the decision resulting from the process implemented?", trigger = "hover")),
        column(width=6,
              selectInput("leader","Process Lead(s)?",choices = c("Government","Management","Fishery","Independent","Scientists","Public","Unknown"),
                          selected = "Scientists",multiple = T,width="450px"),
              bsTooltip(id = "leader", title = "Who lead this MSE process? Select one or more options", trigger = "hover"),),
        column(width=6,
               selectInput("participants","Participants?",choices = c("Government","Management","Fishery","Independent","Scientists","Public","Facilitators","Decision Makers","Experts","Decision Analysts"),
                           selected = "Scientists",multiple = T,width="450px"),
               bsTooltip(id = "participants", title = "Who participated in this MSE process? Select one or more options", trigger = "hover"),),
        column(width=12,
               p(em("Problem Definition")),
               hr(),
               checkboxInput("problemDoc","Problem documented?"),
               bsTooltip(id = "problemDoc", title = "Was the problem defined and documented?", trigger = "hover"),
               textAreaInput("problemDef","How was the problem defined?","What approach to decision making will be support natural resource management decisions?", width="1000px"),
               bsTooltip(id = "problemDef", title = "Enter the problem definition for this MSEn", trigger = "hover"),),
        column(width=12,
               p(em("Objectives")),
               hr(),
               ),
        column(width=3,
               checkboxInput("objDoc","Objective elicitation documented?"),
               bsTooltip(id = "objDoc", title = "Was the objective elicitation process documented?", trigger = "hover"),
               ),
        column(width=4,
               selectInput("objSource","Objectives source",choices = c("Government","Management","Fishery","Independent","Scientists","Public","Decision Makers","Experts","Decision Analysts","Unknown"),
                           selected = "Unknown",multiple = T,width="450px"),
               bsTooltip(id = "objSource", title = "Who provided the objectives?", trigger = "hover"),),
        column(width=4,
               selectInput("subObjSource","Subjective objectives source",choices = c("Government","Management","Fishery","Independent","Scientists","Public","Decision Makers","Experts","Decision Analysts","Unknown"),
                           selected = "Unknown",multiple = T,width="450px"),
               bsTooltip(id = "subObjSource", title = "If it was not explicitly documented, based on your subjective interpretation of the process Who provided the objectives?", trigger = "hover"),),
        column(width=12,
               textAreaInput("elicitationObj","Objective Elicitation Process","Objectives were elicited by a facilitator", width="1000px"),
               bsTooltip(id = "elicitationObj", title = "What process was used to elicit objectives?", trigger = "hover"),
               p("linked objectives table"),
        ),
        column(width=12,
               p(em("Alternatives")),
               hr(),),
        column(width=3,
              checkboxInput("altDoc","Alternative production documented?"),
              bsTooltip(id = "altDoc", title = "Was an alternative production process comppleted and documented?", trigger = "hover"),
               ),
        column(width=4,
               selectInput("altSource","Alternatives source",choices = c("Government","Management","Fishery","Independent","Scientists","Public","Decision Makers","Experts","Decision Analysts","Unknown"),
                           selected = "Unknown",multiple = T,width="450px"),
               bsTooltip(id = "altSource", title = "Who provided the alternatives?", trigger = "hover"),),
        column(width=4,
               selectInput("subAltSource","Subjective alternattives source",choices = c("Government","Management","Fishery","Independent","Scientists","Public","Decision Makers","Experts","Decision Analysts","Unknown"),
                                  selected = "Unknown",multiple = T,width="450px"),
               bsTooltip(id = "subAltSource", title = "If it was not explicitly documented, based on your subjective interpretation of the process Who provided the alternatives?", trigger = "hover"),),
        column(width=12,
               p("linked alternatives table"),
               ),
        column(width=12,
               p(em("Consequences and Tradeoffs")),
               hr(),),
        column(width=6,
               selectInput("predMethod","Prediction Method",choices = c("Dynamic programmings","Expert elicitation","Mental models","Simulation modeling","Unknown"),
                           selected = "Unknown",multiple = T,width="450px"),
               bsTooltip(id = "predMethod", title = "What method was used to predict the consequences of proposed management strategies?", trigger = "hover"),),
        column(width=6,
               selectInput("drivers","Consequence Drivers",choices = c("Allowable catch adjustement","Climate change","Ecosystem based management","Environmental conditions",
                                                                       "Fishing behavior","Habiat change","Implementation Uncertainty","Landing regulations","Management timeline",
                                                                       "Migration","Monitoring methodology","Multiple sectors","Predation","Spatial structure","Species interactions",
                                                                       "Stock Status","Uncertainty"),
                           multiple = T,width="450px"),
               bsTooltip(id = "drivers", title = "What drivers (aka, predictors, factors, model components) were included in the model used to predict the consequences?", trigger = "hover"),),
        column(width=3,
               checkboxInput("tradeoffsDoc","Tradeoffs explicitly evaluated?"),
               bsTooltip(id = "tradeoffsDoc", title = "Were trade-offs between the alternative management strategies evaluated and documented?", trigger = "hover"),
        ),
        column(width=4,
               selectInput("tradeMethod","Trade-off evaluaiton method",choices = c("Mental analysis","MCDA","Negotiation","Visualization","Unknown"),
                           selected = "Unknown",multiple = T,width="450px"),
               bsTooltip(id = "tradeMethod", title = "How were trade-offs evaluated?", trigger = "hover"),),
        column(width=4,
               selectInput("subTradeMethod","Subjective trade-off evaluaiton method",choices = c("Mental analysis","MCDA","Negotiation","Visualization","Unknown"),
                           selected = "Unknown",multiple = T,width="450px"),
               bsTooltip(id = "subTradeMethod", title = "If it was not explicitly documented, based on your subjective interpretation of the process how were trade-offs evaluated?", trigger = "hover"),),
        column(width=12,
               p(em("Notes and Comments")),
               hr(),
               textAreaInput("notes","notes","This is an example text for the note field where you can provide any notes you think would be helpful or of interest to others regarding
               this MSE process and its documentation", width="1000px"),
               bsTooltip(id = "notes", title = "Add any notes you think would be helpful here.", trigger = "hover")),
        column(width=12,
               p(em("Other things to include are: (a) who submitted this review, (b) point of contact's contact info for the MSE"))),
        column(width=12,
               p(em("Submit Review")),
               hr(),
               actionButton("submit", "Submit"))
        ), #end fluidRow
    # Data table
    mainPanel(
             # Show a table of responses
             DT::dataTableOutput("responses", width = 300), 
             tags$hr()
    ), # end mainPanel
) # end fluidpage

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
        data <- as.data.frame(t(sapply(fields, function(x) input[[x]])))
        data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
        saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
        input$submit
        loadData()
    })
} # send server

# Run the application 
shinyApp(ui = ui, server = server)