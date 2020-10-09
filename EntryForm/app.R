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

objTable = data.frame(Category=character(), Objective=character(), Descriptions=character(),Direction=character(),Type=character(),
                      Scale=character(),Metric=character())
altTable = data.frame("Management Tools"=character(), Alternatives=character())

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
    fluidRow(style = "background-color: #d0d6d1;",
        column(width=12,
             p(em("Summary Data"))),
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
               bsTooltip(id = "citation", title = "Enter the full citation", trigger = "hover")),
        column(width=12,
               textInput("poc","Point of contact: contact info","e.g., jcummings@umassd.edu", width="400px"),
               bsTooltip(id = "poc", title = "Provide contact info a point of contact associated with this MSE", trigger = "hover"),
               hr(),
        p(em("Process Documentation"))),
        column(width=2,
               checkboxInput("processDoc", label = "Process Documented?"),
               bsTooltip(id = "processDoc", title = "Was the process used to conduct the MSE documented?", trigger = "hover")),
        column(width=2,
               checkboxInput("rolesDoc", label = "Roles specified?"),
               bsTooltip(id = "rolesDoc", title = "Were participant roles clearly defined and documented?", trigger = "hover")),
        column(width=2,
               checkboxInput("openMeetings", label = "Open meetings?"),
               bsTooltip(id = "openMeetings", title = "Were meetings open to the public?", trigger = "hover")),
        column(width=2,            
               checkboxInput("optimAlt","Best strategy documented?"),
               bsTooltip(id = "optimAlt", title = "Was the optimal management strategy reported in the documentation?", trigger = "hover")),
        column(width=2,
               checkboxInput("decisionDoc","Decision documented?"),
               bsTooltip(id = "decisionDoc", title = "Was a management decision documented?", trigger = "hover")),
        column(width=2,
               checkboxInput("implemented","Results adopted?"),
               bsTooltip(id = "implemented", title = "Was the decision implemented?", trigger = "hover")),
        column(width=6,
               selectInput("leader","Process Lead(s)?",choices = c("Select one or more. Click & Delete to deselect" = "","Government","Management","Fishery","Independent","Scientists","Public","Unknown"),
                           multiple = T,width="450px"),
               bsTooltip(id = "leader", title = "Who lead this MSE process?", trigger = "hover")),
        column(width=6,
               selectInput("participants","Participants?",choices = c("Select one or more. Click & Delete to deselect" = "","Government","Management","Fishery","Independent","Scientists","Public","Facilitators","Decision Makers","Experts","Decision Analysts"),multiple = T,width="450px"),
               bsTooltip(id = "participants", title = "Who participated in this MSE process?", trigger = "hover")),
        column(width=12,
               hr(),
               p(em("Problem Definition"))),
        column(width=2,
               checkboxInput("problemDoc","Problem documented?"),
               bsTooltip(id = "problemDoc", title = "Was the problem defined and documented?", trigger = "hover")),
        column(width=10,
               textAreaInput("problemDef","How was the problem defined?","What approach to decision making will be support natural resource management decisions?", width="900px"),
               bsTooltip(id = "problemDef", title = "Enter the problem definition for this MSE", trigger = "hover")),
        column(width=12,
               hr(),
               p(em("Objective Process"))),
        column(width=3,
               checkboxInput("objDoc","Objective elicitation documented?"),
               bsTooltip(id = "objDoc", title = "Was the objective elicitation process documented?", trigger = "hover"),
               ),
        column(width=4,
               selectInput("objSource","Objectives source",choices = c("Select one or more. Click & Delete to deselect" = "","Government","Management","Fishery","Independent","Scientists","Public","Decision Makers","Experts","Decision Analysts","Unknown"),
                           multiple = T,width="450px"),
               bsTooltip(id = "objSource", title = "Who provided the objectives?", trigger = "hover"),),
        column(width=4,
               selectInput("subObjSource","Subjective objectives source",choices = c("Select one or more. Click & Delete to deselect" = "","Government","Management","Fishery","Independent","Scientists","Public","Decision Makers","Experts","Decision Analysts","Unknown"),
                           multiple = T,width="450px"),
               bsTooltip(id = "subObjSource", title = "If it was not explicitly documented, based on your subjective interpretation of the process who provided the objectives?", trigger = "hover"),),
        column(width=12,
               textAreaInput("elicitationObj","Objective Elicitation Process","Objectives were elicited by a facilitator.", width="1000px"),
               bsTooltip(id = "elicitationObj", title = "What process was used to elicit objectives?", trigger = "hover"),),
    ), # end fluidRow
    fluidRow(style = "background-color: #9faba0;",
        column(width=12,
               p(em("Objectives: Individual Objective Entry"))),
        column(width=3,
               selectInput("objCategory","Category",choices = c("Conservation","Yield","Economic","Social","Utility"),multiple = F),
               bsTooltip(id = "objCategory", title = "What category of objective is it?", trigger = "hover")),
        column(width=3,
               textInput("objName","Objective","Stock status"),
               bsTooltip(id = "objName", title = "Objective name", trigger = "hover")),
        column(width=6,
               textAreaInput("objDescription","Description","Biomass as proportion of unfished biomass over last 10 years of the simulation",width="500px"),
               bsTooltip(id = "objDescription", title = "Provide a description of the objective.", trigger = "hover")),
        column(width=3,
               selectInput("objDirection","Direction",choices = c("Maximize","Minimize","Target","Constraint","Unknown"),multiple = F),
               bsTooltip(id = "objDirection", title = "What is the desired direction or what type of condition is used for the objective?", trigger = "hover")),
        column(width=3,
               selectInput("objType","Type",choices = c("Fundamental","Means","Process","Strategic"),multiple = F),
               bsTooltip(id = "objType", title = "What type of objective is it?", trigger = "hover")),
        column(width=3,
               selectInput("objScale","Scale",choices = c("Natural","Proxy","Constructed"),multiple = F),
               bsTooltip(id = "objScale", title = "What type of scale is used to measure the status of the objective?", trigger = "hover")),
        column(width=3,
               textInput("objMetric","Metric","B/B(0)"),
               bsTooltip(id = "objMetric", title = "What metric or performance measure is used to assess the status of the objective?", trigger = "hover")),
        column(width=12,       
               actionButton("add_btn", "Click here to add the objective information entered above to the objectives table below", class = "btn-primary")),        
        column(width=12,
               p("Objectives Table:"),
               DTOutput("ObjectivesTable"),
               br(),
        ),
    ), #end fluidRow
    fluidRow(style = "background-color: #d0d6d1;",
        column(width=12,
               p(em("Alternatives Process"))),
        column(width=3,
              checkboxInput("altDoc","Alternative production documented?"),
              bsTooltip(id = "altDoc", title = "Was a process for generating alternatives documented?", trigger = "hover"),
               ),
        column(width=4,
               selectInput("altSource","Alternatives source",choices = c("Select one or more. Click & Delete to deselect" = "","Government","Management","Fishery","Independent","Scientists","Public","Decision Makers","Experts","Decision Analysts","Unknown"),
                           multiple = T,width="450px"),
               bsTooltip(id = "altSource", title = "Who provided the alternatives?", trigger = "hover"),),
        column(width=4,
               selectInput("subAltSource","Subjective alternattives source",choices = c("Select one or more. Click & Delete to deselect" = "","Government","Management","Fishery","Independent","Scientists","Public","Decision Makers","Experts","Decision Analysts","Unknown"),
                                  multiple = T,width="450px"),
               bsTooltip(id = "subAltSource", title = "If it was not explicitly documented, based on your subjective interpretation of the process Who provided the alternatives?", trigger = "hover"),),
    ), #end fluidRow
    fluidRow(style = "background-color: #9faba0;",
      column(width=12,
               p(em("Alternatives: Individual Alternative Entry"))),
        column(width=4,
               selectInput("altType","Management Tools",choices = c("Catch Limit","Effort Limit","Size Limit","Access Control",
                                                                    "Share Allocation","Closure","Other"),multiple = F),
               bsTooltip(id = "altType", title = "What type of management tool was evaluated?", trigger = "hover")),
        column(width=8,
               textAreaInput("altAlternatives","Alternatives","5 Alternative harvest control rules (HCR), varrying by explotation rate and biological reference point time period",width="600px"),
               bsTooltip(id = "altAlternatives", title = "Wat alternative actions were evaluated for this management tool?", trigger = "hover")),
        column(width=12,
               actionButton("addAlt_btn", "Click here to add the alternative information entered above to alternatives table below",class = "btn-primary")),
        column(width=12,
               p("Alternatives Table"),
               DTOutput("AlternativesTable"),
               br(),
        ),
    ), # end fluidRow
    fluidRow(style = "background-color: #d0d6d1;",
        column(width=12,
               p(em("Consequences and Tradeoffs"))),
        column(width=6,
               selectInput("predMethod","Prediction Method",choices = c("Select one"="","Dynamic programming","Expert elicitation","Mental models","Simulation modeling","Unknown"),
                           multiple = T,width="450px"),
               bsTooltip(id = "predMethod", title = "What method was used to predict the consequences of proposed management strategies?", trigger = "hover"),),
        column(width=6,
               selectInput("drivers","Consequence Drivers",choices = c("Select one or more. Click & Delete to deselect"="","Allowable catch adjustement","Climate change","Ecosystem based management","Environmental conditions",
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
               selectInput("tradeMethod","Trade-off evaluaiton method",choices = c("Select one or more. Click & Delete to deselect"="","Mental analysis","MCDA","Negotiation","Visualization","Unknown"),
                           multiple = T,width="450px"),
               bsTooltip(id = "tradeMethod", title = "How were trade-offs evaluated?", trigger = "hover"),),
        column(width=4,
               selectInput("subTradeMethod","Subjective trade-off evaluaiton method",choices = c("Select one or more. Click & Delete to deselect"="","Mental analysis","MCDA","Negotiation","Visualization","Unknown"),
                           multiple = T,width="450px"),
               bsTooltip(id = "subTradeMethod", title = "If it was not explicitly documented, based on your subjective interpretation how were trade-offs evaluated?", trigger = "hover"),),
        column(width=12,
               hr(),
               p(em("Notes and Comments")),
               textAreaInput("notes","notes","E.g., Stakeholders were included in this process, and there was some documentation of the process.  Additional documentation could have covered the development of the problem, roles, and the results of the visual trade-off evaluation.", width="1000px"),
               bsTooltip(id = "notes", title = "Add any notes you think would be helpful here.", trigger = "hover"),
               hr()),
    ), # end fluidRow
    fluidRow(style = "background-color: #e0dfc3;",
        column(width=12,
               p(em("Submit Review"))),
        column(width=4,
               textInput("reviewer","Your name","E.g., Jonathan Cummings"),
               bsTooltip(id = "reviewer", title = "Please provide your name", trigger = "hover"),),
        column(width=4,
               textInput("contactInfo","Your contact info","E.g., jcummings@massd.edu"),
               bsTooltip(id = "contactInfo", title = "Please provide your contact information", trigger = "hover"),),
        column(width=4,
               actionButton("submit", "Click here to submit you MSE review",class="btn-warning")),
               p("")
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
    
    objTable <- reactiveVal(objTable)
    
    observeEvent(input$add_btn, {
      t = rbind(data.frame(Category=input$objCategory,Objective=input$objObjective,Descriptions=NA,Direction=NA,
                           Type=NA,Scale=NA,Metric=NA),
                objTable())
      objTable(t)
    })
    
    output$ObjectivesTable <- renderDT({
      datatable(objTable(), selection = 'single', options = list(dom = 't'))
    })
    
    altTable <- reactiveVal(altTable)
    
    observeEvent(input$addAlt_btn, {
      t = rbind(data.frame(Category=input$objCategory,Objective=input$objObjective),
                altTable())
      altTable(t)
    })
    
    output$AlternativesTable <- renderDT({
      datatable(altTable(), selection = 'single', options = list(dom = 't'))
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
} # end server

# Run the application 
shinyApp(ui = ui, server = server)