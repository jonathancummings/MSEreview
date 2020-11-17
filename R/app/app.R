# MSE Problem Framing 
# Literature Review Data Analysis
# 4/9/2020, JWC

##### Meta Data and setup #####
# Description: Import and analyze MSE documentation from database
# Value: TBD

# load libraries
library(tidyverse) # upgrade to base R
library(readxl) # read in excel files
library(shiny) # Shiny App
library(shinythemes) # web themes for Shiny
library(DT) # Data Table
library(ggmap) # map plotting
library(mapdata) # basemap generation
library(odbc) # database connection 
library(RMySQL) # MySQL scripting in R
library(DBI) # database interface
library(here) # file directory assistance
library(digest) # used to create unique file names
library(googlesheets4) # link to Google sheets for storage
library(shinyBS) # enable tooltips for form inputs

#To open a connection to the database:
# Copy into command prompt:
## cloud_sql_proxy -instances=alpine-tracker-230222:us-east1:mse-review=tcp:3306

# # Create connection function
# getSqlConnection <- function(){
#   cn <-
#     dbConnect(
#       RMySQL::MySQL(),
#       dbname = 'MSEreview',
#       host = '127.0.0.1',
#       port=3306,
#       username = 'root',
#       password = '2u8aNDdur1aFdb8z')
#   return(cn)
# }
# # Open connection
# con <- getSqlConnection()
# 
# # Obtain data tables
# study<-dbReadTable(con,"tblStudy")
# # study<-read_xlsx("DB files - Excel/tblStudy.xlsx")
# mgmt<-dbReadTable(con,"tblStudyManagement")
# # mgmt<-read_xlsx("DB files - Excel/tblStudyManagementTools.xlsx")
# # mgmt$fkStudyID<-parse_integer(mgmt$fkStudyID)
# obj<-dbReadTable(con,"tblStudyObjectives")
# # obj<-read_xlsx("DB files - Excel/tblStudyObjectives.xlsx")
# # obj$fkStudyID<-parse_integer(obj$fkStudyID)
# fields<-dbReadTable(con,"tblStudyFields")
# # fields<-read_xlsx("DB files - Excel/tblStudyFields.xlsx")
# 
# # To close the connection
# dbDisconnect(con)

# Load data for app (data is obtained via the analysisexcel.R script)
load("MSEreview.RData")

# Get map background for plotting the map
world <- borders("world", colour="gray50", fill="gray50", alpha=0.75) # create a layer of borders

##### Set up data entry form #####
# provide location to save data
ssid <- as_sheets_id("1YjOTei_N7RS05rxXrVB6iuUptjYTDQC4xTLeoR-8fi8") #ID of google sheet. This sheet must be created properly for the app to function.

# create table to store objective data
objTable = data.frame(Category=character(),Objective=character(),Descriptions=character(),Direction=character(),Type=character(),
                      Scale=character(),Metric=character())
# create table to store alternatives
altTable = data.frame("Management Tools"=character(), Alternatives=character())

# Function to save form entries to google sheet
saveData <- function(data,sheet) {
  # Add the data as a new row
  sheet_append(ssid, data, sheet)
}

# Define the fields we want to save from the data entry form
reviewFields <- c("DOI","author","pubYear","system","location","lat","long","species","citation","authors","title","journal","poc",
                  "processDoc","rolesDoc","openMeetings","optimAlt","decisionDoc","implemented","decision","leader","participants",
                  "problemDoc","problemDef","objDoc","objSource","subObjSource","elicitationObj","altDoc","altSource","subAltSource",
                  "predMethod","drivers","tradeoffsDoc","tradeMethod","subTradeMethod","notes","reviewer","contactInfo")
objFields <- c("objCategory","objName","objDescription","objDirection","objType","objScale","objMetric")
altFields <- c("altType","altAlternatives")

##### Data Processing #####

# Edit tables for joining
study<-study %>% 
  unite(Citation,c(Authors,YearPub),sep=" ",remove=F)

study.join<-study %>% 
  select(ID,Citation)

mgmt.join<-mgmt %>% 
  group_by(fkStudyID) %>%
  summarise(ManagementType = toString(sort(unique(MPManagementTool))),
            AlternativesEvaluated = toString(sort(unique(MPAlternativesEvaluated))))

obj.join<-obj %>% 
  group_by(fkStudyID) %>%
  summarise(ObjectiveCategories = toString(sort(unique(ObjCategory))))

# Join tables
data<-full_join(study,mgmt.join,by=c("ID"="fkStudyID"))
data<-full_join(data,obj.join,by=c("ID"="fkStudyID"))
obj.data<-right_join(study.join,obj,by=c("ID"="fkStudyID"))

# Move comment column to the end
data<-data %>% 
  select(-'Comments', 'Comments')

# Filter data to create separate pub and climate change objects
data_pub<-filter(data,RandomSample==TRUE)
data_CC<-filter(data,str_detect(Drivers,"Climate Change")&UseInPublication==TRUE)

data_pub.join<-data_pub %>%
  select(ID,Citation)
data_CC.join<-data_CC %>%
  select(ID,Citation)

obj.data_pub<-left_join(data_pub.join,obj,by=c("ID"="fkStudyID"))%>%
  select(-c("ID","ID.y"))
obj.data_CC<-left_join(data_CC.join,obj,by=c("ID"="fkStudyID"))%>%
  select(-c("ID","ID.y"))

alt.data_pub<-left_join(data_pub.join,mgmt,by=c("ID"="fkStudyID"))%>%
  select(-c("ID","ID.y"))
alt.data_CC<-left_join(data_CC.join,mgmt,by=c("ID"="fkStudyID"))%>%
  select(-c("ID","ID.y"))

data_pub<-data_pub %>%
  select(-c("ID"))
data_CC<-data_CC %>%
  select(-c("ID"))

data<-select(data,-c("ID"))
obj.data<-select(obj.data,-c("ID","ID.y"))

# Get columns whose width needs editing
targetsC<-match(c("Comments","ProblemDefinition"),names(data))
targetsAE<-match(c("AlternativesEvaluated","FullCitation"),names(data))
targetsSp<-match(c("Species","ObjElicitationMethod"),names(data))
targetsSy<-match(c("System"),names(data))
targetsL<-match(c("Location"),names(data))

# Get columns for study summary
summary.col<-c("Citation",
               "Species",
               "Location",
               "System")
# Get columns for study drivers and problem
prob.col<-c("Citation",
            "ProblemDefinition",
            "Drivers",
            "ConsequencePrediction",
            "TradeOffMethod_Exp",
            "TradeOffMethod_Sub",
            "Decision")
targetsPD<-match(c("ProblemDefinition"),prob.col)
# Get columns for frequency analysis
freq.col<-c("ProcessExplicit",
            "ProblemDefinitionExplicit",
            "ObjectivesExplicit",
            "AlternativesExplicit",
            "TradeOffsExplicit",
            "DecisionExplicit",
            "RoleSpecification",
            "OpenMeetings",
            "ResultsAdopted")
# Get columns for participant analysis
part.col<-c("Leader",
            "Participants",
            "ObjElicitationSource_Exp",
            "ProcedureElicitation_Exp",
            "ObjElicitationSource_Sub",
            "ProcedureElicitation_Sub")
obj.col<-c("ObjType",
           "ObjCategory",
           "ObjDirection",
           "ObjScale")
alt.col<-c("ManagementType",
           "AlternativesEvaluated")
# Get columns for map
map.col<-c("Latitude",
           "Longitude",
           "Citation",
           "Drivers")

##### Shiny App #####
##### UI #####
ui <- fluidPage(
  theme = shinytheme("readable"),
  tabsetPanel(
    #### Tab 1: About ####
    tabPanel("About", 
      h1("An Assessment of Management Strategy Evaluations"),# title of the page
      p("What can be learned from the Management Strategy Evalaution (MSE)
      litearture and how have previous MSE processes been documented?"),
      hr(),
      p("The calls for management to utilize management strategy evaluation
      are both aspirational and estimable in their aim and vision.
      Management strategy evaluation (MSE) is 'widely considered to
      be the most appropriate way to evaluate the trade‐offs achieved by
      alternative management strategies and to assess the consequences of
      uncertainty for achieving management goals' (Punt et al. 2014). Thus,
      MSE is a compelling tool to assess management startegies in the face of
      challenging new conditions. For example, in our changing oceans and
      with the increasing impacts of climate change MSE can be a tool to evaluate 
      climate-responsive approaches to management decisions."),
      p("Is the published literature supporting advancement and improvement of MSE
      processes and how can MSE practioners better learn from previouse efforts?
      We evalaute this question and provide a repository to document MSE processes
      to aid practioners as they take on future MSE processes."),
      h4("Objectives"),
      p("Our objectives are"),
      tags$div(
        tags$ul(
          tags$li("to review the methodlogy and documentation of MSE processes using
          the published literature, thereby assessing what compoents of the process are
          documented in a manner that supports repetiion and learning within the MSE
          practitioner community")
        )
      ),
      tags$div(
        tags$ul(
          tags$li("to provide a repository whre MSE practitioners can learn from previous MSE efforts.")
        )
      ),
      h4("Methods"),
      p("We used the Structured Decision Making process as our framework to assess MSE
      process. Strucutred Decision Making is the most common term used for decison
      analysis in natural resource management and it is a valueable framework for this
      analysis because it decomposes problems and decision making into the component
      parts that make up a decision making process (figure 1)."),
      imageOutput("imageSDM",
                   width=400,
                   height=275),
      p(em("Figure 1. A depiction of the Structured Decision Making process frameowrk
      and the components used to review MSE processes")),
      p("We searched the MSE literature via Web of Science, searching for 
      “management strategy evaluation” by topic across all years on January
      8th, 2019. This search returned 264 results, of which 154 were management
      strategy evaluations after removing articles that were reviews, 
      meta-analyses, or simply cited other MSE articles. We reviewed a random
      sample of 30 of these 154 articles."),
      p("The results of this analysis will be avaialable in a future publication. You may
        also explore the results of the analysis or submit a review of an MSE publication here."),
      h4("Explore the Results"),
      p("Select the results you would like to examine using the radio buttons
      below.  The figures and tables in this application are updated by your
      selection."),
      radioButtons("data_filter",
         "Show results from:",
         choices = c(
           "30 articles reviewed for Cummings et. al. Publication, "="pub",
           "MSE including climate change as a driver"="CC",
           "All MSEs (entered in the database to date)"="all"),
         width='400px'),
        textOutput("radio"),
      hr(),
      h5("Where have management strategy evaluations occurred?"),
      p("Hovering over a point on the map will give the associated citation below, 
        while selecting an area will give all citations in the 'brushed' map area. To
        learn more about those MSEs you can search for the citations in the Data - All
        tab."),
      plotOutput("mse.map",
        brush = brushOpts(id = "map_brush"),
        hover = hoverOpts("map_hover")
      ),
      p("Figure 2. Map of MSE locations. Points represent the approximate 
        center point of the fishery or management region the evalauted in the MSE."),
      fluidRow(
        column(width = 6,
          h4("Hovered points"),
          tableOutput("hover")
        ),
        column(width = 6,
          h4("Brushed points"),
          tableOutput("brush")
        )
      )
    ),
    #### Tab 2: Results - Figures ####
    tabPanel("Results - Figures", 
             h2("MSE literature review results - Figures"),
             p("The figures in this tab show what percentage of the components of a deciison process were indlucded in an MSE's 
               documentation and who participated and at particiular stages in the decision process."),
             hr(),
             h5("Are the components of the decision process explicit in MSEs?"),
             p("For the set of MSEs selected, this figure displays the percentage of those MSEs that explicitly completed and included
               documentation of the selected components of a decision process."),
             plotOutput("Freq.plot",height="100%"),
             p("Figure 3. Percentage of MSEs that included: "),
             p(strong("Process")," - Explicit documentation of the decision making process used to conduct the MSE,",
              strong("Problem")," - Explicit documentation of the problem the MSE is attempting to address,",
              strong("Objectives")," - Explicit documentation of the process used to produce objectives and performance metrics to evaluate the management procedures,",
              strong("Tradeoffs")," - Explicit documentation of the tradeoff evaluation process,",
              strong("Decision")," - Documentation of the alternative selected and implemented as the management procedure going forward,",
              strong("Roles")," - Explicit documentation of the MSE participants' roles,",
              strong("Open Meetings")," - Meetings open to the public or those outside of the set of MSE participants,",
              strong("Adopted*")," - Explicit documentation of decision makers implementing the results of the MSE."),
             p(em("*note that the results of an MSE may have been adopted but not documented because the adoption occurred following
                  completing of the associated literature")),
             h5("Who is involved, and participates in, MSEs?"),
             p("For the set of MSEs selected, this figure displays the percentage of those MSEs that included each type of participant 
               for diffrenet stages in the process."),
             plotOutput("part.plot",height="100%"),
             p("Figure 4. Percentage of MSEs that included each participant type at different stages:"),
             p(strong("Process")," - Who initiated and lead the MSE process?,",
               strong("Participants")," - Who participated in the the MSe process?,",
               strong("Documented Objectives")," - Who provided the objectives based on the documentation of the MSE process?,",
               strong("Subjective Objectives")," - Who most likely provided the objectives based on a subjective reading of the documentation?,",
               strong("Documented Alternatives")," - Who provided the alternatives based on the documentation of the MSE process?,",
               strong("Subjective Alternatives")," - Who most likely provided the alternatives based on a subjective reading of the documentation?,"),
             ),
    #### Tab 3: Results - Tables ####
    tabPanel("Results - Tables", 
             h2("Explict Process Documentation"),
             p("Table 1. The number and percentage of MSE processes that explicitly included each step in the process"),
             tableOutput("MSE.freq"),
             h2("Participation"),
             p("Table 2. Number of MSEs in which each participant group participated by process stage"),
             tableOutput("MSE.part"),
             h2("System Drivers"),
             p("Table 3. Number of MSEs that included consideration of the following drivers in the system model"),
             tableOutput("MSE.drive"),
             h2("Objective Categories"),
             p("Table 4. Number of MSEs that included consideration of the following category of objectives in the evaluation"),
             tableOutput("MSE.objcat"),
             h2("How were objectives defined?"),
             p("Table 5. Number of, percentage of objectives, and Frequency per MSE of objective usage"),
             tableOutput("MSE.obj"),
             p("Table 6. Frequency of objective usage combinations"),
             DT::dataTableOutput("MSE.obj2"),
             h2("What management procedures were evaluated?"),
             p("Table 5. Number of, percentage of objectives, and Frequency per MSE of objective usage"),
             tableOutput("MSE.alt")
             ),
    #### Tab 4: Data - All ####
    tabPanel("Data - All",
             h1("All of the MSE literature review data"),
             h2("Study data"),
             DT::dataTableOutput("MSE.Table"),
             hr(),
             h2("Objectives data"),
             DT::dataTableOutput("MSE.Obj.Table"),
             hr(),
             h2("Field Descriptions"),
             tableOutput("MSE.Fields")
             ),
    #### Tab 5: Data Tables - Subsets ####
    tabPanel("Data Tables - Subsets",
             h1("Selected columns of MSE literature review data"),
             h2("Study Summaries"),
             DT::dataTableOutput("MSE.summary"),
             hr(),
             h2("Problem Components"),
             DT::dataTableOutput("MSE.problem")
             ),
    #### Tab 6: Data Tables - Summaries ####
    tabPanel("Data Tables - Summaries",
             # h1("Summary of all MSE processes"),
             # h2("Study Summaries"),
             # DT::dataTableOutput("imageSDM"),
             # hr(),
             # h2("Counts"),
             # DT::dataTableOutput("MSEcounts")
    ),
    #### Tab 7: Submit MSE - Data Entry Form ####
    tabPanel("Submit MSE - Data Entry Form",
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
                      column(width=3,
                             textAreaInput("authors","All Authors","Bunnefeld, Nils; Hoshino, Eriko; Milner-Gulland, Eleanor J."),
                             bsTooltip(id = "authors", title = "Enter all authors of the publication, in [Last, first initial.; next author;] format", trigger = "hover")),
                      column(width=5,
                             textAreaInput("title","Title","Management strategy evaluation: a powerful tool for conservation?"),
                             bsTooltip(id = "title", title = "Enter the publication title", trigger = "hover")),
                      column(width=4,
                             textInput("journal","Journal","Trends in Ecology and Evolution"),
                             bsTooltip(id = "journal", title = "Enter the full name of the journal", trigger = "hover")),
                      column(width=12,
                             textInput("poc","Point of contact: contact info","e.g., jcummings@umassd.edu", width="400px"),
                             bsTooltip(id = "poc", title = "Provide contact info a point of contact associated with this MSE", trigger = "hover"),
                             
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
                      column(width=12,
                             textAreaInput("decision","Decision Result?"),
                             bsTooltip(id = "decision", title = "If results were adopted, a decision was documented, or an optimal management strategy was identified, what was it?", trigger = "hover")),
                      column(width=6,
                             selectInput("leader","Process Lead(s)?",choices = c("Select one or more. Click & Delete to deselect" = "","Government","Management","Fishery","Independent","Scientists","Public","Unknown"),
                                         multiple = T,width="450px"),
                             bsTooltip(id = "leader", title = "Who lead this MSE process?", trigger = "hover")),
                      column(width=6,
                             selectInput("participants","Participants?",choices = c("Select one or more. Click & Delete to deselect" = "","Government","Management","Fishery","Independent","Scientists","Public","Facilitators","Decision Makers","Experts","Decision Analysts"),multiple = T,width="450px"),
                             bsTooltip(id = "participants", title = "Who participated in this MSE process?", trigger = "hover")),
                      column(width=12,
                             
                             p(em("Problem Definition"))),
                      column(width=2,
                             checkboxInput("problemDoc","Problem documented?"),
                             bsTooltip(id = "problemDoc", title = "Was the problem defined and documented?", trigger = "hover")),
                      column(width=10,
                             textAreaInput("problemDef","How was the problem defined?","What approach to decision making will be support natural resource management decisions?", width="900px"),
                             bsTooltip(id = "problemDef", title = "Enter the problem definition for this MSE", trigger = "hover")),
                      column(width=12,
                             
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
                             bsTooltip(id = "altDoc", title = "Was a process for generating alternatives documented?", trigger = "hover")),
                      column(width=4,
                             selectInput("altSource","Alternatives source",choices = c("Select one or more. Click & Delete to deselect" = "","Government","Management","Fishery","Independent","Scientists","Public","Decision Makers","Experts","Decision Analysts","Unknown"),
                                         multiple = T,width="450px"),
                             bsTooltip(id = "altSource", title = "Who provided the alternatives?", trigger = "hover")),
                      column(width=4,
                             selectInput("subAltSource","Subjective alternattives source",choices = c("Select one or more. Click & Delete to deselect" = "","Government","Management","Fishery","Independent","Scientists","Public","Decision Makers","Experts","Decision Analysts","Unknown"),
                                         multiple = T,width="450px"),
                             bsTooltip(id = "subAltSource", title = "If it was not explicitly documented, based on your subjective interpretation of the process Who provided the alternatives?", trigger = "hover")),
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
                             bsTooltip(id = "drivers", title = "What drivers (aka, predictors, factors, model components) were included in the model used to predict the consequences?", trigger = "hover")),
                      column(width=4,
                             checkboxInput("tradeoffsDoc","Tradeoffs explicitly evaluated?"),
                             bsTooltip(id = "tradeoffsDoc", title = "Were trade-offs between the alternative management strategies evaluated and documented?", trigger = "hover")),
                      column(width=4,
                             selectInput("tradeMethod","Trade-off evaluaiton method",choices = c("Select one or more. Click & Delete to deselect"="","Mental analysis","MCDA","Negotiation","Visualization","Unknown"),
                                         multiple = T,width="450px"),
                             bsTooltip(id = "tradeMethod", title = "How were trade-offs evaluated?", trigger = "hover"),),
                      column(width=4,
                             selectInput("subTradeMethod","Subjective trade-off evaluaiton method",choices = c("Select one or more. Click & Delete to deselect"="","Mental analysis","MCDA","Negotiation","Visualization","Unknown"),
                                         multiple = T,width="450px"),
                             bsTooltip(id = "subTradeMethod", title = "If it was not explicitly documented, based on your subjective interpretation how were trade-offs evaluated?", trigger = "hover")),
                      column(width=12,
                             p(em("Notes and Comments"))),
                      column(width=12,
                             textAreaInput("notes","Notes","E.g., Stakeholders were included in this process, and there was some documentation of the process.  Additional documentation could have covered the development of the problem, roles, and the results of the visual trade-off evaluation.", width="1000px"),
                             bsTooltip(id = "notes", title = "Add any notes you think would be helpful here.", trigger = "hover"))
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
                             actionButton("submit", "Click here to submit your MSE review",class="btn-warning"))
             ) #end fluidRow
    ) # end tabPanel
  )
)

##### Server #####
# Shiny Server Section
server <- function(input, output, session) {   # code to create output using render
  #####-- Data analysis --#####
  # Filter data to include only those reviewed for the publication or all MSEs
  data_reviewed<-reactive({
    if(input$data_filter=="pub"){
      filter(data,RandomSample==TRUE)
    } 
    else 
      if(input$data_filter=="CC"){
        filter(data,str_detect(Drivers,"Climate Change")&UseInPublication==TRUE)
      }
    else 
      if(input$data_filter=="all"){
        filter(data,UseInPublication==TRUE|UseInPublication==FALSE)
    }
  })
  
  # Select study summary data_reviewed
  summary.data<-reactive({data_reviewed() %>%
    select(all_of(summary.col))
  })

  # Select study problem and driver data_reviewed
  prob.data<-reactive({data_reviewed() %>%
    select(all_of(prob.col))
  })
  
  # Frequency of method
  n_mse<-reactive({nrow(data_reviewed())})
  freq.data<-reactive({data_reviewed() %>%
    select(all_of(freq.col)) %>%
    rename("Process"="ProcessExplicit",
           "Problem"="ProblemDefinitionExplicit",
           "Objectives"="ObjectivesExplicit",
           "Alternatives"="AlternativesExplicit",
           "Tradeoffs"="TradeOffsExplicit",
           "Decision"="DecisionExplicit",
           "Roles"="RoleSpecification",
           "Open Meetings"="OpenMeetings",
           "Adopted"="ResultsAdopted") %>%
    summarise_all(sum) %>%
    gather(Explicit) %>%
    mutate(Percent=value/n_mse()*100) %>%
    mutate(Explicit=factor(Explicit,levels=
                             c("Process","Problem","Objectives","Alternatives","Tradeoffs",
                               "Decision","Roles","Open Meetings","Adopted"))) %>%
    rename("Number"="value")
    })

  # Who participates
  part.data_reviewed<-reactive({data_reviewed() %>%
    select(all_of(part.col)) %>%
      rename("Process"="Leader",
             "Doc Objectives"="ObjElicitationSource_Exp",
             "Doc Alternatives"="ProcedureElicitation_Exp",
             "Sub Objectives"="ObjElicitationSource_Sub",
             "Sub Alternatives"="ProcedureElicitation_Sub")
  })
  part.data_reviewed2<- reactive({part.data_reviewed() %>%
    purrr::map(~ strsplit(as.character(.),split=", ")) %>%
    purrr::map(unlist) %>%
    purrr::map(table)
  })

  part.data_reviewed3<-reactive({plyr::ldply(part.data_reviewed2(),data.frame)})
  part.data_reviewed4<-reactive({
    d<-part.data_reviewed3()
    colnames(d)<-c("Stage","Participants","Number")
    d
  })
   
  part.data_reviewed5 <- reactive({
    neworder <- c("Process","Participants","Doc Objectives",
                  "Sub Objectives","Doc Alternatives",
                  "Sub Alternatives")
    newlabels <- c("Process","Participants","Documented Objectives",
                   "Subjective Objectives","Documented Alternatives",
                   "Subjective Alternatives")
    part.data_reviewed4() %>%
    mutate(Percent=Number/n_mse()*100) %>%
    mutate(Stage=factor(Stage,levels=neworder,labels=newlabels)) %>% 
    ungroup() %>%
      # 2. Arrange by
      #   i.  facet group =Stage
      #   ii. bar height
    arrange(Stage, Percent) %>%
      # 3. Add order column of row numbers
    mutate(order = row_number())
  })
   
   part.data_table<-reactive({part.data_reviewed5() %>%
    as_tibble()  %>% 
    select(Stage,Participants,Number)  %>% 
    rename("Participant Group"="Participants") %>%
    spread(Stage,Number,fill=0) 
  })

  # What drivers are considered
  drive.data<-reactive({data_reviewed() %>%
    select(Drivers) %>%
    purrr::map(~ strsplit(as.character(.),split=", ")) %>%
    purrr::map(unlist) %>%
    purrr::map(table) %>%
    plyr::ldply(data.frame) %>%
    select(Var1,Freq) %>%
    rename("Driver"="Var1","Frequency"="Freq") %>%
    mutate(Percent=round(Frequency/n_mse()*100,0)) %>%
    arrange(desc(Frequency))
  })

  # What Objective types are considered
  objcat.data<-reactive({data_reviewed() %>%
    select(ObjectiveCategories) %>%
    purrr::map(~ strsplit(as.character(.),split=", ")) %>%
    purrr::map(unlist) %>%
    purrr::map(table) %>%
    plyr::ldply(data.frame) %>%
    select(Var1,Freq) %>%
    rename("Objective Category"="Var1","Frequency"="Freq") %>%
    mutate(Percent=round(Frequency/n_mse()*100,0)) %>%
    arrange(desc(Frequency))
  })

  # How were objectives defined
  obj.data2<-reactive({obj.data %>%
    select(all_of(obj.col)) %>%
    purrr::map(table)
  })
  
  obj.data3<-reactive({
    d<-plyr::ldply(obj.data2(),data.frame)
    colnames(d)<-c("Objective","Type","Number")
    d
  })

  obj.data_table1 <- reactive({neworder <- c("ObjCategory","ObjType","ObjDirection","ObjScale")
    newlabels <- c("Category","Type","Direction","Scale")
    obj.data3() %>%
    mutate(Percent=sprintf("%.0f",round(Number/nrow(obj.data)*100,0))) %>%
    mutate('Per MSE'=sprintf("%.2f",round(Number/n_mse(),2))) %>%
    mutate(Objective=factor(Objective,levels=neworder,labels=newlabels))
  })

  obj.data_table2<-reactive({
    d<-obj.data %>%
      select(all_of(obj.col)) %>%
      group_by(ObjType,ObjCategory,ObjDirection,ObjScale) %>%
      summarize(n())
    colnames(d)<-c("Type","Category","Direction","Scale","Number")
    d
  })

  # What Alternative types are considered
  altcat.data<-reactive({data_reviewed() %>%
    select(ManagementType) %>%
    purrr::map(~ strsplit(as.character(.),split=", ")) %>%
    purrr::map(unlist) %>%
    purrr::map(~ strsplit(as.character(.),split=", ")) %>% 
    purrr::map(unlist) %>%
    purrr::map(table) %>%
    plyr::ldply(data.frame) %>%
    select(Var1,Freq) %>%
    rename("Management Type"="Var1","Number"="Freq") %>%
    mutate(Percent=sprintf("%.0f",round(Number/n_mse()*100,0))) %>%
    mutate('Per MSE'=sprintf("%.2f",round(Number/n_mse(),2))) %>%
    arrange(desc(Number))
  })

  # Common components

  # Where MSEs have occured
  map.data<-reactive({data_reviewed() %>%
    select(all_of(map.col))
  })

  ##### Tab 1 - Filtering #####
  output$MSEcounts <- renderTable({
    tibble("MSE type"=c("Published","Random Sample","Climate Change"),
           "Count"=c(154,30,16))
  },digits=0)
  output$radio <-renderText(paste0("Number of MSEs in results: ", nrow(data_reviewed())))
  output$imageSDM<-renderImage({
    filename<-normalizePath(file.path('./www',paste("SDMProcessFramework.png")))
    list(src=filename,
         width=400,
         height=275)},deleteFile = FALSE)
  output$mse.map <- renderPlot({
    # plot MSEs on map
    ggplot(data=map.data(),aes(x=Longitude, y=Latitude)) + world +
      geom_point(color="red",size=3) + geom_point(size = 3, colour = "black", shape = 1) +
      theme_void()
  })
  observeEvent(input$map_hover,
               output$hover <- renderTable({
                 nearPoints(data_reviewed(), input$map_hover,"Longitude","Latitude") %>% 
                   select(Citation)
               })
  )
  observeEvent(input$map_brush,
               output$brush <- renderTable({
                 brushedPoints(data_reviewed(), input$map_brush, "Longitude","Latitude")%>% 
                   select(Citation)
               })
  )

  ##### Tab 2 - Results - plots #####
  output$Freq.plot <- renderPlot({
    ggplot(freq.data(),aes(Explicit,Percent))+
      geom_col()+geom_vline(xintercept=3.5,linetype="dashed")+
      coord_flip()+scale_y_continuous(limits=c(0,100))+xlab(NULL)+ylab("Percentage")+
      scale_x_discrete(
          limits=c("Adopted","Open Meetings","Roles","Decision","Tradeoffs",
                   "Objectives","Problem","Process"), 
          labels=c("Adopted","Open Meetings","Roles","Decision","Tradeoffs",
                   "Objectives","Problem","Process")) +
      theme_bw()+theme(text = element_text(size=18))
  },height=300,width=600)
  output$part.plot <- renderPlot({
    ggplot(part.data_reviewed5(),aes(x=order,y=Percent)) +
      facet_wrap(~Stage,scale="free",ncol=2) + geom_col(width=0.8) +  
      scale_x_continuous(
        breaks = part.data_reviewed5()$order,
        labels = part.data_reviewed5()$Participants,
        expand = c(0,0)
      )+scale_y_continuous(limits=c(0,100)) +
      ylab("Percentage")+coord_flip()+xlab(NULL) +
      theme_bw()+theme(text = element_text(size=18))
  },height=600)
  ##### Tab 3 - Results - tables #####
  output$MSE.freq <- renderTable({
    freq.data()
  },digits=0)
  output$MSE.part <- renderTable({
    part.data_table()
  },digits=0)
  output$MSE.drive <- renderTable({
    drive.data()
  },digits=0)
  output$MSE.objcat <- renderTable({
    objcat.data()
  },digits=0)
  output$MSE.obj <- renderTable({
    obj.data_table1()
  })
  output$MSE.obj2 <- DT::renderDataTable({
    obj.data_table2()
  })
  output$MSE.alt <- renderTable({
    altcat.data()
  })
  ##### Tab 4 #####
  output$MSE.Table <- DT::renderDataTable({
    data_reviewed()
  },  options = list(autoWidth = TRUE,
                     columnDefs = list(list(width = '1125px', targets = targetsC), # comments, ProblemDefinition
                                       list(width = '700px', targets = targetsAE), # AlternativesEvaluated, FullCitation
                                       list(width = '500px', targets = targetsSp),
                                       list(width = '250px', targets = targetsSy),
                                       list(width = '75px', targets = targetsL)),
                     scrollX=TRUE)
  )
  output$MSE.Obj.Table <- DT::renderDataTable({
    obj.data
  }) # NEED TO FIX THIS TO FILTER BASED ON THE RESULTS RADIO BUTTON, I.E., ALL REVIEWS OR JUST REVIEWS FOR PUBLICATION
  output$MSE.Fields <- renderTable({
    mse.fields<-arrange(fields,Order) %>% 
      select(-Order)
    mse.fields
  })
  ##### Tab 5 #####
  output$MSE.summary <- DT::renderDataTable({
    arrange(summary.data(),Citation)
  })
  output$MSE.problem <- DT::renderDataTable({
    arrange(prob.data(),Citation)
  }, options = list(autoWidth = TRUE,
                    columnDefs = list(list(width = '800px', targets = targetsPD)), # comments, Problem Definition
                    scrollX=TRUE)
  )
  ##### Tab 6 #####
#   output$imageSDM<-renderImage({
#     filename<-normalizePath(file.path('./www',paste("SDMProcessFramework.png")))
#     list(src=filename,
#          width=400,
#          height=275)},deleteFile = FALSE)

  ##### Tab 7 #####
  # Whenever a field is filled, aggregate all form data
  reviewData <- reactive({
    data <- sapply(reviewFields, function(x) input[[x]]) %>%
      paste() %>%
      t() %>% 
      as.data.frame()
    data
  })
  
  objTable <- reactiveVal(objTable)
  
  observeEvent(input$add_btn, {
    t = rbind(data.frame(Category=input$objCategory,Objective=input$objName,Descriptions=input$objDescription,
                         Direction=input$objDirection,Type=input$objType,Scale=input$objScale,Metric=input$objMetric),
              objTable())
    objTable(t)
  })
  
  output$ObjectivesTable <- renderDT({
    datatable(objTable(), selection = 'single', options = list(dom = 't'))
  })
  
  altTable <- reactiveVal(altTable)
  
  observeEvent(input$addAlt_btn, {
    t = rbind(data.frame("Management Tools"=input$altType,Alternatives=input$altAlternatives),
              altTable())
    altTable(t)
  })
  
  output$AlternativesTable <- renderDT({
    datatable(altTable(), selection = 'single', options = list(dom = 't'))
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(reviewData(),1)
    saveData(objTable(),2)
    saveData(altTable(),3)
  })
}

shinyApp(ui, server)