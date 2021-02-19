# MSE Problem Framing 
# Literature Review Data Analysis
# 4/9/2020, JWC

##### Meta Data and setup #####
# Description: Display results of MSE documentation review, display MSe reposity info, and enable MSE reviews to be submitted

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
library(googledrive) # enable connections to google drive
library(reactable) # create interactive tables
library(shinyWidgets) # create fancy inputs and outputs

# Load data for app (data is obtained via the LoadData.R script)
load(here("data/MSEreview.RData"))

# Get map background for plotting the map
world <- borders("world", colour="gray50", fill="gray50", alpha=0.75) # create a layer of borders

# Authorize the connection to the google sheet by providing the path to the authorization token
gs4_auth(path="alpine-tracker-230222-353a701fc629.json")

# Provide the unique ID for the google sheet to add data to
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

##### Data Processing #####

# Edit tables for joining
study<-study %>% 
  unite(Citation,c(Author,YearPub),sep=" ",remove=F)

study.join<-study %>% 
  select(ID,DOI,Citation)

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
alt.data<-right_join(study.join,mgmt,by=c("ID"="fkStudyID"))

# Move comment column to the end
data<-data %>% 
  select(-'Comments', 'Comments') %>% 
  mutate(ConsequenceExplicit=ConsequencePrediction!="Unknown")

# create names object for use in selecting columns later on
names.data.tab5<-data %>% 
  select(-c('Comments','FullCitation','RandomSample','UseInPublication','AlternativesEvaluated','ProblemDefinition',
            'ObjElicitationMethod'))

# Establish formatting for a "sticky" always in view column in tables.
sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                     borderRight = "1px solid #eee")

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
alt.data<-select(alt.data,-c("ID","ID.y"))

# # Get columns whose width needs editing
# targetsC<-match(c("Comments","ProblemDefinition"),names(data))
# targetsAE<-match(c("AlternativesEvaluated","FullCitation"),names(data))
# targetsSp<-match(c("Species","ObjElicitationMethod"),names(data))
# targetsSy<-match(c("System"),names(data))
# targetsL<-match(c("Location"),names(data))

# Get columns for publication summary
pub.col<-c("DOI",
           "Citation") # place holder until other columns of data are added!!
# pub.col<-c("DOI",
#             "Citation",
#             "AllAuthors",
#             "Title",
#             "Journal",
#             "POC")
# Get columns for study summary
summary.col<-c("DOI",
               "Citation",
               "Location",
               "System",
               "Species")
# Get columns for study drivers and problem
prob.col<-c("DOI",
            "Citation",
            "ProblemDefinition",
            "Drivers",
            "ConsequencePrediction",
            "TradeOffMethod_Exp",
            "TradeOffMethod_Sub",
            "Decision")
# Get column to assigne to an object to adjust column width later
targetsPD<-match(c("ProblemDefinition"),prob.col)
# Get columns for frequency analysis
freq.col<-c("ProcessExplicit",
            "ProblemDefinitionExplicit",
            "ObjectivesExplicit",
            "AlternativesExplicit",
            "ConsequenceExplicit",
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
      h1("BETA version"),
      h1("An Assessment of Management Strategy Evaluations"),# title of the page
      p("What can be learned from the Management Strategy Evaluation (MSE) literature? How have previous MSE processes been documented?"),
      hr(),
      p("The calls for management to utilize management strategy evaluation are both aspirational and estimable in their aim and vision. Management
        strategy evaluation (MSE) is 'widely considered to be the most appropriate way to evaluate the trade‐offs achieved by alternative management
        strategies and to assess the consequences of uncertainty for achieving management goals' (Punt et al. 2014). Thus, MSE is a compelling tool 
        to assess management strategies in the face of challenging new conditions. For example, in our changing oceans and with the increasing impacts
        of climate change MSE can be a tool to evaluate climate-responsive approaches to management decisions."),
      p("Is the published literature supporting advancement and improvement of MSE processes? How can MSE practitioners better learn from previous
        efforts? We evaluate these questions and provide a repository to document MSE processes to aid practitioners as they take on future MSE
        processes."),
      h4("Objectives"),
      p("Our objectives are"),
      tags$div(
        tags$ul(
          tags$li("to review the methodology and documentation of MSE processes using the published literature, assessing what components
                  of the process are documented in a manner that supports repetition and learning within the MSE practitioner community, and")
        )
      ),
      tags$div(
        tags$ul(
          tags$li("to provide a repository where MSE practitioners can learn from previous MSE efforts.")
        )
      ),
      h4("Methods"),
      p("We used the Structured Decision Making process as our framework to assess MSE processes. Structured Decision Making is the most common term
        used for decision analysis in natural resource management and it is a valuable framework for this analysis because it decomposes problems and
        decision making into the component parts that make up a decision making process (figure 1)."),
      imageOutput("imageSDM",
                   width=400,
                   height=275),
      p(em("Figure 1. A depiction of the Structured Decision Making process framework and the components used to review MSE processes")),
      p("We searched the MSE literature via Web of Science, searching for “management strategy evaluation” by topic across all years on
        January 8th, 2019. This search produced 154 MSEs. While the initial search returned 264 results, 154 remained after removing articles that were
        reviews, meta-analyses, or simply cited other MSEs. A random sample of 30 of these 154 articles were reviewed for our forthcoming Cummings et. al.
        publication."),
      h4("Explore the Results"),
      p("Select the results you would like to examine using the radio buttons below. The figures and tables in this application are updated by your
        selection."),
      radioButtons("data_filter",
         "Show results from:",
         choices = c(
           "MESs reviewed for Cummings et. al. Publication, "="pub",
           "All MSEs (entered in the database to date)"="all",
           "MSEs including climate change as a driver"="CC"),
         width='400px'),
        textOutput("radio"),
      h4("Submit a Review"),
      p("To build a more complete MSE repository we encourage you to submit reviews of any MSEs that have not yet been reviewed. To do so click on
        the “Submit MSE - Data Entry Form” tab above."),
      hr(),
      h4("Where have management strategy evaluations occurred?"),
      p("Hovering over a point on the map will give the associated citation below, while clicking and dragging to select an area will give all
        citations in the “brushed” map area. To learn more about those MSEs you can search for the citations in the “Data - All” or the
        “Data - Summary” tabs."),
      plotOutput("mse.map",
        brush = brushOpts(id = "map_brush"),
        hover = hoverOpts("map_hover")
      ),
      p("Figure 2. Map of MSE locations. Points represent the approximate center point of the fishery or management region evaluated in the MSE."),
      fluidRow(
        column(width = 6,
          h5("Hovered points"),
          tableOutput("hover")
        ),
        column(width = 6,
          h5("Brushed points"),
          tableOutput("brush")
        )
      ),
      hr(),
      h4("Comments and Feedback"),
      p("We welcome feedback and suggestions to improve this Shiny application and other comments. You can find contact information here:"),
      tags$a(href="https://drjonathancummings.com/", "drjonathancummings.com")
    ),
    #### Tab 2: Results - Figures ####
    tabPanel("Results - Figures", 
             h2("MSE Review Results - Figures"),
             p("The figures in the first section of this tab show what percentage of the components of a decision process were included in an MSE's documentation,  and who
               participated and at particular stages in the decision process. The second section displays who authors MESs and where they are 
               published. The third section displays how publications have changed through time."),
             hr(),
             h3("What components are documented, who participates, and how?"),
             h4("Are the components of MSE decision processes explicit?"),
             p("For the set of MSEs selected, this figure displays the percentage of those MSEs that explicitly completed and documented the
               decision making components (top), and elements of a decision process (bottom)."),
             plotOutput("Freq.plot",height="100%"),
             p("Figure 3. Percentage of MSEs that included: ", 
              strong("Problem")," - Explicit documentation of the decision problem the MSE is attempting to address,",
              strong("Objectives")," - Explicit documentation of the process used to produce the objectives and performance metrics to evaluate the 
              management strategies,",
              strong("Alternatives")," - Explicit documentation of the process used to produce the alternative management strategies,",
              strong("Consequences")," - Explicit documentation of the process used to predict management strategy performance,",
              strong("Tradeoffs")," - Explicit documentation of the tradeoff evaluation process,",
              strong("Decision")," - Documentation of the alternative selected and implemented as the management strategy going forward,",
              strong("Adopted*")," - Explicit documentation of decision makers implementing the results of the MSE,",
              strong("Process")," - Explicit documentation of the decision making process used to conduct the MSE,",
              strong("Roles")," - Explicit documentation of the MSE participants' roles, and",
              strong("Open Meetings")," - Meetings open to the public or those outside of the set of MSE participants."),
             p(em("*note that the results of an MSE may have been adopted but not documented because the adoption occurred following
                  completing of the associated literature")),
             h4("Who is involved, and participates in, MSEs?"),
             p("For the set of MSEs selected, this figure displays the percentage of those MSEs that included each type of participant for different
               stages in the process."),
             plotOutput("part.plot",height="100%"),
             p("Figure 4. ", strong("Participants")," - Who participated at any stage of the MSE process?,",
               strong("Process")," - Who initiated and lead the MSE process?,",
               strong("Documented Objectives")," - Who provided the objectives based on the documentation of the MSE process?,",
               strong("Subjective Objectives")," - Who most likely provided the objectives based on a subjective reading of the documentation?,",
               strong("Documented Alternatives")," - Who provided the alternatives based on the documentation of the MSE process?, and ",
               strong("Subjective Alternatives")," - Who most likely provided the alternatives based on a subjective reading of the documentation? 
               The x axis displays the percentage of MSEs in the set selected that include each participant type and the y-axis displays the 
               participant types. The unknown participant type represents MSEs where the documentation was inexplicit."),
             hr(),
             h3("Who authors MSEs and where are they published?"),
             plotOutput("pub.plot",height="100%"),
             hr(),
             h3("How has MSE publication changed through time?"),
             plotOutput("year.plot",height="100%")
             ),
    
    #### Tab 3: Results - Tables ####
    tabPanel("Results - Tables", 
             h2("MSE Review Results - Tables"),
             p("The tables in this tab shows how often a participant type contributes to MSEs and what was accounted for
               in the predictive models, as well as what and how objectives were considered, and what type of management
               strategies were considered in MSEs."),
             hr(),
             h4("How often does a group participate in various stages of an MSE process?"),
             textOutput("radio2"),
             tableOutput("MSE.part"),
             h4("What drivers of the system dynamics were included when predicting future conditions?"),
             p("Table 2. Number and percentage of MSEs that included the following drivers in the system model"),
             tableOutput("MSE.drive"),
             hr(),
             h3("What objectives were considered?"),
             h4("How often did MSEs consider each objective category?"),
             p("Table 3. Number and percentage of MSEs that included the following objective categories"),
             tableOutput("MSE.objcat"),
             h4("How were objectives defined?"),
             p("Table 4. Number and percentage of objectives, as well as the number per MSE, by objective classification"),
             tableOutput("MSE.obj"),
             p(strong(em("Definitions"))),
             p(
              strong("Fundamental"), "- A primary objective of the management strategy",
              strong("Means"), "- A means to achieving a fundamental objective of the management strategy",
              strong("Constraint"), "- An objective that is either achieved or not, a binary metric",
              strong("Maximize"), "- An objective that it better achieved as the value of the metric increases",
              strong("Minimize"), "- An objective that it better achieved as the value of the metric decreases",
              strong("Target"), "- An objective that it better achieved as the value of the metric approaches a target value",
              strong("Utility"), "- A single metric that measures multiple objectives",
              strong("Natural"), "- The metric measures the objective directly",
              strong("Proxy"), "- The metric is highly correlated with the objective, but does not directly measure the objective",
              strong("Constructred"), "- The metric formulated in place of a more direct natural or proxy measurement of the objective"
              ),
             hr(),
             h3("What alternative management strategies were evaluated?"),
             p("Table 5. Number and percentage of MSEs that evaluated each type of management strategy"),
             tableOutput("MSE.alt"),
             ),
    #### Tab 4: Data - Summaries ####
    tabPanel("Data - Summaries",
             h2("MSE Review Data - Data Summaries"),
             p("The data tables in this tab summarize publication information, what systems the MSEs evaluated, and how the MSE problem was defined and
             evaluated."),
             h3("Publication Information"),
             h4("What was published, by whom, and where?"),
             DT::dataTableOutput("MSE.pub"),
             h3("Study System"),
             h4("What location, system and species did the MSE evaluate?"),
             DT::dataTableOutput("MSE.summary"),
             hr(),
             h3("Problem Components"),
             h4("How were problems defined, consequences predicted, tradeoffs evaluated, and what was decided?"),
             DT::dataTableOutput("MSE.problem")
    ),
    #### Tab 5: Data - All ####
    tabPanel("Data - All",
             h2("MSE Review Data - All Data Summaries"),
             p("The data tables in this tab contain the complete data tables for each MSE. The objective and alternative tables contain
             multiple rows for each MSE, 1 row per objective or alternative type included in the MSE."),
             hr(),
             h3("Study data"),
             p("This table contains data on each MSE reviewed."),
             pickerInput("select_MSE_rows", "Select columns to display", names(data), selected=names(names.data.tab5),
                         options = list(`actions-box` = TRUE),multiple = T),
             # selectInput("select_MSE_rows", "Select columns to display", names(data),selected =  multiple = TRUE),
             reactableOutput("MSE.Table"),
             # DT::dataTableOutput("MSE.Table"),
             p(class = 'text-center', downloadButton('MSE.Table.active', 'Download Filtered MSE Data')),
             hr(),
             h3("Objectives data"),
             h4("What objectives were evaluated?"),
             p("This table contains data on what objectives were evaluated and how those objectives were measured and
             defined."),
             DT::dataTableOutput("MSE.Obj.Table"),
             p(class = 'text-center', downloadButton('MSE.Obj.active', 'Download Filtered Objectives Data')),
             hr(),
             h3("Alternatives data"),
             h4("What alternative management strategies were evaluated?"),
             p("This table contains data on what types of management strategies were evaluated and some details about
               what those strategies were."),
             DT::dataTableOutput("MSE.Alt.Table"),
             p(class = 'text-center', downloadButton('MSE.Alt.active', 'Download Filtered Alternatives Data')),
             hr(),
             h3("Field Descriptions"),
             h4("How was each data field (column) defined?"),
             p("This table contains the meta data for this analysis, while the name, category, type, and description of
               each field in the MSE review data tables."),
             tableOutput("MSE.Fields")
             ),
    #### Tab 6: Submit MSE - Data Entry Form ####
    tabPanel("Submit MSE - Data Entry Form",
             # Application title
             titlePanel("Management Strategy Evaluation review data entry form"),
             p("To submit a new MSE review enter the data from your review in the form below."),
             hr(),
             p("While we will review your submission prior to adding it to the database and this application please do your best to
             complete all portions of the form. Hovering over a field in the form will bring up a tool tip with more detail about what
             the field is asking for."),
             
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
                             textAreaInput("decision","Decision Result?","MSE is a valuable tool for natural resource management."),
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
                             p(em("Objectives: Individual Objective Entry.")),
                             p("Complete this portion of the form for each objective considered, adding each to the table by clicking the
                               blue button below")),
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
                             p(em("Alternatives: Individual Alternative Type Entry")),
                             p("Complete this portion of the form for each type of management tool considered, describing the alternatives  
                             for each, adding them to the table by clicking the blue button below")),
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
  
  obj.data.selected<-reactive({
    semi_join(obj.data,data_reviewed(),by=c("Citation"="Citation"))
  })
  alt.data.selected<-reactive({
    semi_join(alt.data,data_reviewed(),by=c("Citation"="Citation"))
  })
  
  # Select publication summary data_reviewed
  pub.data<-reactive({data_reviewed() %>%
      select(all_of(pub.col))
  })
  
  # Select study summary data_reviewed
  summary.data<-reactive({data_reviewed() %>%
    select(all_of(summary.col))
  })

  # Select study problem and driver data_reviewed
  prob.data<-reactive({data_reviewed() %>%
    select(all_of(prob.col)) %>% 
    rename("Problem Definition"="ProblemDefinition",
           "Prediction Method"="ConsequencePrediction",
           "Explicit Tradeoff Evaluation Method"="TradeOffMethod_Exp",
           "Subjective Tradeoff Evaluation Method"="TradeOffMethod_Sub",)
  })
  
  # Frequency of method
  n_mse<-reactive({nrow(data_reviewed())})
  freq.data<-reactive({data_reviewed() %>%
    select(all_of(freq.col)) %>%
    rename("Process"="ProcessExplicit",
           "Problem"="ProblemDefinitionExplicit",
           "Objectives"="ObjectivesExplicit",
           "Alternatives"="AlternativesExplicit",
           "Consequences"="ConsequenceExplicit",
           "Tradeoffs"="TradeOffsExplicit",
           "Decision"="DecisionExplicit",
           "Roles"="RoleSpecification",
           "Open Meetings"="OpenMeetings",
           "Adopted"="ResultsAdopted") %>%
    summarise_all(sum,na.rm=T) %>%
    gather(Explicit) %>%
    mutate(Percent=value/n_mse()*100) %>%
    mutate(Explicit=factor(Explicit,levels=
                             c("Process","Problem","Objectives","Alternatives","Consequences","Tradeoffs",
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
    neworder <- c("Participants","Process","Doc Objectives",
                  "Sub Objectives","Doc Alternatives",
                  "Sub Alternatives")
    newlabels <- c("Participants","Process","Documented Objectives",
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
    select(Stage,Participants,Number) %>%
    rename("Participant Group"="Participants") %>%
    spread(Stage,Number,fill=0) %>% 
    mutate("Provided Objectives"=`Documented Objectives`+`Subjective Objectives`) %>% 
    mutate("Provided Alternatives"=`Documented Alternatives`+`Subjective Alternatives`) %>% 
    select(`Participant Group`,Process,Participants,`Provided Objectives`,`Provided Alternatives`) %>%
    mutate(total=Process+Participants+`Provided Objectives`+`Provided Alternatives`) %>% 
    arrange(-total) %>% 
    select(`Participant Group`,Process,Participants,`Provided Objectives`,`Provided Alternatives`) %>%
    rename("Participated in the Process"="Participants",
           "Led the Process"="Process")
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
    arrange(desc(Number))
  })

  # Common components

  # Where MSEs have occurred
  map.data<-reactive({data_reviewed() %>%
    select(all_of(map.col))
  })
  
  # When MSEs have been published
  year.data<-reactive({data_reviewed() %>% 
    select(YearPub) %>% 
    bind_rows(study,.id="set") %>% 
    select(set,YearPub) %>%
    mutate(set = replace(set, set == 1, "Selected")) %>%
    mutate(set = replace(set, set == 2, "All"))
  })
  
  # Where MSEs have been published
  pub.data2<-reactive({data_reviewed() %>% 
      select(Journal) %>% 
      bind_rows(study,.id="set") %>% 
      select(set,Journal) %>%
      mutate(set = replace(set, set == 1, "Selected")) %>%
      mutate(set = replace(set, set == 2, "All"))
  })

  ##### Tab 1: About #####
  output$MSEcounts <- renderTable({
    tibble("MSE type"=c("Published","Random Sample","Climate Change"),
           "Count"=c(154,30,16))
  },digits=0)
  output$radio <-renderText(paste0("Number of MSEs in result: ", nrow(data_reviewed())))
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
                   select(Citation) %>% 
                   arrange(Citation)
               })
  )

  ##### Tab 2: Results - figures #####
  output$Freq.plot <- renderPlot({
    ggplot(freq.data(),aes(Explicit,Percent))+
      geom_col()+geom_vline(xintercept=3.51,linetype="dashed")+
      annotate("label",x = 2, y=75,label="Decision\nProcess",size=6)+
      annotate("label",x = 7, y=75,label="Decision\nComponents",size=6)+
      coord_flip()+scale_y_continuous(limits=c(0,100))+xlab(NULL)+ylab("Percentage")+
      scale_x_discrete(
          limits=c("Open Meetings","Roles","Process","Adopted","Decision","Tradeoffs","Consequences","Alternatives",
                   "Objectives","Problem"), 
          labels=c("Open Meetings","Roles","Process","Adopted","Decision","Tradeoffs","Consequences","Alternatives",
                   "Objectives","Problem")) +
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
  output$pub.plot <- renderPlot({
    ggplot(pub.data2(),x=reorder(Journal, n),y=n,color=set,fill=set)+
    geom_col()+scale_color_grey()+scale_fill_manual(values=c("gray35","gray50"))+
    theme_bw()+labs(col="MSEs",fill="MSEs")+coord_flip()+
    theme(legend.position = c(0.75, 0.125),axis.title.y=element_blank())+labs(y="Publication Count")+
    theme(text = element_text(size=18),legend.text = element_text(size=18))
  },height=600)
  output$year.plot <- renderPlot({
    ggplot(year.data(),aes(x=YearPub,color=set,fill=set))+
    geom_histogram(stat="count",position = "identity")+scale_color_grey()+scale_fill_manual(values=c("gray35","gray50"))+
    theme_bw()+labs(col="MSEs",fill="MSEs",x="Publication Year")+theme(legend.position = c(0.1, 0.85))+
    theme(text = element_text(size=18),legend.text = element_text(size=18))
  },height=600)
  
  ##### Tab 3: Results - tables #####
  # Table 1. participation rate by group
  output$radio2 <-renderText(paste0("Table 1. Number of MSEs (out of ", nrow(data_reviewed()),
                                                   ") in which each participant group:"))
  output$MSE.part <- renderTable({
    part.data_table()
  },digits=0)
  # Table 2. System Drivers
  output$MSE.drive <- renderTable({
    drive.data()
  },digits=0)
  # Table 3. objective Categories
  output$MSE.objcat <- renderTable({
    objcat.data()
  },digits=0)
  # Table 4. Objective Definitions
  output$MSE.obj <- renderTable({
    obj.data_table1()
  })
  # Table 5. Management Procedures
  output$MSE.alt <- renderTable({
    altcat.data()
  })
  
  ##### Tab 4: Data - Summaries #####
  output$MSE.pub <- DT::renderDataTable({
    arrange(pub.data(),Citation)
  })
  output$MSE.summary <- DT::renderDataTable({
    arrange(summary.data(),Citation)
  }, filter="top")
  output$MSE.problem <- DT::renderDataTable({
    arrange(prob.data(),Citation)
  }, options = list(autoWidth = TRUE,
                    columnDefs = list(list(width = '800px', targets = targetsPD)), # comments, Problem Definition
                    scrollX=TRUE),
  filter="top"
  )
  ##### Tab 5: Data - All #####
  output$MSE.Table <- renderReactable({
    reactable(data_reviewed()[,input$select_MSE_rows,drop=FALSE],
              searchable = TRUE, filterable = TRUE, resizable = TRUE, showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 25),
              striped = TRUE, showSortable = TRUE, onClick = "expand", highlight = TRUE,
              columns = list(Citation = colDef(
                  style = sticky_style,
                  headerStyle = sticky_style)
                  ))
  })
  # output$MSE.Table <- renderDT({
  #   columns = names(data_reviewed())
  #   if (!is.null(input$select_MSE_rows)) {
  #     columns = input$select_MSE_rows
  #   }
  #   data_reviewed()[,columns,drop=FALSE]},
  #   filter="top"
  # )
  output$MSE.Table.active = downloadHandler('MSEdata.csv', content = function(file) {
    s = input$MSE.Table_rows_all
    columns = names(data_reviewed())
    if (!is.null(input$select_MSE_rows)) {
      columns = input$select_MSE_rows
    }
    write.csv(study[s, columns, drop = FALSE], file)
  })
  output$MSE.Obj.Table <- renderDataTable({
    obj.data.selected()
  })
  output$MSE.Obj.active = downloadHandler('MSE_Objectives.csv', content = function(file) {
    s = input$MSE.Obj.Table_rows_all
    write.csv(obj.data[s, , drop = FALSE], file)
  })
  output$MSE.Alt.Table <- renderDataTable({
    alt.data.selected()
  })
  output$MSE.Alt.active = downloadHandler('MSE_Alternatives.csv', content = function(file) {
    s = input$MSE.Alt.Table_rows_all
    write.csv(alt.data[s, , drop = FALSE], file)
  })
  output$MSE.Fields <- renderTable({
    mse.fields<-arrange(fields,Order) %>% 
      select(-Order)
    mse.fields
  })

#   output$imageSDM<-renderImage({
#     filename<-normalizePath(file.path('./www',paste("SDMProcessFramework.png")))
#     list(src=filename,
#          width=400,
#          height=275)},deleteFile = FALSE)

  ##### Tab 6: Submit MSE - Data Entry Form#####
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