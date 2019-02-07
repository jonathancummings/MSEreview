# MSE Problem Framing 
# Literature Review Data Analysis
# 1/4/2018, JWC

##### 
# Meta Data and setup
# Description: Import and analyze MSE documentation from database
# Value: TBD

# load libraries
library(tidyverse) # upgrade to base R
library(RODBC) # connect to databases
library(shiny) # Shiny App
library(shinythemes) # web themes for Shiny
library(DT) # Data Table
library(purrr) # for loop alternative
library(ggmap) # map plotting
library(mapdata) # basemap generation

##### 
# Script

#####
###-- Data Processing --###
wd<-getwd()
# Establish connection to Access database
channel<-odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                           DBQ=",wd,"/DB files - Access/MSE Problem Framing_be.accdb"))

# Obtain data tables
study<-as_tibble(sqlFetch(channel,sqtable = "tblStudy"))
mgmt<-as_tibble(sqlFetch(channel,sqtable = "tblStudyManagementTools"))
obj<-as_tibble(sqlFetch(channel,sqtable = "tblStudyObjectives"))
fields<-as_tibble(sqlFetch(channel,sqtable = "tblStudyFields"))
odbcCloseAll()

#####
# Edit tables for joining
study<-study %>% 
  unite(Citation,c(Authors,YearPub),sep=" ",remove=F)

study.join<-study %>% 
  select(ID,Citation)

mgmt.join<-mgmt %>%
  group_by(fkStudyID) %>%
  summarise(ManagementTool = toString(sort(unique(MPManagementTool))),
            AlternativesEvaluated = toString(sort(unique(MPAlternativesEvaluated))))

obj.join<-obj %>% 
  group_by(fkStudyID) %>%
  summarise(ObjectiveCategories = toString(sort(unique(ObjCategory))))

# Join tables
data<-full_join(study,mgmt.join,by=c("ID"="fkStudyID"))
data<-full_join(data,obj.join,by=c("ID"="fkStudyID")) %>% 
  select(-c("ID"))
obj.data<-right_join(study.join,obj,by=c("ID"="fkStudyID"))%>% 
  select(-c("ID","ID.y"))

# Move comment column to the end
data<-data %>% 
  select(-'Comments', 'Comments')

# Get columns whose width needs editing
targets<-match(c("FullCitation","Comments"),names(data))

# Get columns for study summary
summary.col<-c("Citation",
               "Authors",
               "YearPub",
               "Species",
               "Location",
               "System")
# Get columns for study drivers and problem
prob.col<-c("Citation",
            "Drivers",
            "ProblemDefinition",
            "Decision")
# Get columns for frequency analysis
freq.col<-c("ProcessExplicit",
            "ProblemDefinitionExplicit",
            "ObjectivesExplicit",
            "TradeOffsExplicit",
            "DecisionExplicit",
            "RoleSpecification",
            "OpenMeetings",
            "ResultsAdopted")
# Get columns for participant analysis
part.col<-c("Leader",
            "Participants",
            "ObjElicitationSource",
            "ProcedureElicitation")
obj.col<-c("ObjType",
           "ObjCategory",
           "ObjDirection",
           "ObjScale")
alt.col<-c("ManagementTool",
           "AlternativesEvaluated")
# Get columns for map
map.col<-c("Latitude",
           "Longitude",
           "Citation")

#####
###-- Data analysis --###
# Select study summary data
summary.data<-data %>% 
  select(summary.col)

# Select study problem and driver data
prob.data<-data %>% 
  select(prob.col)

# Frequency of method
n_mse<-nrow(data)
freq.data<-data %>% 
  select(freq.col) %>% 
  rename("Process"="ProcessExplicit",
         "Problem"="ProblemDefinitionExplicit",
         "Objectives"="ObjectivesExplicit",
         "Tradeoffs"="TradeOffsExplicit",
         "Decision"="DecisionExplicit",
         "Roles"="RoleSpecification",
         "Open Meetings"="OpenMeetings",
         "Adopted"="ResultsAdopted") %>% 
  summarise_all(funs(sum)) %>%
  gather(Explicit) %>% 
  mutate(Percent=value/n_mse*100) %>% 
  mutate(Explicit=factor(Explicit,levels=
                           c("Process","Problem","Objectives","Tradeoffs","Decision",
                             "Roles","Open Meetings","Adopted"))) %>% 
  rename("Number"="value")

# Plot results
Freq.plot<-ggplot(freq.data,aes(Explicit,Percent))+
  geom_col()+geom_vline(xintercept=5.5,linetype="dashed")+
  scale_y_continuous(limits=c(0,100))+xlab(NULL)

# Who participates
part.data<-data %>% 
  select(part.col) %>% 
  rename("Process"="Leader",
         "Objectives"="ObjElicitationSource",
         "Alternatives"="ProcedureElicitation")
part.data2<- part.data %>% 
  purrr::map(~ strsplit(as.character(.),split=";")) %>%
  purrr::map(unlist) %>%
  purrr::map(table)

part.data3<-plyr::ldply(part.data2,data.frame)
part.col2<-names(part.data3)<-c("Stage","Participants","Number")

neworder <- c("Process","Participants","Objectives","Alternatives")
newlabels <- c("Process Leadership","Participants","Objectives","Alternatives")
part.data3 <- part.data3 %>% 
  mutate(Percent=Number/n_mse*100) %>% 
  mutate(Stage=factor(Stage,levels=neworder,labels=newlabels))

part.plot<-ggplot(part.data3,aes(Participants,y=Percent)) +
  facet_wrap("Stage",scale="free") + geom_col() + scale_y_continuous(limits=c(0,100)) + 
  ylab("Percent")

part.data4<-part.data3 %>%
  as_tibble() %>%
  select(Stage,Participants,Number) %>% 
  rename("Participant Group"="Participants") %>% 
  spread(Stage,Number,fill=0)

# What drivers are considered
drive.data<-data %>%
  select(Drivers) %>% 
  purrr::map(~ strsplit(as.character(.),split=";")) %>%
  purrr::map(unlist) %>%
  purrr::map(table) %>% 
  plyr::ldply(data.frame) %>% 
  select(Var1,Freq) %>% 
  rename("Driver"="Var1","Frequency"="Freq") %>% 
  mutate(Percent=Frequency/n_mse*100) %>% 
  arrange(desc(Frequency))

# What Objective types are considered
objcat.data<-data %>%
  select(ObjectiveCategories) %>% 
  purrr::map(~ strsplit(as.character(.),split=",")) %>%
  purrr::map(unlist) %>%
  purrr::map(table) %>% 
  plyr::ldply(data.frame) %>% 
  select(Var1,Freq) %>% 
  rename("Objective Category"="Var1","Frequency"="Freq") %>% 
  mutate(Percent=Frequency/n_mse*100) %>% 
  arrange(desc(Frequency))

# How were objectives defined
obj.data2<-obj.data %>%
  select(obj.col) %>%
  purrr::map(table)

obj.data3<-plyr::ldply(obj.data2,data.frame)
names(obj.data3)<-c("Objective","Type","Number")

neworder <- c("ObjCategory","ObjType","ObjDirection","ObjScale")
newlabels <- c("Category","Type","Direction","Scale")
obj.data3 <- obj.data3 %>% 
  mutate(Percent=Number/nrow(obj.data)*100) %>%
  mutate('Per MSE'=Number/n_mse) %>% 
  mutate(Objective=factor(Objective,levels=neworder,labels=newlabels))

obj.data4<-obj.data %>%
  select(obj.col) %>%
  group_by(ObjType,ObjCategory,ObjDirection,ObjScale) %>%
  summarize(n())

names(obj.data4)<-c("Type","Category","Direction","Scale","Number")

# What Alternative types are considered
altcat.data<-data %>%
  select(ManagementTool) %>% 
  purrr::map(~ strsplit(as.character(.),split=",")) %>%
  purrr::map(unlist) %>%
  purrr::map(table) %>% 
  plyr::ldply(data.frame) %>% 
  select(Var1,Freq) %>% 
  rename("Management Tool"="Var1","Number"="Freq") %>% 
  mutate(Percent=Number/n_mse*100) %>% 
  mutate('Per MSE'=Number/n_mse) %>%
  arrange(desc(Number))

# Common components

# Where MSEs have occured
map.data<-data %>% 
  select(map.col)

# Get map background
world <- borders("world", colour="gray50", fill="gray50",alpha=0.75) # create a layer of borders
# plot MSEs on map
mse.map <- ggplot(data=map.data,aes(x=Longitude, y=Latitude)) + world +
  geom_point(color="red",size=1.5)


#####
###-- Shiny App --###

ui <- fluidPage(
  theme = shinytheme("readable"),
  tabsetPanel(
    tabPanel("Filter Results", 
             headerPanel("An Assessment of Management Strategy Evaluations"),# title of the page
             h2("MSE literature review results"),
             hr(),
             radioButtons("data_filter",
                          "Show results from:",
                          choices = c("All MSEs"="all",
                                      "MSEs reviewed for Cummings et. al. Publication"="pub")),
             hr(),
             plotOutput("mse.map",
                        brush = brushOpts(id = "map_brush"),
                        hover = hoverOpts("map_hover")
                        ),
             p("Figure 1. Map of MSE locations. Points represent the approximate 
               center point of the fishery the MSE evaluated."),
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
    tabPanel("Results - Plots", 
             headerPanel("An Assessment of Management Strategy Evaluations"),# title of the page
             h2("MSE literature review results"),
             hr(),
             plotOutput("Freq.plot"),
             p("Figure 2. Percentage of MSEs that included: "),
             p("'Decision' - Documentation of the alternative selected and implemented as the management procedure going forward,"),
             p("'Objectives' - Explicit documentation of the process used to produce objectives and performance metrics to evaluate the management procedures,"),
             p("'Open Meetings' - Meetings open to the public or those outside of the MSE participants,"),
             p("'Problem' - Explicit documentation of the problem the MSE is attempting to address,"),
             p("'Process' - Explicit documentation of the decision making process used to conduct the MSE,"),
             p("'Roles' - Explicit documentation of the MSE participants roles,"),
             p("'Tradeoffs' - Explicit documentation of the tradeoff evaluation process."),
             plotOutput("part.plot"),
             p("Figure 3. Participation at different stages of the MSE process")
             ),
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
    tabPanel("Data - All",
             h1("All of the MSE literature review data"),
             h2("Field Descriptions"),
             tableOutput("MSE.Fields"),
             hr(),
             h2("Study data"),
             DT::dataTableOutput("MSE.Table"),
             hr(),
             h2("Objectives data"),
             DT::dataTableOutput("MSE.Obj.Table")
             ),
    tabPanel("Data Tables - Subsets",
             h1("Selected columns of MSE literature review data"),
             h2("Study Summaries"),
             DT::dataTableOutput("MSE.summary"),
             hr(),
             h2("Problem Components"),
             DT::dataTableOutput("MSE.problem")
    )
  )
)

#####
# Shiny Server Section
server <- function(input, output) {   # code to create output using render
  # Tab 1 - Filtering
  output$mse.map <- renderPlot({
    mse.map
  })
  observeEvent(input$map_hover,
               output$hover <- renderTable({
                 nearPoints(data, input$map_hover,"Longitude","Latitude") %>% 
                   select(Citation)
               })
  )
  observeEvent(input$map_brush,
               output$brush <- renderTable({
                 brushedPoints(data, input$map_brush, "Longitude","Latitude")%>% 
                   select(Citation)
               })
  )

  # Tab 2 - Results - plots
  output$Freq.plot <- renderPlot({
    Freq.plot
  })
  output$part.plot <- renderPlot({
    part.plot
  })
  # Tab 3 - Results - tables
  output$MSE.freq <- renderTable({
    freq.data
  })
  output$MSE.part <- renderTable({
    part.data4
  })
  output$MSE.drive <- renderTable({
    drive.data
  })
  output$MSE.objcat <- renderTable({
    objcat.data
  })
  output$MSE.obj <- renderTable({
    obj.data3
  })
  output$MSE.obj2 <- DT::renderDataTable({
    obj.data4
  })
  output$MSE.alt <- renderTable({
    altcat.data
  })
  # Tab 4
  output$MSE.Fields <- renderTable({
    arrange(fields,Order)
  })
  output$MSE.Table <- DT::renderDataTable({
    data
  },  options = list(autoWidth = TRUE,
                     columnDefs = list(list(width = '600px', targets = targets)),
                     scrollX=TRUE
                     )
  )
  output$MSE.Obj.Table <- DT::renderDataTable({
    obj.data
  })
  # Tab 5
  output$MSE.summary <- DT::renderDataTable({
    arrange(summary.data,Citation)
  })
  output$MSE.problem <- DT::renderDataTable({
    arrange(prob.data,Citation)
  })
}

shinyApp(ui, server)
