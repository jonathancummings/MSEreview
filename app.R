# MSE Problem Framing 
# Literature Review Data Analysis
# 3/8/2019, JWC

##### Meta Data and setup #####
# Description: Import and analyze MSE documentation from database
# Value: TBD

# load libraries
library(tidyverse) # upgrade to base R
library(readxl) # read in excel files
library(shiny) # Shiny App
library(shinythemes) # web themes for Shiny
library(DT) # Data Table
library(purrr) # for loop alternative
library(ggmap) # map plotting
library(mapdata) # basemap generation
library(odbc) # database connection 
library(RMySQL) # MySQL scripting in R
library(DBI) # database interface

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

load("MSEreview.RData")


# Get map background for plotting the map
world <- borders("world", colour="gray50", fill="gray50", alpha=0.75) # create a layer of borders


##### Data Processing #####

# Edit tables for joining
study<-study %>% 
  unite(Citation,c(Authors,YearPub),sep=" ",remove=F)

study.join<-study %>% 
  select(ID,Citation)

mgmt.join<-mgmt %>% 
  group_by(fkStudyID) %>%
  summarise(ManagementType = toString(sort(unique(ManagementType))),
            AlternativesEvaluated = toString(sort(unique(AlternativesEvaluated))))

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
data_pub<-filter(data,IncludeInPublication==TRUE)
data_CC<-filter(data,str_detect(Drivers,"Climate Change"))

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
            "RolesExplicit",
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

ui <- fluidPage(
  theme = shinytheme("readable"),
  tabsetPanel(
    #### Tab 1: About ####
    tabPanel("About", 
      h1("An Assessment of Management Strategy Evaluations"),# title of the page
      hr(),
      p("The calls for fisheries management to utilize management strategy
      evaluation are both aspirational and estimable in their aim and vision.
      Management strategy evaluation (MSE) is 'widely considered to
      be the most appropriate way to evaluate the trade‐offs achieved by
      alternative management strategies and to assess the consequences of
      uncertainty for achieving management goals' (Punt et al. 2014). Thus,
      MSE is a compelling tool to assess fisheries management procedures.  For
      example, in our changing oceans and increasing climate change impacts MSE
      can evaluate climate-ready options for fisheries management decisions."),
      p("Adaptive management arose to address uncertainties and accelerate 
      progress towards meeting management objectives. In the case of fisheries
      management, are scientific publications of management strategy evaluations
      supporting adaptive management?"),
      h4("Objective"),
      p("Our objective is to review MSE methods and their documentation to 
      determine how well MSE documentation supports learning within the MSE
      practitioner community and to provide a location to facilitate learning
      from previous MSE. We used the structured decision making (SDM) process 
      - the decision making framework in which adaptive management occurs - 
      as our review framework (Figure 1) to assess:"),
      p("How do published MSE projects utilize standard decision making 
      components and support learning within the MSE practitioner community?"),
      imageOutput("imageSDM",
                   width=400,
                   height=275),
      p(em("Figure 1. Structured Decision Making process used as review framework")),
      h4("Methods"),
      p("We searched the MSE literature via Web of Science, searching for 
      “management strategy evaluation” by topic across all years on January
      8th, 2019. This search returned 264 results, of which 154 were management
      strategy evaluations after removing articles that were reviews, 
      meta-analyses, or simply cited other MSE articles. We reviewed a random
      sample of 30 of these 154 articles."),
      h4("Explore the Results"),
      p("Select the results you would like to examine using the radio buttons
      below.  The figures and tables in this application are updated by your
      selection."),
      fluidRow(
       column(width = 4,
        tableOutput("MSEcounts")
       ),
       column(width = 8,
        radioButtons("data_filter",
         "Show results from:",
         choices = c(
           "30 articles reviewed for Cummings et. al. Publication, "="pub",
           "MSE included climate change as a driver"="CC",
           "All MSEs (entered in the database to date)"="all"),
         width='400px'),
        textOutput("radio")
       )
      ),
      hr(),
      h5("Where have management strategy evaluations occurred?"),
      plotOutput("mse.map",
        brush = brushOpts(id = "map_brush"),
        hover = hoverOpts("map_hover")
      ),
      p("Figure 2. Map of MSE locations. Points represent the approximate 
        center point of the fishery the MSE evaluated."),
      p("Hovering over a point on the map will give the associated citation below, 
        while selecting an area will give all citations in the 'brushed' map area."),
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
             h1("An Assessment of Management Strategy Evaluations"),# title of the page
             h2("MSE literature review results - Figures"),
             hr(),
             h5("Are structured decision making steps explicit in MSEs?"),
             plotOutput("Freq.plot",height="100%"),
             p("Figure 3. Percentage of MSEs that included: "),
             p(strong("Process")," - Explicit documentation of the decision making process used to conduct the MSE,",
              strong("Problem")," - Explicit documentation of the problem the MSE is attempting to address,",
              strong("Objectives")," - Explicit documentation of the process used to produce objectives and performance metrics to evaluate the management procedures,",
              strong("Tradeoffs")," - Explicit documentation of the tradeoff evaluation process,",
              strong("Decision")," - Documentation of the alternative selected and implemented as the management procedure going forward,",
              strong("Roles")," - Explicit documentation of the MSE participants roles,",
              strong("Open Meetings")," - Meetings open to the public or those outside of the MSE participants,",
              strong("Adopted")," - Explicit documentation of decision makers implementing the results of the MSE."),
             h5("Who is involved, and participates in, MSEs?"),
             plotOutput("part.plot",height="100%"),
             p("Figure 4. Participation at different stages of the MSE process")
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
    )
  )
)

#####
# Shiny Server Section
server <- function(input, output, session) {   # code to create output using render
  #####
  ###-- Data analysis --###
  # Filter data to include only those reviewed for the publication or all MSEs
  data_reviewed<-reactive({
    if(input$data_filter=="pub"){
      filter(data,IncludeInPublication==TRUE)
    } 
    else 
      if(input$data_filter=="CC"){
        filter(data,str_detect(Drivers,"Climate Change"))
      }
    else 
      if(input$data_filter=="all"){
        filter(data,IncludeInPublication==TRUE|IncludeInPublication==FALSE)
    }
  })
  
  # Select study summary data_reviewed
  summary.data<-reactive({data_reviewed() %>%
    select(summary.col)
  })

  # Select study problem and driver data_reviewed
  prob.data<-reactive({data_reviewed() %>%
    select(prob.col)
  })
  
  # Frequency of method
  n_mse<-reactive({nrow(data_reviewed())})
  freq.data<-reactive({data_reviewed() %>%
    select(freq.col) %>%
    rename("Process"="ProcessExplicit",
           "Problem"="ProblemDefinitionExplicit",
           "Objectives"="ObjectivesExplicit",
           "Alternatives"="AlternativesExplicit",
           "Tradeoffs"="TradeOffsExplicit",
           "Decision"="DecisionExplicit",
           "Roles"="RolesExplicit",
           "Open Meetings"="OpenMeetings",
           "Adopted"="ResultsAdopted") %>%
    summarise_all(funs(sum)) %>%
    gather(Explicit) %>%
    mutate(Percent=value/n_mse()*100) %>%
    mutate(Explicit=factor(Explicit,levels=
                             c("Process","Problem","Objectives","Alternatives","Tradeoffs",
                               "Decision","Roles","Open Meetings","Adopted"))) %>%
    rename("Number"="value")
    })

  # Who participates
  part.data_reviewed<-reactive({data_reviewed() %>%
    select(part.col) %>%
      rename("Process"="Leader",
             "Doc Objectives"="ObjElicitationSource_Exp",
             "Doc Alternatives"="ProcedureElicitation_Exp",
             "Sub Objectives"="ObjElicitationSource_Sub",
             "Sub Alternatives"="ProcedureElicitation_Sub")
  })
  part.data_reviewed2<- reactive({part.data_reviewed() %>%
    purrr::map(~ strsplit(as.character(.),split=",")) %>%
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
    purrr::map(~ strsplit(as.character(.),split=",")) %>%
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
    select(obj.col) %>%
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
      select(obj.col) %>%
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
    purrr::map(~ strsplit(as.character(.),split=",")) %>% 
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
    select(map.col)
  })

  # Tab 1 - Filtering
  # To test if the reactive works
  output$radio <-renderText(paste0("Number of MSEs in results: ", nrow(data_reviewed())))
  output$mse.map <- renderPlot({
    # plot MSEs on map
    ggplot(data=map.data(),aes(x=Longitude, y=Latitude)) + world +
      geom_point(color="red",size=1.5)
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

  # Tab 2 - Results - plots
  output$Freq.plot <- renderPlot({
    ggplot(freq.data(),aes(Explicit,Percent))+
      geom_col()+geom_vline(xintercept=3.5,linetype="dashed")+
      coord_flip()+scale_y_continuous(limits=c(0,100))+xlab(NULL)+
      scale_x_discrete(
          limits=c("Adopted","Open Meetings","Roles","Decision","Tradeoffs",
                   "Objectives","Problem","Process"), 
          labels=c("Adopted","Open Meetings","Roles","Decision","Tradeoffs",
                   "Objectives","Problem","Process")) +
      theme(text = element_text(size=18))
  },height=300,width=600)
  output$part.plot <- renderPlot({
    ggplot(part.data_reviewed5(),aes(x=order,y=Percent)) +
      facet_wrap(~Stage,scale="free",ncol=2) + geom_col(width=0.8) +  
      scale_x_continuous(
        breaks = part.data_reviewed5()$order,
        labels = part.data_reviewed5()$Participants,
        expand = c(0,0)
      )+scale_y_continuous(limits=c(0,100)) +
      ylab("Percent")+coord_flip()+xlab(NULL) +
      theme(text = element_text(size=18))
  },height=600)
  # Tab 3 - Results - tables
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
  # Tab 4
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
  # Tab 5
  output$MSE.summary <- DT::renderDataTable({
    arrange(summary.data(),Citation)
  })
  output$MSE.problem <- DT::renderDataTable({
    arrange(prob.data(),Citation)
  }, options = list(autoWidth = TRUE,
                    columnDefs = list(list(width = '800px', targets = targetsPD)), # comments, ProblemDefinition
                    scrollX=TRUE)
  )
  # Tab 6
  output$imageSDM<-renderImage({
    filename<-normalizePath(file.path('./www',paste("SDMProcessFramework.png")))
    list(src=filename,
         width=400,
         height=275)},deleteFile = FALSE)
  output$MSEcounts <- renderTable({
    tibble("MSE type"=c("Published","Random Sample","Climate Change"),
               "Count"=c(154,30,11))
},digits=0)
}

shinyApp(ui, server)
