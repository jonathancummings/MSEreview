#### Save and load data from R environment ####
# saveData <- function(data) {
#   data <- as.data.frame(t(data))
#   if (exists("responses")) {
#     responses <<- rbind(responses, data)
#   } else {
#     responses <<- data
#   }
# }
# 
# loadData <- function() {
#   if (exists("responses")) {
#     responses
#   }
# }

#### Save and load data from local drive ####
# outputDir <- "responses"

# saveData <- function(data) {
#   data <- t(data)
#   # Create a unique file name
#   fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
#   # Write the file to the local system
#   write.csv(
#     x = data,
#     file = file.path(outputDir, fileName), 
#     row.names = FALSE, quote = TRUE
#   )
# }
# 
# loadData <- function() {
#   # Read all the files into a list
#   files <- list.files(outputDir, full.names = TRUE)
#   data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
#   # Concatenate all data together into one data.frame
#   data <- do.call(rbind, data)
#   data
# }

#### Save and load data from SQLite (which comes with shinyapp.io!) ####
# library(RSQLite)
# sqlitePath <- "/path/to/sqlite/database" #note you need to have SQLite located on your machine
# table <- "responses"
# 
# saveData <- function(data) {
#   # Connect to the database
#   db <- dbConnect(SQLite(), sqlitePath)
#   # Construct the update query by looping over the data fields
#   query <- sprintf(
#     "INSERT INTO %s (%s) VALUES ('%s')",
#     table, 
#     paste(names(data), collapse = ", "),
#     paste(data, collapse = "', '")
#   )
#   # Submit the update query and disconnect
#   dbGetQuery(db, query)
#   dbDisconnect(db)
# }
# 
# loadData <- function() {
#   # Connect to the database
#   db <- dbConnect(SQLite(), sqlitePath)
#   # Construct the fetching query
#   query <- sprintf("SELECT * FROM %s", table)
#   # Submit the fetch query and disconnect
#   data <- dbGetQuery(db, query)
#   dbDisconnect(db)
#   data
# }

#### Save and load data from MySQL ####
library(RMySQL)
library(shiny)
library(tidyverse)

# Define the fields we want to save from the form
fields <- c("name", "used_shiny", "r_num_years")

options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root", # set to correct username for database
  "password" = "tLenKdQCs9M2pqK" # set to correct password for database
))
databaseName <- "MSEreview" #set to correct database name
table <- "responses"

saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

logicToBit<-function(input){
  if(input==TRUE){
    return(1)
  }
  else if(input==FALSE){
    return(0) 
  }
  else{
    return(input)
  }
}

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    DT::dataTableOutput("responses", width = 300), tags$hr(),
    textInput("name", "Name", ""),
    checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
    sliderInput("r_num_years", "Number of years using R",
                0, 25, 2, ticks = FALSE),
    actionButton("submit", "Submit")
  ),
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
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
  }
)

responses<-data.frame(name=character(), used_shiny=character(),r_num_years=numeric())
data <- dbWriteTable(con, "responses", responses,overwrite=TRUE)

