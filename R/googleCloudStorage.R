library(googleCloudStorageR)

Sys.setenv("GCS_CLIENT_ID" = "mykey",
           "GCS_CLIENT_SECRET" = "mysecretkey",
           "GCS_WEB_CLIENT_ID" = "my-shiny-key",
           "GCS_WEB_CLIENT_SECRET" = "my-shiny-secret-key",
           "GCS_DEFAULT_BUCKET" = "my-default-bucket",
           "GCS_AUTH_FILE" = "/Users/jcummings/OneDrive/Documents/Work/MSE Review-a6a1ba69ce12.json")


gcs_get_global_bucket()
gcs_list_buckets()
gcs_list_objects()
gcs_auth()
proj <- "MSE Review"
proj <-"alpine-tracker-230222"

library(odbc)
library(RMySQL)

#To open a connection to the database:
# Copy into command prompt:
## cloud_sql_proxy -instances=alpine-tracker-230222:us-east1:mse-review=tcp:3306

# Create connection function
getSqlConnection <- function(){
  cn <-
    dbConnect(
      RMySQL::MySQL(),
      dbname = 'MSEreview',
      host = '127.0.0.1',
      port=3306,
      username = 'root',
      password = 'tLenKdQCs9M2pqK')
  return(cn)
}
# Open connection
con <- getSqlConnection()

res <- dbListTables(con)

dbReadTable(con, "tblTesting")

dbSendQuery(con, "DELETE FROM tblStudyFields")
print(res)

data <- dbWriteTable(con, "iris", iris)

dbWriteTable(conn, "mtcars", mtcars[1:5, ])
dbReadTable(conn, "mtcars")

dbWriteTable(conn, "mtcars", mtcars[6:10, ], append = TRUE)
dbReadTable(conn, "mtcars")

dbWriteTable(conn, "mtcars", mtcars[1:11, ], overwrite = TRUE)
dbReadTable(conn, "mtcars")

# No row names
dbWriteTable(conn, "mtcars", mtcars[1:10, ], overwrite = TRUE, row.names = FALSE)
dbReadTable(conn, "mtcars")

dbRemoveTable(con, "mtcars")

result <- dbSendQuery(con, "SELECT flight, tailnum, origin FROM flights ORDER BY origin")

# Retrieve the first 100 results
first_100 <- dbFetch(result, n = 100)

# Retrieve the rest of the results
rest <- dbFetch(result)

dbListTables(con)

# List tables beginning with f
dbListTables(con, table_name = "f%")

# List all fields in the 'flights' database
dbListFields(con, "flights")

dbDisconnect(con)
