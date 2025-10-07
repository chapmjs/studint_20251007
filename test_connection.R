# Database Connection Test Script
# Run this before deploying your Shiny app to verify database connectivity

library(DBI)
library(RMySQL)

cat("=== MySQL Connection Test ===\n\n")

# Step 1: Check environment variables
cat("Step 1: Checking environment variables...\n")
host <- Sys.getenv("MYSQL_HOST")
port <- Sys.getenv("MYSQL_PORT")
user <- Sys.getenv("MYSQL_USER")
password <- Sys.getenv("MYSQL_PASSWORD")
database <- Sys.getenv("MYSQL_DATABASE")

if (host == "") {
  stop("ERROR: MYSQL_HOST not set in .Renviron")
}
if (user == "") {
  stop("ERROR: MYSQL_USER not set in .Renviron")
}
if (password == "") {
  stop("ERROR: MYSQL_PASSWORD not set in .Renviron")
}
if (database == "") {
  stop("ERROR: MYSQL_DATABASE not set in .Renviron")
}

cat(sprintf("  Host: %s\n", host))
cat(sprintf("  Port: %s\n", ifelse(port == "", "3306 (default)", port)))
cat(sprintf("  User: %s\n", user))
cat(sprintf("  Password: %s\n", ifelse(password == "", "(empty)", "***set***")))
cat(sprintf("  Database: %s\n", database))
cat("✓ All environment variables are set\n\n")

# Step 2: Attempt connection
cat("Step 2: Attempting to connect to database...\n")
con <- NULL
tryCatch({
  con <- dbConnect(
    MySQL(),
    host = host,
    port = if(port == "") 3306 else as.integer(port),
    user = user,
    password = password,
    dbname = database
  )
  cat("✓ Successfully connected to database!\n\n")
}, error = function(e) {
  cat("✗ Connection FAILED\n")
  cat(sprintf("Error message: %s\n\n", e$message))
  cat("Common issues:\n")
  cat("  1. MySQL server is not running\n")
  cat("  2. Host/port are incorrect\n")
  cat("  3. Username/password are incorrect\n")
  cat("  4. Database doesn't exist\n")
  cat("  5. MySQL server doesn't allow remote connections\n")
  cat("  6. Firewall is blocking the connection\n")
  cat("  7. User doesn't have permission to connect remotely\n\n")
  cat("To allow remote connections:\n")
  cat("  1. In MySQL: GRANT ALL ON *.* TO 'username'@'%' IDENTIFIED BY 'password';\n")
  cat("  2. Edit my.cnf: bind-address = 0.0.0.0\n")
  cat("  3. Restart MySQL service\n")
  stop("Connection test failed")
})

# Step 3: List tables
cat("Step 3: Checking for required tables...\n")
tryCatch({
  tables <- dbListTables(con)
  cat(sprintf("  Found %d table(s): %s\n", length(tables), paste(tables, collapse=", ")))

  if ("students" %in% tables) {
    cat("  ✓ 'students' table exists\n")
  } else {
    cat("  ✗ 'students' table NOT found - you need to run the SQL schema\n")
  }

  if ("interactions" %in% tables) {
    cat("  ✓ 'interactions' table exists\n")
  } else {
    cat("  ✗ 'interactions' table NOT found - you need to run the SQL schema\n")
  }
  cat("\n")
}, error = function(e) {
  cat(sprintf("✗ Error listing tables: %s\n\n", e$message))
})

# Step 4: Test queries
cat("Step 4: Testing basic queries...\n")
if ("students" %in% dbListTables(con)) {
  tryCatch({
    result <- dbGetQuery(con, "SELECT COUNT(*) as count FROM students")
    cat(sprintf("  ✓ Students table: %d record(s)\n", result$count))
  }, error = function(e) {
    cat(sprintf("  ✗ Error querying students: %s\n", e$message))
  })
} else {
  cat("  - Skipping students query (table doesn't exist)\n")
}

if ("interactions" %in% dbListTables(con)) {
  tryCatch({
    result <- dbGetQuery(con, "SELECT COUNT(*) as count FROM interactions")
    cat(sprintf("  ✓ Interactions table: %d record(s)\n", result$count))
  }, error = function(e) {
    cat(sprintf("  ✗ Error querying interactions: %s\n", e$message))
  })
} else {
  cat("  - Skipping interactions query (table doesn't exist)\n")
}
cat("\n")

# Step 5: Test insert capability
cat("Step 5: Testing write permissions...\n")
if ("students" %in% dbListTables(con)) {
  tryCatch({
    # Try to insert a test record
    test_query <- "INSERT INTO students (first_name, last_name) VALUES ('TEST', 'USER')"
    dbExecute(con, test_query)
    cat("  ✓ Successfully inserted test record\n")

    # Delete the test record
    dbExecute(con, "DELETE FROM students WHERE first_name='TEST' AND last_name='USER'")
    cat("  ✓ Successfully deleted test record\n")
    cat("  ✓ Write permissions confirmed\n")
  }, error = function(e) {
    cat(sprintf("  ✗ Write test failed: %s\n", e$message))
    cat("  Your user may not have INSERT/DELETE permissions\n")
  })
} else {
  cat("  - Skipping write test (table doesn't exist)\n")
}
cat("\n")

# Close connection
if (!is.null(con)) {
  dbDisconnect(con)
  cat("Step 6: Connection closed\n\n")
}

cat("=== Test Complete ===\n")
cat("\nSummary:\n")
cat("If all steps passed with ✓, your database is ready!\n")
cat("If you see ✗ errors, fix those issues before deploying the app.\n")
