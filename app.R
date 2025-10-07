# Student Interactions Tracker
# A Shiny app for recording student meetings and interactions

library(shiny)
library(DBI)
library(RMySQL)
library(DT)
library(shinyjs)

# Database connection function
get_db_connection <- function() {
  dbConnect(
    MySQL(),
    host = Sys.getenv("MYSQL_HOST"),
    port = as.integer(Sys.getenv("MYSQL_PORT")),
    user = Sys.getenv("MYSQL_USER"),
    password = Sys.getenv("MYSQL_PASSWORD"),
    dbname = Sys.getenv("MYSQL_DATABASE")
  )
}

# UI
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Student Interactions Tracker"),
  
  fluidRow(
    # Left Column: Student Directory
    column(3,
           wellPanel(
             h4("Student Directory"),
             textInput("student_search", "Search:", placeholder = "Search by name..."),
             actionButton("add_student_btn", "+ Add New Student", 
                          class = "btn-success btn-block", 
                          style = "margin-bottom: 10px;"),
             hr(),
             div(style = "height: 600px; overflow-y: auto;",
                 uiOutput("student_list")
             )
           )
    ),
    
    # Center Column: Log Interaction
    column(5,
           wellPanel(
             h4("Log Interaction"),
             uiOutput("selected_student_display"),
             dateInput("interaction_date", "Date:", value = Sys.Date()),
             textInput("interaction_time", "Time:", value = format(Sys.time(), "%H:%M")),
             textInput("location", "Location:", placeholder = "e.g., Office, Hallway, Cafeteria"),
             textAreaInput("notes", "Notes:", rows = 10, 
                           placeholder = "Record details of your interaction..."),
             actionButton("save_interaction", "Save Interaction", 
                          class = "btn-primary btn-block")
           )
    ),
    
    # Right Column: Interaction History
    column(4,
           wellPanel(
             h4(textOutput("history_title")),
             hr(),
             div(style = "height: 700px; overflow-y: auto;",
                 uiOutput("interaction_history")
             )
           )
    )
  ),
  
  # Modal for adding new student
  tags$div(id = "add_student_modal",
           modalDialog(
             title = "Add New Student",
             textInput("new_first_name", "First Name:*", placeholder = "Required"),
             textInput("new_last_name", "Last Name:*", placeholder = "Required"),
             textInput("new_phone", "Phone:", placeholder = "Optional"),
             textInput("new_email", "Email:", placeholder = "Optional"),
             textInput("new_graduation_month", "Graduation Month:", 
                       placeholder = "e.g., May, December"),
             numericInput("new_graduation_year", "Graduation Year:", 
                          value = as.integer(format(Sys.Date(), "%Y")), 
                          min = 2020, max = 2040),
             textInput("new_hometown", "Hometown:", placeholder = "Optional"),
             textInput("new_major", "Major:", placeholder = "Optional"),
             textInput("new_linkedin", "LinkedIn URL:", placeholder = "Optional"),
             textInput("new_social", "Instagram/Social Media:", placeholder = "Optional"),
             footer = tagList(
               modalButton("Cancel"),
               actionButton("save_student", "Save Student", class = "btn-primary")
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    selected_student_id = NULL,
    refresh = 0
  )
  
  # Get all students
  get_students <- reactive({
    rv$refresh
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    query <- "SELECT * FROM students ORDER BY last_name, first_name"
    students <- dbGetQuery(con, query)
    return(students)
  })
  
  # Filter students based on search
  filtered_students <- reactive({
    students <- get_students()
    search_term <- tolower(input$student_search)
    
    if (search_term != "") {
      students <- students[grepl(search_term, 
                                 tolower(paste(students$first_name, students$last_name))), ]
    }
    return(students)
  })
  
  # Render student list
  output$student_list <- renderUI({
    students <- filtered_students()
    
    if (nrow(students) == 0) {
      return(p("No students found", style = "text-align: center; color: #999;"))
    }
    
    lapply(1:nrow(students), function(i) {
      student <- students[i, ]
      is_selected <- !is.null(rv$selected_student_id) && 
        rv$selected_student_id == student$student_id
      
      div(
        class = if(is_selected) "student-card selected" else "student-card",
        style = paste0(
          "padding: 10px; margin-bottom: 5px; cursor: pointer; ",
          "border: 1px solid #ddd; border-radius: 4px; ",
          if(is_selected) "background-color: #e3f2fd; border-color: #2196F3;" else "background-color: white;"
        ),
        onclick = sprintf("Shiny.setInputValue('student_clicked', %d, {priority: 'event'})", 
                          student$student_id),
        strong(paste(student$first_name, student$last_name)),
        br(),
        span(style = "font-size: 0.9em; color: #666;",
             paste0(student$major, " • ", student$graduation_year))
      )
    })
  })
  
  # Handle student click
  observeEvent(input$student_clicked, {
    rv$selected_student_id <- input$student_clicked
  })
  
  # Display selected student info
  output$selected_student_display <- renderUI({
    if (is.null(rv$selected_student_id)) {
      return(div(
        style = "padding: 20px; background-color: #f5f5f5; border-radius: 4px; text-align: center;",
        p("Select a student from the left to log an interaction", 
          style = "color: #999; margin: 0;")
      ))
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    query <- sprintf("SELECT * FROM students WHERE student_id = %d", rv$selected_student_id)
    student <- dbGetQuery(con, query)
    
    if (nrow(student) == 0) return(NULL)
    
    div(
      style = "padding: 15px; background-color: #e3f2fd; border-radius: 4px; margin-bottom: 15px;",
      h5(paste(student$first_name, student$last_name), style = "margin-top: 0;"),
      p(style = "margin: 5px 0; font-size: 0.9em;",
        strong("Major: "), student$major, br(),
        strong("Grad: "), paste(student$graduation_month, student$graduation_year), br(),
        strong("Email: "), student$email
      )
    )
  })
  
  # Save interaction
  observeEvent(input$save_interaction, {
    if (is.null(rv$selected_student_id)) {
      showNotification("Please select a student first", type = "error")
      return()
    }
    
    if (input$notes == "") {
      showNotification("Please enter some notes", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    query <- sprintf(
      "INSERT INTO interactions (student_id, interaction_date, interaction_time, location, notes) 
       VALUES (%d, '%s', '%s', '%s', '%s')",
      rv$selected_student_id,
      input$interaction_date,
      input$interaction_time,
      dbEscapeStrings(con, input$location),
      dbEscapeStrings(con, input$notes)
    )
    
    dbExecute(con, query)
    
    # Clear form
    updateTextInput(session, "location", value = "")
    updateTextAreaInput(session, "notes", value = "")
    updateTextInput(session, "interaction_time", value = format(Sys.time(), "%H:%M"))
    
    # Refresh history
    rv$refresh <- rv$refresh + 1
    
    showNotification("Interaction saved successfully!", type = "message")
  })
  
  # History title
  output$history_title <- renderText({
    if (is.null(rv$selected_student_id)) {
      return("Recent Interactions (All Students)")
    } else {
      con <- get_db_connection()
      on.exit(dbDisconnect(con))
      
      query <- sprintf("SELECT first_name, last_name FROM students WHERE student_id = %d", 
                       rv$selected_student_id)
      student <- dbGetQuery(con, query)
      
      return(paste0(student$first_name, " ", student$last_name, "'s History"))
    }
  })
  
  # Render interaction history
  output$interaction_history <- renderUI({
    rv$refresh
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    if (is.null(rv$selected_student_id)) {
      # Show all recent interactions
      query <- "
        SELECT i.*, s.first_name, s.last_name 
        FROM interactions i
        JOIN students s ON i.student_id = s.student_id
        ORDER BY i.interaction_date DESC, i.interaction_time DESC
        LIMIT 20
      "
    } else {
      # Show interactions for selected student
      query <- sprintf(
        "SELECT i.*, s.first_name, s.last_name 
         FROM interactions i
         JOIN students s ON i.student_id = s.student_id
         WHERE i.student_id = %d
         ORDER BY i.interaction_date DESC, i.interaction_time DESC",
        rv$selected_student_id
      )
    }
    
    interactions <- dbGetQuery(con, query)
    
    if (nrow(interactions) == 0) {
      return(p("No interactions recorded yet", 
               style = "text-align: center; color: #999; padding: 20px;"))
    }
    
    lapply(1:nrow(interactions), function(i) {
      int <- interactions[i, ]
      
      div(
        style = "padding: 12px; margin-bottom: 10px; border: 1px solid #ddd; 
                 border-radius: 4px; background-color: white;",
        div(
          style = "margin-bottom: 8px;",
          if (is.null(rv$selected_student_id)) {
            strong(paste(int$first_name, int$last_name), style = "color: #2196F3;")
          } else {
            NULL
          },
          if (is.null(rv$selected_student_id)) br() else NULL,
          span(style = "color: #666; font-size: 0.9em;",
               format(as.Date(int$interaction_date), "%B %d, %Y"),
               " • ",
               substr(int$interaction_time, 1, 5),
               if (!is.na(int$location) && int$location != "") 
                 paste0(" • ", int$location) else ""
          )
        ),
        p(style = "margin: 0; font-size: 0.95em; white-space: pre-wrap;",
          int$notes
        )
      )
    })
  })
  
  # Show add student modal
  observeEvent(input$add_student_btn, {
    showModal(
      modalDialog(
        title = "Add New Student",
        textInput("new_first_name", "First Name:*", placeholder = "Required"),
        textInput("new_last_name", "Last Name:*", placeholder = "Required"),
        textInput("new_phone", "Phone:", placeholder = "Optional"),
        textInput("new_email", "Email:", placeholder = "Optional"),
        textInput("new_graduation_month", "Graduation Month:", 
                  placeholder = "e.g., May, December"),
        numericInput("new_graduation_year", "Graduation Year:", 
                     value = as.integer(format(Sys.Date(), "%Y")), 
                     min = 2020, max = 2040),
        textInput("new_hometown", "Hometown:", placeholder = "Optional"),
        textInput("new_major", "Major:", placeholder = "Optional"),
        textInput("new_linkedin", "LinkedIn URL:", placeholder = "Optional"),
        textInput("new_social", "Instagram/Social Media:", placeholder = "Optional"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_student", "Save Student", class = "btn-primary")
        )
      )
    )
  })
  
  # Save new student
  observeEvent(input$save_student, {
    if (input$new_first_name == "" || input$new_last_name == "") {
      showNotification("First and last name are required", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    query <- sprintf(
      "INSERT INTO students 
       (first_name, last_name, phone, email, graduation_month, graduation_year, 
        hometown, major, linkedin_url, social_media) 
       VALUES ('%s', '%s', '%s', '%s', '%s', %s, '%s', '%s', '%s', '%s')",
      dbEscapeStrings(con, input$new_first_name),
      dbEscapeStrings(con, input$new_last_name),
      dbEscapeStrings(con, input$new_phone),
      dbEscapeStrings(con, input$new_email),
      dbEscapeStrings(con, input$new_graduation_month),
      ifelse(is.na(input$new_graduation_year), "NULL", input$new_graduation_year),
      dbEscapeStrings(con, input$new_hometown),
      dbEscapeStrings(con, input$new_major),
      dbEscapeStrings(con, input$new_linkedin),
      dbEscapeStrings(con, input$new_social)
    )
    
    dbExecute(con, query)
    
    rv$refresh <- rv$refresh + 1
    removeModal()
    showNotification("Student added successfully!", type = "message")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
