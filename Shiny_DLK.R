library(shiny)
library(DBI)
library(RSQLite)

# ---------------- DATABASE ----------------
DB_FILE <- "C:/Users/daves/Documents/Digital_Legacy_Keeper.db"

# Create global connection function
get_connection <- function() {
  conn <- dbConnect(SQLite(), dbname = DB_FILE)
  
  dbExecute(conn, "CREATE TABLE IF NOT EXISTS legacy_messages (
    message_id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL,
    message TEXT NOT NULL,
    emotion TEXT,
    release_date TEXT,
    status TEXT DEFAULT 'LOCKED'
  )")
  
  dbExecute(conn, "CREATE TABLE IF NOT EXISTS users (
    user_id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE NOT NULL,
    password TEXT NOT NULL,
    full_name TEXT
  )")
  
  # Add default user if not exists
  users <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM users")
  if (users$count == 0) {
    dbExecute(conn, "INSERT INTO users (username, password, full_name) VALUES ('admin', 'admin', 'Administrator')")
  }
  
  return(conn)
}

# ---------------- SHINY APP ----------------

# UI Definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Login Page Styles */
      .login-container {
        max-width: 550px;
        margin: 50px auto;
        background-color: #fdf2f8;
        border-radius: 10px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        padding: 20px;
      }
      
      .login-header {
        background-color: #fbcfe8;
        padding: 30px;
        text-align: center;
        border-radius: 10px 10px 0 0;
        margin: -20px -20px 30px -20px;
      }
      
      .login-form {
        padding: 0 40px 20px 40px;
      }
      
      .login-btn {
        background-color: #be185d;
        color: white;
        border: none;
        padding: 12px 50px;
        font-weight: bold;
        font-size: 16px;
        border-radius: 5px;
        width: 100%;
        margin-top: 20px;
      }
      
      .login-footer {
        text-align: center;
        color: #9ca3af;
        font-size: 12px;
        margin-top: 30px;
        padding-top: 20px;
        border-top: 1px solid #eee;
      }
      
      /* Main Application Styles */
      .main-header {
        background-color: #fbcfe8;
        padding: 15px;
        text-align: center;
        color: #4b0082;
        font-weight: bold;
        font-size: 20px;
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        height: 60px;
        z-index: 1000;
      }
      
      .sidebar {
        background-color: #4b0082;
        position: fixed;
        left: 0;
        top: 60px;
        bottom: 0;
        width: 220px;
        padding: 20px 0;
        overflow-y: auto;
      }
      
      .sidebar-btn {
        background-color: #4b0082;
        color: white;
        border: none;
        text-align: left;
        padding: 15px 30px;
        width: 100%;
        font-size: 15px;
        cursor: pointer;
        transition: background-color 0.3s;
      }
      
      .sidebar-btn:hover {
        background-color: #5a1a9a;
      }
      
      .content-area {
        margin-left: 220px;
        margin-top: 60px;
        padding: 20px;
        background-color: #fff0f5;
        min-height: calc(100vh - 60px);
      }
      
      .card {
        background-color: white;
        border-radius: 10px;
        padding: 20px;
        margin: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        display: inline-block;
        width: 200px;
        height: 150px;
        vertical-align: top;
        text-align: center;
      }
      
      .card-title {
        font-size: 14px;
        color: #666;
        margin-bottom: 10px;
      }
      
      .card-value {
        font-size: 24px;
        font-weight: bold;
        margin-top: 20px;
      }
      
      .upcoming-box {
        background-color: white;
        padding: 30px;
        text-align: center;
        border: 2px solid #4b0082;
        border-radius: 10px;
        margin: 30px auto;
        max-width: 800px;
        font-size: 16px;
        font-weight: bold;
      }
      
      .create-btn {
        background-color: #ff69b4;
        color: white;
        border: none;
        padding: 12px 30px;
        font-weight: bold;
        border-radius: 5px;
        font-size: 14px;
        cursor: pointer;
        display: block;
        margin: 40px auto;
      }
      
      .form-container {
        background-color: white;
        padding: 40px;
        border-radius: 10px;
        max-width: 900px;
        margin: 0 auto;
      }
      
      .form-label {
        font-weight: bold;
        font-size: 14px;
        color: #333;
      }
      
      .text-output {
        background-color: white;
        padding: 20px;
        font-family: 'Courier New', monospace;
        font-size: 12px;
        border: 1px solid #ddd;
        border-radius: 5px;
        white-space: pre;
        overflow-x: auto;
        max-height: 400px;
        overflow-y: auto;
      }
      
      .action-panel {
        background-color: white;
        padding: 20px;
        border-radius: 10px;
        margin-top: 20px;
      }
      
      .error-message {
        color: red;
        font-weight: bold;
        padding: 10px;
        text-align: center;
      }
    "))
  ),
  
  shinyjs::useShinyjs(),
  
  div(id = "login-page",
      div(class = "login-container",
          div(class = "login-header",
              h2("✨ Digital Legacy Keeper Portal"),
              p("Secure access to your digital heritage", style = "font-style: italic; color: #6b7280;")
          ),
          div(class = "login-form",
              textInput("username", "Username:", value = ""),
              passwordInput("password", "Password:", value = ""),
              checkboxInput("show_password", "Show Password", value = FALSE),
              actionButton("login", "UNLOCK VAULT", class = "login-btn")
          ),
          div(class = "login-footer",
              p("© 2025 Digital Legacy Keeper System"),
              p("Rizza Constantino_BSIT 4-1", style = "font-weight: bold; color: #6b7280;")
          )
      )
  ),
  
  div(id = "main-app", style = "display: none;",
      div(class = "main-header",
          "⏳ DIGITAL LEGACY KEEPER SYSTEM 🔐"
      ),
      div(class = "sidebar",
          div(style = "color: pink; font-size: 16px; font-weight: bold; text-align: center; padding: 25px 0;",
              "MENU"
          ),
          actionButton("dashboard_btn", "🏠  Dashboard", class = "sidebar-btn"),
          actionButton("new_msg_btn", "✍  new Message", class = "sidebar-btn"),
          actionButton("view_all_btn", "📋  View All Messages", class = "sidebar-btn"),
          actionButton("released_btn", "🔓  Released Messages", class = "sidebar-btn"),
          actionButton("archives_btn", "🗄  Archives", class = "sidebar-btn"),
          actionButton("logout_btn", "🚪  Log Out", class = "sidebar-btn")
      ),
      div(class = "content-area",
          uiOutput("main_content")
      )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Initialize reactive values
  values <- reactiveValues(
    conn = NULL,
    current_view = "dashboard",
    logged_in = FALSE,
    error_message = NULL
  )
  
  # Initialize database connection when needed
  observe({
    if (values$logged_in && is.null(values$conn)) {
      values$conn <- get_connection()
    }
  })
  
  # Toggle password visibility - FIXED VERSION
  observe({
    if (input$show_password) {
      shinyjs::runjs("$('#password').attr('type', 'text');")
    } else {
      shinyjs::runjs("$('#password').attr('type', 'password');")
    }
  })
  
  # Login functionality
  observeEvent(input$login, {
    tryCatch({
      # Create connection for login
      temp_conn <- get_connection()
      on.exit(dbDisconnect(temp_conn))
      
      user <- dbGetQuery(temp_conn, 
                         "SELECT * FROM users WHERE username = ? AND password = ?", 
                         params = list(input$username, input$password))
      
      if (nrow(user) > 0) {
        # Successful login
        values$logged_in <- TRUE
        values$error_message <- NULL
        shinyjs::hide("login-page")
        shinyjs::show("main-app")
        values$current_view <- "dashboard"
      } else {
        values$error_message <- "Invalid Credentials"
        showModal(modalDialog(
          title = "Error",
          "Invalid Credentials",
          easyClose = TRUE
        ))
      }
    }, error = function(e) {
      values$error_message <- paste("Database error:", e$message)
      showModal(modalDialog(
        title = "Error",
        paste("Database error:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Sidebar navigation
  observeEvent(input$dashboard_btn, { 
    values$current_view <- "dashboard"
  })
  
  observeEvent(input$new_msg_btn, { 
    values$current_view <- "new_message"
  })
  
  observeEvent(input$view_all_btn, { 
    values$current_view <- "view_all"
  })
  
  observeEvent(input$released_btn, { 
    values$current_view <- "released"
  })
  
  observeEvent(input$archives_btn, { 
    values$current_view <- "archives"
  })
  
  observeEvent(input$logout_btn, {
    if (!is.null(values$conn)) {
      dbDisconnect(values$conn)
      values$conn <- NULL
    }
    values$logged_in <- FALSE
    shinyjs::show("login-page")
    shinyjs::hide("main-app")
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    updateCheckboxInput(session, "show_password", value = FALSE)
  })
  
  # Refresh database logic
  refresh_db_logic <- function() {
    if (!is.null(values$conn)) {
      tryCatch({
        dbExecute(values$conn, 
                  "UPDATE legacy_messages SET status='RELEASED' WHERE release_date <= ? AND status != 'DELETED'", 
                  params = list(as.character(Sys.Date())))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Database Error",
          paste("Error updating statuses:", e$message),
          easyClose = TRUE
        ))
      })
    }
  }
  
  # Main content rendering
  output$main_content <- renderUI({
    req(values$logged_in)
    
    # Refresh statuses for relevant views
    if (values$current_view %in% c("dashboard", "view_all", "released")) {
      refresh_db_logic()
    }
    
    # Show loading if no connection
    if (is.null(values$conn)) {
      return(div(style = "text-align: center; padding: 100px;", 
                 h4("Loading..."),
                 tags$img(src = "https://media.giphy.com/media/3o7TKtnuHOHHUjR38Y/giphy.gif", height = "100px")))
    }
    
    # Dashboard view
    if (values$current_view == "dashboard") {
      tryCatch({
        # Get statistics
        total <- dbGetQuery(values$conn, 
                            "SELECT COUNT(*) as c FROM legacy_messages WHERE status != 'DELETED'")$c
        locked <- dbGetQuery(values$conn, 
                             "SELECT COUNT(*) as c FROM legacy_messages WHERE status='LOCKED'")$c
        released <- dbGetQuery(values$conn, 
                               "SELECT COUNT(*) as c FROM legacy_messages WHERE status='RELEASED'")$c
        next_msg <- dbGetQuery(values$conn, 
                               "SELECT title, release_date FROM legacy_messages WHERE status='LOCKED' ORDER BY release_date ASC LIMIT 1")
        
        tagList(
          div(style = "text-align: center; margin-bottom: 30px;",
              h3("Dashboard Overview", style = "color: #4b0082;")
          ),
          div(style = "text-align: center;",
              div(class = "card",
                  div(class = "card-title", "Total Active"),
                  div(class = "card-value", style = "color: #0277bd;", total)
              ),
              div(class = "card",
                  div(class = "card-title", "Locked"),
                  div(class = "card-value", style = "color: #6a1b9a;", locked)
              ),
              div(class = "card",
                  div(class = "card-title", "Released"),
                  div(class = "card-value", style = "color: #2e7d32;", released)
              )
          ),
          
          div(class = "upcoming-box",
              if(nrow(next_msg) > 0) {
                days_left <- as.numeric(as.Date(next_msg$release_date) - Sys.Date())
                paste0("Next Release: '", next_msg$title, "' in ", days_left, " days.")
              } else {
                "No upcoming messages scheduled."
              }
          ),
          
          div(style = "text-align: center;",
              actionButton("create_new", "＋ Create New Message", class = "create-btn")
          ),
          
          div(style = "position: fixed; bottom: 20px; left: 240px; color: #666; font-size: 10px;",
              "Rizza Constantino_BSIT 4-1"
          )
        )
      }, error = function(e) {
        div(class = "error-message",
            paste("Error loading dashboard:", e$message)
        )
      })
      
    } else if (values$current_view == "new_message") {
      # New message view
      tagList(
        div(class = "form-container",
            h3("Create New Legacy Message", style = "color: black; margin-bottom: 20px;"),
            
            fluidRow(
              column(12,
                     div(class = "form-label", "Title:"),
                     textInput("title_input", NULL, value = "", width = "100%", placeholder = "Enter message title")
              )
            ),
            
            fluidRow(
              column(12,
                     div(class = "form-label", "Emotion:"),
                     selectInput("emotion_input", NULL, 
                                 choices = c(
                                   "--- Positive Emotions ---",
                                   "Happy", "Joyful", "Excited", "Content", "Hopeful", "Grateful",
                                   "Loving", "Peaceful", "Proud", "Inspired", "Playful", "Confident",
                                   "Relaxed", "Cheerful", "Amused",
                                   "--- Negative Emotions ---",
                                   "Sad", "Angry", "Frustrated", "Lonely", "Anxious", "Guilty",
                                   "Jealous", "Hopeless", "Stressed", "Overwhelmed", "Regretful",
                                   "Disappointed", "Hurt", "Confused", "Insecure",
                                   "--- Neutral / Complex Emotions ---",
                                   "Nostalgic", "Reflective", "Curious", "Thoughtful", "Surprised",
                                   "Conflicted", "Sentimental", "Ambivalent", "Shocked", "Contemplative"
                                 ),
                                 selected = "Grateful", width = "100%")
              )
            ),
            
            fluidRow(
              column(12,
                     div(class = "form-label", "Release Date:"),
                     fluidRow(
                       column(8,
                              dateInput("date_input", NULL, value = Sys.Date(), format = "yyyy-mm-dd", width = "100%")
                       ),
                       column(4,
                              br(),
                              actionButton("select_date", "📅 Select Date", 
                                           style = "background-color: #be185d; color: white; width: 100%; margin-top: 5px;")
                       )
                     )
              )
            ),
            
            fluidRow(
              column(12,
                     div(class = "form-label", "Message:"),
                     textAreaInput("message_input", NULL, value = "", rows = 10, width = "100%", 
                                   placeholder = "Write your message here...")
              )
            ),
            
            div(style = "text-align: right; margin-top: 30px;",
                actionButton("save_message", "🔒 Lock into Legacy", 
                             style = "background-color: #ba55d3; color: white; font-weight: bold; padding: 10px 30px;")
            )
        )
      )
      
    } else if (values$current_view %in% c("view_all", "released")) {
      # Table views
      tryCatch({
        mode <- ifelse(values$current_view == "view_all", "ALL", "RELEASED")
        query <- if(mode == "ALL") {
          "SELECT * FROM legacy_messages WHERE status != 'DELETED' ORDER BY message_id DESC"
        } else {
          "SELECT * FROM legacy_messages WHERE status='RELEASED' ORDER BY release_date DESC"
        }
        
        data <- dbGetQuery(values$conn, query)
        
        # Format data for display
        formatted_data <- ""
        if(nrow(data) > 0) {
          header <- sprintf("%-4s | %-25s | %-12s | %-12s | %-10s\n", "ID", "Title", "Emotion", "Date", "Status")
          sep <- paste0(rep("-", 75), collapse="")
          formatted_data <- paste0(header, sep, "\n")
          
          for(i in 1:nrow(data)) {
            clean_title <- substr(data$title[i], 1, 25)
            row_str <- sprintf("%-4s | %-25s | %-12s | %-12s | %-10s\n", 
                               data$message_id[i], clean_title, data$emotion[i], 
                               data$release_date[i], data$status[i])
            formatted_data <- paste0(formatted_data, row_str)
          }
        } else {
          formatted_data <- "\n--- No records found ---\n"
        }
        
        tagList(
          h3(if(mode == "ALL") "All Active Legacy Records" else "Released Messages", 
             style = "color: #4b0082;"),
          
          div(class = "text-output",
              HTML(gsub("\n", "<br>", gsub(" ", "&nbsp;", formatted_data)))
          ),
          
          div(class = "action-panel",
              textInput("action_id", "Enter ID:", value = ""),
              br(),
              if(mode == "ALL") {
                tagList(
                  actionButton("update_msg", "Update", 
                               style = "background-color: orange; color: white; margin-right: 10px;"),
                  actionButton("delete_msg", "Delete", 
                               style = "background-color: red; color: white; margin-right: 10px;")
                )
              } else {
                actionButton("view_msg", "View Message", 
                             style = "background-color: blue; color: white; margin-right: 10px;")
              },
              actionButton("refresh_table", "Refresh", 
                           style = "background-color: #4b0082; color: white;")
          )
        )
      }, error = function(e) {
        div(class = "error-message",
            paste("Error loading data:", e$message)
        )
      })
      
    } else if (values$current_view == "archives") {
      # Archives view
      tryCatch({
        data <- dbGetQuery(values$conn, "SELECT * FROM legacy_messages WHERE status='DELETED' ORDER BY message_id DESC")
        
        # Format data for display
        formatted_data <- ""
        if(nrow(data) > 0) {
          header <- sprintf("%-4s | %-25s | %-12s | %-12s | %-10s\n", "ID", "Title", "Emotion", "Date", "Status")
          sep <- paste0(rep("-", 75), collapse="")
          formatted_data <- paste0(header, sep, "\n")
          
          for(i in 1:nrow(data)) {
            clean_title <- substr(data$title[i], 1, 25)
            row_str <- sprintf("%-4s | %-25s | %-12s | %-12s | %-10s\n", 
                               data$message_id[i], clean_title, data$emotion[i], 
                               data$release_date[i], data$status[i])
            formatted_data <- paste0(formatted_data, row_str)
          }
        } else {
          formatted_data <- "\n--- No archived messages found ---\n"
        }
        
        tagList(
          h3("Archived Messages", style = "color: #4b0082;"),
          
          div(class = "text-output",
              HTML(gsub("\n", "<br>", gsub(" ", "&nbsp;", formatted_data)))
          ),
          
          div(class = "action-panel",
              textInput("restore_id_input", "Enter ID to Restore:", value = ""),
              br(),
              actionButton("restore_msg", "Restore", 
                           style = "background-color: #2e7d32; color: white; margin-right: 10px;"),
              actionButton("refresh_archives", "Refresh", 
                           style = "background-color: #4b0082; color: white;")
          )
        )
      }, error = function(e) {
        div(class = "error-message",
            paste("Error loading archives:", e$message)
        )
      })
    }
  })
  
  # Create new message from dashboard
  observeEvent(input$create_new, {
    values$current_view <- "new_message"
  })
  
  # Save message
  observeEvent(input$save_message, {
    req(values$conn, input$title_input, input$message_input)
    
    if(trimws(input$title_input) == "" || trimws(input$message_input) == "") {
      showModal(modalDialog(
        title = "Warning",
        "Title and Message content cannot be empty!",
        easyClose = TRUE
      ))
      return()
    }
    
    emotion <- input$emotion_input
    if(grepl("^---.*---$", emotion) || emotion == "") {
      showModal(modalDialog(
        title = "Invalid Selection",
        "Please select an emotion, not a category.",
        easyClose = TRUE
      ))
      return()
    }
    
    tryCatch({
      dbExecute(values$conn, 
                "INSERT INTO legacy_messages (title, message, emotion, release_date) VALUES (?,?,?,?)",
                params = list(input$title_input, input$message_input, emotion, as.character(input$date_input)))
      
      showModal(modalDialog(
        title = "Success",
        "Message Saved Successfully",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
      # Clear form
      updateTextInput(session, "title_input", value = "")
      updateSelectInput(session, "emotion_input", selected = "Grateful")
      updateDateInput(session, "date_input", value = Sys.Date())
      updateTextAreaInput(session, "message_input", value = "")
      
      # Go back to dashboard
      values$current_view <- "dashboard"
    }, error = function(e) {
      showModal(modalDialog(
        title = "Database Error",
        paste("Error saving message:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Update message
  observeEvent(input$update_msg, {
    req(values$conn, input$action_id)
    
    mid <- input$action_id
    if(mid == "") {
      showModal(modalDialog(
        title = "Error",
        "Please enter an ID first.",
        easyClose = TRUE
      ))
      return()
    }
    
    tryCatch({
      res <- dbGetQuery(values$conn, 
                        "SELECT status, title, release_date FROM legacy_messages WHERE message_id=? AND status != 'DELETED'", 
                        params = list(mid))
      
      if(nrow(res) == 0) {
        showModal(modalDialog(
          title = "Error",
          "ID not found.",
          easyClose = TRUE
        ))
        return()
      }
      
      if(res$status[1] == "RELEASED") {
        showModal(modalDialog(
          title = "Action Forbidden",
          "Released messages cannot be updated.",
          easyClose = TRUE
        ))
        return()
      }
      
      # Show update modal
      showModal(modalDialog(
        title = "Update Message",
        size = "m",
        textInput("update_title", "Title:", value = res$title[1]),
        dateInput("update_date", "Release Date:", value = as.Date(res$release_date[1])),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_update", "Save Changes", 
                       style = "background-color: orange; color: white;")
        )
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Database Error",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Save update
  observeEvent(input$save_update, {
    req(values$conn, input$action_id, input$update_title, input$update_date)
    
    tryCatch({
      dbExecute(values$conn, 
                "UPDATE legacy_messages SET title=?, release_date=? WHERE message_id=?", 
                params = list(input$update_title, as.character(input$update_date), input$action_id))
      
      removeModal()
      updateTextInput(session, "action_id", value = "")
      
      showModal(modalDialog(
        title = "Success",
        "Message updated successfully!",
        easyClose = TRUE
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Error updating message:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Delete message
  observeEvent(input$delete_msg, {
    req(values$conn, input$action_id)
    
    mid <- input$action_id
    if(mid == "") {
      showModal(modalDialog(
        title = "Error",
        "Please enter an ID first.",
        easyClose = TRUE
      ))
      return()
    }
    
    tryCatch({
      res <- dbGetQuery(values$conn, 
                        "SELECT status FROM legacy_messages WHERE message_id=? AND status != 'DELETED'", 
                        params = list(mid))
      
      if(nrow(res) == 0) {
        showModal(modalDialog(
          title = "Error",
          "ID not found.",
          easyClose = TRUE
        ))
        return()
      }
      
      if(res$status[1] == "RELEASED") {
        showModal(modalDialog(
          title = "Error",
          "Cannot delete released message.",
          easyClose = TRUE
        ))
        return()
      }
      
      dbExecute(values$conn, 
                "UPDATE legacy_messages SET status='DELETED' WHERE message_id=?", 
                params = list(mid))
      
      updateTextInput(session, "action_id", value = "")
      
      showModal(modalDialog(
        title = "Success",
        "Message moved to archives!",
        easyClose = TRUE
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Database Error",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # View released message
  observeEvent(input$view_msg, {
    req(values$conn, input$action_id)
    
    mid <- input$action_id
    if(mid == "") {
      showModal(modalDialog(
        title = "Error",
        "Please enter an ID first.",
        easyClose = TRUE
      ))
      return()
    }
    
    tryCatch({
      msg <- dbGetQuery(values$conn, 
                        "SELECT message FROM legacy_messages WHERE message_id=? AND status='RELEASED'", 
                        params = list(mid))
      
      if(nrow(msg) > 0) {
        showModal(modalDialog(
          title = "Legacy Content",
          size = "l",
          div(style = "padding: 20px; background-color: #f8f9fa; border-radius: 8px; max-height: 400px; overflow-y: auto;",
              HTML(gsub("\n", "<br>", msg$message[1]))
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      } else {
        showModal(modalDialog(
          title = "Error",
          "Message Locked or ID wrong",
          easyClose = TRUE
        ))
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Database Error",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Refresh table
  observeEvent(input$refresh_table, {
    # Just trigger re-render by invalidating
    values$current_view <- values$current_view
  })
  
  # Restore message
  observeEvent(input$restore_msg, {
    req(values$conn, input$restore_id_input)
    
    mid <- input$restore_id_input
    if(mid == "") {
      showModal(modalDialog(
        title = "Error",
        "Please enter an ID first.",
        easyClose = TRUE
      ))
      return()
    }
    
    tryCatch({
      res <- dbGetQuery(values$conn, 
                        "SELECT * FROM legacy_messages WHERE message_id=? AND status='DELETED'", 
                        params = list(mid))
      
      if(nrow(res) > 0) {
        dbExecute(values$conn, 
                  "UPDATE legacy_messages SET status='LOCKED' WHERE message_id=?", 
                  params = list(mid))
        
        showModal(modalDialog(
          title = "Success",
          "Message restored successfully!",
          easyClose = TRUE
        ))
        
        updateTextInput(session, "restore_id_input", value = "")
      } else {
        showModal(modalDialog(
          title = "Error",
          "ID not found in archives.",
          easyClose = TRUE
        ))
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Database Error",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Refresh archives
  observeEvent(input$refresh_archives, {
    values$current_view <- "archives"
  })
  
  # Date selection
  observeEvent(input$select_date, {
    showModal(modalDialog(
      title = "Select Date",
      size = "s",
      dateInput("modal_date", NULL, value = input$date_input, format = "yyyy-mm-dd"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_date", "Confirm Date", 
                     style = "background-color: #be185d; color: white;")
      )
    ))
  })
  
  observeEvent(input$confirm_date, {
    updateDateInput(session, "date_input", value = input$modal_date)
    removeModal()
  })
  
  # Clean up on session end
  session$onSessionEnded(function() {
    if (!is.null(values$conn)) {
      try(dbDisconnect(values$conn), silent = TRUE)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)