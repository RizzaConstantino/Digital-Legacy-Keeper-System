library(shiny)
library(shinyjs)
library(fontawesome) # for icons

# ---------------- UI COMPONENTS ----------------

# 1. Login UI
login_ui <- function() {
  div(class = "page",
      div(class = "login-panel",
          h1("Digital Legacy Keeper"),
          div(class = "subtitle", "Secure access to your digital heritage"),
          
          # Username
          div(class = "field",
              tags$label("Username"),
              textInput("username", NULL, width = "100%"),
              div(id="username_error_space", 
                  div(textOutput("username_error"), style = "color: red; font-size: 13px; height: 18px; margin-top: 4px;")
              )
          ),
          
          # Password with toggle eye
          div(style="position: relative; width: 100%; max-width: 300px; margin-bottom: 20px;",
              tags$label("Password"),
              passwordInput("password", NULL, width = "100%"),
              div(id="password_error_space", 
                  div(textOutput("password_error"), style = "color: red; font-size: 13px; height: 18px; margin-top: 4px;")
              ),
              tags$span(
                id="toggle_eye",
                style="position: absolute; right: 10px; top: 50%; transform: translateY(-50%); cursor: pointer; z-index: 10;",
                HTML('
<svg id="eye_icon" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor" width="24" height="24">
  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.477 0 8.268 2.943 9.542 7-1.274 4.057-5.065 7-9.542 7-4.477 0-8.268-2.943-9.542-7z"/>
  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"/>
</svg>')
              )
          ),
          
          # Login button
          actionButton("login", "UNLOCK VAULT"),
          
          # Footer
          div(class = "footer",
              HTML("© 2025 Digital Legacy Keeper System<br>Rizza Constantino_BSIT 4-1")
          )
      )
  )
}

# 2. Dashboard UI
dashboard_ui <- function() {
  div(class = "dashboard-container",
      div(class = "header",
          div(class = "header-left",
              img(src = "https://cdn-icons-png.flaticon.com/512/3135/3135715.png", width = "50px", height = "50px", style="margin-right: 15px;"),
              span("DIGITAL LEGACY KEEPER SYSTEM", style="font-size: 26px; font-weight: bold; color: white;")
          ),
          div(class = "header-right",
              span("Admin", style="margin-right:10px;"),
              img(src = "https://i.pravatar.cc/150?u=admin", class = "profile-img")
          )
      ),
      div(style = "display: flex; flex: 1; height: calc(100vh - 70px);",
          # Sidebar
          div(class = "sidebar", id="sidebar",
              div(class = "menu-toggle", icon("bars"), onclick="toggleSidebar()"),
              div(class = "nav-item active", icon("th-large"), "Dashboard"),
              div(class = "nav-item", icon("comment-alt"), "New Message"),
              div(class = "nav-item", icon("play"), "View All Messages"),
              div(class = "nav-item", icon("unlock"), "Released Messages"),
              div(class = "nav-item", icon("lock"), "Locked Messages"),
              div(class = "nav-item", icon("archive"), "Archives")
          ),
          # Content
          div(class = "content",
              h2("DASHBOARD"),
              div(class = "card-container",
                  div(class = "card card-dark", div(class="card-title", "TOTAL ACTIVE"), div(class="card-value", "30")),
                  div(class = "card card-medium", div(class="card-title", "LOCKED"), div(class="card-value", "10")),
                  div(class = "card card-light", div(class="card-title", "RELEASED"), div(class="card-value", "03"))
              ),
              h3("UPCOMING LEGACY MESSAGE", style="margin-top: 40px; font-weight: bold;"),
              div(class = "message-box"),
              actionButton("create_msg", label = div(icon("plus"), "CREATE MESSAGE"), class = "create-btn")
          )
      )
  )
}

# ---------------- MAIN UI ----------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(HTML("
      function toggleSidebar() {
        var sidebar = document.getElementById('sidebar');
        if(sidebar.style.width === '0px'){
          sidebar.style.width = '250px';
        } else {
          sidebar.style.width = '0px';
        }
      }
    ")),
    tags$style(HTML("
      html, body { height: 100%; margin: 0; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; overflow: hidden; }
      .page { display: flex; height: 100vh; width: 100vw; background-image: url('https://miro.medium.com/v2/resize:fit:1100/format:webp/1*SZgDOXLKcBDxnLwKvfLWAw.png'); background-size: cover; }
      .login-panel { margin-left: auto; width: 40%; background: rgba(217,217,217,0.85); padding: 70px 40px; display: flex; flex-direction: column; justify-content: center; align-items: center; position: relative; }
      h1 { font-family: Georgia, serif; font-size: 42px; margin-bottom: 10px; text-align: center; }
      .subtitle { text-align: center; font-size: 18px; margin-bottom: 40px; color: #222; }
      .field { margin-bottom: 20px; width: 100%; max-width: 300px; position: relative; }
      .field label { font-weight: bold; font-size: 16px; display: block; margin-bottom: 6px; }
      .shiny-input-container input { width: 100%; height: 36px; border-radius: 6px; border: none; background: #E0E0E0; padding: 6px 10px; font-size: 14px; box-sizing: border-box; }
      #login { width: 100%; max-width: 300px; height: 44px; border-radius: 6px; border: none; font-size: 16px; font-weight: bold; color: white; background: linear-gradient(90deg, rgba(4,7,20,0.8), rgba(47,79,109,0.8)); margin-top: 20px; }
      .footer { position: absolute; bottom: 20px; text-align: center; font-size: 13px; color: #000; width: 100%; }
      /* Dashboard */
      .dashboard-container { display: flex; flex-direction: column; height: 100vh; width: 100vw; }
      .header { background-color: #5F96A8; color: white; padding: 10px 20px; display: flex; justify-content: space-between; align-items: center; font-weight: bold; font-size: 24px; }
      .header-left { display: flex; align-items: center; }
      .header-right { display: flex; align-items: center; font-size: 16px; }
      .profile-img { border-radius: 50%; width: 40px; margin-left: 10px; border: 2px solid white; }
      .sidebar { width: 250px; background-color: #2C445C; color: white; display: flex; flex-direction: column; padding-top: 20px; transition: width 0.3s; overflow: hidden; }
      .nav-item { padding: 15px 20px; font-size: 16px; display: flex; align-items: center; cursor: pointer; white-space: nowrap; }
      .nav-item i { margin-right: 15px; width: 20px; }
      .nav-item:hover { background-color: #3d5a75; }
      .nav-item.active { background-color: #ffffff22; }
      .menu-toggle { padding-left: 10px; margin-bottom: 20px; font-size: 20px; cursor: pointer; }
      .content { flex: 1; padding: 20px 30px; background-color: #fff; overflow-y: auto; } /* reduced left padding */
      .card-container { display: flex; gap: 30px; margin-top: 20px; flex-wrap: wrap; }
      .card { width: 200px; height: 150px; border-radius: 8px; color: white; padding: 20px; text-align: center; }
      .card-dark { background-color: #1A263F; }
      .card-medium { background-color: #345670; }
      .card-light { background-color: #6997A9; }
      .card-title { font-size: 14px; font-weight: bold; margin-bottom: 10px; }
      .card-value { font-size: 48px; font-weight: bold; }
      .message-box { border: 2px solid #2C445C; border-radius: 8px; height: 50px; width: 400px; margin: 15px 0; }
      .create-btn { background-color: #1A263F !important; color: white !important; border: none !important; padding: 10px 20px !important; border-radius: 6px !important; display: flex; align-items: center; gap: 10px; }
      @media (max-width: 768px) { .sidebar { width: 0px; } }
    "))
  ),
<<<<<<< HEAD
  uiOutput("main_ui")
=======
  
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
>>>>>>> 6e9d621ea322af1afde4e1a000bf62180ca70126
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  logged_in <- reactiveVal(FALSE)
  
  # Dynamically switch UI
  output$main_ui <- renderUI({
    if(!logged_in()) login_ui() else dashboard_ui()
  })
  
  # Handle login
  observeEvent(input$login, {
    output$username_error <- renderText(" ")
    output$password_error <- renderText(" ")
    
    username <- input$username
    password <- input$password
    
    if(username == "") output$username_error <- renderText("Username cannot be blank ❌")
    if(password == "") output$password_error <- renderText("Password cannot be blank ❌")
    
    if(username != "" && password != "") {
      if(username == "admin" && password == "admin123") {
        logged_in(TRUE)
      } else {
        output$password_error <- renderText("Incorrect username or password ❌")
      }
    }
  })
  
  # Toggle password visibility
  observe({
    runjs("
      $(document).on('click', '#toggle_eye', function() {
        var pwd = $('#password');
        var eye = $('#eye_icon');
        if(pwd.attr('type') === 'password'){
          pwd.attr('type', 'text');
          eye.html('<svg xmlns=\"http://www.w3.org/2000/svg\" fill=\"none\" viewBox=\"0 0 24 24\" stroke=\"currentColor\"><path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M13.875 18.825A10.05 10.05 0 0112 19c-4.478 0-8.269-2.943-9.543-7a10.05 10.05 0 012.227-3.419m1.636-1.636A9.966 9.966 0 0112 5c4.477 0 8.268 2.943 9.542 7a9.97 9.97 0 01-1.528 2.968M15 12a3 3 0 11-6 0 3 3 0 016 0z\"/></svg>');
        } else {
          pwd.attr('type', 'password');
          eye.html('<svg xmlns=\"http://www.w3.org/2000/svg\" fill=\"none\" viewBox=\"0 0 24 24\" stroke=\"currentColor\"><path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M2.458 12C3.732 7.943 7.523 5 12 5c4.477 0 8.268 2.943 9.542 7-1.274 4.057-5.065 7-9.542 7-4.477 0-8.268-2.943-9.542-7z\"/><path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M15 12a3 3 0 11-6 0 3 3 0 016 0z\"/></svg>');
        }
      });
    ")
  })
}

shinyApp(ui, server)
