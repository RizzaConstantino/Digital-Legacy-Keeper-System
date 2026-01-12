library(shiny)
library(shinyjs)
library(fontawesome)
library(base64enc) # for converting cropped image to base64

# ---------------- UI COMPONENTS ----------------

# 1. Login UI
login_ui <- function() {
  div(class = "page",
      div(class = "login-panel",
          
          h1(style="margin-bottom:5px;", "Digital Legacy Keeper"),
          div(class = "subtitle",
              style="margin-bottom:35px;",
              "Secure access to your digital heritage"),
          
          div(class = "field",
              tags$label("Username"),
              textInput("username", label = NULL, placeholder = "Enter username", width = "100%"),
              div(textOutput("username_error"),
                  style="color:red;font-size:13px;height:18px;margin-top:6px;")
          ),
          
          div(class = "field", style="position:relative;",
              tags$label("Password"),
              passwordInput("password", label = NULL, placeholder = "Enter password", width = "100%"),
              tags$span(id="toggle_eye",
                        style="position:absolute; right:12px; top:45%; transform: translateY(-50%); cursor:pointer;",
                        icon("eye")),
              div(textOutput("password_error"),
                  style="color:red;font-size:13px;height:18px;margin-top:6px;")
          ),
          
          div(style="height:10px;"),
          actionButton("login", "UNLOCK VAULT"),
          
          div(class = "footer",
              HTML("© 2025 Digital Legacy Keeper System<br>Rizza Constantino_BSIT 4-1")
          )
      )
  )
}

# ---------------- DASHBOARD UI ----------------
dashboard_ui <- function(tab = "dashboard") {
  
  emotions <- c(
    "Happy","Joyful","Excited","Content","Hopeful","Grateful",
    "Loving","Peaceful","Proud","Inspired","Playful","Confident",
    "Relaxed","Cheerful","Amused",
    "Sad","Angry","Frustrated","Lonely","Anxious","Guilty",
    "Jealous","Hopeless","Stressed","Overwhelmed","Regretful",
    "Disappointed","Hurt","Confused","Insecure",
    "Nostalgic","Reflective","Curious","Thoughtful","Surprised",
    "Conflicted","Sentimental","Ambivalent","Shocked","Contemplative"
  )
  
  div(class = "dashboard-container",
      
      # HEADER
      div(class = "header",
          div(class = "header-left",
              span(class="sidebar-toggle", icon("bars")),
              img(
                src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTsC3DWWLFdr8NuRx8Czhb33s8L6yi7E6qrpQ&s",
                class = "logo-circle"
              ),
              span("DIGITAL LEGACY KEEPER SYSTEM",
                   style="font-size:22px;font-weight:bold;")
          ),
          div(class = "header-right",
              span("Admin", style="margin-right:10px;"),
              img(src="https://i.pravatar.cc/150?u=admin",
                  class="profile-img", 
                  id="user_profile_btn", 
                  style="cursor:pointer;")
          )
      ),
      
      # BODY
      div(style="display:flex; flex:1;",
          
          # SIDEBAR
          div(class="sidebar", id="sidebar",
              div(class=paste("nav-item", if(tab=="dashboard") "active" else ""), icon("th-large"), "Dashboard", id="tab_dashboard"),
              div(class=paste("nav-item", if(tab=="new_message") "active" else ""), icon("comment-alt"), "New Message", id="tab_new_message"),
              div(class="nav-item", icon("play"), "View All Messages"),
              div(class="nav-item", icon("unlock"), "Released Messages"),
              div(class="nav-item", icon("lock"), "Locked Messages"),
              div(class="nav-item", icon("archive"), "Archives")
          ),
          
          # CONTENT
          div(class="content-wrapper centered", id="contentWrapper",
              div(class=paste0("content ", if(tab=="new_message") "no-bg" else ""),
                  if(tab == "dashboard"){
                    tagList(
                      h2("DASHBOARD", style="text-align:center;"),
                      div(class="card-container",
                          div(class="card card-dark clickable-card", title = "Total Active Messages",
                              div(style="display:flex; align-items:center; justify-content:center; gap:12px;",
                                  div(class="card-value","30"),
                                  div(class="card-icon", icon("users"))
                              ),
                              div(class="card-title","TOTAL ACTIVE")
                          ),
                          div(class="card card-medium clickable-card", title = "Locked Messages",
                              div(style="display:flex; align-items:center; justify-content:center; gap:12px;",
                                  div(class="card-value","10"),
                                  div(class="card-icon", icon("lock"))
                              ),
                              div(class="card-title","LOCKED")
                          ),
                          div(class="card card-light clickable-card", title = "Released Messages",
                              div(style="display:flex; align-items:center; justify-content:center; gap:12px;",
                                  div(class="card-value","03"),
                                  div(class="card-icon", icon("check-circle"))
                              ),
                              div(class="card-title","RELEASED")
                          )
                      ),
                      
                      h3("UPCOMING LEGACY MESSAGE", style="margin-top:40px;font-weight:bold; text-align:center;"),
                      div(class="upcoming-box",
                          style="background:#ffffffaa; border:2px dashed #1A263F; border-radius:12px; padding:12px; min-height:50px; max-height:100px; overflow-y:auto; width:100%; max-width:700px; margin: 0 auto;",
                          p("No upcoming messages yet.", style="color:#1A263F; font-size:16px;")
                      ),
                      div(style="margin-top:20px; text-align:center;",
                          actionButton("create_msg", label=div(icon("plus"),"CREATE MESSAGE"), class="create-btn")
                      )
                    )
                  } else if(tab == "new_message"){
                    tagList(
                      div(class="new-msg-card",
                          div(class="card-header",
                              h2(icon("comment-alt"), "Create New Legacy Message")
                          ),
                          
                          div(style="display:flex; gap:10px; flex-wrap:wrap; margin-bottom:15px;",
                              div(style="flex:1;", 
                                  textInput("msg_title", "Title", placeholder = "Enter message title...", width = "100%"),
                                  div(textOutput("msg_title_error"), style="color:red;font-size:11px;min-height:15px;")
                              ),
                              div(style="flex:1;", 
                                  selectizeInput("msg_emotion", "Emotion", choices = emotions, width = "100%",
                                                 options = list(create = TRUE, placeholder = "Select emotion...")),
                                  div(textOutput("msg_emotion_error"), style="color:red;font-size:11px;min-height:15px;")
                              )
                          ),
                          
                          div(style="margin-bottom:15px;",
                              dateInput("msg_date", "Released Date", width = "100%"),
                              div(textOutput("msg_date_error"), style="color:red;font-size:11px;min-height:15px;")
                          ),
                          
                          div(style="margin-bottom:15px;",
                              textAreaInput("msg_body", "Message", width="100%", height="180px",
                                            placeholder = "Write your legacy message here..."),
                              div(textOutput("msg_body_error"), style="color:red;font-size:11px;min-height:15px;")
                          ),
                          
                          div(style="text-align:center; margin-top:10px;",
                              actionButton("lock_msg", label=div(icon("lock"),"Lock into Legacy"), class="create-btn")
                          )
                      )
                    )
                  }
              )
          )
      )
  )
}

# ---------------- MAIN UI ----------------
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/cropperjs/1.5.13/cropper.min.js"),
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/cropperjs/1.5.13/cropper.min.css"),
    
    tags$script(HTML("
      $(document).on('click', '#tab_dashboard', function() { Shiny.setInputValue('tab_dashboard_click', Math.random()); });
      $(document).on('click', '#tab_new_message', function() { Shiny.setInputValue('tab_new_message_click', Math.random()); });

      /* Profile Click Listener */
      $(document).on('click', '#user_profile_btn', function() { Shiny.setInputValue('profile_clicked', Math.random()); });

      $(document).on('click', '.sidebar-toggle', function () {
        $('#sidebar').toggleClass('collapsed');
      });

      $(document).on('click', '#toggle_eye', function () {
        var pwd = $('#password');
        var icon = $(this).find('i');
        if(pwd.attr('type') === 'password'){
          pwd.attr('type','text');
          icon.removeClass('fa-eye').addClass('fa-eye-slash');
        } else {
          pwd.attr('type','password');
          icon.removeClass('fa-eye-slash').addClass('fa-eye');
        }
      });
    ")),
    
    tags$style(HTML("
      html, body { height:100%; margin:0; font-family:'Segoe UI'; overflow:hidden; }
      .page { display:flex; height:100vh; background-image:url('https://miro.medium.com/v2/resize:fit:1100/format:webp/1*SZgDOXLKcBDxnLwKvfLWAw.png'); background-size:cover; }
      .login-panel { margin-left:auto; width:40%; background:rgba(217,217,217,0.85); padding:60px 40px; display:flex; flex-direction:column; align-items:center; justify-content:flex-start; position:relative; }
      .subtitle { font-size:18px; text-align:center; }
      .field { width:100%; max-width:300px; margin-bottom:18px; }
      .shiny-input-container input, .shiny-input-container select, .shiny-input-container textarea { border-radius:6px; border:1px solid #ccc; padding:6px 10px; font-size:16px; }
      #login { width:300px; height:44px; background:#1A263F; color:white; font-weight:bold; border:none; border-radius:6px; }
      .footer { position:absolute; bottom:20px; font-size:13px; }

      .dashboard-container { display:flex; flex-direction:column; height:100vh; background-image:url('https://68.media.tumblr.com/a50b25a17a28aa70feda8826f672fae1/tumblr_nvnhc3XFIC1txt22yo1_500.gif'); background-size:cover; background-position:center; }
      .header { height:70px; background:rgba(95,150,168,0.9); display:flex; justify-content:space-between; align-items:center; padding:0 30px; color:white; }
      .header-left { display:flex; align-items:center; }
      .sidebar-toggle { cursor:pointer; font-size:22px; margin-right:10px; }
      .logo-circle { width:45px; height:45px; border-radius:50%; object-fit:cover; border:2px solid white; margin:0 15px; }
      .profile-img { width:40px; border-radius:50%; border:2px solid white; }
      .sidebar { width:250px; background:#2C445C; color:white; transition:width .3s; overflow:hidden; }
      .sidebar.collapsed { width:0; }
      .nav-item { padding:15px 25px; display:flex; align-items:center; cursor:pointer; }
      .nav-item i { margin-right:15px; }
      .nav-item:hover { background:#3d5a75; }
      .nav-item.active { background:#ffffff22; }

      .content-wrapper { flex:1; display:flex; justify-content:center; align-items:center; transition:.3s; padding:20px; }
      .no-bg { background:transparent !important; padding:0 !important; border-radius:0; box-shadow:none; backdrop-filter:none; }
      .content { width:100%; max-width:900px; padding:40px; backdrop-filter:blur(6px); background:rgba(255,255,255,0.7); border-radius:8px; display:flex; flex-direction:column; align-items:center; }
      
      .card-container { display:flex; gap:30px; flex-wrap:wrap; justify-content:center; width:100%; }
      .card { flex:1; min-width:200px; max-width:250px; height:160px; border-radius:16px; color:white; padding:20px; text-align:center; background:linear-gradient(135deg, #1a263f, #345670); box-shadow:0 10px 25px rgba(0,0,0,0.2); transition:transform 0.2s; display:flex; flex-direction:column; justify-content:center; align-items:center; position:relative; overflow:hidden; cursor:pointer; }
      .clickable-card:hover { transform: scale(1.03); }
      .card-dark { background:linear-gradient(135deg, #1A263F, #2C3E50); }
      .card-medium { background:linear-gradient(135deg, #345670, #1A263F); }
      .card-light { background:linear-gradient(135deg, #6997A9, #345670); }

      .card-value { font-size:48px; font-weight:bold; }
      .card-icon { font-size:42px; }

      .new-msg-card { background:white; padding:30px; border-radius:12px; box-shadow:0 8px 20px rgba(0,0,0,0.15); width:100%; max-width:650px; margin:auto; }
      .new-msg-card .card-header { background:#1A263F; color:white; padding:10px 15px; border-radius:8px 8px 0 0; margin:-30px -30px 20px -30px; text-align:center; font-size:18px; }
      .create-btn { background:#1A263F !important; color:white !important; border:none !important; padding:12px 30px !important; border-radius:8px !important; font-size:16px !important; display:inline-flex !important; align-items:center !important; gap:10px; transition:0.2s; cursor:pointer; }

      /* RIGHT-SIDE POSITIONING FOR PROFILE OPTIONS */
      .right-modal .modal-dialog { position: fixed; margin: 0; right: 20px; top: 75px; width: 220px; }
      .right-modal .modal-content { border-radius: 12px; box-shadow: 0 5px 15px rgba(0,0,0,0.2); border: none; }
      
      .profile-opt-btn { width: 100%; text-align: left; padding: 12px 20px; border: none; background: white; font-size: 16px; cursor: pointer; display: flex; align-items: center; gap: 15px; transition: 0.2s; border-radius: 8px; }
      .profile-opt-btn:hover { background: #f0f2f5; }
      
      /* EDIT PROFILE BOX (Centered by default modal behavior) */
      .edit-profile-box { background: #ADD8E6; padding: 30px; border-radius: 15px; text-align: center; }
      .profile-pic-large { width: 130px; height: 130px; border-radius: 50%; border: 4px solid white; margin-bottom: 10px; object-fit: cover; }
    "))
  ),
  
  uiOutput("main_ui")
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  logged_in <- reactiveVal(FALSE)
  current_tab <- reactiveVal("dashboard")
  
  output$main_ui <- renderUI({
    if(!logged_in()) login_ui() else dashboard_ui(current_tab())
  })
  
  # Login Logic
  observeEvent(input$login, {
    output$username_error <- renderText("")
    output$password_error <- renderText("")
    username <- trimws(input$username)
    password <- trimws(input$password)
    
    if(username=="") { output$username_error <- renderText("Username cannot be blank!"); return() }
    if(password=="") { output$password_error <- renderText("Password cannot be blank!"); return() }
    if(username!="admin") { output$username_error <- renderText("Wrong username!"); return() }
    if(password!="admin123") { output$password_error <- renderText("Wrong password!"); return() }
    
    logged_in(TRUE)
  })
  
  # Navigation
  observeEvent(input$create_msg, { current_tab("new_message") })
  observeEvent(input$tab_dashboard_click, { current_tab("dashboard") })
  observeEvent(input$tab_new_message_click, { current_tab("new_message") })
  
  # 1. SHOW PROFILE OPTIONS
  observeEvent(input$profile_clicked, {
    showModal(div(class="right-modal", 
                  modalDialog(
                    title = NULL,
                    easyClose = TRUE,
                    footer = NULL,
                    div(style = "padding: 5px 0;",
                        actionButton("go_edit", label = div(icon("edit"), "Edit"), class = "profile-opt-btn"),
                        actionButton("logout_trigger", label = div(icon("sign-out-alt"), "Log out"), class = "profile-opt-btn")
                    )
                  )
    ))
  })
  
  # 2. SHOW EDIT PROFILE MODAL
  observeEvent(input$go_edit, {
    removeModal()
    showModal(modalDialog(
      title = div(style="background:#1A263F; color:white; padding:15px; margin:-15px -15px 15px -15px; border-radius:5px 5px 0 0;", "Edit Profile"),
      size = "m",
      easyClose = TRUE,
      footer = div(
        style = "display: flex; justify-content: flex-end; gap: 10px;",
        modalButton("Cancel"),
        actionButton("save_changes", "Save Changes", style="background:#1A263F; color:white; border:none; padding:8px 20px; border-radius:5px;")
      ),
      
      div(class = "edit-profile-box",
          img(src="https://i.pravatar.cc/150?u=admin", class="profile-pic-large"),
          h2("Admin", style="margin: 5px 0; font-weight: bold; color: #1A263F;"),
          
          div(style="margin-top: 15px; text-align: left; max-width: 300px; margin-left: auto; margin-right: auto;",
              p("Choose New Profile Picture", style="font-weight:bold; margin-bottom:5px; text-align:center;"),
              fileInput("new_pic", label = NULL, width = "100%"),
              p("Selected file will update instantly.", style="font-size: 11px; color: #555; text-align: center; margin-top:-10px;")
          )
      )
    ))
  })
  
  # --- Cropper logic ---
  observe({
    req(input$new_pic)
    encoded <- dataURI(file = input$new_pic$datapath, mime="image/png")
    shinyjs::runjs(sprintf("
      var imgTag = document.querySelector('.edit-profile-box img');
      imgTag.src = '%s';
      if(window.cropper) window.cropper.destroy();
      window.cropper = new Cropper(imgTag, { aspectRatio: 1, viewMode: 1, autoCropArea: 1 });
    ", encoded))
  })
  
  observeEvent(input$save_changes, {
    shinyjs::runjs("
      if(window.cropper){
        window.cropper.getCroppedCanvas().toBlob(function(blob){
          var reader = new FileReader();
          reader.onloadend = function() {
            Shiny.setInputValue('cropped_img', reader.result, {priority: 'event'});
          }
          reader.readAsDataURL(blob);
        });
      } else {
        Shiny.setInputValue('cropped_img', null, {priority: 'event'});
      }
    ")
  })
  
  observeEvent(input$cropped_img, {
    removeModal()
    showNotification("Profile updated successfully!", type="message")
    if(!is.null(input$cropped_img)){
      shinyjs::runjs(sprintf("
        document.getElementById('user_profile_btn').src = '%s';
      ", input$cropped_img))
    }
  })
  
  # Logout
  observeEvent(input$logout_trigger, {
    removeModal()
    logged_in(FALSE)
    updateTextInput(session, "username", value = "")
    updatePasswordInput(session, "password", value = "")
  })
  
  # Lock Message Logic
  observeEvent(input$lock_msg, {
    output$msg_title_error <- renderText("")
    output$msg_emotion_error <- renderText("")
    output$msg_date_error <- renderText("")
    output$msg_body_error <- renderText("")
    
    valid <- TRUE
    if(trimws(input$msg_title)=="") { output$msg_title_error <- renderText("Title is required!"); valid <- FALSE }
    if(input$msg_emotion=="") { output$msg_emotion_error <- renderText("Please select an emotion!"); valid <- FALSE }
    if(is.null(input$msg_date)) { output$msg_date_error <- renderText("Please select a date!"); valid <- FALSE }
    if(trimws(input$msg_body)=="") { output$msg_body_error <- renderText("Message cannot be blank!"); valid <- FALSE }
    
    if(valid){
      showModal(modalDialog(title="Success", "Message locked into legacy!", easyClose=TRUE, footer=NULL))
      updateTextInput(session, "msg_title", value="")
      updateSelectizeInput(session, "msg_emotion", selected="")
      updateDateInput(session, "msg_date", value=Sys.Date())
      updateTextAreaInput(session, "msg_body", value="")
    }
  })
}

# ---------------- RUN APP ----------------
shinyApp(ui, server)
