library(shiny)
library(shinyjs)
library(fontawesome)
library(base64enc)
library(RSQLite)
library(DBI)

# ---------------- DATABASE SETTINGS ----------------
db_path <- "DLK_Database.db"

init_db <- function() {
  con <- dbConnect(SQLite(), db_path)
  # Main Table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS legacy_messages (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      Title TEXT,
      Emotion TEXT,
      Date TEXT,
      Status TEXT,
      Body TEXT
    )
  ")
  # Archive Table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS archived_messages (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      Original_ID INTEGER,
      Title TEXT,
      Emotion TEXT,
      Date TEXT,
      Status TEXT,
      Body TEXT
    )
  ")
  # User Profile Table for permanent storage
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_profile (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      Username TEXT UNIQUE,
      ProfilePicture TEXT
    )
  ")
  # Initialize default admin profile if not exists
  existing_profile <- dbGetQuery(con, "SELECT COUNT(*) as count FROM user_profile WHERE Username = 'admin'")$count
  if (existing_profile == 0) {
    dbExecute(con, "
      INSERT INTO user_profile (Username, ProfilePicture) 
      VALUES ('admin', 'https://i.pravatar.cc/150?u=admin')
    ")
  }
  dbDisconnect(con)
}

get_db_data <- function(query) {
  con <- dbConnect(SQLite(), db_path)
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

execute_query <- function(query, params = NULL) {
  con <- dbConnect(SQLite(), db_path)
  dbExecute(con, query, params = params)
  dbDisconnect(con)
}

# ---------------- PROFILE PICTURE MANAGEMENT ----------------
get_profile_picture <- function(username = "admin") {
  con <- dbConnect(SQLite(), db_path)
  result <- dbGetQuery(con, "SELECT ProfilePicture FROM user_profile WHERE Username = ?", params = list(username))
  dbDisconnect(con)
  
  if (nrow(result) > 0 && !is.na(result$ProfilePicture[1]) && result$ProfilePicture[1] != "") {
    return(result$ProfilePicture[1])
  } else {
    return("https://i.pravatar.cc/150?u=admin")
  }
}

save_profile_picture <- function(base64_image, username = "admin") {
  con <- dbConnect(SQLite(), db_path)
  # Update or insert profile picture
  dbExecute(con, "
    INSERT OR REPLACE INTO user_profile (Username, ProfilePicture) 
    VALUES (?, ?)
  ", params = list(username, base64_image))
  dbDisconnect(con)
}

# ---------------- UI COMPONENTS ----------------

login_ui <- function() {
  div(class = "page",
      div(class = "login-panel",
          h1(style="margin-bottom:5px;", "Digital Legacy Keeper"),
          div(class = "subtitle", style="margin-bottom:35px;", "Secure access to your digital heritage"),
          div(class = "field",
              tags$label("Username"),
              textInput("username", label = NULL, placeholder = "Enter username", width = "100%"),
              div(textOutput("username_error"), style="color:red;font-size:13px;height:18px;margin-top:6px;")
          ),
          div(class = "field", style="position:relative;",
              tags$label("Password"),
              passwordInput("password", label = NULL, placeholder = "Enter password", width = "100%"),
              tags$span(id="toggle_eye", style="position:absolute; right:12px; top:45%; transform: translateY(-50%); cursor:pointer;", icon("eye")),
              div(textOutput("password_error"), style="color:red;font-size:13px;height:18px;margin-top:6px;")
          ),
          div(style="height:10px;"),
          actionButton("login", "UNLOCK VAULT"),
          div(class = "footer", HTML("© 2025 Digital Legacy Keeper System<br>Rizza Constantino_BSIT 4-1"))
      )
  )
}

dashboard_ui <- function(tab = "dashboard", counts = list(total=0, locked=0, released=0), profile_pic = NULL, upcoming_message = NULL) {
  emotions <- c("Happy","Joyful","Excited","Content","Hopeful","Grateful","Loving","Peaceful","Proud","Inspired","Playful","Confident","Relaxed","Cheerful","Amused","Sad","Angry","Frustrated","Lonely","Anxious","Guilty","Jealous","Hopeless","Stressed","Overwhelmed","Regretful","Disappointed","Hurt","Confused","Insecure","Nostalgic","Reflective","Curious","Thoughtful","Surprised","Conflicted","Sentimental","Ambivalent","Shocked","Contemplative")
  
  # Get upcoming message display
  upcoming_display <- if (!is.null(upcoming_message) && nrow(upcoming_message) > 0) {
    div(
      h4(upcoming_message$Title[1], style="color:#1A263F; font-weight:bold; margin-bottom:8px; text-align:center;"),
      p(paste("Scheduled:", upcoming_message$Date[1]), style="color:#345670; font-size:14px; text-align:center; margin:0;"),
      p(paste("Status:", upcoming_message$Status[1]), style="color:#345670; font-size:14px; text-align:center; margin-top:4px;")
    )
  } else {
    p("Stay tuned for your next legacy.", style="color:#1A263F; font-size:16px; text-align:center;")
  }
  
  div(class = "dashboard-container",
      div(class = "header",
          div(class = "header-left",
              span(class="sidebar-toggle", icon("bars")),
              img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTsC3DWWLFdr8NuRx8Czhb33s8L6yi7E6qrpQ&s", class = "logo-circle"),
              span("DIGITAL LEGACY KEEPER SYSTEM", style="font-size:22px;font-weight:bold;")
          ),
          div(class = "header-right",
              span("Admin", style="margin-right:10px;"),
              img(src=profile_pic, class="profile-img", id="user_profile_btn", style="cursor:pointer;")
          )
      ),
      div(style="display:flex; flex:1;",
          div(class="sidebar", id="sidebar",
              div(class=paste("nav-item", if(tab=="dashboard") "active" else ""), icon("th-large"), "Dashboard", id="tab_dashboard"),
              div(class=paste("nav-item", if(tab=="new_message") "active" else ""), icon("comment-alt"), "New Message", id="tab_new_message"),
              div(class=paste("nav-item", if(tab=="view_all") "active" else ""), icon("play"), "View All Messages", id="tab_view_all"),
              div(class=paste("nav-item", if(tab=="released") "active" else ""), icon("unlock"), "Released Messages", id="tab_released"),
              div(class=paste("nav-item", if(tab=="locked") "active" else ""), icon("lock"), "Locked Messages", id="tab_locked"),
              div(class=paste("nav-item", if(tab=="archives") "active" else ""), icon("archive"), "Archives", id="tab_archives")
          ),
          div(class="content-wrapper centered", id="contentWrapper",
              div(class=paste0("content ", if(tab %in% c("new_message", "view_all", "released", "locked", "archives")) "no-bg" else ""),
                  if(tab == "dashboard"){
                    tagList(
                      h2("DASHBOARD", style="text-align:center;"),
                      div(class="card-container",
                          div(class="card card-dark clickable-card", div(style="display:flex; align-items:center; justify-content:center; gap:12px;", div(class="card-value", counts$total), div(class="card-icon", icon("users"))), div(class="card-title","TOTAL ACTIVE")),
                          div(class="card card-medium clickable-card", div(style="display:flex; align-items:center; justify-content:center; gap:12px;", div(class="card-value", counts$locked), div(class="card-icon", icon("lock"))), div(class="card-title","LOCKED")),
                          div(class="card card-light clickable-card", div(style="display:flex; align-items:center; justify-content:center; gap:12px;", div(class="card-value", counts$released), div(class="card-icon", icon("check-circle"))), div(class="card-title","RELEASED"))
                      ),
                      h3("UPCOMING LEGACY MESSAGE", style="margin-top:40px;font-weight:bold; text-align:center;"),
                      div(class="upcoming-box", style="background:#ffffffaa; border:2px dashed #1A263F; border-radius:12px; padding:20px; min-height:80px; max-height:120px; overflow-y:auto; width:100%; max-width:700px; margin: 0 auto; display:flex; align-items:center; justify-content:center;", 
                          upcoming_display
                      ),
                      div(style="margin-top:20px; text-align:center;", actionButton("create_msg", label=div(icon("plus"),"CREATE MESSAGE"), class="create-btn"))
                    )
                  } else if(tab == "new_message"){
                    tagList(
                      div(class="new-msg-card",
                          div(class="card-header", h2(icon("comment-alt"), "Create New Legacy Message")),
                          div(style="display:flex; gap:10px; flex-wrap:wrap; margin-bottom:15px;",
                              div(style="flex:1;", textInput("msg_title", "Title", placeholder = "Enter message title...", width = "100%"), div(textOutput("msg_title_error"), style="color:red;font-size:11px;min-height:15px;")),
                              div(style="flex:1;", selectizeInput("msg_emotion", "Emotion", choices = emotions, width = "100%", options = list(create = TRUE, placeholder = "Select emotion...")), div(textOutput("msg_emotion_error"), style="color:red;font-size:11px;min-height:15px;"))
                          ),
                          div(style="margin-bottom:15px; position:relative; display:flex; flex-direction:column; align-items:center;",
                              tags$label("Released Date", style="width:100%; margin-bottom:5px; font-weight:bold;"),
                              div(style="width:100%; max-width:300px;",
                                  dateInput("msg_date", label = NULL, width = "100%"),
                                  div(textOutput("msg_date_error"), style="color:red;font-size:11px;min-height:15px;")
                              )
                          ),
                          div(style="margin-bottom:15px;", textAreaInput("msg_body", "Message", width="100%", height="180px", placeholder = "Write your legacy message here..."), div(textOutput("msg_body_error"), style="color:red;font-size:11px;min-height:15px;")),
                          div(style="text-align:center; margin-top:10px;", actionButton("lock_msg", label=div(icon("lock"),"Lock into Legacy"), class="create-btn"))
                      )
                    )
                  } else {
                    title_text <- switch(tab, "view_all" = "ALL ACTIVE LEGACY RECORDS", "released" = "RELEASED MESSAGES", "locked" = "LOCKED MESSAGES", "archives" = "ARCHIVES")
                    tagList(
                      div(class="view-all-container",
                          h2(title_text, style="text-align:center; color:#1A263F; font-weight:bold; margin-bottom:30px;"),
                          uiOutput("messages_table_ui")
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
      $(document).on('click', '#tab_view_all', function() { Shiny.setInputValue('tab_view_all_click', Math.random()); });
      $(document).on('click', '#tab_released', function() { Shiny.setInputValue('tab_released_click', Math.random()); });
      $(document).on('click', '#tab_locked', function() { Shiny.setInputValue('tab_locked_click', Math.random()); });
      $(document).on('click', '#tab_archives', function() { Shiny.setInputValue('tab_archives_click', Math.random()); });
      $(document).on('click', '#user_profile_btn', function() { Shiny.setInputValue('profile_clicked', Math.random()); });
      $(document).on('click', '.sidebar-toggle', function () { $('#sidebar').toggleClass('collapsed'); });
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
      
      // Function to update cropped image preview
      function updateCroppedPreview(base64Image) {
        $('#cropped_preview_img').attr('src', base64Image);
        $('#cropped_preview_container').show();
        $('#crop_instruction').hide();
      }
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
      .dashboard-container { display:flex; flex-direction:column; height:100vh; background-image:url('https://images.steamusercontent.com/ugc/836957960630326440/A7A5DA84E434D11499AAB35119C25A268D160D0C/?imw=5000&imh=5000&ima=fit&impolicy=Letterbox&imcolor=%23000000&letterbox=false'); background-size:cover; background-position:center; }
      .header { height:70px; background:rgba(95,150,168,0.9); display:flex; justify-content:space-between; align-items:center; padding:0 30px; color:white; }
      .header-left { display:flex; align-items:center; }
      .sidebar-toggle { cursor:pointer; font-size:22px; margin-right:10px; }
      .logo-circle { width:45px; height:45px; border-radius:50%; object-fit:cover; border:2px solid white; margin:0 15px; }
      .profile-img { width:40px; height:40px; border-radius:50%; border:2px solid white; object-fit: cover; }
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
      .view-all-container { background:white; padding:30px; border-radius:12px; box-shadow:0 8px 20px rgba(0,0,0,0.15); width:95%; max-width:1000px; margin:auto; }
      .legacy-table { width:100%; border-collapse: collapse; border: 1px solid #1A263F; border-radius: 8px; overflow: hidden; }
      .legacy-table th { background: #A9D1D1; color: #1A263F; padding: 12px; text-align: center; border: 1px solid #1A263F; }
      .legacy-table td { padding: 10px; text-align: center; border: 1px solid #1A263F; color: #333; }
      .status-released { background: #1A263F; color: white; padding: 4px 12px; border-radius: 15px; font-size: 14px; }
      .status-locked { background: #A9D1D1; color: #1A263F; padding: 4px 12px; border-radius: 15px; font-size: 14px; }
      .btn-open { background: #5BC0DE; color: white; border: none; padding: 4px 15px; border-radius: 12px; font-size: 12px; cursor: pointer; }
      .action-icons i { cursor: pointer; margin: 0 5px; font-size: 18px; }
      .icon-edit { color: #5CB85C; }
      .icon-delete { color: #D9534F; }
      .icon-restore { color: #1A263F; }
      .legacy-pop-container { padding: 20px; background: #FFF8F0; border-radius: 15px; border: 1px solid #CCC; }
      .pop-header { background: #A9D1D1; padding: 15px; border-radius: 10px; text-align: center; margin-bottom: 20px; border: 1px solid #88A; }
      .pop-header h3 { margin: 0; color: #1A263F; font-weight: bold; letter-spacing: 1px; }
      .pop-meta { display: flex; justify-content: space-between; border: 1px solid #1A263F; border-radius: 10px; padding: 10px 15px; margin-bottom: 20px; background: white; }
      .pop-meta span { font-weight: bold; color: #333; }
      .pop-body { border: 1px solid #1A263F; border-radius: 10px; padding: 15px; min-height: 200px; background: white; font-size: 16px; }
      .btn-pop-close { background: black; color: white; border: none; padding: 8px 25px; border-radius: 10px; cursor: pointer; margin-top: 20px; font-weight: bold; }
      .update-modal-body { background-color: #7DB0BC; padding: 30px; border-radius: 10px; color: black; }
      .update-modal-body h2 { font-weight: bold; text-align: center; margin-bottom: 25px; }
      .update-field { display: flex; align-items: center; margin-bottom: 15px; }
      .update-field label { width: 130px; font-weight: bold; font-size: 17px; }
      .update-btn-container { display: flex; justify-content: center; gap: 15px; margin-top: 20px; }
      .btn-cancel { background: black; color: white; border: none; padding: 6px 25px; border-radius: 10px; font-weight: bold; cursor:pointer; }
      .btn-update { background: #345670; color: white; border: none; padding: 6px 25px; border-radius: 10px; font-weight: bold; cursor:pointer; }
      .modal-dialog { display: flex; align-items: center; justify-content: center; min-height: 100vh; margin: 0 auto !important; }
      .modal-content { border-radius: 12px; box-shadow: 0 5px 15px rgba(0,0,0,0.2); border: none; width: 100%; }
      .right-aligned-modal .modal-dialog { position: fixed; right: 20px; top: 75px; min-height: auto; margin: 0; width: 220px; align-items: flex-start; }
      .profile-opt-btn { width: 100%; text-align: left; padding: 12px 20px; border: none; background: white; font-size: 16px; cursor: pointer; display: flex; align-items: center; gap: 15px; transition: 0.2s; border-radius: 8px; }
      .profile-opt-btn:hover { background: #f0f2f5; }
      .edit-profile-box { background: #f0f8ff; padding: 25px; border-radius: 15px; text-align: center; border: 1px solid #d1e3f0; min-height: 400px; display: flex; flex-direction: column; justify-content: center; align-items: center; }
      .crop-container { width: 100%; height: 300px; background-color: #e9ecef; border-radius: 8px; overflow: hidden; display: flex; justify-content: center; align-items: center; }
      .crop-container img { max-width: 100%; display: block; }
      .profile-pic-large { width: 160px; height: 160px; border-radius: 50%; border: 4px solid white; margin-bottom: 20px; object-fit: cover; box-shadow: 0 4px 10px rgba(0,0,0,0.15); }
      .cropper-line, .cropper-point { background-color: #1A263F; }
      .cropper-view-box { outline-color: #1A263F; border-radius: 50%; }
      
      /* Cropped Preview Container */
      .cropped-preview-container { 
        display: none; 
        margin-top: 20px; 
        text-align: center; 
        border: 2px dashed #1A263F; 
        border-radius: 10px; 
        padding: 15px; 
        background-color: #f8f9fa;
      }
      .cropped-preview-img { 
        width: 150px; 
        height: 150px; 
        border-radius: 50%; 
        object-fit: cover; 
        border: 3px solid #1A263F;
        margin: 0 auto 10px auto;
      }
      .preview-label { 
        font-weight: bold; 
        color: #1A263F; 
        margin-bottom: 10px; 
        font-size: 16px;
      }
      
      /* Calendar positioning styles */
      .datepicker { display: flex; justify-content: center; }
      .datepicker .form-control { text-align: center; }
      .datepicker .dropdown-menu { position: fixed !important; transform: translateX(-50%) !important; left: 50% !important; top: auto !important; margin-top: 10px !important; }
      
      /* Confirmation Modal Styling - Transparent Background */
      .confirmation-modal { 
        background: transparent !important; 
        border-radius: 12px; 
        padding: 0 !important; 
        box-shadow: none !important; 
        max-width: 450px; 
        margin: 0 auto; 
      }
      .confirmation-title { 
        background: rgba(26, 38, 63, 0.95) !important; 
        color: white; 
        padding: 20px; 
        border-radius: 12px 12px 0 0; 
        text-align: center; 
        margin: 0; 
        font-size: 20px; 
        font-weight: bold; 
        backdrop-filter: blur(5px);
      }
      .confirmation-content { 
        text-align: center; 
        padding: 30px 20px; 
        background: rgba(255, 255, 255, 0.95) !important; 
        backdrop-filter: blur(10px);
        border-radius: 0 0 12px 12px;
      }
      .confirmation-message { 
        font-size: 18px; 
        color: #1A263F; 
        line-height: 1.5; 
        margin-bottom: 10px; 
        font-weight: 500;
      }
      .confirmation-submessage { 
        font-size: 15px; 
        color: #666; 
        font-style: italic; 
        margin-top: 10px; 
        line-height: 1.4;
      }
      .confirmation-buttons { 
        display: flex; 
        justify-content: center; 
        gap: 20px; 
        margin-top: 30px; 
      }
      .btn-confirm-cancel { 
        background: rgba(169, 209, 209, 0.9) !important; 
        color: #1A263F !important; 
        border: 2px solid #A9D1D1 !important; 
        padding: 12px 30px; 
        border-radius: 8px; 
        font-weight: bold; 
        cursor: pointer; 
        min-width: 120px; 
        backdrop-filter: blur(5px);
      }
      .btn-confirm-action { 
        background: rgba(26, 38, 63, 0.9) !important; 
        color: white !important; 
        border: 2px solid #1A263F !important; 
        padding: 12px 30px; 
        border-radius: 8px; 
        font-weight: bold; 
        cursor: pointer; 
        min-width: 120px; 
        backdrop-filter: blur(5px);
      }
      .btn-confirm-delete { 
        background: rgba(217, 83, 79, 0.9) !important; 
        color: white !important; 
        border: 2px solid #D9534F !important; 
        padding: 12px 30px; 
        border-radius: 8px; 
        font-weight: bold; 
        cursor: pointer; 
        min-width: 120px; 
        backdrop-filter: blur(5px);
      }
      .btn-confirm-restore { 
        background: rgba(92, 184, 92, 0.9) !important; 
        color: white !important; 
        border: 2px solid #5CB85C !important; 
        padding: 12px 30px; 
        border-radius: 8px; 
        font-weight: bold; 
        cursor: pointer; 
        min-width: 120px; 
        backdrop-filter: blur(5px);
      }
      .btn-confirm-update-profile { 
        background: rgba(91, 192, 222, 0.9) !important; 
        color: white !important; 
        border: 2px solid #5BC0DE !important; 
        padding: 12px 30px; 
        border-radius: 8px; 
        font-weight: bold; 
        cursor: pointer; 
        min-width: 150px; 
        backdrop-filter: blur(5px);
      }
      .btn-confirm-logout { 
        background: rgba(217, 83, 79, 0.9) !important; 
        color: white !important; 
        border: 2px solid #D9534F !important; 
        padding: 12px 30px; 
        border-radius: 8px; 
        font-weight: bold; 
        cursor: pointer; 
        min-width: 120px; 
        backdrop-filter: blur(5px);
      }
      
      /* Notification styling - position at top */
      .shiny-notification {
        position: fixed;
        top: 80px;
        right: 20px;
        width: 350px;
        z-index: 9999;
        border-radius: 10px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255,255,255,0.2);
      }
      
      .shiny-notification-message {
        background: rgba(26, 38, 63, 0.95) !important;
        color: white !important;
        border: 2px solid rgba(169, 209, 209, 0.5) !important;
        padding: 15px 20px !important;
        border-radius: 10px !important;
        font-size: 15px !important;
        font-weight: 500 !important;
      }
      
      .shiny-notification-close {
        color: white !important;
        opacity: 0.8;
      }
      
      .shiny-notification-close:hover {
        opacity: 1;
      }
      
      /* Success notification */
      .shiny-notification-message[style*='background-color: #1A263F'],
      .shiny-notification-message[style*='background-color:#1A263F'] {
        background: rgba(26, 38, 63, 0.95) !important;
        border-color: rgba(169, 209, 209, 0.5) !important;
      }
      
      /* Warning notification */
      .shiny-notification-message[style*='background-color: #f89406'] {
        background: rgba(248, 148, 6, 0.95) !important;
        border-color: rgba(255, 193, 7, 0.5) !important;
      }
      
      /* Error notification */
      .shiny-notification-message[style*='background-color: #d9534f'] {
        background: rgba(217, 83, 79, 0.95) !important;
        border-color: rgba(255, 107, 107, 0.5) !important;
      }
      
      @keyframes bounce { 0%, 20%, 50%, 80%, 100% { transform: translateY(0); } 40% { transform: translateY(-10px); } 60% { transform: translateY(-5px); } }
      @keyframes pulse { 0% { transform: scale(1); opacity: 1; } 50% { transform: scale(1.2); opacity: 0.7; } 100% { transform: scale(1); opacity: 1; } }
      @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
      .card-dark .card-icon { animation: bounce 2s infinite; }
      .card-medium .card-icon { animation: pulse 1.5s infinite; }
      .card-light .card-icon { animation: spin 3s linear infinite; }
    "))
  ),
  uiOutput("main_ui")
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  init_db()
  
  logged_in <- reactiveVal(FALSE)
  current_tab <- reactiveVal("dashboard")
  temp_cropped_res <- reactiveVal(NULL)
  db_trigger <- reactiveVal(0)
  profile_trigger <- reactiveVal(0)
  
  # Store temporary data for confirmations
  edit_msg_id_temp <- reactiveVal(NULL)
  archive_msg_id_temp <- reactiveVal(NULL)
  restore_msg_id_temp <- reactiveVal(NULL)
  
  # Get profile picture from database
  current_profile_pic <- reactive({
    profile_trigger()
    if (logged_in()) {
      return(get_profile_picture("admin"))
    } else {
      return("https://i.pravatar.cc/150?u=admin")
    }
  })
  
  # Get upcoming message (nearest future released message)
  get_upcoming_message <- reactive({
    db_trigger()
    # Get the nearest future message that is still locked
    upcoming <- get_db_data("
      SELECT * FROM legacy_messages 
      WHERE Status = 'Locked' 
      AND Date > DATE('now') 
      ORDER BY Date ASC 
      LIMIT 1
    ")
    
    if (nrow(upcoming) == 0) {
      # If no future locked messages, get the most recent released message
      upcoming <- get_db_data("
        SELECT * FROM legacy_messages 
        WHERE Status = 'Released' 
        ORDER BY Date DESC 
        LIMIT 1
      ")
    }
    
    return(upcoming)
  })
  
  dashboard_counts <- reactive({
    db_trigger()
    total <- get_db_data("SELECT COUNT(*) as count FROM legacy_messages")$count
    locked <- get_db_data("SELECT COUNT(*) as count FROM legacy_messages WHERE Status = 'Locked'")$count
    released <- get_db_data("SELECT COUNT(*) as count FROM legacy_messages WHERE Status = 'Released'")$count
    list(total = total, locked = locked, released = released)
  })
  
  output$main_ui <- renderUI({ 
    if(!logged_in()) { 
      login_ui() 
    } else { 
      dashboard_ui(current_tab(), 
                   dashboard_counts(), 
                   current_profile_pic(),
                   get_upcoming_message()) 
    }
  })
  
  observeEvent(input$login, {
    output$username_error <- renderText(""); output$password_error <- renderText("")
    username <- trimws(input$username); password <- trimws(input$password)
    if(username=="") { output$username_error <- renderText("Username cannot be blank!"); return() }
    if(password=="") { output$password_error <- renderText("Password cannot be blank!"); return() }
    if(username!="admin" || password!="admin123") { output$password_error <- renderText("Invalid credentials!"); return() }
    logged_in(TRUE)
    # Trigger profile picture load
    profile_trigger(profile_trigger() + 1)
  })
  
  observeEvent(input$create_msg, { current_tab("new_message") })
  observeEvent(input$tab_dashboard_click, { current_tab("dashboard") })
  observeEvent(input$tab_new_message_click, { current_tab("new_message") })
  observeEvent(input$tab_view_all_click, { current_tab("view_all") })
  observeEvent(input$tab_released_click, { current_tab("released") })
  observeEvent(input$tab_locked_click, { current_tab("locked") })
  observeEvent(input$tab_archives_click, { current_tab("archives") })
  
  output$messages_table_ui <- renderUI({
    db_trigger()
    
    if(current_tab() == "archives") {
      df <- get_db_data("SELECT * FROM archived_messages")
    } else {
      query <- switch(current_tab(),
                      "view_all" = "SELECT * FROM legacy_messages",
                      "released" = "SELECT * FROM legacy_messages WHERE Status = 'Released'",
                      "locked"   = "SELECT * FROM legacy_messages WHERE Status = 'Locked'",
                      "SELECT * FROM legacy_messages")
      df <- get_db_data(query)
    }
    
    if(nrow(df) == 0) return(p("No records found.", style="text-align:center; padding: 20px;"))
    
    rows <- lapply(1:nrow(df), function(i) {
      status_class <- if(df$Status[i] == "Released") "status-released" else "status-locked"
      
      # ACTION LOGIC
      if(current_tab() == "archives") {
        action_ui <- div(class="action-icons",
                         icon("undo", class="icon-restore", title="Restore Message",
                              onclick = sprintf("Shiny.setInputValue('restore_msg_id', %d, {priority: 'event'});", df$ID[i])))
      } else if(df$Status[i] == "Released") {
        action_ui <- tags$button(class="btn-open", "Open", 
                                 onclick = sprintf("Shiny.setInputValue('open_msg_id', %d, {priority: 'event'});", df$ID[i]))
      } else {
        # Locked Messages can be Edited or Deleted (Archive)
        action_ui <- div(class="action-icons",
                         icon("edit", class="icon-edit", 
                              onclick = sprintf("Shiny.setInputValue('edit_msg_id', %d, {priority: 'event'});", df$ID[i])),
                         icon("archive", class="icon-delete",
                              onclick = sprintf("Shiny.setInputValue('archive_msg_id', %d, {priority: 'event'});", df$ID[i]))
        )
      }
      
      tags$tr(
        tags$td(if(current_tab() == "archives") df$Original_ID[i] else df$ID[i]),
        tags$td(df$Title[i]),
        tags$td(df$Emotion[i]),
        tags$td(df$Date[i]),
        tags$td(span(class=status_class, df$Status[i])),
        tags$td(action_ui)
      )
    })
    
    tags$table(class="legacy-table",
               tags$thead(
                 tags$tr(tags$th("ID"), tags$th("TITLE"), tags$th("EMOTION"), tags$th("DATE"), tags$th("STATUS"), tags$th("ACTION"))
               ),
               tags$tbody(rows)
    )
  })
  
  # --- AUTO STATUS UPDATE OBSERVER ---
  observe({
    # Trigger on relevant events
    db_trigger()
    input$close_modal
    input$save_update
    input$lock_msg
    
    # Get current date
    today <- as.character(Sys.Date())
    
    # Update messages where Date <= today to "Released"
    execute_query(
      "UPDATE legacy_messages SET Status = 'Released' WHERE Date <= ? AND Status != 'Released'",
      params = list(today)
    )
    
    # Update messages where Date > today to "Locked"
    execute_query(
      "UPDATE legacy_messages SET Status = 'Locked' WHERE Date > ? AND Status != 'Locked'",
      params = list(today)
    )
    
    # Update archive table similarly (for consistency if restored)
    execute_query(
      "UPDATE archived_messages SET Status = 'Released' WHERE Date <= ? AND Status != 'Released'",
      params = list(today)
    )
    
    execute_query(
      "UPDATE archived_messages SET Status = 'Locked' WHERE Date > ? AND Status != 'Locked'",
      params = list(today)
    )
  })
  
  # --- EDIT MESSAGE (with confirmation) ---
  observeEvent(input$edit_msg_id, {
    edit_msg_id_temp(input$edit_msg_id)
    showConfirmationModal(
      title = "Confirm Update",
      message = "Are you sure you want to update this information?",
      submessage = "Please review your changes before proceeding.",
      action = "update",
      action_button = "Update"
    )
  })
  
  # Handle update confirmation
  observeEvent(input$confirm_update, {
    removeModal()
    msg <- get_db_data(paste0("SELECT * FROM legacy_messages WHERE ID = ", edit_msg_id_temp()))
    showModal(modalDialog(title = NULL, footer = NULL, size = "m", easyClose = TRUE,
                          div(class = "update-modal-body", h2("LEGACY MESSAGE"),
                              div(class = "update-field", tags$label("Title:"), textInput("edit_title", label = NULL, value = msg$Title, width = "100%")),
                              div(class = "update-field", tags$label("Emotion:"), textInput("edit_emotion_hide", label = NULL, value = "********", width = "100%")),
                              div(class = "update-field", tags$label("Released Date:"), dateInput("edit_date", label = NULL, value = msg$Date, width = "100%")),
                              div(class = "update-field", style = "align-items: flex-start;", tags$label("Message:"), textAreaInput("edit_body_hide", label = NULL, value = "\n\n 📬This message will unlock when the time comes! \n\n", width = "100%", height = "120px")),
                              div(class = "update-btn-container", 
                                  tags$button(class = "btn-cancel", "Cancel", onclick = "Shiny.setInputValue('close_modal', Math.random())"), 
                                  actionButton("save_update_final", "Update", class = "btn-update"))
                          )
    ))
    shinyjs::disable("edit_emotion_hide"); shinyjs::disable("edit_body_hide")
  })
  
  # Save update after confirmation
  observeEvent(input$save_update_final, {
    execute_query("UPDATE legacy_messages SET Title = ?, Date = ? WHERE ID = ?", 
                  params = list(input$edit_title, as.character(input$edit_date), edit_msg_id_temp()))
    db_trigger(db_trigger() + 1); 
    removeModal(); 
    showNotification("The information has been updated successfully.", type = "message")
  })
  
  # --- ARCHIVE LOGIC (Delete with confirmation) ---
  observeEvent(input$archive_msg_id, {
    archive_msg_id_temp(input$archive_msg_id)
    showConfirmationModal(
      title = "Confirm Deletion",
      message = "Are you sure you want to delete this item?",
      submessage = "This action cannot be undone.",
      action = "delete",
      action_button = "Delete"
    )
  })
  
  # Handle delete confirmation
  observeEvent(input$confirm_delete, {
    removeModal()
    msg <- get_db_data(paste0("SELECT * FROM legacy_messages WHERE ID = ", archive_msg_id_temp()))
    # Move to archived_messages
    execute_query("INSERT INTO archived_messages (Original_ID, Title, Emotion, Date, Status, Body) VALUES (?, ?, ?, ?, ?, ?)",
                  params = list(msg$ID, msg$Title, msg$Emotion, msg$Date, msg$Status, msg$Body))
    # Delete from main
    execute_query("DELETE FROM legacy_messages WHERE ID = ?", params = list(archive_msg_id_temp()))
    db_trigger(db_trigger() + 1)
    showNotification("The item has been deleted successfully.", type = "message")
  })
  
  # --- RESTORE LOGIC (with confirmation) ---
  observeEvent(input$restore_msg_id, {
    restore_msg_id_temp(input$restore_msg_id)
    showConfirmationModal(
      title = "Confirm Restore",
      message = "Are you sure you want to restore this item?",
      submessage = "It will be returned to its previous state.",
      action = "restore",
      action_button = "Restore"
    )
  })
  
  # Handle restore confirmation
  observeEvent(input$confirm_restore, {
    removeModal()
    msg <- get_db_data(paste0("SELECT * FROM archived_messages WHERE ID = ", restore_msg_id_temp()))
    # Return to main table
    execute_query("INSERT INTO legacy_messages (Title, Emotion, Date, Status, Body) VALUES (?, ?, ?, ?, ?)",
                  params = list(msg$Title, msg$Emotion, msg$Date, msg$Status, msg$Body))
    # Delete from archive
    execute_query("DELETE FROM archived_messages WHERE ID = ?", params = list(restore_msg_id_temp()))
    db_trigger(db_trigger() + 1)
    showNotification("The item has been restored successfully.", type = "message")
  })
  
  # --- OTHERS (Original Handlers) ---
  observeEvent(input$open_msg_id, {
    msg <- get_db_data(paste0("SELECT * FROM legacy_messages WHERE ID = ", input$open_msg_id))
    showModal(modalDialog(title = NULL, footer = NULL, size = "m", easyClose = TRUE,
                          div(class = "legacy-pop-container", div(class = "pop-header", h3("LEGACY MESSAGE")),
                              div(class = "pop-meta", span(paste("Emotion:", msg$Emotion)), span(paste("Released:", msg$Date))),
                              div(class = "pop-body", msg$Body),
                              div(style="text-align:center;", tags$button(class="btn-pop-close", "Closed", onclick="Shiny.setInputValue('close_modal', Math.random())"))
                          )
    ))
  })
  
  observeEvent(input$lock_msg, {
    output$msg_title_error <- renderText(""); output$msg_emotion_error <- renderText(""); output$msg_date_error <- renderText(""); output$msg_body_error <- renderText("")
    valid <- TRUE
    if(trimws(input$msg_title)=="") { output$msg_title_error <- renderText("Title is required!"); valid <- FALSE }
    if(input$msg_emotion=="") { output$msg_emotion_error <- renderText("Please select an emotion!"); valid <- FALSE }
    if(is.null(input$msg_date)) { output$msg_date_error <- renderText("Please select a date!"); valid <- FALSE }
    if(trimws(input$msg_body)=="") { output$msg_body_error <- renderText("Message cannot be blank!"); valid <- FALSE }
    
    if(valid){
      execute_query("INSERT INTO legacy_messages (Title, Emotion, Date, Status, Body) VALUES (?, ?, ?, ?, ?)",
                    params = list(input$msg_title, input$msg_emotion, as.character(input$msg_date), "Locked", input$msg_body))
      db_trigger(db_trigger() + 1); showModal(modalDialog(title="Success", "Message locked into Database!", easyClose=TRUE, footer=NULL))
      updateTextInput(session, "msg_title", value=""); updateSelectizeInput(session, "msg_emotion", selected=""); updateDateInput(session, "msg_date", value=Sys.Date()); updateTextAreaInput(session, "msg_body", value="")
    }
  })
  
  observeEvent(input$close_modal, { removeModal() })
  
  # --- PROFILE MENU ---
  observeEvent(input$profile_clicked, {
    showModal(div(class="right-aligned-modal", modalDialog(title = NULL, easyClose = TRUE, footer = NULL,
                                                           div(style = "padding: 5px 0;", 
                                                               actionButton("go_edit", label = div(icon("edit"), "Edit"), class = "profile-opt-btn"), 
                                                               actionButton("logout_profile_menu", label = div(icon("sign-out-alt"), "Log out"), class = "profile-opt-btn"))
    )))
  })
  
  # --- LOGOUT CONFIRMATION ---
  observeEvent(input$logout_profile_menu, {
    removeModal()
    showConfirmationModal(
      title = "Confirm Logout",
      message = "Are you sure you want to logout?",
      submessage = "You will need to enter your credentials again to access the system.",
      action = "logout",
      action_button = "Logout"
    )
  })
  
  # Handle logout confirmation
  observeEvent(input$confirm_logout, {
    removeModal()
    
    # Small delay to ensure modal is closed before switching UI
    shinyjs::delay(100, {
      logged_in(FALSE) # Triggers the UI to switch back to login_ui()
      current_tab("dashboard") # Resets the tab for the next session
      
      # Added feedback
      showNotification("Vault Locked. Session ended.", type = "message")
      
      # Clears the input fields for security
      updateTextInput(session, "username", value = "") 
      updatePasswordInput(session, "password", value = "")
    })
  })
  
  # --- PROFILE UPDATE (with confirmation) ---
  observeEvent(input$go_edit, {
    removeModal()
    showConfirmationModal(
      title = "Confirm Profile Update",
      message = "Are you sure you want to update your profile information?",
      submessage = "Your changes will be saved permanently.",
      action = "update_profile",
      action_button = "Update Profile"
    )
  })
  
  # Handle profile update confirmation
  observeEvent(input$confirm_update_profile, {
    removeModal()
    showUpdateModal()
  })
  
  showUpdateModal <- function() {
    showModal(modalDialog(title = div(style="background:#1A263F; color:white; padding:15px; margin:-15px -15px 15px -15px; border-radius:5px 5px 0 0;", "Update Profile Picture"), 
                          size = "m", 
                          easyClose = TRUE,
                          footer = div(style = "display: flex; justify-content: flex-end; gap: 10px;", 
                                       modalButton("Cancel"), 
                                       actionButton("save_final", "Save", style="background:#1A263F; color:white; border:none; padding:8px 25px; border-radius:5px;")),
                          div(class = "edit-profile-box", 
                              img(src = if(!is.null(temp_cropped_res())) temp_cropped_res() else current_profile_pic(), 
                                  class="profile-pic-large", 
                                  id="update_modal_preview"), 
                              h2("Admin", style="margin: 5px 0; font-weight: bold; color: #1A263F;"),
                              
                              # Cropped Preview Container (initially hidden)
                              div(id = "cropped_preview_container", class = "cropped-preview-container",
                                  div(class = "preview-label", "Cropped Preview"),
                                  img(src = "", class = "cropped-preview-img", id = "cropped_preview_img")
                              ),
                              
                              # Instruction text
                              div(id = "crop_instruction", style = "margin-top: 10px; color: #666; font-style: italic;",
                                  "Select a new photo to crop and preview"),
                              
                              div(style="margin-top: 20px; width: 100%; max-width: 300px;", 
                                  fileInput("new_pic", label = "Choose Photo from Gallery", width = "100%", accept = c('image/png', 'image/jpeg')))
                          )
    ))
  }
  
  observeEvent(input$new_pic, {
    req(input$new_pic); 
    encoded <- dataURI(file = input$new_pic$datapath, mime="image/png"); 
    removeModal()
    
    showModal(modalDialog(title = div(style="background:#1A263F; color:white; padding:15px; margin:-15px -15px 15px -15px; border-radius:5px 5px 0 0;", "Crop Your Photo"), 
                          size = "m", 
                          easyClose = FALSE,
                          footer = div(style = "display: flex; justify-content: flex-end; gap: 10px;", 
                                       actionButton("cancel_crop", "Cancel", style="padding:8px 20px;"), 
                                       actionButton("apply_crop", "Apply Crop", style="background:#1A263F; color:white; border:none; padding:8px 20px; border-radius:5px;")),
                          div(class = "edit-profile-box", 
                              div(class = "crop-container", 
                                  img(src=encoded, id="cropper_img_target")
                              ), 
                              p("Adjust the circle to crop your new profile picture.", style="margin-top:15px; color:#555;")
                          )
    ))
    
    shinyjs::runjs("
      setTimeout(function(){ 
        var imgTag = document.getElementById('cropper_img_target'); 
        if(window.cropper) window.cropper.destroy(); 
        window.cropper = new Cropper(imgTag, { 
          aspectRatio: 1, 
          viewMode: 1, 
          autoCropArea: 1, 
          responsive: true, 
          background: false,
          guides: false,
          highlight: false,
          movable: true,
          rotatable: false,
          scalable: false,
          zoomable: false,
          zoomOnTouch: false,
          zoomOnWheel: false,
          cropBoxMovable: true,
          cropBoxResizable: true,
          toggleDragModeOnDblclick: false
        }); 
      }, 300);
    ")
  })
  
  observeEvent(input$apply_crop, { 
    shinyjs::runjs("
      if(window.cropper){ 
        var canvas = window.cropper.getCroppedCanvas({ width: 250, height: 250 }); 
        var croppedDataURL = canvas.toDataURL();
        Shiny.setInputValue('cropped_base64', croppedDataURL, {priority: 'event'}); 
        // Update preview immediately
        updateCroppedPreview(croppedDataURL);
      }
    ") 
  })
  
  observeEvent(input$cropped_base64, { 
    # Store the cropped image
    temp_cropped_res(input$cropped_base64)
    
    # Close the crop modal
    removeModal()
    
    # Show the update modal again with the cropped preview
    showUpdateModal()
    
    # Update the preview in the modal
    shinyjs::runjs(sprintf("
      setTimeout(function() {
        $('#update_modal_preview').attr('src', '%s');
        updateCroppedPreview('%s');
      }, 300);
    ", input$cropped_base64, input$cropped_base64))
  })
  
  observeEvent(input$cancel_crop, { 
    removeModal()
    # Reset temp_cropped_res if no crop was applied
    temp_cropped_res(NULL)
    showUpdateModal()
  })
  
  observeEvent(input$save_final, { 
    # Save profile picture to database permanently
    if (!is.null(temp_cropped_res())) {
      save_profile_picture(temp_cropped_res())
      
      # Update profile picture trigger to refresh UI
      profile_trigger(profile_trigger() + 1)
      
      removeModal(); 
      showNotification("Your profile picture has been updated successfully and saved permanently.", type = "message")
    } else {
      removeModal()
      showNotification("No changes were made to your profile picture.", type = "warning")
    }
  })
  
  # --- HELPER FUNCTION FOR CONFIRMATION MODALS ---
  showConfirmationModal <- function(title, message, submessage = "", action, action_button) {
    button_class <- switch(action,
                           "update" = "btn-confirm-action",
                           "delete" = "btn-confirm-delete",
                           "restore" = "btn-confirm-restore",
                           "update_profile" = "btn-confirm-update-profile",
                           "logout" = "btn-confirm-logout",
                           "btn-confirm-action")
    
    action_id <- switch(action,
                        "update" = "confirm_update",
                        "delete" = "confirm_delete",
                        "restore" = "confirm_restore",
                        "update_profile" = "confirm_update_profile",
                        "logout" = "confirm_logout",
                        "confirm_action")
    
    showModal(modalDialog(
      title = NULL,
      footer = NULL,
      easyClose = FALSE,
      div(class = "confirmation-modal",
          div(class = "confirmation-title", title),
          div(class = "confirmation-content",
              div(class = "confirmation-message", message),
              if(nzchar(submessage)) div(class = "confirmation-submessage", submessage)
          ),
          div(class = "confirmation-buttons",
              actionButton("confirm_cancel", "Cancel", class = "btn-confirm-cancel"),
              actionButton(action_id, action_button, class = button_class)
          )
      )
    ))
  }
  
  # Handle cancel for all confirmation modals
  observeEvent(input$confirm_cancel, {
    removeModal()
  })
  
  # --- CALENDAR POSITIONING FIX ---
  observe({
    shinyjs::runjs("
      $(document).on('show.bs.dropdown', '.datepicker', function() {
        var $dropdown = $(this).find('.dropdown-menu');
        var $input = $(this).find('input');
        var inputRect = $input[0].getBoundingClientRect();
        
        // Center the dropdown under the input
        $dropdown.css({
          'left': '50%',
          'transform': 'translateX(-50%)',
          'top': (inputRect.bottom + 5) + 'px',
          'position': 'fixed'
        });
      });
    ")
  })
}

shinyApp(ui, server)