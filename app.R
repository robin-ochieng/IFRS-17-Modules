library(shiny)
library(shinyWidgets)
library(rmarkdown) 
library(bs4Dash)
library(ggplot2)
library(bslib)
library(plotly)
library(DT) 
library(dplyr)
library(lubridate)
library(shinyjs)
library(shinyBS)
library(scales)
library(shinyFeedback)
shinyjs::useShinyjs()

# Source authentication and database functions
source("supabase_config.R")
source("modules/authModule.R")

source("modules/introModule.R")
source("modules/measurementModule.R")
source("modules/caseStudiesModule.R")
source("modules/module1Module.R") 
source("modules/module2Module.R")
source("modules/module3Module.R")
source("modules/module4Module.R")
source("modules/module5Module.R")
source("modules/module6Module.R")
source("modules/module7Module.R")
source("modules/module8Module.R")
source("modules/module9Module.R")
source("modules/module10Module.R")
source("modules/module11Module.R")
source("modules/module12Module.R")
source("modules/module13Module.R")
source("modules/module14Module.R")
source("modules/module15Module.R")


# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  info = "#17a2b8",
  secondary = "#00BFA5",
  base_font = font_google("Nunito Sans"),
  heading_font = font_google("Nunito Sans"),
  code_font = font_google("Nunito Sans"),
  navbar_bg = "#333333",  
  navbar_fg = "#ffffff"  
)

# Define UI with authentication
ui <- tagList(
  # Authentication overlay (always present)
  authUI("auth"),
  
  # Main dashboard (hidden until authenticated)
  div(
    id = "main_app",
    dashboardPage(
      title = "IFRS 17 Digital Training Module",
      freshTheme = my_theme,
      dark = NULL,
      help = NULL,
      fullscreen = FALSE,
      scrollToTop = TRUE,
      dashboardHeader(
        disable = FALSE,
        fixed = TRUE,
        # User info in header
        rightUi = tagList(
          dropdownMenu(
            type = "notifications",
            icon = icon("user"),
            headerText = "User Menu",
            notificationItem(
              text = textOutput("user_welcome", inline = TRUE),
              icon = icon("user")
            ),
            notificationItem(
              text = "View Progress",
              href = "#",
              icon = icon("chart-line")
            ),
            notificationItem(
              text = "Logout",
              href = "#",
              icon = icon("sign-out-alt"),
              inputId = "logout_btn"
            )
          )
        )
      ),
      dashboardSidebar(
        div(class = "logos",
            img(src = "images/ira_logo_.png", class = "ira-logo") 
        ),
        # User progress indicator
        div(
          class = "user-progress-sidebar",
          style = "padding: 15px; background: #f8f9fa; margin: 10px; border-radius: 5px;",
          h5("Your Progress", style = "margin-bottom: 10px;"),
          div(
            id = "progress_indicator",
            style = "font-size: 14px; color: #666;",
            "Loading progress..."
          )
        ),        
        tags$div(
          class = "menu-container", 
          sidebarMenu(
            id = "sidebar", 
            # top‐level items
            menuItem("Introduction",  tabName = "intro",  icon = icon("info-circle")),
            menuItem("Foundations and Standards",  tabName = "measurement",  icon = icon("balance-scale")),
            # grouped IFRS-17 Modules
            menuItem(
              "IFRS-17 Modules", icon = icon("th-list"),
              menuSubItem(" | Module 1",  tabName = "module1", icon = icon("book")),
              menuSubItem(" | Module 2",  tabName = "module2", icon = icon("book")),
              menuSubItem(" | Module 3",  tabName = "module3", icon = icon("book")),
              menuSubItem(" | Module 4",  tabName = "module4", icon = icon("book")),
              menuSubItem(" | Module 5",  tabName = "module5", icon = icon("book")),
              menuSubItem(" | Module 6",  tabName = "module6", icon = icon("book")),
              menuSubItem(" | Module 7",  tabName = "module7", icon = icon("book")),
              menuSubItem(" |  Module 8",  tabName = "module8", icon = icon("book")),
              menuSubItem(" | Module 9",  tabName = "module9", icon = icon("book")),
              menuSubItem(" | Module 10", tabName = "module10", icon = icon("book")),
              menuSubItem(" | Module 11", tabName = "module11", icon = icon("book")),
              menuSubItem(" | Module 12", tabName = "module12", icon = icon("book")),
              menuSubItem(" | Module 13", tabName = "module13", icon = icon("book")),
              menuSubItem(" | Module 14", tabName = "module14", icon = icon("book")),
              menuSubItem(" | Module 15", tabName = "module15", icon = icon("book"))
            )
            # final item
            # menuItem("Case Studies", tabName = "cases", icon = icon("book-open"))
          )
        )
  ), 
  dashboardBody(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    tags$head(
      includeCSS("www/css/intro.css"),
      includeCSS("www/css/media_queries.css"),
      includeCSS("www/css/custom_styles.css"),
      includeCSS("www/css/measurement.css"),
      includeCSS("www/css/module1.css"), 
      includeCSS("www/css/module2.css"),
      includeCSS("www/css/module3.css"),
      includeCSS("www/css/module4.css"),
      includeCSS("www/css/module5.css"),
      includeCSS("www/css/module6.css"),
      includeCSS("www/css/module7.css"),      
      includeCSS("www/css/module9.css"),  
      includeCSS("www/css/module11.css"),  
      includeCSS("www/css/module13.css"), 
      includeCSS("www/css/module14.css"),
            # Add authentication CSS
          tags$style(HTML("
            .auth-overlay {
              font-family: 'Nunito Sans', sans-serif;
              padding: 20px;
              box-sizing: border-box;
            }
            .auth-container {
              position: relative;
              z-index: 10000;
            }
            .nav-tabs {
              display: flex;
              border-bottom: 1px solid #ddd;
              margin-bottom: 0;
            }
            .nav-tabs .nav-link {
              padding: 8px 16px;
              margin-right: 5px;
              border: 1px solid #ddd;
              border-bottom: none;
              background: #f8f9fa;
              color: #495057;
              cursor: pointer;
              text-decoration: none;
              border-radius: 5px 5px 0 0;
            }
            .nav-tabs .nav-link.active {
              background: white;
              color: #495057;
              border-bottom: 1px solid white;
              margin-bottom: -1px;
            }
            .auth-form {
              padding-top: 20px;
              min-height: 400px;
            }
            .form-group {
              margin-bottom: 15px;
            }
            .form-control {
              padding: 10px 12px;
              font-size: 14px;
              border: 1px solid #ddd;
              border-radius: 4px;
            }
            .btn {
              border-radius: 4px;
              border: none;
              cursor: pointer;
              transition: background-color 0.2s;
            }
            .btn-primary {
              background-color: #007bff;
              color: white;
            }
            .btn-primary:hover {
              background-color: #0056b3;
            }
            .btn-success {
              background-color: #28a745;
              color: white;
            }
            .btn-success:hover {
              background-color: #1e7e34;
            }
            .alert {
              padding: 10px 15px;
              margin-top: 10px;
              border-radius: 4px;
            }
            .alert-success {
              background-color: #d4edda;
              border: 1px solid #c3e6cb;
              color: #155724;
            }
            .alert-danger {
              background-color: #f8d7da;
              border: 1px solid #f5c6cb;
              color: #721c24;
            }
            .alert-warning {
              background-color: #fff3cd;
              border: 1px solid #ffeaa7;
              color: #856404;
            }
            .user-progress-sidebar {
              border-left: 4px solid #007bff;
            }
            .progress-item {
              display: flex;
              justify-content: space-between;
              align-items: center;
              padding: 2px 0;
            }
            .progress-badge {
              font-size: 10px;
              padding: 2px 6px;
              border-radius: 10px;
            }
            .completed { background: #28a745; color: white; }
            .in-progress { background: #ffc107; color: black; }
            .not-started { background: #6c757d; color: white; }
            
            /* Ensure modal is properly positioned */
            @media (max-height: 600px) {
              .auth-container {
                max-height: 80vh;
                padding: 20px;
              }
              .auth-form {
                min-height: auto;
              }
            }
          ")),
      tags$script(src = "js/custom.js"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Nunito+Sans:wght@400;700&display=swap", 
        rel = "stylesheet")
    ),
    tabItems(
      tabItem(tabName = "intro", IFRS17TrainingIntroUI("intro")),
      tabItem(tabName = "measurement", IFRS17MeasurementUI("measurement")),
      # tabItem(tabName = "cases", IFRS17CaseStudiesUI("cases")),

      # Additional Modules 2–16
      tabItem(tabName = "module1",     IFRS17Module1UI("module1")),
      tabItem(tabName = "module2",     IFRS17Module2UI("module2")),
      tabItem(tabName = "module3",     IFRS17Module3UI("module3")),
      tabItem(tabName = "module4",     IFRS17Module4UI("module4")),
      tabItem(tabName = "module5",     IFRS17Module5UI("module5")),
      tabItem(tabName = "module6",     IFRS17Module6UI("module6")),
      tabItem(tabName = "module7",     IFRS17Module7UI("module7")),
      tabItem(tabName = "module8",     IFRS17Module8UI("module8")),
      tabItem(tabName = "module9",     IFRS17Module9UI("module9")),
      tabItem(tabName = "module10",    IFRS17Module10UI("module10")),
      tabItem(tabName = "module11",    IFRS17Module11UI("module11")),
      tabItem(tabName = "module12",    IFRS17Module12UI("module12")),
      tabItem(tabName = "module13",    IFRS17Module13UI("module13")),
      tabItem(tabName = "module14",    IFRS17Module14UI("module14")),
      tabItem(tabName = "module15",    IFRS17Module15UI("module15"))
    ),
    div(
      class = "app-footer",
      style = "text-align: center; padding: 10px; color: #bbb; font-size: 14px;",
      tags$span("Powered by "),
      tags$img(src = "images/kenbright.png", style = "height: 25px; vertical-align: middle; margin-left: 5px;")
      )
     )
    ) 
  )
)

# Define the server logic
server <- function(input, output, session) {

  # Initialize authentication
  user_data <- authServer("auth")  

  # Show/hide main app based on authentication
  observe({
    if (user_data$is_authenticated) {
      shinyjs::show("main_app")
    } else {
      shinyjs::hide("main_app")
    }
  })

  # User welcome message
  output$user_welcome <- renderText({
    if (user_data$is_authenticated && !is.null(user_data$profile)) {
      paste("Welcome,", user_data$profile$full_name)
    } else if (user_data$is_authenticated) {
      paste("Welcome,", user_data$email)
    } else {
      ""
    }
  })
  
  # Load and display user progress
  user_progress <- reactive({
    if (user_data$is_authenticated) {
      get_user_progress(user_data$user_id, user_data$token)
    } else {
      NULL
    }
  })
  
  # Update progress indicator
  observe({
    progress <- user_progress()
    if (!is.null(progress)) {
      modules <- c("intro", "measurement", paste0("module", 1:15))
      completed_modules <- sapply(progress, function(x) x$module_name)
      
      progress_html <- sapply(modules, function(mod) {
        status <- if (mod %in% completed_modules) "completed" else "not-started"
        badge_text <- if (status == "completed") "✓" else "○"
        
        paste0(
          '<div class="progress-item">',
          '<span>', stringr::str_to_title(gsub("module(\\d+)", "Module \\1", mod)), '</span>',
          '<span class="progress-badge ', status, '">', badge_text, '</span>',
          '</div>'
        )
      })
      
      shinyjs::html("progress_indicator", paste(progress_html, collapse = ""))
    }
  })
  
  # Logout handler
  observeEvent(input$logout_btn, {
    if (user_data$is_authenticated) {
      supabase_signout(user_data$token)
      user_data$is_authenticated <- FALSE
      user_data$user_id <- NULL
      user_data$email <- NULL
      user_data$token <- NULL
      user_data$profile <- NULL
      
      # Reset UI
      updateTabItems(session, "sidebar", "intro")
      showNotification("Logged out successfully", type = "message")
    }
  })


  # Modified module servers with user data integration
  intro_nav <- IFRS17TrainingIntroServer("intro", user_data)
  measurement_nav <- IFRS17MeasurementServer("measurement", user_data)
  module1_nav <- IFRS17Module1Server("module1", user_data)
  module2_nav <- IFRS17Module2Server("module2", user_data)
  module3_nav <- IFRS17Module3Server("module3", user_data)
  module4_nav <- IFRS17Module4Server("module4", user_data)
  module5_nav <- IFRS17Module5Server("module5", user_data)  
  module6_nav <- IFRS17Module6Server("module6", user_data)
  module7_nav <- IFRS17Module7Server("module7", user_data)
  module8_nav <- IFRS17Module8Server("module8", user_data)
  module9_nav <- IFRS17Module9Server("module9", user_data)
  module10_nav <- IFRS17Module10Server("module10", user_data)
  module11_nav <- IFRS17Module11Server("module11", user_data)
  module12_nav <- IFRS17Module12Server("module12", user_data)
  module13_nav <- IFRS17Module13Server("module13", user_data)
  module14_nav <- IFRS17Module14Server("module14", user_data)


  
  # When the user clicks “Next: Measurement Models”, jump the sidebar
  observeEvent(intro_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "measurement"
    )
  })



  # When the user clicks “Next: Module 1 - Introduction to IFRS 17”, jump the sidebar 
  observeEvent(measurement_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module1"
    )
  })

  
  # When the user clicks “Next: Case Studies”, jump the sidebar
  observeEvent(module1_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module2"
    )
  })  

  # When the user clicks “Next: Module 3 - Combination & Separation of Insurance Contracts”, jump the sidebar
  observeEvent(module2_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module3"
    )
  })


  # When the user clicks “Next: Module 4 - Level of Aggregation”, jump the sidebar
  observeEvent(module3_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module4"
    )
  })


  # When the user clicks “Next: Module 5 - Recognition”, jump the sidebar
  observeEvent(module4_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module5"
    )
  })


  # When the user clicks “Next: Module 6 - Measurement on Initial Recognition”, jump the sidebar
  observeEvent(module5_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module6"
    )
  })


  # When the user clicks “Next: Module 7 - Subsequent Measurement”, jump the sidebar
  observeEvent(module6_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module7"
    )
  })


  # When the user clicks “Next: Module 8 - Discounting, CSM & Risk Adjustment”, jump the sidebar
  observeEvent(module7_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module8"
    )
  })


  # When the user clicks “Next: Module 9 - Onerous Contracts”, jump the sidebar
  observeEvent(module8_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module9"
    )
  })


  # When the user clicks “Next: Module 10 - Premium Allocation Approach”, jump the sidebar
  observeEvent(module9_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module10"
    )
  })


  # When the user clicks “Next: Module 11 - Reinsurance Contracts Held”, jump the sidebar
  observeEvent(module10_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module11"
    )
  })


  # When the user clicks “Next: Module 12 - Investment Contracts with Discretionary Participation Features”, jump the sidebar
  observeEvent(module11_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module12"
    )
  })


  # When the user clicks “Next: Module 13 - Modification & Derecognition”, jump the sidebar
  observeEvent(module12_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module13"
    )
  })


  # When the user clicks “Next: Module 14 - Presentation of Financial Statements”, jump the sidebar
  observeEvent(module13_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module14"
    )
  })

  
  # When the user clicks “Next: Module 15 - Insurance Service Result”, jump the sidebar
  observeEvent(module14_nav(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module15"
    )
  })

  # # This captures the module 15 navigation event
  # module15_nav <- IFRS17Module15Server("module15")
  # # When the user clicks “Next: Module 16 - Insurance Finance Income or Expenses”, jump the sidebar
  # observeEvent(module15_nav(), {
  #   updateTabItems(
  #     session,            # THIS session is the **root** session
  #     inputId   = "sidebar",
  #     selected  = "cases"
  #   )
  # })

  # This captures the module 16 navigation event
  IFRS17Module15Server("module15", user_data)

  # This captures the Case Studies navigation event 
  # IFRS17CaseStudiesServer("cases")
}

# Run the Shiny app
shinyApp(ui, server)  






