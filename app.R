#app.R

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
        fixed = FALSE,
        rightUi = dropdownMenu(
          type = "notifications",
          icon = icon("user"),
          headerText = "User Menu",
          
          # User welcome
          notificationItem(
            text = textOutput("user_welcome", inline = TRUE),
            icon = icon("user")
          ),

          
          # Progress (moved here from outside)
          notificationItem(
            text = textOutput("progress_display", inline = TRUE),
            icon = icon("chart-line"),
            href = "#",
            inputId = "show_progress_dashboard"
          ),

          # Logout
          notificationItem(
            text = "Logout",
            href = "#",
            icon = icon("sign-out-alt"),
            inputId = "logout_btn"
          )
        )
      ),
      dashboardSidebar(
        div(class = "logos",
            img(src = "images/ira_logo_.png", class = "ira-logo") 
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
              menuSubItem(" | Module 8",  tabName = "module8", icon = icon("book")),
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
      includeCSS("www/css/auth.css"),
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
    if (user_data$is_authenticated && user_data$is_guest) {
      "Guest User"
    } else if (user_data$is_authenticated && !is.null(user_data$profile)) {
      paste("Welcome,", user_data$profile$full_name)
    } else if (user_data$is_authenticated) {
      paste("Welcome,", user_data$email)
    } else {
      ""
    }
  })

  # Add a reactive value to trigger progress refresh
  progress_refresh_trigger <- reactiveVal(0)
  
  # Load and display user progress
  user_progress <- reactive({
    progress_refresh_trigger()
    if (user_data$is_authenticated && !user_data$is_guest) {
      get_user_progress(user_data$user_id, user_data$token)
    } else {
      NULL
    }
  })
  
  # Update progress indicator
  observe({
    if (user_data$is_guest) {
      shinyjs::html("progress_text", "Guest Mode")
    } else if (user_data$is_authenticated) {
      progress <- user_progress()
      if (!is.null(progress) && length(progress) > 0) {
        total_modules <- 15  #  15 modules
        completed_count <- length(progress)
        percentage <- round((completed_count / total_modules) * 100)
        shinyjs::html("progress_text", paste0(completed_count, "/", total_modules, " (", percentage, "%)"))
      } else {
        shinyjs::html("progress_text", "0/15 (0%)")
      }
    } else {
      shinyjs::html("progress_text", "Not logged in")
    }
  })

  output$progress_display <- renderText({
    if (isTRUE(user_data$is_guest)) {
      "📊 Progress: Guest Mode"
    } else if (isTRUE(user_data$is_authenticated)) {
      progress <- user_progress()
      if (!is.null(progress) && length(progress) > 0) {
        paste0("📊 Progress: ", length(progress), "/15")
      } else {
        "📊 Progress: 0/15"
      }
    } else {
      "📊 Progress: Not logged in"
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

  module1_result <- IFRS17Module1Server("module1", user_data)
  module2_result <- IFRS17Module2Server("module2", user_data)
  module3_result <- IFRS17Module3Server("module3", user_data)
  module4_result <- IFRS17Module4Server("module4", user_data)
  module5_result <- IFRS17Module5Server("module5", user_data)  
  module6_result <- IFRS17Module6Server("module6", user_data)
  module7_result <- IFRS17Module7Server("module7", user_data)
  module8_result <- IFRS17Module8Server("module8", user_data)
  module9_result <- IFRS17Module9Server("module9", user_data)
  module10_result <- IFRS17Module10Server("module10", user_data)
  module11_result <- IFRS17Module11Server("module11", user_data)
  module12_result <- IFRS17Module12Server("module12", user_data)
  module13_result <- IFRS17Module13Server("module13", user_data)
  module14_result <- IFRS17Module14Server("module14", user_data)
  module15_result <- IFRS17Module15Server("module15", user_data)
  
  # Observe progress updates from Module 1
  observeEvent(module1_result$progress_trigger(), {
    if (module1_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 2
  observeEvent(module2_result$progress_trigger(), {
    if (module2_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 3
  observeEvent(module3_result$progress_trigger(), {
    if (module3_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 4
  observeEvent(module4_result$progress_trigger(), {
    if (module4_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 5
  observeEvent(module5_result$progress_trigger(), {
    if (module5_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 6
  observeEvent(module6_result$progress_trigger(), {
    if (module6_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 7
  observeEvent(module7_result$progress_trigger(), {
    if (module7_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 8
  observeEvent(module8_result$progress_trigger(), {
    if (module8_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 9
  observeEvent(module9_result$progress_trigger(), {
    if (module9_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 10
  observeEvent(module10_result$progress_trigger(), {
    if (module10_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 11
  observeEvent(module11_result$progress_trigger(), {
    if (module11_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 12
  observeEvent(module12_result$progress_trigger(), {
    if (module12_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 13
  observeEvent(module13_result$progress_trigger(), {
    if (module13_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 14
  observeEvent(module14_result$progress_trigger(), {
    if (module14_result$progress_trigger() > 0) {
      # Trigger a refresh of the user progress
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })

  # Observe progress updates from Module 15
  observeEvent(module15_result$progress_trigger(), {
    if (module15_result$progress_trigger() > 0) {
      progress_refresh_trigger(progress_refresh_trigger() + 1)
    }
  })
  
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
  observeEvent(module1_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module2"
    )
  })  

  # When the user clicks “Next: Module 3 - Combination & Separation of Insurance Contracts”, jump the sidebar
  observeEvent(module2_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module3"
    )
  })


  # When the user clicks “Next: Module 4 - Level of Aggregation”, jump the sidebar
  observeEvent(module3_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module4"
    )
  })


  # When the user clicks “Next: Module 5 - Recognition”, jump the sidebar
  observeEvent(module4_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module5"
    )
  })


  # When the user clicks “Next: Module 6 - Measurement on Initial Recognition”, jump the sidebar
  observeEvent(module5_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module6"
    )
  })


  # When the user clicks “Next: Module 7 - Subsequent Measurement”, jump the sidebar
  observeEvent(module6_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module7"
    )
  })


  # When the user clicks “Next: Module 8 - Discounting, CSM & Risk Adjustment”, jump the sidebar
  observeEvent(module7_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module8"
    )
  })


  # When the user clicks “Next: Module 9 - Onerous Contracts”, jump the sidebar
  observeEvent(module8_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module9"
    )
  })


  # When the user clicks “Next: Module 10 - Premium Allocation Approach”, jump the sidebar
  observeEvent(module9_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module10"
    )
  })


  # When the user clicks “Next: Module 11 - Reinsurance Contracts Held”, jump the sidebar
  observeEvent(module10_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module11"
    )
  })


  # When the user clicks “Next: Module 12 - Investment Contracts with Discretionary Participation Features”, jump the sidebar
  observeEvent(module11_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module12"
    )
  })


  # When the user clicks “Next: Module 13 - Modification & Derecognition”, jump the sidebar
  observeEvent(module12_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module13"
    )
  })


  # When the user clicks “Next: Module 14 - Presentation of Financial Statements”, jump the sidebar
  observeEvent(module13_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module14"
    )
  })

  
  # When the user clicks “Next: Module 15 - Insurance Service Result”, jump the sidebar
  observeEvent(module14_result$navigate(), {
    updateTabItems(
      session,            # THIS session is the **root** session
      inputId   = "sidebar",
      selected  = "module15"
    )
  })


  # This observer is for the progress dashboard:
  observeEvent(input$show_progress_dashboard, {
    if (!user_data$is_authenticated) {
      showNotification("Please log in to view progress", type = "info")
      return()
    }
    
    if (user_data$is_guest) {
      showNotification("Progress tracking is not available in guest mode", type = "warning")
      return()
    }
    
    # Get user progress
    progress_data <- user_progress()
    
    # Define all modules (only modules 1-15)
    all_modules <- paste0("module", 1:15)
    module_names <- c(
      "Introduction & Scope",
      "Combination & Separation",
      "Level of Aggregation",
      "Recognition",
      "Measurement on Initial Recognition",
      "Subsequent Measurement",
      "Discounting, CSM & Risk Adjustment",
      "Onerous Contracts",
      "Premium Allocation Approach",
      "Reinsurance Contracts Held",
      "Investment Contracts with DPF",
      "Modification & Derecognition",
      "Presentation of Financial Statements",
      "Insurance Service Result",
      "Insurance Finance Income or Expenses"
    )
    
    # Create progress summary
    if (!is.null(progress_data) && length(progress_data) > 0) {
      # Convert progress data to a more usable format
      progress_df <- data.frame(
        module = character(),
        score = numeric(),
        percentage = numeric(),
        completed_at = character(),
        stringsAsFactors = FALSE
      )
      
      for (p in progress_data) {
        progress_df <- rbind(progress_df, data.frame(
          module = p$module_name,
          score = as.numeric(p$score),
          percentage = as.numeric(p$percentage),
          completed_at = format(as.POSIXct(p$completed_at), "%B %d, %Y"),
          stringsAsFactors = FALSE
        ))
      }
    } else {
      progress_df <- data.frame(
        module = character(),
        score = numeric(),
        percentage = numeric(),
        completed_at = character(),
        stringsAsFactors = FALSE
      )
    }
    
    # Show modal with progress dashboard
    showModal(
      modalDialog(
        title = tagList(
          icon("chart-line"),
          "Your Learning Progress Dashboard"
        ),
        size = "l",
        easyClose = TRUE,
        
        # Overall Progress Card
        div(
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                  padding: 30px; border-radius: 15px; color: white; 
                  text-align: center; margin-bottom: 30px;",
          h2(
            paste0(nrow(progress_df), " / ", length(all_modules)),
            style = "font-size: 48px; margin: 0; font-weight: bold;"
          ),
          p("Modules Completed", style = "font-size: 18px; margin: 10px 0;"),
          
          # Progress bar
          div(
            style = "background: rgba(255,255,255,0.3); height: 20px; 
                    border-radius: 10px; margin-top: 20px; overflow: hidden;",
            div(
              style = paste0(
                "background: white; height: 100%; width: ",
                round(nrow(progress_df) / length(all_modules) * 100), "%; ",
                "transition: width 0.5s ease;"
              )
            )
          ),
          
          # Average score if modules completed
          if (nrow(progress_df) > 0) {
            avg_score <- round(mean(progress_df$percentage), 1)
            p(
              paste0("Average Score: ", avg_score, "%"),
              style = "font-size: 16px; margin-top: 15px; opacity: 0.9;"
            )
          }
        ),
        
        # Module Progress Grid
        h4("Module Progress Details", style = "margin-bottom: 20px;"),
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); 
                  gap: 15px; max-height: 400px; overflow-y: auto; padding: 10px;",
          
          lapply(seq_along(all_modules), function(i) {
            module_id <- all_modules[i]
            module_name <- module_names[i]
            
            # Check if module is completed
            module_progress <- progress_df[progress_df$module == module_id, ]
            is_completed <- nrow(module_progress) > 0
            
            div(
              style = paste0(
                "background: ", if(is_completed) "#f0f9ff" else "#f8f9fa", "; ",
                "border: 2px solid ", if(is_completed) "#3b82f6" else "#e5e7eb", "; ",
                "border-radius: 10px; padding: 15px; position: relative;"
              ),
              
              # Module number badge
              div(
                style = paste0(
                  "position: absolute; top: -10px; right: -10px; ",
                  "background: ", if(is_completed) "#3b82f6" else "#9ca3af", "; ",
                  "color: white; width: 30px; height: 30px; ",
                  "border-radius: 50%; display: flex; align-items: center; ",
                  "justify-content: center; font-weight: bold;"
                ),
                as.character(i)
              ),
              
              # Module name
              h5(
                module_name,
                style = paste0(
                  "margin: 0 0 10px 0; font-size: 14px; ",
                  "color: ", if(is_completed) "#1e40af" else "#6b7280", ";"
                )
              ),
              
              # Status and score
              if (is_completed) {
                tagList(
                  div(
                    style = "display: flex; justify-content: space-between; 
                            align-items: center; margin-bottom: 5px;",
                    span(
                      icon("check-circle", style = "color: #10b981;"),
                      " Completed",
                      style = "color: #10b981; font-weight: 500;"
                    ),
                    span(
                      paste0(module_progress$percentage, "%"),
                      style = paste0(
                        "font-weight: bold; font-size: 18px; ",
                        "color: ", if(module_progress$percentage >= 70) "#10b981" else "#ef4444", ";"
                      )
                    )
                  ),
                  p(
                    paste("Completed on", module_progress$completed_at),
                    style = "font-size: 12px; color: #6b7280; margin: 0;"
                  )
                )
              } else {
                div(
                  icon("circle", style = "color: #9ca3af;"),
                  " Not Started",
                  style = "color: #9ca3af;"
                )
              }
            )
          })
        ),
        
        footer = tagList(
          if (nrow(progress_df) == length(all_modules)) {
            actionButton(
              "generate_certificate",
              "Generate Certificate",
              icon = icon("certificate"),
              class = "btn-success"
            )
          },
          modalButton("Close")
        )
      )
    )
  })

  # Add this observer to handle certificate generation
  observeEvent(input$generate_certificate, {
    progress_data <- user_progress()
    
    if (!is.null(progress_data) && length(progress_data) > 0) {
      # Convert to dataframe
      progress_df <- data.frame(
        module = character(),
        score = numeric(),
        percentage = numeric(),
        stringsAsFactors = FALSE
      )
      
      for (p in progress_data) {
        progress_df <- rbind(progress_df, data.frame(
          module = p$module_name,
          score = as.numeric(p$score),
          percentage = as.numeric(p$percentage),
          stringsAsFactors = FALSE
        ))
      }
      
      # Calculate overall statistics
      overall_average <- round(mean(progress_df$percentage), 1)
      total_modules <- 15
      
      # Module names for display
      module_display_names <- c(
        "module1" = "Introduction & Scope of IFRS 17",
        "module2" = "Combination & Separation of Insurance Contracts",
        "module3" = "Level of Aggregation",
        "module4" = "Recognition",
        "module5" = "Measurement on Initial Recognition",
        "module6" = "Subsequent Measurement",
        "module7" = "Discounting, CSM & Risk Adjustment",
        "module8" = "Onerous Contracts",
        "module9" = "Premium Allocation Approach",
        "module10" = "Reinsurance Contracts Held",
        "module11" = "Investment Contracts with DPF",
        "module12" = "Modification & Derecognition",
        "module13" = "Presentation of Financial Statements",
        "module14" = "Insurance Service Result",
        "module15" = "Insurance Finance Income or Expenses"
      )
      
      showModal(
        modalDialog(
          title = "IFRS 17 Training Certificate",
          size = "l",
          easyClose = TRUE,
          
          div(
            id = "certificate_content",
            style = "background: white; padding: 40px; position: relative;",
            
            # Certificate Border
            div(
              style = "border: 3px solid #1e40af; padding: 30px; position: relative;",
              
              # Header with logos
              div(
                style = "text-align: center; margin-bottom: 30px;",
                img(src = "images/ira_logo_.png", 
                    style = "height: 80px; margin-bottom: 20px;"),
                
                h1("Certificate of Completion",
                  style = "color: #1e40af; font-size: 36px; 
                            font-weight: bold; margin: 10px 0;"),
                
                div(
                  style = "width: 200px; height: 3px; background: #3b82f6; 
                          margin: 20px auto;"
                )
              ),
              
              # Recipient Information
              div(
                style = "text-align: center; margin: 30px 0;",
                p("This is to certify that", 
                  style = "font-size: 18px; color: #4b5563;"),
                
                h2(user_data$profile$full_name,
                  style = "color: #1e40af; font-size: 32px; 
                            margin: 15px 0; font-weight: bold;"),
                
                p("has successfully completed the",
                  style = "font-size: 18px; color: #4b5563;"),
                
                h3("IFRS 17 Digital Training Program",
                  style = "color: #1e40af; font-size: 24px; margin: 15px 0;")
              ),
              
              # Scores Table
              div(
                style = "margin: 30px 0;",
                h4("Module Performance Summary",
                  style = "text-align: center; color: #1e40af; 
                            margin-bottom: 20px;"),
                
                # Create a two-column layout for modules
                div(
                  style = "display: grid; grid-template-columns: 1fr 1fr; 
                          gap: 10px; font-size: 14px;",
                  
                  lapply(names(module_display_names), function(mod_id) {
                    mod_data <- progress_df[progress_df$module == mod_id, ]
                    if (nrow(mod_data) > 0) {
                      div(
                        style = "display: flex; justify-content: space-between; 
                                padding: 5px 10px; background: #f8f9fa; 
                                border-radius: 5px;",
                        span(module_display_names[[mod_id]], 
                            style = "flex: 1; font-size: 12px;"),
                        span(paste0(mod_data$percentage, "%"),
                            style = paste0(
                              "font-weight: bold; margin-left: 10px; ",
                              "color: ", 
                              if(mod_data$percentage >= 70) "#10b981" else "#ef4444"
                            ))
                      )
                    }
                  })
                )
              ),
              
              # Overall Performance
              div(
                style = "text-align: center; margin: 30px 0; 
                        background: #eff6ff; padding: 20px; 
                        border-radius: 10px;",
                h4("Overall Performance",
                  style = "color: #1e40af; margin-bottom: 10px;"),
                
                div(
                  style = "font-size: 36px; font-weight: bold; 
                          color: #3b82f6;",
                  paste0(overall_average, "%")
                ),
                
                p(paste0("Average score across ", nrow(progress_df), 
                        " completed modules"),
                  style = "color: #6b7280; margin-top: 5px;")
              ),
              
              # Completion Date and Signatures
              div(
                style = "margin-top: 40px; display: grid; 
                        grid-template-columns: 1fr 1fr; gap: 40px;",
                
                div(
                  style = "text-align: center;",
                  p(format(Sys.Date(), "%B %d, %Y"),
                    style = "font-weight: bold; margin-bottom: 5px;"),
                  div(style = "border-top: 2px solid #d1d5db; 
                              margin-top: 40px; padding-top: 10px;",
                      "Date of Completion")
                ),
                
                div(
                  style = "text-align: center;",
                  img(src = "images/signature.png", 
                      style = "height: 40px; margin-bottom: 5px;",
                      onerror = "this.style.display='none'"),
                  div(style = "border-top: 2px solid #d1d5db; 
                              margin-top: 40px; padding-top: 10px;",
                      "Program Director")
                )
              ),
              
              # Certificate ID
              div(
                style = "text-align: center; margin-top: 30px; 
                        color: #9ca3af; font-size: 12px;",
                paste("Certificate ID:", 
                      paste0("IFRS17-", 
                            format(Sys.Date(), "%Y%m"),
                            "-",
                            substr(digest::digest(user_data$user_id), 1, 8)))
              )
            )
          ),
          
          footer = tagList(
            actionButton(
              "print_certificate",
              "Print Certificate",
              icon = icon("print"),
              class = "btn-primary"
            ),
            modalButton("Close")
          )
        )
      )
    }
  })

  # Add print functionality
  observeEvent(input$print_certificate, {
    runjs('
      var content = document.getElementById("certificate_content").innerHTML;
      var printWindow = window.open("", "_blank");
      printWindow.document.write(`
        <html>
          <head>
            <title>IFRS 17 Training Certificate</title>
            <style>
              @page { size: landscape; margin: 0.5in; }
              body { 
                font-family: Arial, sans-serif; 
                margin: 0; 
                padding: 20px;
              }
              @media print {
                body { margin: 0; }
              }
            </style>
          </head>
          <body>
            ${content}
          </body>
        </html>
      `);
      printWindow.document.close();
      printWindow.focus();
      setTimeout(() => {
        printWindow.print();
      }, 250);
    ')
  })










}


# Run the Shiny app
shinyApp(ui, server)  






