# modules/authModule.R
# Authentication UI and Server Module

library(shiny)
library(shinyjs)
library(bs4Dash)

# Authentication UI
authUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    
    # Login/Register Modal
    div(
      id = ns("auth_overlay"),
      class = "auth-overlay",
      style = "
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0,0,0,0.8);
        z-index: 9999;
        display: flex;
        align-items: center;
        justify-content: center;
      ",
      
      div(
        class = "auth-container",
        style = "
          background: white;
          padding: 40px;
          border-radius: 10px;
          max-width: 450px;
          width: 90%;
          max-height: 90vh;
          overflow-y: auto;
          box-shadow: 0 4px 20px rgba(0,0,0,0.3);
        ",
        
        # Logo
        div(
          class = "text-center mb-4",
          img(src = "images/ira_logo_.png", style = "height: 60px;"),
          h3("IFRS 17 Training Platform", class = "mt-2")
        ),
        
        # Tab Navigation
        div(
          class = "nav nav-tabs mb-3",
          tags$button(
            "Login",
            id = ns("login_tab"),
            class = "nav-link active",
            onclick = paste0("showTab('", ns("login_form"), "', '", ns("register_form"), "', '", ns("login_tab"), "', '", ns("register_tab"), "')")
          ),
          tags$button(
            "Register",
            id = ns("register_tab"),
            class = "nav-link",
            onclick = paste0("showTab('", ns("register_form"), "', '", ns("login_form"), "', '", ns("register_tab"), "', '", ns("login_tab"), "')")
          )
        ),
        
        # Login Form
        div(
          id = ns("login_form"),
          class = "auth-form",
          h4("Welcome Back"),
          p("Please sign in to continue your training."),
          
          div(
            class = "form-group",
            textInput(
              ns("login_email"),
              "Email",
              placeholder = "Enter your email"
            )
          ),
          
          div(
            class = "form-group",
            passwordInput(
              ns("login_password"),
              "Password",
              placeholder = "Enter your password"
            )
          ),
          
          div(
            class = "form-group text-center",
            actionButton(
              ns("login_btn"),
              "Sign In",
              class = "btn btn-primary btn-block",
              style = "width: 100%;"
            )
          ),
          
          div(id = ns("login_message"), class = "alert", style = "display: none;")
        ),
        
        # Register Form
        div(
          id = ns("register_form"),
          class = "auth-form",
          style = "display: none;",
          h4("Create Account"),
          p("Join our IFRS 17 training program."),
          
          div(
            class = "form-group",
            textInput(
              ns("register_name"),
              "Full Name",
              placeholder = "Enter your full name"
            )
          ),
          
          div(
            class = "form-group",
            textInput(
              ns("register_email"),
              "Email",
              placeholder = "Enter your email"
            )
          ),
          
          div(
            class = "form-group",
            textInput(
              ns("register_organization"),
              "Organization (Optional)",
              placeholder = "Your organization"
            )
          ),
          
          div(
            class = "form-group",
            passwordInput(
              ns("register_password"),
              "Password",
              placeholder = "Create a password (min 6 characters)"
            )
          ),
          
          div(
            class = "form-group",
            passwordInput(
              ns("register_confirm"),
              "Confirm Password",
              placeholder = "Confirm your password"
            )
          ),
          
          div(
            class = "form-group text-center",
            actionButton(
              ns("register_btn"),
              "Create Account",
              class = "btn btn-success btn-block",
              style = "width: 100%; padding: 12px; font-size: 16px; font-weight: bold;"
            )
          ),
          
          div(id = ns("register_message"), class = "alert", style = "display: none;")
        )
      )
    ),
    
    # JavaScript for tab switching
    tags$script(HTML("
      function showTab(showId, hideId, activeTab, inactiveTab) {
        document.getElementById(showId).style.display = 'block';
        document.getElementById(hideId).style.display = 'none';
        document.getElementById(activeTab).classList.add('active');
        document.getElementById(inactiveTab).classList.remove('active');
      }
    "))
  )
}

# Authentication Server
authServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for user state
    user_data <- reactiveValues(
      is_authenticated = FALSE,
      user_id = NULL,
      email = NULL,
      token = NULL,
      profile = NULL
    )
    
    # Show/hide auth overlay based on authentication status
    observe({
      if (user_data$is_authenticated) {
        shinyjs::hide("auth_overlay")
      } else {
        shinyjs::show("auth_overlay")
      }
    })
    
    # Login handler
    observeEvent(input$login_btn, {
      req(input$login_email, input$login_password)
      
      # Validate email format
      if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", input$login_email)) {
        shinyjs::html("login_message", "Please enter a valid email address")
        shinyjs::show("login_message")
        shinyjs::addCssClass("login_message", "alert-danger")
        return()
      }
      
      # Attempt login
      result <- supabase_signin(input$login_email, input$login_password)
      
      if (result$success) {
        user_data$is_authenticated <- TRUE
        user_data$user_id <- result$user$id
        user_data$email <- result$user$email
        user_data$token <- result$session$access_token
        
        # Get user profile
        profile <- get_user_profile(user_data$user_id, user_data$token)
        user_data$profile <- profile
        
        shinyjs::html("login_message", "Login successful! Welcome back.")
        shinyjs::show("login_message")
        shinyjs::addCssClass("login_message", "alert-success")
        
        # Hide auth overlay after short delay
        shinyjs::delay(1000, shinyjs::hide("auth_overlay"))
        
      } else {
        shinyjs::html("login_message", paste("Login failed:", result$error))
        shinyjs::show("login_message")
        shinyjs::addCssClass("login_message", "alert-danger")
      }
    })
    
    # Register handler
    observeEvent(input$register_btn, {
      req(input$register_name, input$register_email, input$register_password, input$register_confirm)
      
      # Validation
      if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", input$register_email)) {
        shinyjs::html("register_message", "Please enter a valid email address")
        shinyjs::show("register_message")
        shinyjs::addCssClass("register_message", "alert-danger")
        return()
      }
      
      if (nchar(input$register_password) < 6) {
        shinyjs::html("register_message", "Password must be at least 6 characters long")
        shinyjs::show("register_message")
        shinyjs::addCssClass("register_message", "alert-danger")
        return()
      }
      
      if (input$register_password != input$register_confirm) {
        shinyjs::html("register_message", "Passwords do not match")
        shinyjs::show("register_message")
        shinyjs::addCssClass("register_message", "alert-danger")
        return()
      }
      
      # Attempt registration
      result <- supabase_signup(input$register_email, input$register_password)
      
      if (result$success) {
        # Create user profile
        profile_created <- create_user_profile(
          result$user$id, 
          input$register_name, 
          input$register_organization,
          result$session$access_token
        )
        
        if (profile_created) {
          user_data$is_authenticated <- TRUE
          user_data$user_id <- result$user$id
          user_data$email <- result$user$email
          user_data$token <- result$session$access_token
          user_data$profile <- list(
            full_name = input$register_name,
            organization = input$register_organization
          )
          
          shinyjs::html("register_message", "Registration successful! Welcome to IFRS 17 Training.")
          shinyjs::show("register_message")
          shinyjs::addCssClass("register_message", "alert-success")
          
          # Hide auth overlay after short delay
          shinyjs::delay(1500, shinyjs::hide("auth_overlay"))
        } else {
          shinyjs::html("register_message", "Registration successful, but profile creation failed. Please contact support.")
          shinyjs::show("register_message")
          shinyjs::addCssClass("register_message", "alert-warning")
        }
        
      } else {
        shinyjs::html("register_message", paste("Registration failed:", result$error))
        shinyjs::show("register_message")
        shinyjs::addCssClass("register_message", "alert-danger")
      }
    })
    
    # Return user data for other modules to use
    return(user_data)
  })
}