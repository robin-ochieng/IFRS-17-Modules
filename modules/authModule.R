# modules/authModule.R
# Simplified Authentication UI and Server Module

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
            onclick = paste0("showAuthTab('", ns(""), "', 'login')")
          ),
          tags$button(
            "Register",
            id = ns("register_tab"),
            class = "nav-link",
            onclick = paste0("showAuthTab('", ns(""), "', 'register')")
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
          class = "form-group text-right",
          actionLink(
            ns("forgot_link"),
            "Forgot Password?",
            style = "color: #007bff; font-size: 14px;"
          )
        ),
          div(
            class = "form-group text-center",
            actionButton(
              ns("login_btn"),
              "Sign In",
              class = "btn btn-primary btn-block",
              style = "width: 100%; margin-bottom: 10px;"
            ),
            
            # Guest mode button
            actionButton(
              ns("guest_btn"),
              "Continue as Guest",
              class = "btn btn-secondary btn-block",
              style = "width: 100%; background-color: #6c757d; border: none;"
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
        ),
        # Forgot Password Form (hidden by default)
        div(
          id = ns("forgot_form"),
          class = "auth-form",
          style = "display: none;",
          h4("Reset Password"),
          p("Enter your email address to receive a password reset link."),
          
          div(
            class = "form-group",
            textInput(
              ns("reset_email"),
              "Email",
              placeholder = "Enter your registered email"
            )
          ),
          
          div(
            class = "form-group text-center",
            actionButton(
              ns("send_reset"),
              "Send Reset Link",
              class = "btn btn-primary btn-block",
              style = "width: 100%; margin-bottom: 10px;"
            ),
            
            actionLink(
              ns("back_to_login_link"),
              "Back to Login",
              style = "display: block; margin-top: 10px; color: #007bff;"
            )
          ),
          
          div(id = ns("reset_message"), class = "alert", style = "display: none;")
        )

      )
    ),
    
    # JavaScript for tab switching
    tags$script(HTML(sprintf("
      function showAuthTab(nsPrefix, tab) {
        // Hide all forms
        document.getElementById(nsPrefix + 'login_form').style.display = 'none';
        document.getElementById(nsPrefix + 'register_form').style.display = 'none';
        
        // ADD THIS LINE:
        document.getElementById(nsPrefix + 'forgot_form').style.display = 'none';
        
        if (tab === 'login') {
          document.getElementById(nsPrefix + 'login_form').style.display = 'block';
          document.getElementById(nsPrefix + 'tab_navigation').style.display = 'flex';
          document.getElementById(nsPrefix + 'login_tab').classList.add('active');
          document.getElementById(nsPrefix + 'register_tab').classList.remove('active');
        } else if (tab === 'register') {
          document.getElementById(nsPrefix + 'register_form').style.display = 'block';
          document.getElementById(nsPrefix + 'tab_navigation').style.display = 'flex';
          document.getElementById(nsPrefix + 'register_tab').classList.add('active');
          document.getElementById(nsPrefix + 'login_tab').classList.remove('active');
        } else if (tab === 'forgot') {
          // ADD THIS NEW CONDITION:
          document.getElementById(nsPrefix + 'forgot_form').style.display = 'block';
          document.getElementById(nsPrefix + 'tab_navigation').style.display = 'none';
        }
      }
    ")))
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
      profile = NULL,
      is_guest = FALSE
    )
    
    # Show/hide auth overlay based on authentication status
    observe({
      if (user_data$is_authenticated) {
        shinyjs::hide("auth_overlay")
      } else {
        shinyjs::show("auth_overlay")
      }
    })
    
    # Guest mode handler
    observeEvent(input$guest_btn, {
      guest_session <- create_guest_session()
      
      user_data$is_authenticated <- guest_session$is_authenticated
      user_data$user_id <- guest_session$user_id
      user_data$email <- guest_session$email
      user_data$token <- guest_session$token
      user_data$profile <- guest_session$profile
      user_data$is_guest <- guest_session$is_guest
      
      showNotification("Continuing as guest. Your progress will not be saved.", 
                      type = "warning", duration = 5)
      
      shinyjs::hide("auth_overlay")
    })
    
    # Login handler
    observeEvent(input$login_btn, {
      req(input$login_email, input$login_password)
      
      # Clear previous messages
      shinyjs::hide("login_message")
      shinyjs::removeClass("login_message", "alert-danger")
      shinyjs::removeClass("login_message", "alert-success")
      
      # Basic validation
      if (nchar(trimws(input$login_email)) == 0) {
        shinyjs::html("login_message", "Please enter your email")
        shinyjs::show("login_message")
        shinyjs::addClass("login_message", "alert-danger")
        return()
      }
      
      if (nchar(input$login_password) == 0) {
        shinyjs::html("login_message", "Please enter your password")
        shinyjs::show("login_message")
        shinyjs::addClass("login_message", "alert-danger")
        return()
      }
      
      # Show loading state
      shinyjs::disable("login_btn")
      shinyjs::html("login_btn", "Signing in...")
      
      # Attempt login
      result <- supabase_signin(input$login_email, input$login_password)
      
      if (result$success) {
        user_data$is_authenticated <- TRUE
        user_data$user_id <- result$user$id
        user_data$email <- result$user$email
        user_data$token <- result$session$access_token
        user_data$profile <- result$profile
        user_data$is_guest <- FALSE
        
        shinyjs::html("login_message", "Login successful! Welcome back.")
        shinyjs::show("login_message")
        shinyjs::addClass("login_message", "alert-success")
        
        # Clear form
        updateTextInput(session, "login_email", value = "")
        updateTextInput(session, "login_password", value = "")
        
        # Hide auth overlay after short delay
        shinyjs::delay(1000, shinyjs::hide("auth_overlay"))
        
      } else {
        error_msg <- result$error
        if (grepl("Invalid login credentials", error_msg, ignore.case = TRUE)) {
          error_msg <- "Invalid email or password. Please try again."
        }
        
        shinyjs::html("login_message", error_msg)
        shinyjs::show("login_message")
        shinyjs::addClass("login_message", "alert-danger")
      }
      
      # Reset button state
      shinyjs::enable("login_btn")
      shinyjs::html("login_btn", "Sign In")
    })
    
    # Register handler
    observeEvent(input$register_btn, {
      req(input$register_name, input$register_email, 
          input$register_password, input$register_confirm)
      
      # Clear previous messages
      shinyjs::hide("register_message")
      shinyjs::removeClass("register_message", "alert-danger")
      shinyjs::removeClass("register_message", "alert-success")
      
      # Validation
      if (nchar(trimws(input$register_name)) == 0) {
        shinyjs::html("register_message", "Please enter your full name")
        shinyjs::show("register_message")
        shinyjs::addClass("register_message", "alert-danger")
        return()
      }
      
      if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", input$register_email)) {
        shinyjs::html("register_message", "Please enter a valid email address")
        shinyjs::show("register_message")
        shinyjs::addClass("register_message", "alert-danger")
        return()
      }
      
      if (nchar(input$register_password) < 6) {
        shinyjs::html("register_message", "Password must be at least 6 characters long")
        shinyjs::show("register_message")
        shinyjs::addClass("register_message", "alert-danger")
        return()
      }
      
      if (input$register_password != input$register_confirm) {
        shinyjs::html("register_message", "Passwords do not match")
        shinyjs::show("register_message")
        shinyjs::addClass("register_message", "alert-danger")
        return()
      }
      
      # Show loading state
      shinyjs::disable("register_btn")
      shinyjs::html("register_btn", "Creating account...")
      
      # Attempt registration
      result <- supabase_signup(
        input$register_email, 
        input$register_password,
        input$register_name
      )
      
      if (result$success) {
        user_data$is_authenticated <- TRUE
        user_data$user_id <- result$user$id
        user_data$email <- result$user$email
        user_data$token <- result$session$access_token
        user_data$profile <- list(
          full_name = input$register_name,
          email = input$register_email
        )
        user_data$is_guest <- FALSE
        
        shinyjs::html("register_message", "Registration successful! Welcome to IFRS 17 Training.")
        shinyjs::show("register_message")
        shinyjs::addClass("register_message", "alert-success")
        
        # Clear form
        updateTextInput(session, "register_name", value = "")
        updateTextInput(session, "register_email", value = "")
        updateTextInput(session, "register_password", value = "")
        updateTextInput(session, "register_confirm", value = "")
        
        # Hide auth overlay after short delay
        shinyjs::delay(1500, shinyjs::hide("auth_overlay"))
        
      } else {
        error_msg <- result$error
        if (grepl("already registered", error_msg, ignore.case = TRUE)) {
          error_msg <- "This email is already registered. Please login instead."
        }
        
        shinyjs::html("register_message", error_msg)
        shinyjs::show("register_message")
        shinyjs::addClass("register_message", "alert-danger")
      }
      
      # Reset button state
      shinyjs::enable("register_btn")
      shinyjs::html("register_btn", "Create Account")
    })

    # Forgot Password Link Handler
    observeEvent(input$forgot_link, {
      # Show forgot password form inline
      runjs(sprintf("showAuthTab('%s', 'forgot')", ns("")))
      
      # Clear any previous messages
      shinyjs::hide("reset_message")
      updateTextInput(session, "reset_email", value = "")
    })
    
    # Back to Login Handler
    observeEvent(input$back_to_login_link, {
      runjs(sprintf("showAuthTab('%s', 'login')", ns("")))
    })
    
    # Send Reset Email Handler (keep the same)
    observeEvent(input$send_reset, {
      req(input$reset_email)
      
      # Clear previous messages
      shinyjs::hide("reset_message")
      shinyjs::removeClass("reset_message", "alert-danger")
      shinyjs::removeClass("reset_message", "alert-success")
      
      if (grepl("@", input$reset_email)) {
        # Show loading state
        shinyjs::disable("send_reset")
        shinyjs::html("send_reset", "Sending...")
        
        success <- supabase_reset_password(input$reset_email)
        
        if (success) {
          shinyjs::html("reset_message", 
            "If that email exists in our system, you'll receive a reset link shortly.")
          shinyjs::show("reset_message")
          shinyjs::addClass("reset_message", "alert-success")
          
          # Clear form
          updateTextInput(session, "reset_email", value = "")
          
          # Go back to login after delay
          shinyjs::delay(3000, {
            runjs(sprintf("showAuthTab('%s', 'login')", ns("")))
          })
        } else {
          shinyjs::html("reset_message", 
            "Unable to process request. Please try again later.")
          shinyjs::show("reset_message")
          shinyjs::addClass("reset_message", "alert-danger")
        }
        
        # Reset button
        shinyjs::enable("send_reset")
        shinyjs::html("send_reset", "Send Reset Link")
      } else {
        shinyjs::html("reset_message", "Please enter a valid email address")
        shinyjs::show("reset_message")
        shinyjs::addClass("reset_message", "alert-danger")
      }
    })
    
    # Send Reset Email
    observeEvent(input$send_reset, {
      req(input$reset_email)
      
      if (grepl("@", input$reset_email)) {
        success <- supabase_reset_password(input$reset_email)
        
        removeModal()
        
        if (success) {
          showNotification(
            "If that email exists in our system, you'll receive a reset link shortly.",
            type = "message",
            duration = 5
          )
        } else {
          showNotification(
            "Unable to process request. Please try again later.",
            type = "error",
            duration = 5
          )
        }
      } else {
        showNotification("Please enter a valid email address", type = "warning")
      }
    })


    # Return user data for other modules to use
    return(user_data)
  })
}