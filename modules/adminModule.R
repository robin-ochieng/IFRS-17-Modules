# modules/adminModule.R

# Helper function to validate total users count
validate_total_users <- function(n) {
  if (is.null(n) || !is.numeric(n) || n < 0 || !is.integer(as.integer(n))) {
    return(FALSE)
  }
  return(TRUE)
}

# Function to get total users count from database
get_total_users_count <- function(token) {
  if (is.null(make_supabase_request) || is.null(token)) {
    return(NULL)
  }
  
  tryCatch({
    # Use the existing get_all_users function for consistency
    # This ensures we count the same users displayed in the Users tab
    users <- get_all_users(token)
    
    # Debug: Log the actual data we receive
    message("Debug - get_total_users_count: users data type: ", class(users))
    if (!is.null(users)) {
      message("Debug - get_total_users_count: users length: ", length(users))
      if (length(users) > 0 && is.list(users)) {
        message("Debug - get_total_users_count: first user fields: ", paste(names(users[[1]]), collapse = ", "))
      }
    }
    
    if (!is.null(users) && is.list(users)) {
      user_count <- length(users)
      
      # Validate the count before returning
      if (validate_total_users(user_count)) {
        message("Debug - get_total_users_count: returning count: ", user_count)
        return(user_count)
      }
    }
    
    message("Debug - get_total_users_count: returning NULL")
    return(NULL)
    
  }, error = function(e) {
    if (getOption("shiny.dev", FALSE)) {
      showNotification(
        paste("Error fetching user count:", e$message),
        type = "error",
        duration = 5
      )
    }
    message("Error in get_total_users_count: ", e$message)
    return(NULL)
  })
}

# Admin Dashboard UI
adminDashboardUI <- function(id) {
  ns <- NS(id)

  # Add a wrapper div with light theme class
  div(
    class = "admin-light-theme",
    style = "background-color: #f8f9fa; min-height: 100vh; padding: 20px;",
      
  tagList(    
    # Header
    div(
      class = "admin-header",
      h1("📊 Admin Dashboard", style = "margin: 0; color: #1a202c"),
      p("IFRS 17 Training Platform Analytics", style = "margin: 0; opacity: 0.9; color: #4a5568;"),
    ),
    
    # Key Metrics Row
    fluidRow(
      column(3,
        div(
          class = "metric-card",
          icon("users", style = "font-size: 2rem; color: #5b6cd4;"),
          div(class = "metric-number", textOutput(ns("total_users"))),
          div(class = "metric-label", "Total Users")
        )
      ),
      column(3,
        div(
          class = "metric-card",
          icon("graduation-cap", style = "font-size: 2rem; color: #00a896;"),
          div(class = "metric-number", textOutput(ns("total_completions"))),
          div(class = "metric-label", "Module Completions")
        )
      ),
      column(3,
        div(
          class = "metric-card",
          icon("percentage", style = "font-size: 2rem; color: #d64dc7;"),
          div(class = "metric-number", textOutput(ns("avg_score"))),
          div(class = "metric-label", "Average Score")
        )
      ),
      column(3,
        div(
          class = "metric-card",
          icon("chart-line", style = "font-size: 2rem; color: #028090;"),
          div(class = "metric-number", textOutput(ns("completion_rate"))),
          div(class = "metric-label", "Completion Rate")
        )
      )
    ),
    
    br(),
    
    # Tab Navigation
    tabsetPanel(
      id = ns("admin_tabs"),
      type = "pills",
      
      # Overview Tab
      tabPanel("Overview",
        icon = icon("dashboard"),
        br(),
        fluidRow(
          column(6,
            div(
              class = "chart-container",
              h4("📈 Module Performance", style = "color: #2d3748; margin-bottom: 1rem; font-weight: 600;"),
              plotlyOutput(ns("module_performance_chart"), height = "350px")
            )
          ),
          column(6,
            div(
              class = "chart-container",
              h4("📊 Score Distribution", style = "color: #2d3748; margin-bottom: 1rem; font-weight: 600;"),
              plotlyOutput(ns("score_distribution"), height = "350px")
            )
          )
        ),
        br(),
        fluidRow(
          column(12,
            div(
              class = "chart-container",
              h4("📅 Activity Timeline", style = "color: #2d3748; margin-bottom: 1rem; font-weight: 600;"),
              plotlyOutput(ns("activity_timeline"), height = "300px")
            )
          )
        )
      ),
      
      # Users Tab
      tabPanel("Users",
        icon = icon("users"),
        br(),
        fluidRow(
          column(12,
            div(
              class = "user-table-container",
              fluidRow(
                column(6,
                  h4("👥 User Management", style = "color: #2d3748; font-weight: 600;")
                ),
                column(6,
                  div(
                    style = "text-align: right;",
                    textInput(
                      ns("user_search"),
                      label = NULL,
                      placeholder = "Search users...",
                      width = "300px"
                    )
                  )
                )
              ),
              br(),
              DTOutput(ns("users_table"))
            )
          )
        )
      ),
      
      # Quiz Analytics Tab
      tabPanel("Quiz Analytics",
        icon = icon("question-circle"),
        br(),
        fluidRow(
          column(3,
            div(
              class = "metric-card",
              icon("clipboard-check", style = "font-size: 2rem; color: #9f7aea;"),
              div(class = "metric-number", textOutput(ns("total_quiz_attempts"))),
              div(class = "metric-label", "Total Quiz Attempts")
            )
          ),
          column(3,
            div(
              class = "metric-card",
              icon("users", style = "font-size: 2rem; color: #4299e1;"),
              div(class = "metric-number", textOutput(ns("quiz_participants"))),
              div(class = "metric-label", "Quiz Participants")
            )
          ),
          column(3,
            div(
              class = "metric-card",
              icon("percentage", style = "font-size: 2rem; color: #48bb78;"),
              div(class = "metric-number", textOutput(ns("quiz_accuracy"))),
              div(class = "metric-label", "Overall Accuracy")
            )
          ),
          column(3,
            div(
              class = "metric-card",
              icon("book", style = "font-size: 2rem; color: #ed8936;"),
              div(class = "metric-number", textOutput(ns("total_questions"))),
              div(class = "metric-label", "Total Questions")
            )
          )
        ),
        br(),
        fluidRow(
          column(6,
            div(
              class = "chart-container",
              h4("📊 Quiz Performance by Module", style = "color: #2d3748; margin-bottom: 1rem; font-weight: 600;"),
              plotlyOutput(ns("quiz_module_performance"), height = "400px")
            )
          ),
          column(6,
            div(
              class = "chart-container",
              h4("🎯 Question Difficulty Analysis", style = "color: #2d3748; margin-bottom: 1rem; font-weight: 600;"),
              plotlyOutput(ns("question_difficulty"), height = "400px")
            )
          )
        ),
        br(),
        fluidRow(
          column(12,
            div(
              class = "chart-container",
              h4("📋 Question Performance Details", style = "color: #2d3748; margin-bottom: 1rem; font-weight: 600;"),
              div(
                style = "margin-bottom: 15px;",
                selectInput(
                  ns("quiz_module_filter"),
                  label = "Filter by Module:",
                  choices = c("All Modules" = "all", paste0("Module ", 1:15)),
                  selected = "all",
                  width = "250px"
                )
              ),
              DTOutput(ns("question_performance_table"))
            )
          )
        ),
        br(),
        fluidRow(
          column(12,
            div(
              class = "chart-container",
              h4("📚 All Questions & Answers Reference", style = "color: #2d3748; margin-bottom: 1rem; font-weight: 600;"),
              div(
                style = "margin-bottom: 15px;",
                fluidRow(
                  column(4,
                    selectInput(
                      ns("qa_module_filter"),
                      label = "Filter by Module:",
                      choices = c("All Modules" = "all", paste0("Module ", 1:15)),
                      selected = "all",
                      width = "100%"
                    )
                  ),
                  column(4,
                    downloadButton(
                      ns("download_qa"),
                      "Download Q&A",
                      icon = icon("download"),
                      style = "margin-top: 25px; width: 100%;"
                    )
                  )
                )
              ),
              DTOutput(ns("questions_answers_table"))
            )
          )
        )
      ),
      
      # Module Analytics Tab
      tabPanel("Module Analytics",
        icon = icon("chart-bar"),
        br(),
        fluidRow(
          column(12,
            div(
              class = "chart-container",
              h4("📊 Module Completion Rates", style = "color: #2d3748; margin-bottom: 1rem; font-weight: 600;"),
              plotlyOutput(ns("module_completion_rates"), height = "400px")
            )
          )
        ),
        br(),
        fluidRow(
          column(6,
            div(
              class = "chart-container",
              h4("🎯 Pass/Fail Distribution", style = "color: #2d3748; margin-bottom: 1rem; font-weight: 600;"),
              plotlyOutput(ns("pass_fail_chart"), height = "350px")
            )
          ),
          column(6,
            div(
              class = "chart-container",
              h4("⏱️ Module Difficulty Analysis", style = "color: #2d3748; margin-bottom: 1rem; font-weight: 600;"),
              plotlyOutput(ns("difficulty_chart"), height = "350px")
            )
          )
        )
      ),
      
      # Recent Activity Tab
      tabPanel("Recent Activity",
        icon = icon("clock"),
        br(),
        fluidRow(
          column(12,
            div(
              h4("🔔 Recent Activity (Last 7 Days)", style = "color: 2d3748; margin-bottom: 1rem; font-weight: 600;"),
              div(id = ns("activity_feed"))
            )
          )
        )
      )
    ),
    
    
    # User Details Modal placeholder
    uiOutput(ns("user_modal"))
  )
 ) 
}


# Admin Dashboard Server 
adminDashboardServer <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for storing data
    admin_data <- reactiveValues(
      users = NULL,
      progress = NULL,
      stats = NULL,
      recent_activity = NULL,
      quiz_questions = NULL,
      quiz_stats = NULL,
      question_performance = NULL
    )
    
    # Check if user is admin
    is_admin_user <- reactive({
      req(user_data$is_authenticated)
      if (!is.null(user_data$email)) {
        return(is_admin(user_data$email))
      }
      return(FALSE)
    })
    
    # Load admin data
    observe({
      if (is_admin_user() && !is.null(user_data$token)) {
        # Get all data
        admin_data$users <- get_all_users(user_data$token)
        admin_data$progress <- get_all_progress(user_data$token)
        admin_data$stats <- get_progress_stats(user_data$token)
        admin_data$recent_activity <- get_recent_activity(user_data$token)

        # Get quiz data
        admin_data$quiz_questions <- get_all_quiz_questions(user_data$token)
        admin_data$quiz_stats <- get_quiz_statistics(user_data$token)
        admin_data$question_performance <- get_question_performance(NULL, user_data$token)        
      }
    })
    
    # Reactive poll for total users count - updates every 30 seconds for testing
    total_users_reactive <- reactivePoll(
      intervalMillis = 30000,  # 30 seconds for testing, change to 60000 later
      session = session,
      checkFunc = function() {
        # Simple timestamp-based check
        Sys.time()
      },
      valueFunc = function() {
        if (is_admin_user() && !is.null(user_data$token)) {
          count <- get_total_users_count(user_data$token)
          return(count)
        }
        return(NULL)
      }
    )
    
    # Key Metrics Outputs
    output$total_users <- renderText({
      # Get the reactive count
      user_count <- total_users_reactive()
      
      if (!is.null(user_count) && validate_total_users(user_count)) {
        as.character(user_count)
      } else {
        "—"  # Show dash on error or when data unavailable
      }
    })
    
    output$total_completions <- renderText({
      if (!is.null(admin_data$stats)) {
        admin_data$stats$total_completions
      } else {
        "0"
      }
    })
    
    output$avg_score <- renderText({
      if (!is.null(admin_data$stats)) {
        paste0(admin_data$stats$avg_score, "%")
      } else {
        "0%"
      }
    })
    
    output$completion_rate <- renderText({
      user_count <- total_users_reactive()
      
      if (!is.null(user_count) && validate_total_users(user_count) && !is.null(admin_data$stats)) {
        total_possible <- user_count * 15  # 15 modules
        if (total_possible > 0) {
          rate <- round((admin_data$stats$total_completions / total_possible) * 100, 1)
          paste0(rate, "%")
        } else {
          "0%"
        }
      } else {
        "—"  # Show dash when user count is unavailable
      }
    })
    
    # Module Performance Chart
    output$module_performance_chart <- renderPlotly({
      req(admin_data$stats)
      
      if (!is.null(admin_data$stats$modules_stats) && nrow(admin_data$stats$modules_stats) > 0) {
        # Extract module numbers for ordering
        admin_data$stats$modules_stats$module_num <- as.numeric(gsub("module", "", admin_data$stats$modules_stats$module_name))
        admin_data$stats$modules_stats <- admin_data$stats$modules_stats[order(admin_data$stats$modules_stats$module_num), ]
        
        p <- plot_ly(
          data = admin_data$stats$modules_stats,
          x = ~module_name,
          y = ~completions,
          type = 'bar',
          name = 'Completions',
          marker = list(color = '#5b6cd4'),
          hovertemplate = 'Module: %{x}<br>Completions: %{y}<extra></extra>'
        ) %>%
          add_trace(
            y = ~avg_score,
            name = 'Avg Score',
            yaxis = 'y2',
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = '#d64dc7', width = 3),
            marker = list(size = 8),
            hovertemplate = 'Module: %{x}<br>Avg Score: %{y}%<extra></extra>'
          ) %>%
          layout(
            title = NULL,
            xaxis = list(
              title = "Module",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              tickangle = -45,
              tickfont = list(color = "#4a5568")
            ),
            yaxis = list(
              title = "Completions",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              tickfont = list(color = "#4a5568")
            ),
            yaxis2 = list(
              title = "Average Score (%)",
              overlaying = "y",
              side = "right",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              tickfont = list(color = "#4a5568")
            ),
            plot_bgcolor = "#ffffff",
            paper_bgcolor = "#ffffff",
            font = list(color = "#2d3748"),
            showlegend = TRUE,
            legend = list(
              x = 0.7,
              y = 1,
              bgcolor = "rgba(255,255,255,0.9)",
              bordercolor = "#e2e8f0",
              borderwidth = 1,
              font = list(color = "#4a5568")
            ),
            margin = list(b = 80),
            hoverlabel = list(
              bgcolor = "white",
              font = list(color = "#2d3748")
            )
          )
      } else {
        empty_plot("No module data available")
      }
    })
    
    # Score Distribution Chart
    output$score_distribution <- renderPlotly({
      req(admin_data$progress)
      
      if (length(admin_data$progress) > 0) {
        scores <- sapply(admin_data$progress, function(x) as.numeric(x$percentage %||% 0))
        
        p <- plot_ly(
          x = scores,
          type = "histogram",
          nbinsx = 20,
          marker = list(
            color = '#00a896',
            line = list(color = '#ffffff', width = 1)
          ),
          hovertemplate = 'Score Range: %{x}<br>Count: %{y}<extra></extra>'
        ) %>%
          layout(
            title = NULL,
            xaxis = list(
              title = "Score (%)",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              tickfont = list(color = "#4a5568"),
              zeroline = FALSE
            ),
            yaxis = list(
              title = "Number of Completions",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              tickfont = list(color = "#4a5568")
            ),
            plot_bgcolor = "#ffffff",
            paper_bgcolor = "#ffffff",
            font = list(color = "#2d3748"),
            bargap = 0.1,
            hoverlabel = list(
              bgcolor = "white",
              font = list(color = "#2d3748")
            )
          )
      } else {
        empty_plot("No score data available")
      }
    })
    
    # Activity Timeline
    output$activity_timeline <- renderPlotly({
      req(admin_data$progress)
      
      if (length(admin_data$progress) > 0) {
        # Convert to dataframe for easier manipulation
        activity_df <- do.call(rbind, lapply(admin_data$progress, function(x) {
          data.frame(
            date = as.Date(x$completed_at %||% Sys.Date()),
            stringsAsFactors = FALSE
          )
        }))
        
        # Count by date
        daily_counts <- activity_df %>%
          group_by(date) %>%
          summarise(count = n()) %>%
          arrange(date)
        
        # Fill in missing dates
        if (nrow(daily_counts) > 0) {
          date_range <- seq(min(daily_counts$date), max(daily_counts$date), by = "day")
          complete_dates <- data.frame(date = date_range)
          daily_counts <- merge(complete_dates, daily_counts, by = "date", all.x = TRUE)
          daily_counts$count[is.na(daily_counts$count)] <- 0
        }
        
        p <- plot_ly(
          data = daily_counts,
          x = ~date,
          y = ~count,
          type = 'scatter',
          mode = 'lines+markers',
          fill = 'tozeroy',
          fillcolor = 'rgba(91, 108, 212, 0.15)',
          line = list(color = '#5b6cd4', width = 3),
          marker = list(size = 8, color = '#5b6cd4'),
          hovertemplate = 'Date: %{x}<br>Completions: %{y}<extra></extra>'
        ) %>%
          layout(
            title = NULL,
            xaxis = list(
              title = "Date",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              type = "date",
              tickfont = list(color = "#4a5568")
            ),
            yaxis = list(
              title = "Module Completions",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              tickfont = list(color = "#4a5568")
            ),
            plot_bgcolor = "#ffffff",
            paper_bgcolor = "#ffffff",
            font = list(color = "#2d3748"),
            hoverlabel = list(
              bgcolor = "white",
              font = list(color = "#2d3748")
            )
          )
      } else {
        empty_plot("No activity data available")
      }
    })

    
    # Users Table
    output$users_table <- renderDT({
      req(admin_data$users)
      
      # Create user summary with proper field handling
      users_df <- do.call(rbind, lapply(admin_data$users, function(user) {
        # Count progress for this user
        user_progress <- if (!is.null(admin_data$progress)) {
          Filter(function(p) p$user_id == user$user_id, admin_data$progress)
        } else {
          list()
        }

        # Get email from user_id if it looks like an email, otherwise show user_id
        display_email <- if (!is.null(user$user_id) && grepl("@", user$user_id)) {
          user$user_id
        } else {
          user$user_id %||% "Unknown ID"
        }
        
        data.frame(
          Name = user$full_name %||% "Unknown User",
          Email = display_email,
          `Modules Completed` = length(user_progress),
          `Average Score` = if(length(user_progress) > 0) {
            round(mean(sapply(user_progress, function(p) as.numeric(p$percentage %||% 0))), 1)
          } else { 0 },
          `Joined` = format(as.Date(user$created_at %||% Sys.Date()), "%b %d, %Y"),
          Organization = user$organization %||% "",
          user_id = user$user_id,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }))
      
      # Apply search filter if provided
      if (!is.null(input$user_search) && nchar(input$user_search) > 0) {
        search_term <- tolower(input$user_search)
        users_df <- users_df[
          grepl(search_term, tolower(users_df$Name)) | 
          grepl(search_term, tolower(users_df$Email)) |
          grepl(search_term, tolower(users_df$Organization)),
        ]
      }

      # Select columns to display - include Organization if it has values
      has_organization <- any(nchar(users_df$Organization) > 0)
      
      if (has_organization) {
        display_columns <- c("Name", "Email", "Organization", "Modules Completed", "Average Score", "Joined")
      } else {
        display_columns <- c("Name", "Email", "Modules Completed", "Average Score", "Joined")
      }

      datatable(
        users_df[, display_columns, drop = FALSE],
        options = list(
          pageLength = 10,
          dom = 'rtip',
          searching = FALSE,  # We handle search manually
          initComplete = JS(
            "function(settings, json) {",
            "  $(this.api().table().container()).find('th').css({",
            "     'background-color': '#f7fafc',",
            "     'color': '#2d3748',",
            "     'font-weight': '600',",
            "     'border-bottom': '2px solid #e2e8f0'",
            "  });",
            "}"
          )
        ),
        class = 'cell-border stripe hover',
        rownames = FALSE,
        selection = 'single'
      ) %>%
        formatStyle(
          columns = 1:length(display_columns),
          color = '#2d3748',
          backgroundColor = '#ffffff'
        ) %>%
        formatStyle(
          'Average Score',
          background = '#ffffff',
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })
    
    # Module Completion Rates
    output$module_completion_rates <- renderPlotly({
      req(admin_data$stats)
      user_count <- total_users_reactive()
      
      if (!is.null(admin_data$stats$modules_stats) && 
          nrow(admin_data$stats$modules_stats) > 0 && 
          !is.null(user_count) && 
          validate_total_users(user_count) &&
          user_count > 0) {
        
        # Calculate completion rate for each module using reactive user count
        module_rates <- admin_data$stats$modules_stats %>%
          mutate(
            completion_rate = round((completions / user_count) * 100, 1),
            module_num = as.numeric(gsub("module", "", module_name))
          ) %>%
          arrange(module_num)
        
        p <- plot_ly(
          data = module_rates,
          x = ~module_name,
          y = ~completion_rate,
          type = 'bar',
          marker = list(
            color = ~completion_rate,
            colorscale = list(
              c(0, '#ef5350'),
              c(0.5, '#ffa726'),
              c(1, '#66bb6a')
            ),
            showscale = TRUE,
            colorbar = list(
              title = "Rate %",
              titlefont = list(color = "#4a5568"),
              tickfont = list(color = "#4a5568"),
              bgcolor = "rgba(255,255,255,0.9)",
              bordercolor = "#e2e8f0",
              borderwidth = 1
            )
          ),
          text = ~paste0(completion_rate, "%"),
          textposition = "outside",
          textfont = list(color = "#4a5568", size = 12),
          hovertemplate = 'Module: %{x}<br>Completion Rate: %{y}%<br>Students: %{customdata}<extra></extra>',
          customdata = ~completions
        ) %>%
          layout(
            title = NULL,
            xaxis = list(
              title = "Module",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              tickangle = -45,
              tickfont = list(color = "#4a5568")
            ),
            yaxis = list(
              title = "Completion Rate (%)",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              range = c(0, 110),
              tickfont = list(color = "#4a5568")
            ),
            plot_bgcolor = "#ffffff",
            paper_bgcolor = "#ffffff",
            font = list(color = "#2d3748"),
            margin = list(b = 100, t = 40),
            hoverlabel = list(
              bgcolor = "white",
              font = list(color = "#2d3748")
            )
          )
      } else {
        empty_plot("No completion rate data available")
      }
    })
    
    # Pass/Fail Chart
    output$pass_fail_chart <- renderPlotly({
      req(admin_data$progress)
      
      if (length(admin_data$progress) > 0) {
        scores <- sapply(admin_data$progress, function(x) as.numeric(x$percentage %||% 0))
        pass_count <- sum(scores >= 70)
        fail_count <- sum(scores < 70)
        
        p <- plot_ly(
          labels = c("Pass (≥70%)", "Fail (<70%)"),
          values = c(pass_count, fail_count),
          type = 'pie',
          marker = list(
            colors = c('#66bb6a', '#ef5350'),
            line = list(color = '#ffffff', width = 2)
          ),
          textinfo = 'label+percent',
          textfont = list(color = '#ffffff', size = 14),
          hovertemplate = '%{label}<br>Count: %{value}<br>%{percent}<extra></extra>'
        ) %>%
          layout(
            title = NULL,
            plot_bgcolor = "#ffffff",
            paper_bgcolor = "#ffffff",
            font = list(color = "#2d3748"),
            showlegend = TRUE,
            legend = list(
              font = list(color = "#4a5568"),
              bgcolor = "rgba(255,255,255,0.9)",
              bordercolor = "#e2e8f0",
              borderwidth = 1
            ),
            hoverlabel = list(
              bgcolor = "white",
              font = list(color = "#2d3748")
          )
        )
      } else {
        empty_plot("No pass/fail data available")
      }
    })
    
    # Module Difficulty Chart
    output$difficulty_chart <- renderPlotly({
      req(admin_data$stats)
      
      if (!is.null(admin_data$stats$modules_stats) && nrow(admin_data$stats$modules_stats) > 0) {
        difficulty_data <- admin_data$stats$modules_stats %>%
          mutate(
            difficulty = case_when(
              avg_score >= 80 ~ "Easy",
              avg_score >= 70 ~ "Medium",
              avg_score >= 60 ~ "Hard",
              TRUE ~ "Very Hard"
            ),
            module_num = as.numeric(gsub("module", "", module_name))
          ) %>%
          arrange(avg_score)
        
        p <- plot_ly(
          data = difficulty_data,
          x = ~avg_score,
          y = ~reorder(module_name, avg_score),
          type = 'bar',
          orientation = 'h',
          marker = list(
            color = ~avg_score,
            colorscale = list(
              c(0, '#ef5350'),
              c(0.5, '#ffa726'),
              c(1, '#66bb6a')
            ),
            showscale = FALSE
          ),
          text = ~paste0(avg_score, "%"),
          textposition = "outside",
          textfont = list(color = "#4a5568", size = 12),
          hovertemplate = 'Module: %{y}<br>Average Score: %{x}%<br>Difficulty: %{customdata}<extra></extra>',
          customdata = ~difficulty
        ) %>%
          layout(
            title = NULL,
            xaxis = list(
              title = "Average Score (%)",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              range = c(0, 105),
              tickfont = list(color = "#4a5568")
            ),
            yaxis = list(
              title = NULL,
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              tickfont = list(color = "#4a5568", size = 11)
            ),
            plot_bgcolor = "#ffffff",
            paper_bgcolor = "#ffffff",
            font = list(color = "#2d3748"),
            margin = list(l = 120, r = 40),
            hoverlabel = list(
              bgcolor = "white",
              font = list(color = "#2d3748")
            )
          )
      } else {
        empty_plot("No difficulty data available")
      }
    })
    
    # Recent Activity Feed
    output$activity_feed <- renderUI({
      req(admin_data$recent_activity)
      
      if (!is.null(admin_data$recent_activity) && nrow(admin_data$recent_activity) > 0) {
        # Get user names
        user_names <- setNames(
          sapply(admin_data$users, function(u) u$full_name %||% "Unknown"),
          sapply(admin_data$users, function(u) u$user_id)
        )
        
        # Create activity items
        activity_items <- lapply(1:min(nrow(admin_data$recent_activity), 20), function(i) {
          activity <- admin_data$recent_activity[i, ]
          user_name <- user_names[activity$user_id] %||% "Unknown User"
          
          div(
            class = "activity-item",
            style = "background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 15px; margin-bottom: 10px;",
            fluidRow(
              column(1,
                icon(
                  if(activity$percentage >= 70) "check-circle" else "times-circle",
                  style = paste0(
                    "font-size: 2rem; color: ",
                    if(activity$percentage >= 70) "#66bb6a" else "#ef5350"
                  )
                )
              ),
              column(11,
                h5(user_name, style = "margin: 0; color: #2d3748; font-weight: 600;"),
                p(
                  paste0(
                    "Completed ", activity$module_name,
                    " with ", activity$percentage, "% score"
                  ),
                  style = "margin: 0; color: #718096;"
                ),
                p(
                  format(activity$completed_at, "%B %d, %Y at %I:%M %p"),
                  style = "margin: 0; font-size: 0.85rem; color: #a0aec0;"
                )
              )
            )
          )
        })
        
        tagList(activity_items)
      } else {
        div(
          class = "text-center-admin",
          style = "color: #718096; padding: 3rem;",
          icon("inbox", style = "font-size: 3rem; color: #cbd5e0;"),
          h4("No recent activity", style = "color: #4a5568; margin-top: 1rem;"),
          p("Module completions from the last 7 days will appear here")
        )
      }
    })
    
    # Helper function for empty plots
    empty_plot <- function(message) {
      plot_ly() %>%
        layout(
          title = NULL,
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          plot_bgcolor = "#ffffff",
          paper_bgcolor = "#ffffff",
          annotations = list(
            list(
              text = message,
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5,
              font = list(size = 16, color = "#718096")
            )
          )
        )
    }

    # Quiz Analytics Outputs
    output$total_quiz_attempts <- renderText({
      if (!is.null(admin_data$quiz_stats)) {
        format(admin_data$quiz_stats$total_attempts, big.mark = ",")
      } else {
        "0"
      }
    })

    output$quiz_participants <- renderText({
      if (!is.null(admin_data$quiz_stats)) {
        format(admin_data$quiz_stats$unique_users, big.mark = ",")
      } else {
        "0"
      }
    })

    output$quiz_accuracy <- renderText({
      if (!is.null(admin_data$quiz_stats)) {
        paste0(admin_data$quiz_stats$overall_accuracy, "%")
      } else {
        "0%"
      }
    })

    output$total_questions <- renderText({
      if (!is.null(admin_data$quiz_questions)) {
        format(length(admin_data$quiz_questions), big.mark = ",")
      } else {
        "0"
      }
    })

    # Quiz Performance by Module Chart
    output$quiz_module_performance <- renderPlotly({
      req(admin_data$quiz_stats)
      
      if (!is.null(admin_data$quiz_stats$module_stats) && nrow(admin_data$quiz_stats$module_stats) > 0) {
        # Extract module numbers for ordering
        admin_data$quiz_stats$module_stats$module_num <- as.numeric(gsub("module", "", admin_data$quiz_stats$module_stats$module_name))
        admin_data$quiz_stats$module_stats <- admin_data$quiz_stats$module_stats[order(admin_data$quiz_stats$module_stats$module_num), ]
        
        p <- plot_ly(
          data = admin_data$quiz_stats$module_stats,
          x = ~module_name,
          y = ~attempts,
          type = 'bar',
          name = 'Attempts',
          marker = list(color = '#9f7aea'),
          hovertemplate = 'Module: %{x}<br>Attempts: %{y}<extra></extra>'
        ) %>%
          add_trace(
            y = ~accuracy,
            name = 'Accuracy %',
            yaxis = 'y2',
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = '#48bb78', width = 3),
            marker = list(size = 8),
            hovertemplate = 'Module: %{x}<br>Accuracy: %{y}%<extra></extra>'
          ) %>%
          layout(
            title = NULL,
            xaxis = list(
              title = "Module",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              tickangle = -45,
              tickfont = list(color = "#4a5568")
            ),
            yaxis = list(
              title = "Number of Attempts",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              tickfont = list(color = "#4a5568")
            ),
            yaxis2 = list(
              title = "Accuracy (%)",
              overlaying = "y",
              side = "right",
              color = "#4a5568",
              gridcolor = "#e2e8f0",
              tickfont = list(color = "#4a5568"),
              range = c(0, 100)
            ),
            plot_bgcolor = "#ffffff",
            paper_bgcolor = "#ffffff",
            font = list(color = "#2d3748"),
            showlegend = TRUE,
            legend = list(
              x = 0.7,
              y = 1,
              bgcolor = "rgba(255,255,255,0.9)",
              bordercolor = "#e2e8f0",
              borderwidth = 1,
              font = list(color = "#4a5568")
            ),
            margin = list(b = 80),
            hoverlabel = list(
              bgcolor = "white",
              font = list(color = "#2d3748")
            )
          )
      } else {
        empty_plot("No quiz data available")
      }
    })

    # Question Difficulty Analysis
    output$question_difficulty <- renderPlotly({
      req(admin_data$question_performance)
      
      if (length(admin_data$question_performance) > 0) {
        # Convert to dataframe and calculate difficulty
        difficulty_df <- do.call(rbind, lapply(admin_data$question_performance, function(q) {
          data.frame(
            question = paste0(q$module_name, " Q", q$question_number),
            accuracy = as.numeric(q$accuracy %||% NA),
            attempts = q$total_attempts,
            stringsAsFactors = FALSE
          )
        }))
        
        # Remove questions with no attempts
        difficulty_df <- difficulty_df[!is.na(difficulty_df$accuracy) & difficulty_df$attempts > 0, ]
        
        if (nrow(difficulty_df) > 0) {
          # Sort by accuracy (lower accuracy = higher difficulty)
          difficulty_df <- difficulty_df[order(difficulty_df$accuracy), ]
          
          # Take top 20 most difficult questions
          if (nrow(difficulty_df) > 20) {
            difficulty_df <- difficulty_df[1:20, ]
          }
          
          p <- plot_ly(
            data = difficulty_df,
            x = ~accuracy,
            y = ~reorder(question, -accuracy),
            type = 'bar',
            orientation = 'h',
            marker = list(
              color = ~accuracy,
              colorscale = list(
                c(0, '#ef5350'),
                c(0.5, '#ffa726'),
                c(1, '#66bb6a')
              ),
              showscale = TRUE,
              colorbar = list(
                title = "Accuracy %",
                titlefont = list(color = "#4a5568"),
                tickfont = list(color = "#4a5568")
              )
            ),
            text = ~paste0(accuracy, "%"),
            textposition = "outside",
            textfont = list(color = "#4a5568", size = 11),
            hovertemplate = 'Question: %{y}<br>Accuracy: %{x}%<br>Attempts: %{customdata}<extra></extra>',
            customdata = ~attempts
          ) %>%
            layout(
              title = NULL,
              xaxis = list(
                title = "Accuracy (%)",
                color = "#4a5568",
                gridcolor = "#e2e8f0",
                range = c(0, 105),
                tickfont = list(color = "#4a5568")
              ),
              yaxis = list(
                title = NULL,
                color = "#4a5568",
                gridcolor = "#e2e8f0",
                tickfont = list(color = "#4a5568", size = 10)
              ),
              plot_bgcolor = "#ffffff",
              paper_bgcolor = "#ffffff",
              font = list(color = "#2d3748"),
              margin = list(l = 100, r = 40),
              hoverlabel = list(
                bgcolor = "white",
                font = list(color = "#2d3748")
              )
            )
        } else {
          empty_plot("No question performance data available")
        }
      } else {
        empty_plot("No question performance data available")
      }
    })

    # Question Performance Table
    output$question_performance_table <- renderDT({
      req(admin_data$question_performance)
      
      if (length(admin_data$question_performance) > 0) {
        # Convert to dataframe
        performance_df <- do.call(rbind, lapply(admin_data$question_performance, function(q) {
          data.frame(
            Module = q$module_name,
            `Question #` = q$question_number,
            `Question Text` = paste0(substr(q$question_text, 1, 60), "..."),
            Attempts = q$total_attempts,
            `Correct` = q$correct_attempts,
            `Accuracy` = ifelse(is.na(q$accuracy), 0, q$accuracy),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        }))
        
        # Apply module filter
        if (!is.null(input$quiz_module_filter) && input$quiz_module_filter != "all") {
          module_num <- as.numeric(gsub("Module ", "", input$quiz_module_filter))
          performance_df <- performance_df[performance_df$Module == paste0("module", module_num), ]
        }
        
        # Sort by module and question number
        performance_df$module_num <- as.numeric(gsub("module", "", performance_df$Module))
        performance_df <- performance_df[order(performance_df$module_num, performance_df$`Question #`), ]
        performance_df$module_num <- NULL
        
        datatable(
          performance_df,
          options = list(
            pageLength = 15,
            dom = 'rtip',
            columnDefs = list(
              list(width = '80px', targets = c(0, 1)),
              list(width = '400px', targets = 2),
              list(className = 'dt-center', targets = c(1, 3, 4, 5))
            ),
            initComplete = JS(
              "function(settings, json) {",
              "  $(this.api().table().container()).find('th').css({",
              "    'background-color': '#f7fafc',",
              "    'color': '#2d3748',",
              "    'font-weight': '600'",
              "  });",
              "}"
            )
          ),
          class = 'cell-border stripe hover',
          rownames = FALSE
        ) %>%
          formatStyle(
            columns = 1:6,
            color = '#2d3748',
            backgroundColor = '#ffffff'
          ) %>%
          formatStyle(
            'Accuracy',
            background = styleColorBar(c(0, 100), '#9f7aea'),
            backgroundSize = '100% 90%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          ) %>%
          formatPercentage('Accuracy', 0)
      } else {
        datatable(
          data.frame(Message = "No question performance data available"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      }
    })

    # Update question performance when module filter changes
    observeEvent(input$quiz_module_filter, {
      if (!is.null(user_data$token)) {
        if (input$quiz_module_filter == "all") {
          admin_data$question_performance <- get_question_performance(NULL, user_data$token)
        } else {
          module_num <- as.numeric(gsub("Module ", "", input$quiz_module_filter))
          admin_data$question_performance <- get_question_performance(paste0("module", module_num), user_data$token)
        }
      }
    })    
    
    # User Details Modal (when clicking on a user in the table)
    observeEvent(input$users_table_rows_selected, {
      req(input$users_table_rows_selected, admin_data$users)
      
      selected_row <- input$users_table_rows_selected
      if (length(admin_data$users) >= selected_row) {
        selected_user <- admin_data$users[[selected_row]]
        user_details <- get_user_details(selected_user$user_id, user_data$token)
        
        showModal(
          modalDialog(
            title = tagList(
              icon("user"),
              paste("User Details:", selected_user$full_name %||% "Unknown")
            ),
            size = "l",
            easyClose = TRUE,
            
            fluidRow(
              column(6,
                h4("Profile Information", style = "color: #2d3748;"),
                tags$dl(
                  tags$dt("Email:"),
                  tags$dd(selected_user$email %||% "N/A"),
                  tags$dt("User ID:"),
                  tags$dd(selected_user$user_id),
                  tags$dt("Joined:"),
                  tags$dd(format(as.Date(selected_user$created_at %||% Sys.Date()), "%B %d, %Y"))
                )
              ),
              column(6,
                h4("Progress Summary", style = "color: #2d3748;"),
                if (!is.null(user_details$progress) && length(user_details$progress) > 0) {
                  progress_summary <- data.frame(
                    modules_completed = length(user_details$progress),
                    avg_score = round(mean(sapply(user_details$progress, function(p) 
                      as.numeric(p$percentage %||% 0))), 1),
                    total_modules = 15
                  )
                  
                  tags$dl(
                    style = "color: #4a5568;",
                    tags$dt("Modules Completed:"),
                    tags$dd(paste0(progress_summary$modules_completed, " / ", progress_summary$total_modules)),
                    tags$dt("Average Score:"),
                    tags$dd(paste0(progress_summary$avg_score, "%")),
                    tags$dt("Completion Rate:"),
                    tags$dd(paste0(round((progress_summary$modules_completed / progress_summary$total_modules) * 100, 1), "%"))
                  )
                } else {
                  p("No progress recorded", style = "color: #a0aec0;")
                }
              )
            ),
            
            hr(),
            
            h4("Module Progress Details", style = "color: #2d3748;"),
            if (!is.null(user_details$progress) && length(user_details$progress) > 0) {
              DT::renderDataTable({
                progress_df <- do.call(rbind, lapply(user_details$progress, function(p) {
                  data.frame(
                    Module = p$module_name,
                    Score = paste0(p$score, " / ", p$total_questions %||% "?"),
                    Percentage = paste0(p$percentage, "%"),
                    `Completed On` = format(as.POSIXct(p$completed_at), "%b %d, %Y"),
                    stringsAsFactors = FALSE
                  )
                }))
                
                datatable(
                  progress_df,
                  options = list(
                    pageLength = 15,
                    dom = 't',
                    ordering = FALSE
                  ),
                  rownames = FALSE,
                  class = 'cell-border stripe'
                ) %>%
                  formatStyle(
                    columns = 1:4,
                    color = '#2d3748',
                    backgroundColor = '#ffffff'
                  )
              })
            } else {
              p("No modules completed yet", style = "text-align: center; color: #a0aec0; padding: 2rem;")
            },
            
            footer = modalButton("Close")
          )
        )
      }
    })

    # Questions & Answers Reference Table
    output$questions_answers_table <- renderDT({
      req(admin_data$quiz_questions)
      
      if (length(admin_data$quiz_questions) > 0) {
        # Convert to dataframe
        qa_df <- do.call(rbind, lapply(admin_data$quiz_questions, function(q) {
          data.frame(
            Module = q$module_name,
            `Q#` = q$question_number,
            Question = q$question_text,
            `Option A` = q$option_a,
            `Option B` = q$option_b,
            `Option C` = q$option_c,
            `Option D` = q$option_d,
            `Correct Answer` = q$correct_answer,
            Explanation = q$explanation,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        }))
        
        # Apply module filter
        if (!is.null(input$qa_module_filter) && input$qa_module_filter != "all") {
          module_num <- as.numeric(gsub("Module ", "", input$qa_module_filter))
          qa_df <- qa_df[qa_df$Module == paste0("module", module_num), ]
        }
        
        # Sort by module and question number
        qa_df$module_num <- as.numeric(gsub("module", "", qa_df$Module))
        qa_df <- qa_df[order(qa_df$module_num, qa_df$`Q#`), ]
        qa_df$module_num <- NULL
        
        # Format module names for display
        qa_df$Module <- gsub("module", "Module ", qa_df$Module)
        
        datatable(
          qa_df,
          options = list(
            pageLength = 10,
            dom = 'lrtip',
            scrollX = TRUE,
            columnDefs = list(
              list(width = '70px', targets = 0),  # Module
              list(width = '40px', targets = 1),   # Q#
              list(width = '300px', targets = 2),  # Question
              list(width = '150px', targets = c(3, 4, 5, 6)),  # Options
              list(width = '150px', targets = 7),  # Correct Answer
              list(width = '300px', targets = 8),  # Explanation
              list(className = 'dt-center', targets = c(0, 1))
            ),
            initComplete = JS(
              "function(settings, json) {",
              "  $(this.api().table().container()).find('th').css({",
              "    'background-color': '#f7fafc',",
              "    'color': '#2d3748',",
              "    'font-weight': '600'",
              "  });",
              "}"
            )
          ),
          class = 'cell-border stripe hover compact',
          rownames = FALSE,
          escape = FALSE
        ) %>%
          formatStyle(
            columns = 1:9,
            color = '#2d3748',
            backgroundColor = '#ffffff',
            fontSize = '13px'
          ) %>%
          formatStyle(
            'Correct Answer',
            backgroundColor = '#e6fffa',
            fontWeight = 'bold',
            color = '#00a896'
          ) %>%
          formatStyle(
            'Explanation',
            fontSize = '12px',
            color = '#4a5568'
          )
      } else {
        datatable(
          data.frame(Message = "No questions data available"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      }
    })

    # Download handler for Q&A
    output$download_qa <- downloadHandler(
      filename = function() {
        if (input$qa_module_filter == "all") {
          paste0("IFRS17_All_Questions_Answers_", Sys.Date(), ".csv")
        } else {
          module_num <- gsub("Module ", "", input$qa_module_filter)
          paste0("IFRS17_Module", module_num, "_Questions_Answers_", Sys.Date(), ".csv")
        }
      },
      content = function(file) {
        req(admin_data$quiz_questions)
        
        # Create the same dataframe as in the table
        qa_df <- do.call(rbind, lapply(admin_data$quiz_questions, function(q) {
          data.frame(
            Module = q$module_name,
            Question_Number = q$question_number,
            Question = q$question_text,
            Option_A = q$option_a,
            Option_B = q$option_b,
            Option_C = q$option_c,
            Option_D = q$option_d,
            Correct_Answer = q$correct_answer,
            Explanation = q$explanation,
            stringsAsFactors = FALSE
          )
        }))
        
        # Apply filter if needed
        if (input$qa_module_filter != "all") {
          module_num <- as.numeric(gsub("Module ", "", input$qa_module_filter))
          qa_df <- qa_df[qa_df$Module == paste0("module", module_num), ]
        }
        
        # Sort and format
        qa_df$module_num <- as.numeric(gsub("module", "", qa_df$Module))
        qa_df <- qa_df[order(qa_df$module_num, qa_df$Question_Number), ]
        qa_df$module_num <- NULL
        qa_df$Module <- gsub("module", "Module ", qa_df$Module)
        
        # Write CSV
        write.csv(qa_df, file, row.names = FALSE)
      }
    )

    # Update quiz questions when filter changes
    observeEvent(input$qa_module_filter, {
      # The table will automatically update due to reactive dependency
      # This observer is here if you need to do any additional processing
    })



  })
}