# supabase_config.R
# Simplified Supabase Configuration and Authentication Functions

library(httr)
library(jsonlite)
library(shiny)
library(shinyjs)

# Try to load environment variables
if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

# Supabase Configuration
SUPABASE_URL      <- Sys.getenv("SUPABASE_URL", "")
SUPABASE_ANON_KEY <- Sys.getenv("SUPABASE_ANON_KEY", "")

# For testing only - replace with your actual values
if (nchar(SUPABASE_URL) == 0) {
  warning("Supabase credentials not found. Please set SUPABASE_URL and SUPABASE_ANON_KEY environment variables.")
}

# Only set auth URLs if credentials are available
if (nchar(SUPABASE_URL) > 0 && nchar(SUPABASE_ANON_KEY) > 0) {
  SUPABASE_AUTH_URL <- paste0(SUPABASE_URL, "/auth/v1")
  SUPABASE_REST_URL <- paste0(SUPABASE_URL, "/rest/v1")
} else {
  SUPABASE_AUTH_URL <- ""
  SUPABASE_REST_URL <- ""
}

# Helper function to make authenticated requests
make_supabase_request <- function(endpoint, method = "GET", body = NULL, token = NULL) {
  if (nchar(SUPABASE_URL) == 0 || nchar(SUPABASE_ANON_KEY) == 0) {
    warning("Supabase not configured")
    return(NULL)
  }
  
  headers <- c(
    "apikey" = as.character(SUPABASE_ANON_KEY),
    "Content-Type" = "application/json"
  )
  
  if (!is.null(token) && token != "guest-token") {
    headers["Authorization"] <- paste("Bearer", as.character(token))
  }
  
  url <- paste0(SUPABASE_REST_URL, endpoint)
  
  tryCatch({
    if (method == "GET") {
      response <- GET(url, add_headers(.headers = headers))
    } else if (method == "POST") {
      response <- POST(url, add_headers(.headers = headers), body = body, encode = "json")
    } else if (method == "PATCH") {
      response <- PATCH(url, add_headers(.headers = headers), body = body, encode = "json")
    } else if (method == "DELETE") {
      response <- DELETE(url, add_headers(.headers = headers))
    }
    
    return(response)
  }, error = function(e) {
    message("Supabase request failed: ", e$message)
    return(NULL)
  })
}

# Simplified Authentication Functions
supabase_signup <- function(email, password, full_name = NULL) {
  if (nchar(SUPABASE_URL) == 0 || nchar(SUPABASE_ANON_KEY) == 0) {
    return(list(success = FALSE, error = "Supabase not configured. Please contact administrator."))
  }
  
  url <- paste0(SUPABASE_AUTH_URL, "/signup")
  
  # Include metadata in signup
  body <- list(
    email = email, 
    password = password,
    options = list(
      data = list(
        full_name = full_name
      )
    )
  )
  
  headers <- c(
    "apikey" = as.character(SUPABASE_ANON_KEY),
    "Content-Type" = "application/json"
  )
  
  tryCatch({
    response <- POST(
      url,
      add_headers(.headers = headers),
      body = body,
      encode = "json"
    )
    
    content <- content(response, "parsed")
    
    if (status_code(response) == 200) {
      # Automatically create profile after signup
      if (!is.null(content$user) && !is.null(full_name)) {
        create_user_profile_simple(
          content$user$id, 
          full_name, 
          content$session$access_token,
          email
        )
      }
      
      return(list(
        success = TRUE, 
        user = content$user, 
        session = content$session
      ))
    } else {
      error_msg <- content$msg %||% content$error_description %||% "Unknown error"
      return(list(success = FALSE, error = error_msg))
    }
  }, error = function(e) {
    return(list(success = FALSE, error = paste("Connection error:", e$message)))
  })
}

supabase_signin <- function(email, password) {
  if (nchar(SUPABASE_URL) == 0 || nchar(SUPABASE_ANON_KEY) == 0) {
    return(list(success = FALSE, error = "Supabase not configured. Please contact administrator."))
  }
  
  url <- paste0(SUPABASE_AUTH_URL, "/token?grant_type=password")
  body <- list(email = email, password = password)
  
  headers <- c(
    "apikey" = as.character(SUPABASE_ANON_KEY),
    "Content-Type" = "application/json"
  )
  
  tryCatch({
    response <- POST(
      url,
      add_headers(.headers = headers),
      body = body,
      encode = "json"
    )
    
    if (status_code(response) == 200) {
      content <- content(response, "parsed")
      
      # Try to get or create profile
      profile <- get_or_create_user_profile(
        content$user$id, 
        content$user$email,
        content$access_token
      )
      
      return(list(
        success = TRUE, 
        user = content$user, 
        session = content,
        profile = profile
      ))
    } else {
      error_content <- content(response, "parsed")
      error_msg <- error_content$error_description %||% error_content$error %||% "Invalid credentials"
      return(list(success = FALSE, error = error_msg))
    }
  }, error = function(e) {
    return(list(success = FALSE, error = paste("Connection error:", e$message)))
  })
}

supabase_signout <- function(token) {
  if (is.null(token) || token == "guest-token") {
    return(TRUE)
  }
  
  if (nchar(SUPABASE_URL) == 0 || nchar(SUPABASE_ANON_KEY) == 0) {
    return(TRUE)
  }
  
  url <- paste0(SUPABASE_AUTH_URL, "/logout")
  
  headers <- c(
    "apikey" = as.character(SUPABASE_ANON_KEY),
    "Authorization" = paste("Bearer", as.character(token))
  )
  
  tryCatch({
    response <- POST(url, add_headers(.headers = headers))
    return(status_code(response) %in% c(200, 204))
  }, error = function(e) {
    return(TRUE)
  })
}

# Simplified Profile Functions
create_user_profile_simple <- function(user_id, full_name, token, email = NULL) {
  endpoint <- "/user_profiles"

  # Ensure we have a valid full name
  if (is.null(full_name) || nchar(trimws(full_name)) == 0) {
    full_name <- "User"  # Default fallback
  }  

  # Use upsert to avoid conflicts
  body <- list(
    user_id = user_id,
    full_name = trimws(full_name),  
    email = email,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  
  response <- make_supabase_request(
    paste0(endpoint, "?on_conflict=user_id"), 
    "POST", 
    body, 
    token
  )
  
  if (!is.null(response)) {
    if (status_code(response) %in% c(200, 201)) {
      print(paste("Profile created/updated for user:", user_id, "with name:", full_name))
      return(TRUE)
    } else {
      print(paste("Failed to create profile. Status:", status_code(response)))
      print(content(response, "text"))
    }
  }
  return(FALSE)
}

get_or_create_user_profile <- function(user_id, email, token) {
  # First try to get existing profile
  endpoint <- paste0("/user_profiles?user_id=eq.", user_id, "&select=*")
  response <- make_supabase_request(endpoint, "GET", token = token)
  
  if (!is.null(response) && status_code(response) == 200) {
    profiles <- content(response, "parsed")
    if (length(profiles) > 0) {
      profile <- profiles[[1]]
      # Ensure we have all necessary fields
      if (is.null(profile$full_name) || nchar(trimws(profile$full_name)) == 0) {
        profile$full_name <- strsplit(email, "@")[[1]][1]
      }
      # Add email if missing
      if (is.null(profile$email)) {
        profile$email <- email
      }      
      return(profile)
    }
  }
  
  # If no profile exists, create a basic one
  # Extract name from email if needed
  display_name <- strsplit(email, "@")[[1]][1]
  
  if (create_user_profile_simple(user_id, display_name, token, email)) {
    # Return basic profile
    return(list(
      user_id = user_id,
      full_name = display_name,
      email = email
    ))
  }
  
  # If all else fails, return a minimal profile
  return(list(
    user_id = user_id,
    full_name = strsplit(email, "@")[[1]][1],
    email = email
  ))
}

# User Progress Functions
save_user_progress <- function(user_id, module_name, score, percentage, completed_at, token) {
  if (is.null(make_supabase_request)) {
    return(FALSE)
  }
  
  endpoint <- "/user_progress"
  
  # Create the progress record
  body <- list(
    user_id = user_id,
    module_name = module_name,
    score = as.numeric(score),
    total_questions = if (grepl("module", module_name)) {
      # Set total questions based on module
      switch(module_name,
        "module1" = 10,
        "module2" = 8,
        "module3" = 10,  # Update these based on your actual modules
        "module4" = 10,
        "module5" = 10,
        "module6" = 11,
        "module7" = 15,
        "module8" = 10,
        "module9" = 10,
        "module10" = 10,
        "module11" = 11,
        "module12" = 14,
        "module13" = 10,
        "module14" = 14,
        "module15" = 14,
        10  # default
      )
    } else {
      10
    },
    percentage = as.numeric(percentage),
    completed_at = format(completed_at, "%Y-%m-%dT%H:%M:%S"),
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  
  # Use upsert to update if record exists
  response <- make_supabase_request(
    paste0(endpoint, "?on_conflict=user_id,module_name"), 
    "POST", 
    body, 
    token
  )
  
  if (!is.null(response)) {
    return(status_code(response) %in% c(200, 201))
  }
  return(FALSE)
}

# Enhanced function to get detailed progress
get_user_progress <- function(user_id, token) {
  if (is.null(make_supabase_request)) {
    return(NULL)
  }
  
  # Get progress ordered by module name
  endpoint <- paste0("/user_progress?user_id=eq.", user_id, "&order=module_name")
  response <- make_supabase_request(endpoint, "GET", token = token)
  
  if (!is.null(response) && status_code(response) == 200) {
    progress <- content(response, "parsed")
    # Filter to only include module1 through module15
    progress <- Filter(function(x) {
      grepl("^module([1-9]|1[0-5])$", x$module_name)
    }, progress)
    return(progress)
  }
  return(NULL)
}

# Function to update user's full name

update_user_full_name <- function(user_id, full_name, token) {
  if (nchar(SUPABASE_URL) == 0 || nchar(SUPABASE_ANON_KEY) == 0) {
    return(FALSE)
  }
  
  endpoint <- paste0("/user_profiles?user_id=eq.", user_id)
  
  body <- list(
    full_name = full_name,
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  
  response <- make_supabase_request(endpoint, "PATCH", body, token)
  
  if (!is.null(response)) {
    return(status_code(response) %in% c(200, 204))
  }
  return(FALSE)
}


# Guest Mode Support
create_guest_session <- function() {
  list(
    is_authenticated = TRUE,
    user_id = paste0("guest_", format(Sys.time(), "%Y%m%d%H%M%S")),
    email = "guest@example.com",
    token = "guest-token",
    profile = list(
      full_name = "Guest User",
      organization = "Guest"
    ),
    is_guest = TRUE
  )
}

# Simple password reset - just sends the email
supabase_reset_password <- function(email) {
  if (nchar(SUPABASE_URL) == 0 || nchar(SUPABASE_ANON_KEY) == 0) {
    print("Supabase not configured")
    return(FALSE)
  }
  
  url <- paste0(SUPABASE_AUTH_URL, "/recover")
  
  headers <- c(
    "apikey" = as.character(SUPABASE_ANON_KEY),
    "Content-Type" = "application/json"
  )
  
  # Add redirect URL for better compatibility
  body <- list(
    email = email,
    redirect_to = "http://localhost:3838/"  # Update this to your app URL
  )
  
  tryCatch({
    response <- POST(
      url,
      add_headers(.headers = headers),
      body = body,
      encode = "json"
    )
    
    # Debug output
    print(paste("Reset password response status:", status_code(response)))
    
    if (status_code(response) != 200) {
      # Print error details for debugging
      error_content <- content(response, "text")
      print(paste("Supabase error:", error_content))
    }
    
    return(status_code(response) == 200)
  }, error = function(e) {
    print(paste("Reset password error:", e$message))
    return(FALSE)
  })
}


# Add these admin functions to your supabase_config.R file

# Check if user is admin (add admin emails to this list)
is_admin <- function(email) {
  admin_emails <- c(
    "rochieng@kenbright.africa", 
    "robinochieng73@gmail.com"  
  )
  return(tolower(email) %in% tolower(admin_emails))
}

# Get all users with their profiles
get_all_users <- function(token) {
  if (is.null(make_supabase_request)) {
    return(NULL)
  }
  
  endpoint <- "/user_profiles?select=*&order=created_at.desc"
  response <- make_supabase_request(endpoint, "GET", token = token)
  
  if (!is.null(response) && status_code(response) == 200) {
    users <- content(response, "parsed")
    return(users)
  }
  return(NULL)
}

# Get all progress records
get_all_progress <- function(token) {
  if (is.null(make_supabase_request)) {
    return(NULL)
  }
  
  endpoint <- "/user_progress?select=*"
  response <- make_supabase_request(endpoint, "GET", token = token)
  
  if (!is.null(response) && status_code(response) == 200) {
    progress <- content(response, "parsed")
    return(progress)
  }
  return(NULL)
}

# Get progress summary statistics
get_progress_stats <- function(token) {
  progress_data <- get_all_progress(token)
  if (is.null(progress_data) || length(progress_data) == 0) {
    return(list(
      total_users = 0,
      total_completions = 0,
      avg_score = 0,
      modules_stats = list()
    ))
  }
  
  # Convert to dataframe for easier analysis
  progress_df <- do.call(rbind, lapply(progress_data, function(x) {
    data.frame(
      user_id = x$user_id %||% NA,
      module_name = x$module_name %||% NA,
      score = as.numeric(x$score %||% 0),
      percentage = as.numeric(x$percentage %||% 0),
      completed_at = x$completed_at %||% NA,
      stringsAsFactors = FALSE
    )
  }))
  
  # Calculate statistics
  stats <- list(
    total_users = length(unique(progress_df$user_id)),
    total_completions = nrow(progress_df),
    avg_score = round(mean(progress_df$percentage, na.rm = TRUE), 1),
    modules_stats = progress_df %>%
      group_by(module_name) %>%
      summarise(
        completions = n(),
        avg_score = round(mean(percentage, na.rm = TRUE), 1),
        pass_rate = round(sum(percentage >= 70) / n() * 100, 1)
      ) %>%
      arrange(module_name)
  )
  
  return(stats)
}

# Get recent activity
get_recent_activity <- function(token, days = 7) {
  progress_data <- get_all_progress(token)
  if (is.null(progress_data) || length(progress_data) == 0) {
    return(NULL)
  }
  
  # Convert to dataframe
  activity_df <- do.call(rbind, lapply(progress_data, function(x) {
    data.frame(
      user_id = x$user_id %||% NA,
      module_name = x$module_name %||% NA,
      percentage = as.numeric(x$percentage %||% 0),
      completed_at = as.POSIXct(x$completed_at %||% NA),
      stringsAsFactors = FALSE
    )
  }))
  
  # Filter recent activity
  cutoff_date <- Sys.Date() - days
  recent_activity <- activity_df %>%
    filter(completed_at >= cutoff_date) %>%
    arrange(desc(completed_at))
  
  return(recent_activity)
}

# Get user details with progress
get_user_details <- function(user_id, token) {
  # Get user profile
  profile_endpoint <- paste0("/user_profiles?user_id=eq.", user_id)
  profile_response <- make_supabase_request(profile_endpoint, "GET", token = token)
  
  # Get user progress
  progress_endpoint <- paste0("/user_progress?user_id=eq.", user_id, "&order=module_name")
  progress_response <- make_supabase_request(progress_endpoint, "GET", token = token)
  
  user_info <- list(profile = NULL, progress = NULL)
  
  if (!is.null(profile_response) && status_code(profile_response) == 200) {
    profiles <- content(profile_response, "parsed")
    if (length(profiles) > 0) {
      user_info$profile <- profiles[[1]]
    }
  }
  
  if (!is.null(progress_response) && status_code(progress_response) == 200) {
    user_info$progress <- content(progress_response, "parsed")
  }
  
  return(user_info)
}

# Function to insert quiz questions
insert_quiz_questions <- function(questions_data, token) {
  endpoint <- "/quiz_questions"
  
  # Use upsert to avoid duplicates
  response <- make_supabase_request(
    paste0(endpoint, "?on_conflict=module_name,question_number"), 
    "POST", 
    questions_data, 
    token
  )
  
  if (!is.null(response)) {
    return(status_code(response) %in% c(200, 201))
  }
  return(FALSE)
}

# Function to get all quiz questions
get_all_quiz_questions <- function(token) {
  endpoint <- "/quiz_questions?order=module_number,question_number"
  response <- make_supabase_request(endpoint, "GET", token = token)
  
  if (!is.null(response) && status_code(response) == 200) {
    return(content(response, "parsed"))
  }
  return(NULL)
}

# Function to get quiz questions by module
get_module_quiz_questions <- function(module_name, token) {
  endpoint <- paste0("/quiz_questions?module_name=eq.", module_name, "&order=question_number")
  response <- make_supabase_request(endpoint, "GET", token = token)
  
  if (!is.null(response) && status_code(response) == 200) {
    return(content(response, "parsed"))
  }
  return(NULL)
}

# Function to record quiz attempt
record_quiz_attempt <- function(user_id, module_name, question_id, selected_answer, is_correct, time_spent = NULL, token) {
  endpoint <- "/quiz_attempts"
  
  body <- list(
    user_id = user_id,
    module_name = module_name,
    question_id = question_id,
    selected_answer = selected_answer,
    is_correct = is_correct,
    attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  
  if (!is.null(time_spent)) {
    body$time_spent <- time_spent
  }
  
  response <- make_supabase_request(endpoint, "POST", body, token)
  
  if (!is.null(response)) {
    return(status_code(response) %in% c(200, 201))
  }
  return(FALSE)
}

# Function to get quiz statistics
get_quiz_statistics <- function(token) {
  # Get all attempts
  endpoint <- "/quiz_attempts?select=*"
  response <- make_supabase_request(endpoint, "GET", token = token)
  
  if (!is.null(response) && status_code(response) == 200) {
    attempts <- content(response, "parsed")
    
    if (length(attempts) > 0) {
      # Convert to dataframe for analysis
      attempts_list <- list()    

      for (i in seq_along(attempts)) {
        attempt <- attempts[[i]]
        
        # Handle is_correct field safely
        is_correct_value <- attempt$is_correct
        if (is.character(is_correct_value)) {
          is_correct_value <- tolower(is_correct_value) %in% c("true", "t", "1")
        } else if (is.numeric(is_correct_value)) {
          is_correct_value <- is_correct_value == 1
        } else if (is.list(is_correct_value)) {
          is_correct_value <- ifelse(length(is_correct_value) > 0, 
                                   as.logical(is_correct_value[[1]]), 
                                   FALSE)
        }
        
        attempts_list[[i]] <- data.frame(
          user_id = attempt$user_id %||% NA,
          module_name = attempt$module_name %||% NA,
          question_id = attempt$question_id %||% NA,
          is_correct = isTRUE(is_correct_value),
          attempted_at = attempt$attempted_at %||% NA,
          stringsAsFactors = FALSE
        )
      }      

      # Convert to dataframe for analysis
      attempts_df <- do.call(rbind, attempts_list) 

      
      # Calculate statistics
      stats <- list(
        total_attempts = nrow(attempts_df),
        unique_users = length(unique(attempts_df$user_id)),
        overall_accuracy = round(mean(attempts_df$is_correct) * 100, 1),
        module_stats = attempts_df %>%
          group_by(module_name) %>%
          summarise(
            attempts = n(),
            accuracy = round(mean(is_correct) * 100, 1),
            unique_users = n_distinct(user_id)
          ) %>%
          arrange(module_name)
      )
      
      return(stats)
    }
  }
  
  return(list(
    total_attempts = 0,
    unique_users = 0,
    overall_accuracy = 0,
    module_stats = data.frame()
  ))
}

# Function to get question performance
get_question_performance <- function(module_name = NULL, token) {
  # Get questions and attempts
  questions_endpoint <- if (!is.null(module_name)) {
    paste0("/quiz_questions?module_name=eq.", module_name, "&select=*")
  } else {
    "/quiz_questions?select=*"
  }
  
  attempts_endpoint <- if (!is.null(module_name)) {
    paste0("/quiz_attempts?module_name=eq.", module_name, "&select=*")
  } else {
    "/quiz_attempts?select=*"
  }
  
  questions_response <- make_supabase_request(questions_endpoint, "GET", token = token)
  attempts_response <- make_supabase_request(attempts_endpoint, "GET", token = token)
  
  if (!is.null(questions_response) && status_code(questions_response) == 200 &&
      !is.null(attempts_response) && status_code(attempts_response) == 200) {
    
    questions <- content(questions_response, "parsed")
    attempts <- content(attempts_response, "parsed")
    
    # Create performance summary
    performance <- lapply(questions, function(q) {
      q_attempts <- Filter(function(a) a$question_id == q$id, attempts)
     
      # Calculate correct attempts safely
      correct_count <- 0
      total_attempts <- length(q_attempts)
      
      if (total_attempts > 0) {
        # Safely count correct attempts
        for (attempt in q_attempts) {
          # Handle different possible formats of is_correct
          is_correct_value <- attempt$is_correct
          
          # Convert to logical if needed
          if (is.character(is_correct_value)) {
            is_correct_value <- tolower(is_correct_value) %in% c("true", "t", "1")
          } else if (is.numeric(is_correct_value)) {
            is_correct_value <- is_correct_value == 1
          } else if (is.list(is_correct_value)) {
            # If it's a list, try to extract the first element
            is_correct_value <- ifelse(length(is_correct_value) > 0, 
                                     as.logical(is_correct_value[[1]]), 
                                     FALSE)
          }
          
          # Only count if it's TRUE
          if (isTRUE(is_correct_value)) {
            correct_count <- correct_count + 1
          }
        }
      }

      list(
        module_name = q$module_name,
        question_number = q$question_number,
        question_text =  q$question_text,
        total_attempts = total_attempts,
        correct_attempts = correct_count,
        accuracy = if(length(q_attempts) > 0) {
          round((correct_count / total_attempts) * 100, 1)
        } else {
          NA
         }
      )
    })
    
    return(performance)
  }
  
  return(NULL)
}
