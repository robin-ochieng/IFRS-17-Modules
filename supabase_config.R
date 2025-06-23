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
        create_user_profile_simple(content$user$id, full_name, content$session$access_token)
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
create_user_profile_simple <- function(user_id, full_name, token) {
  endpoint <- "/user_profiles"
  
  # Use upsert to avoid conflicts
  body <- list(
    user_id = user_id,
    full_name = full_name,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  
  response <- make_supabase_request(
    paste0(endpoint, "?on_conflict=user_id"), 
    "POST", 
    body, 
    token
  )
  
  if (!is.null(response)) {
    return(status_code(response) %in% c(200, 201))
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
      return(profiles[[1]])
    }
  }
  
  # If no profile exists, create a basic one
  # Extract name from email if needed
  display_name <- strsplit(email, "@")[[1]][1]
  
  if (create_user_profile_simple(user_id, display_name, token)) {
    # Return basic profile
    return(list(
      user_id = user_id,
      full_name = display_name,
      email = email
    ))
  }
  
  return(NULL)
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