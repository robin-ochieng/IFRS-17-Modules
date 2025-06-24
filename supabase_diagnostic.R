# supabase_diagnostic.R
# Diagnostic tool to test Supabase configuration and connection

library(httr)
library(jsonlite)

# Function to test Supabase configuration
test_supabase_config <- function() {
  cat("=== SUPABASE CONFIGURATION DIAGNOSTIC ===\n\n")
  
  # 1. Check environment variables
  cat("1. Checking environment variables...\n")
  supabase_url <- Sys.getenv("SUPABASE_URL", "")
  supabase_key <- Sys.getenv("SUPABASE_ANON_KEY", "")
  
  if (nchar(supabase_url) == 0) {
    cat("❌ SUPABASE_URL is not set\n")
    cat("   Please set your Supabase URL in your .env file or environment\n")
    cat("   Example: SUPABASE_URL=https://your-project.supabase.co\n\n")
    return(FALSE)
  } else {
    cat("✅ SUPABASE_URL found:", supabase_url, "\n")
  }
  
  if (nchar(supabase_key) == 0) {
    cat("❌ SUPABASE_ANON_KEY is not set\n")
    cat("   Please set your Supabase anonymous key in your .env file or environment\n")
    cat("   Example: SUPABASE_ANON_KEY=your-anon-key-here\n\n")
    return(FALSE)
  } else {
    cat("✅ SUPABASE_ANON_KEY found:", paste0(substr(supabase_key, 1, 20), "..."), "\n")
  }
  
  # 2. Test basic connection
  cat("\n2. Testing basic connection to Supabase...\n")
  test_url <- paste0(supabase_url, "/rest/v1/")
  
  tryCatch({
    response <- GET(
      test_url,
      add_headers(
        "apikey" = supabase_key,
        "Content-Type" = "application/json"
      ),
      timeout(10)
    )
    
    if (status_code(response) %in% c(200, 400, 401)) {  # 400/401 are ok - means server is responding
      cat("✅ Successfully connected to Supabase\n")
    } else {
      cat("❌ Unexpected response code:", status_code(response), "\n")
      cat("   Response:", rawToChar(response$content), "\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("❌ Connection failed:", e$message, "\n")
    cat("   Please check your SUPABASE_URL and internet connection\n")
    return(FALSE)
  })
  
  # 3. Test auth endpoint
  cat("\n3. Testing auth endpoint...\n")
  auth_url <- paste0(supabase_url, "/auth/v1/signup")
  
  tryCatch({
    # Try with empty body to see if endpoint responds
    response <- POST(
      auth_url,
      add_headers(
        "apikey" = supabase_key,
        "Content-Type" = "application/json"
      ),
      body = list(),
      encode = "json",
      timeout(10)
    )
    
    # Any response (even error) means the endpoint is working
    if (status_code(response) %in% c(200, 400, 422)) {
      cat("✅ Auth endpoint is responding\n")
    } else {
      cat("❌ Auth endpoint returned unexpected code:", status_code(response), "\n")
      cat("   Response:", rawToChar(response$content), "\n")
    }
  }, error = function(e) {
    cat("❌ Auth endpoint failed:", e$message, "\n")
    return(FALSE)
  })
  
  # 4. Test database table access
  cat("\n4. Testing database table access...\n")
  
  # Test user_profiles table
  profiles_url <- paste0(supabase_url, "/rest/v1/user_profiles?limit=1")
  
  tryCatch({
    response <- GET(
      profiles_url,
      add_headers(
        "apikey" = supabase_key,
        "Content-Type" = "application/json"
      ),
      timeout(10)
    )
    
    if (status_code(response) == 200) {
      cat("✅ user_profiles table is accessible\n")
      profiles <- content(response, "parsed")
      cat("   Found", length(profiles), "existing profile(s)\n")
    } else if (status_code(response) == 401) {
      cat("⚠️  user_profiles table exists but may have RLS restrictions\n")
      cat("   This is normal - RLS should prevent anonymous access\n")
    } else {
      cat("❌ user_profiles table access failed, code:", status_code(response), "\n")
      cat("   Response:", rawToChar(response$content), "\n")
      cat("   Please ensure the user_profiles table exists in your database\n")
    }
  }, error = function(e) {
    cat("❌ Database table test failed:", e$message, "\n")
  })
  
  # 5. Check table schema
  cat("\n5. Recommended user_profiles table schema:\n")
  cat("   CREATE TABLE user_profiles (\n")
  cat("     id UUID DEFAULT gen_random_uuid() PRIMARY KEY,\n")
  cat("     user_id UUID REFERENCES auth.users(id) ON DELETE CASCADE UNIQUE,\n")
  cat("     full_name TEXT,\n")
  cat("     organization TEXT,\n")
  cat("     created_at TIMESTAMPTZ DEFAULT NOW(),\n")
  cat("     updated_at TIMESTAMPTZ DEFAULT NOW()\n")
  cat("   );\n\n")
  
  # 6. Check RLS policies
  cat("6. Recommended RLS policies for user_profiles:\n")
  cat("   -- Enable RLS\n")
  cat("   ALTER TABLE user_profiles ENABLE ROW LEVEL SECURITY;\n\n")
  cat("   -- Allow users to read their own profile\n")
  cat("   CREATE POLICY \"Users can view own profile\" ON user_profiles\n")
  cat("     FOR SELECT USING (auth.uid() = user_id);\n\n")
  cat("   -- Allow users to insert their own profile\n")
  cat("   CREATE POLICY \"Users can insert own profile\" ON user_profiles\n")
  cat("     FOR INSERT WITH CHECK (auth.uid() = user_id);\n\n")
  cat("   -- Allow users to update their own profile\n")
  cat("   CREATE POLICY \"Users can update own profile\" ON user_profiles\n")
  cat("     FOR UPDATE USING (auth.uid() = user_id);\n\n")
  
  cat("=== DIAGNOSTIC COMPLETE ===\n")
  cat("If all checks passed, your Supabase configuration should be working.\n")
  cat("If you're still having issues, check the Supabase dashboard for auth logs.\n\n")
  
  return(TRUE)
}

# Function to test a specific signup
test_signup_flow <- function(email = "test@example.com", password = "testpass123", full_name = "Test User") {
  cat("=== TESTING SIGNUP FLOW ===\n\n")
  
  supabase_url <- Sys.getenv("SUPABASE_URL", "")
  supabase_key <- Sys.getenv("SUPABASE_ANON_KEY", "")
  
  if (nchar(supabase_url) == 0 || nchar(supabase_key) == 0) {
    cat("❌ Supabase not configured. Please run test_supabase_config() first.\n")
    return(FALSE)
  }
  
  # Test signup
  cat("Testing signup with email:", email, "\n")
  
  url <- paste0(supabase_url, "/auth/v1/signup")
  
  body <- list(
    email = email, 
    password = password,
    options = list(
      data = list(
        full_name = full_name
      )
    )
  )
  
  cat("Request URL:", url, "\n")
  cat("Request body:", toJSON(body, auto_unbox = TRUE), "\n\n")
  
  tryCatch({
    response <- POST(
      url,
      add_headers(
        "apikey" = supabase_key,
        "Content-Type" = "application/json"
      ),
      body = body,
      encode = "json",
      timeout(30)
    )
    
    status <- status_code(response)
    content <- content(response, "parsed")
    
    cat("Response status:", status, "\n")
    cat("Response content:", toJSON(content, auto_unbox = TRUE, pretty = TRUE), "\n\n")
    
    if (status %in% c(200, 201)) {
      cat("✅ Signup request successful!\n")
      
      if (!is.null(content$user)) {
        cat("User ID:", content$user$id %||% "Not provided", "\n")
        cat("User email:", content$user$email %||% "Not provided", "\n")
        
        if (!is.null(content$session)) {
          cat("Session created:", !is.null(content$session$access_token), "\n")
          
          # Test profile creation
          if (!is.null(content$user$id) && !is.null(content$session$access_token)) {
            cat("\nTesting profile creation...\n")
            test_profile_creation(content$user$id, full_name, content$session$access_token)
          }
        }
      }
      
      return(TRUE)
    } else {
      cat("❌ Signup failed with status:", status, "\n")
      
      if (!is.null(content$message)) {
        cat("Error message:", content$message, "\n")
      }
      if (!is.null(content$error_description)) {
        cat("Error description:", content$error_description, "\n")
      }
      
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("❌ Signup request failed:", e$message, "\n")
    return(FALSE)
  })
}

# Function to test profile creation
test_profile_creation <- function(user_id, full_name, token) {
  supabase_url <- Sys.getenv("SUPABASE_URL", "")
  supabase_key <- Sys.getenv("SUPABASE_ANON_KEY", "")
  
  url <- paste0(supabase_url, "/rest/v1/user_profiles")
  
  body <- list(
    user_id = user_id,
    full_name = full_name,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S+00:00"),
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S+00:00")
  )
  
  cat("Profile creation URL:", url, "\n")
  cat("Profile body:", toJSON(body, auto_unbox = TRUE), "\n")
  
  tryCatch({
    response <- POST(
      url,
      add_headers(
        "apikey" = supabase_key,
        "Authorization" = paste("Bearer", token),
        "Content-Type" = "application/json"
      ),
      body = body,
      encode = "json",
      timeout(30)
    )
    
    status <- status_code(response)
    content <- content(response, "parsed")
    
    cat("Profile creation status:", status, "\n")
    cat("Profile creation response:", toJSON(content, auto_unbox = TRUE, pretty = TRUE), "\n")
    
    if (status %in% c(200, 201)) {
      cat("✅ Profile created successfully!\n")
      return(TRUE)
    } else {
      cat("❌ Profile creation failed\n")
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("❌ Profile creation error:", e$message, "\n")
    return(FALSE)
  })
}

# Convenience function to run all diagnostics
run_full_diagnostic <- function() {
  cat("Starting full Supabase diagnostic...\n\n")
  
  config_ok <- test_supabase_config()
  
  if (config_ok) {
    cat("\n" %R% paste(rep("=", 50), collapse = "") %R% "\n")
    test_signup_flow()
  }
  
  cat("\n=== DIAGNOSTIC SUMMARY ===\n")
  if (config_ok) {
    cat("✅ Configuration tests passed\n")
    cat("If signup test also passed, your Supabase setup should be working.\n")
    cat("If you're still having issues in the app, please check:\n")
    cat("1. Browser console for JavaScript errors\n")
    cat("2. Supabase dashboard Auth logs\n")
    cat("3. Database logs in Supabase dashboard\n")
  } else {
    cat("❌ Configuration issues found\n")
    cat("Please fix the configuration issues before testing signup\n")
  }
}

# Example usage:
# To run the diagnostic, source this file and run:
# run_full_diagnostic()
# 
# Or run individual tests:
# test_supabase_config()
# test_signup_flow("your-test-email@example.com", "yourpassword", "Your Name")