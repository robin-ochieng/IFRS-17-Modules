# Load Supabase Config
source("supabase_config.R")

# Load required package
library(httr)

# Test connection
test_response <- httr::GET(
  paste0(SUPABASE_REST_URL, "/modules"),  # assumes you have a 'modules' table
  httr::add_headers("apikey" = SUPABASE_ANON_KEY)
)

if (httr::status_code(test_response) == 200) {
  print("✅ Supabase connection successful!")
} else {
  print("❌ Connection failed - check your URL and key")
}
