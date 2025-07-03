# insert_quiz_questions.R
# Run this script once to populate the quiz questions in Supabase

# Source the necessary files
source("supabase_config.R")

# You'll need to authenticate first - use an admin token
# For testing, you can get a token by signing in as an admin
admin_auth <- supabase_signin("rochieng@kenbright.africa", "Sakamentoh9/")
admin_token <- admin_auth$session$access_token

print("Authentication successful!")

# Modified insert function with better error handling
insert_quiz_questions_with_error_info <- function(questions_data, token) {
  tryCatch({
    # Convert list to data frame
    questions_df <- do.call(rbind, lapply(questions_data, as.data.frame, stringsAsFactors = FALSE))
    
    # Make the API call
    response <- httr::POST(
      url = paste0(SUPABASE_URL, "/rest/v1/quiz_questions"),
      httr::add_headers(
        "apikey" = SUPABASE_ANON_KEY,
        "Authorization" = paste("Bearer", token),
        "Content-Type" = "application/json",
        "Prefer" = "return=minimal"
      ),
      body = jsonlite::toJSON(questions_df, auto_unbox = TRUE),
      encode = "raw"
    )
    
    status_code <- httr::status_code(response)
    
    if (status_code >= 200 && status_code < 300) {
      return(list(success = TRUE, message = "Success"))
    } else {
      error_content <- httr::content(response, "text")
      return(list(success = FALSE, message = error_content))
    }
  }, error = function(e) {
    return(list(success = FALSE, message = e$message))
  })
}

# Function to prepare questions for a module
# Fixed function to handle multi-part questions properly
prepare_module_questions <- function(module_num, questions, correct_answers) {
  module_name <- paste0("module", module_num)
  
  questions_data <- list()
  
  # Create a mapping for multi-part questions
  question_counter <- 1
  
  for (i in seq_along(questions)) {
    q_id <- names(questions)[i]
    
    # Extract question text and options
    q_text <- questions[[q_id]]$title
    options <- questions[[q_id]]$choices
    
    # Get correct answer and explanation
    correct_info <- correct_answers[[q_id]]
    
    # For multi-part questions, include the part identifier in the question text
    if (grepl("_part", q_id)) {
      # Extract base number and part
      base_num <- gsub("q(\\d+)_part\\d+", "\\1", q_id)
      part_num <- gsub("q\\d+_part(\\d+)", "\\1", q_id)
      # Add part identifier to question text if not already there
      if (!grepl(paste0("\\b", base_num, "\\.", part_num, "\\)"), q_text)) {
        q_text <- paste0(base_num, ".", part_num, ") ", q_text)
      }
    }
    
    question_entry <- list(
      module_name = module_name,
      module_number = module_num,
      question_number = question_counter,  # Always use integer
      question_text = q_text,
      option_a = options[1],
      option_b = options[2],
      option_c = options[3],
      option_d = options[4],
      correct_answer = correct_info$answer,
      explanation = correct_info$explanation
    )
    
    questions_data[[length(questions_data) + 1]] <- question_entry
    question_counter <- question_counter + 1
  }
  
  return(questions_data)
}

# Test with one question first
print("\n=== Testing with single question ===")
test_questions <- list(
  q1 = list(
    title = "1. What is the primary objective of IFRS 17?",
    choices = c(
      "To standardize insurance accounting globally",
      "To replace IFRS 16",
      "To define financial instruments",
      "To measure investment property"
    )
  )
)

test_answers <- list(
  q1 = list(
    answer = "To standardize insurance accounting globally",
    explanation = "IFRS 17 aims to create a consistent accounting framework for insurance contracts to improve transparency and comparability."
  )
)

test_data <- prepare_module_questions(1, test_questions, test_answers)
result <- insert_quiz_questions_with_error_info(test_data, admin_token)

if(result$success) {
  print("✅ Test successful! Proceeding with full data...")
} else {
  print("❌ Test failed with error:")
  print(result$message)
  stop("Please fix the error before proceeding")
}


# Module 1 Questions
module1_questions <- list(
  q1 = list(
    title = "1. What is the primary objective of IFRS 17?",
    choices = c(
      "To standardize insurance accounting globally",
      "To replace IFRS 16",
      "To define financial instruments",
      "To measure investment property"
    )
  ),
  q2 = list(
    title = "2. What does IFRS 17 replace?",
    choices = c("IAS 37", "IFRS 4", "IFRS 9", "IAS 40")
  ),
  q3 = list(
    title = "3. What was the official date of initial application for IFRS 17?",
    choices = c(
      "1st January 2022",
      "31st December 2022",
      "1st January 2023",
      "1st January 2021"
    )
  ),
  q4 = list(
    title = "4. IFRS 17 applies to?",
    choices = c(
      "All insurance entities only",
      "Any entity issuing insurance contracts",
      "Reinsurers only",
      "Investment banks only"
    )
  ),
  q5 = list(
    title = "5. How does IFRS 17 define an insurance contract?",
    choices = c(
      "Contract transferring insurance risk",
      "Contract transferring investment risk",
      "Contract transferring liquidity risk",
      "Contract for investment advice"
    )
  ),
  q6 = list(
    title = "6. How does IFRS 17 define 'insurance risk'?",
    choices = c(
      "The risk of policyholder default",
      "The risk of future investment losses",
      "The risk transferred from the policyholder to the insurer due to uncertain future events",
      "Exchange rate risk"
    )
  ),
  q7 = list(
    title = "7. Which of the following contracts falls under the scope of IFRS 17?",
    choices = c(
      "Product warranty issued by a retailer",
      "Lease contract under IFRS 16",
      "Financial guarantee contract under IFRS 9",
      "Reinsurance contract held by an insurer"
    )
  ),
  q8 = list(
    title = "8. Which contracts are only within IFRS 17 if the issuer also issues insurance contracts?",
    choices = c(
      "Leases",
      "Derivatives",
      "Term Deposits",
      "Investment contracts with discretionary participation features"
    )
  ),
  q9 = list(
    title = "9. Are product warranties issued by a retailer within IFRS 17?",
    choices = c(
      "Yes, always",
      "No, they fall under IAS 37",
      "Only for 12-month terms",
      "Yes, if embedded in insurance"
    )
  ),
  q10 = list(
    title = "10. What type of contract is explicitly excluded from IFRS 17 scope?",
    choices = c(
      "Group life insurance",
      "Reinsurance contracts",
      "Insurance-linked investments",
      "Financial guarantees (under IFRS 9)"
    )
  )
)

correct_answers_module1 <- list(
  q1 = list(
    answer = "To standardize insurance accounting globally",
    explanation = "IFRS 17 aims to create a consistent accounting framework for insurance contracts to improve transparency and comparability."
  ),
  q2 = list(
    answer = "IFRS 4",
    explanation = "IFRS 17 replaced IFRS 4, which was an interim standard."
  ),
  q3 = list(
    answer = "1st January 2023",
    explanation = "The initial application date for IFRS 17 was 1st January 2023."
  ),
  q4 = list(
    answer = "Any entity issuing insurance contracts",
    explanation = "This reflects IFRS 17's scope, which applies to any entity that issues insurance contracts."
  ),
  q5 = list(
    answer = "Contract transferring insurance risk",
    explanation = "This captures the essential element of IFRS 17: transferring insurance risk from policyholder to insurer."
  ),
  q6 = list(
    answer = "The risk transferred from the policyholder to the insurer due to uncertain future events",
    explanation = "Insurance risk under IFRS 17 involves uncertainty about future events that may trigger insurer payment."
  ),
  q7 = list(
    answer = "Reinsurance contract held by an insurer",
    explanation = "Reinsurance contracts held are explicitly included under IFRS 17's scope."
  ),
  q8 = list(
    answer = "Investment contracts with discretionary participation features",
    explanation = "These contracts are only within the scope of IFRS 17 if issued by entities that also issue insurance contracts."
  ),
  q9 = list(
    answer = "No, they fall under IAS 37",
    explanation = "Retail product warranties are covered by IAS 37, not IFRS 17."
  ),
  q10 = list(
    answer = "Financial guarantees (under IFRS 9)",
    explanation = "Financial guarantee contracts are usually treated under IFRS 9 unless specifically designated as insurance."
  )
)

# Module 2 Questions
module2_questions <- list(
  q1 = list(
    title = "1. An insurer enters into two separate contracts with the same policyholder at the same time. Contract A provides insurance coverage, while Contract B negates the financial exposure of Contract A entirely. According to IFRS 17, how should the insurer report these contracts?",
    choices = c(
      "Treat the contracts as a single arrangement because they achieve an overall commercial effect",
      "Report both contracts separately as independent arrangements",
      "Recognize only Contract A since it was issued first",
      "Disclose both contracts but report them under IFRS 9"
    )
  ),
  q2 = list(
    title = "2. An insurer bundles multiple policies for a corporate client into a package with interdependent pricing. Some policies provide coverage, while others hedge specific risks associated with the insured entity. Under IFRS 17, how should these contracts be accounted for?",
    choices = c(
      "Each contract must be evaluated individually regardless of interdependencies",
      "The bundled contracts should be treated as a single unit if they collectively achieve an overall commercial effect",
      "Contracts should be separated since they have different durations",
      "Each contract should be reported based on legal form rather than economic substance"
    )
  ),
  q3 = list(
    title = "3. Which of the following scenarios would not require the combination of contracts under IFRS 17?",
    choices = c(
      "Two insurance contracts issued simultaneously to the same policyholder, with pricing designed to work together",
      "A reinsurance contract that fully offsets the risk of an insurance policy issued by the same insurer",
      "An insurance contract and an investment product sold separately with no dependency in pricing or risk",
      "A life insurance contract and a rider that cancels all coverage in the main policy"
    )
  ),
  q4 = list(
    title = "4. A life insurer offers a package where a main policy includes both insurance coverage and an investment component. The investment feature provides financial returns that could exist independently without the insurance portion. How should the insurer treat this arrangement under IFRS 17?",
    choices = c(
      "Recognize it as a single insurance contract",
      "Treat the entire contract under IFRS 9",
      "Combine the investment component only if it exceeds 50% of total premiums",
      "Separate the investment component if it can be sold independently"
    )
  ),
  q5 = list(
    title = "5. An insurer issues two separate policies to the same corporate client—one covering property damage and another covering business interruption losses linked to that property. The premiums are interdependent and structured as a bundle to provide a cohesive risk solution. What is the appropriate IFRS 17 treatment?",
    choices = c(
      "The contracts should always be separated",
      "The contracts should be combined if pricing is interdependent",
      "The contracts must be accounted for under IFRS 9",
      "The contracts should be combined only if policyholders request it"
    )
  ),
  q6 = list(
    title = "6. An insurance contract includes an embedded derivative feature that alters cash flows based on a financial index. According to IFRS 17, how should this embedded derivative be accounted for?",
    choices = c(
      "It must always remain part of the insurance contract",
      "It should be ignored unless the insurer requests separation",
      "It must be reported only in the contract disclosures",
      "It must be separated and accounted for under IFRS 9 if required"
    )
  ),
  q7 = list(
    title = "7. An insurer offers a contract that includes both insurance coverage and an investment component that can exist independently in the market. How should the investment component be treated under IFRS 17?",
    choices = c(
      "It should remain embedded in the insurance contract",
      "It should be accounted for under IFRS 15",
      "It must be separated only if it is distinct",
      "It should be reported only if the policyholder requests separate treatment"
    )
  ),
  q8 = list(
    title = "8. An insurance contract includes health coverage and an add-on subscription service for wellness programs, such as gym memberships and nutrition consultations. How should this non-insurance component be accounted for under IFRS 17?",
    choices = c(
      "It must remain part of the insurance contract under IFRS 17",
      "It should be accounted for separately using IFRS 15 if distinct",
      "It should be reclassified as an investment component under IFRS 9",
      "It must only be disclosed in the insurer's financial statements"
    )
  )
)

correct_answers_module2 <- list(
  q1 = list(
    answer = "Treat the contracts as a single arrangement because they achieve an overall commercial effect",
    explanation = "When contracts are designed to achieve an overall commercial effect (such as one negating the obligations of another), IFRS 17 requires treating them as a single arrangement to reflect the economic substance."
  ),
  q2 = list(
    answer = "The bundled contracts should be treated as a single unit if they collectively achieve an overall commercial effect",
    explanation = "IFRS 17 mandates that contracts designed to work together as a package with shared pricing or risk mitigation should be combined to reflect their true economic impact."
  ),
  q3 = list(
    answer = "An insurance contract and an investment product sold separately with no dependency in pricing or risk",
    explanation = "If contracts have no interdependent pricing or risk structure, they do not need to be combined under IFRS 17. Separation is appropriate in such cases."
  ),
  q4 = list(
    answer = "Separate the investment component if it can be sold independently",
    explanation = "IFRS 17 requires separating investment components if they can function independently, ensuring accurate financial reporting."
  ),
  q5 = list(
    answer = "The contracts should be combined if pricing is interdependent",
    explanation = "IFRS 17 requires combining contracts that are designed to function together commercially, particularly if pricing reflects mutual risk dependencies."
  ),
  q6 = list(
    answer = "It must be separated and accounted for under IFRS 9 if required",
    explanation = "IFRS 17 directs insurers to apply IFRS 9 to determine whether an embedded derivative should be separated and how it should be accounted for."
  ),
  q7 = list(
    answer = "It must be separated only if it is distinct",
    explanation = "Investment components should be separated if they are distinct, meaning they can function independently. IFRS 9 applies unless the component qualifies for discretionary participation features under IFRS 17."
  ),
  q8 = list(
    answer = "It should be accounted for separately using IFRS 15 if distinct",
    explanation = "IFRS 17 requires separating non-insurance services if they provide distinct goods or services. The insurer must apply IFRS 15 to allocate cash flows and account for them separately."
  )
)

# Module 3 Questions
module3_questions <- list(
  q1 = list(
    title = "1. What is the main purpose of aggregation under IFRS 17?",
    choices = c(
      "To reduce the number of contracts reported",
      "To ensure accurate timing of profit and loss recognition",
      "To make contract management easier",
      "To avoid having to assess individual contracts"
    )
  ),
  q2 = list(
    title = "2. Under IFRS 17, contracts grouped into the same portfolio must share:",
    choices = c(
      "The same inception date",
      "The same profit margin",
      "The same policyholder",
      "Similar risk characteristics and management structure"
    )
  ),
  q3 = list(
    title = "3. How far apart can contract issuance dates be within the same group?",
    choices = c(
      "Any number of years",
      "Two years",
      "Not more than one year",
      "Three years if risk is similar"
    )
  ),
  q4 = list(
    title = "4. What is the first step in the aggregation process under IFRS 17?",
    choices = c(
      "Grouping by issuance year",
      "Subdividing portfolios",
      "Grouping by portfolio",
      "Assessing profitability"
    )
  ),
  q5 = list(
    title = "5. Under IFRS 17, why are insurers not allowed to reassess contract groups after initial recognition?",
    choices = c(
      "To maintain consistency and transparency in reporting",
      "To reduce workload",
      "To allow for more flexibility later",
      "Because contracts cannot change after issuance"
    )
  ),
  q6 = list(
    title = "6. How does IFRS 17 recommend handling groups of contracts under the Premium Allocation Approach (PAA)?",
    choices = c(
      "Assume they are always profitable",
      "Assume all contracts are onerous",
      "Group them based on product type only",
      "Assume none are onerous at initial recognition unless facts suggest otherwise"
    )
  ),
  q7 = list(
    title = "7. What additional check must be done for policies eligible for the General Measurement Model (GMM)?",
    choices = c(
      "Verification of market premium rates",
      "Sensitivity testing and internal report reviews",
      "Reinsurance matching",
      "Underwriter interviews"
    )
  ),
  q8 = list(
    title = "8. Which of the following best describes a 'portfolio' under IFRS 17?",
    choices = c(
      "A collection of policies sold by one agent",
      "Contracts grouped based on risk and management similarity",
      "Contracts grouped by coverage period",
      "All insurance contracts issued in one year"
    )
  ),
  q9 = list(
    title = "9. What should an entity use to assess whether a contract might become onerous later?",
    choices = c(
      "Market interest rates",
      "Past claims history only",
      "Likelihood of changes in applicable facts and circumstances",
      "Broker recommendations"
    )
  ),
  q10 = list(
    title = "10. What happens if a contract becomes onerous after initial recognition?",
    choices = c(
      "The group composition remains unchanged",
      "It is moved to the 'onerous' group retroactively",
      "The contract is cancelled",
      "A new group is created"
    )
  )
)

correct_answers_module3 <- list(
  q1 = list(
    answer = "To ensure accurate timing of profit and loss recognition",
    explanation = "Aggregation helps ensure that profits and losses are recognized accurately and consistently in financial reporting."
  ),
  q2 = list(
    answer = "Similar risk characteristics and management structure",
    explanation = "Portfolios are formed based on risk similarity and being managed together."
  ),
  q3 = list(
    answer = "Not more than one year",
    explanation = "IFRS 17 requires that all contracts in a group are issued no more than one year apart."
  ),
  q4 = list(
    answer = "Grouping by portfolio",
    explanation = "Aggregation begins by forming portfolios based on similar risks and management structures."
  ),
  q5 = list(
    answer = "To maintain consistency and transparency in reporting",
    explanation = "Fixing the groupings at initial recognition supports consistent, unbiased financial reporting over time."
  ),
  q6 = list(
    answer = "Assume none are onerous at initial recognition unless facts suggest otherwise",
    explanation = "IFRS 17 allows insurers applying the PAA to assume contracts are not onerous at initial recognition, unless evidence indicates otherwise."
  ),
  q7 = list(
    answer = "Sensitivity testing and internal report reviews",
    explanation = "Sensitivity testing and internal reporting are used to confirm profitability assumptions for GMM-eligible contracts."
  ),
  q8 = list(
    answer = "Contracts grouped based on risk and management similarity",
    explanation = "A portfolio consists of contracts that have similar risk characteristics and are managed together."
  ),
  q9 = list(
    answer = "Likelihood of changes in applicable facts and circumstances",
    explanation = "Entities must consider whether new or changing circumstances might render a contract onerous in the future."
  ),
  q10 = list(
    answer = "The group composition remains unchanged",
    explanation = "Group compositions are fixed at initial recognition, even if a contract's status changes later."
  )
)

# Module 4 Questions
module4_questions <- list(
  q1 = list(
    title = "1. When must a group of insurance contracts be recognized under IFRS 17?",
    choices = c(
      "At the end of the reporting period",
      "When the last payment is received",
      "When the policyholder signs the contract",
      "At the earliest of the coverage period start, first payment due, or when the group becomes onerous"
    )
  ),
  q2 = list(
    title = "2. If there is no contractual due date for the first payment, when is it considered due?",
    choices = c(
      "At the end of the month",
      "When it is received",
      "After coverage starts",
      "When billed"
    )
  ),
  q3 = list(
    title = "3. When should an insurer assess if a contract is onerous?",
    choices = c(
      "After recognition",
      "Before the earlier of coverage start or payment due",
      "At the end of the financial year",
      "Only when a loss is reported"
    )
  ),
  q4 = list(
    title = "4. What is the treatment if IACFs are not immediately expensed?",
    choices = c(
      "They are recognized as an asset or liability",
      "They are deferred revenue",
      "They are added to the CSM",
      "They are amortized over the contract term"
    )
  ),
  q5 = list(
    title = "5. When is the acquisition asset or liability removed from the books?",
    choices = c(
      "When the last premium is received",
      "When the policyholder cancels",
      "When the related group of contracts is recognized",
      "At the year-end"
    )
  ),
  q6 = list(
    title = "6. What is the condition for including a contract in a group?",
    choices = c(
      "It must be active",
      "It must be issued by the end of the reporting period",
      "It must be profitable",
      "It must be short-term"
    )
  ),
  q7 = list(
    title = "7. What happens if new contracts added to a group affect the discount rate?",
    choices = c(
      "The rate must be updated and applied from the start of the reporting period",
      "Nothing changes",
      "It only applies to new contracts",
      "The group must be split"
    )
  ),
  q8 = list(
    title = "8. Which of the following is TRUE regarding onerous contracts?",
    choices = c(
      "They must be recognized immediately",
      "They are ignored under IFRS 17",
      "They are grouped with profitable contracts",
      "They are only assessed annually"
    )
  ),
  q9 = list(
    title = "9. How often can the discount rate be changed for a group?",
    choices = c(
      "Monthly",
      "Only if new contracts are added that change it",
      "Once a year",
      "Never"
    )
  ),
  q10 = list(
    title = "10. Why is the initial recognition timing important under IFRS 17?",
    choices = c(
      "It determines when revenue and expenses are recorded",
      "It helps identify reinsurers",
      "It is used to calculate tax",
      "It helps with customer satisfaction"
    )
  )
)

correct_answers_module4 <- list(
  q1 = list(
    answer = "At the earliest of the coverage period start, first payment due, or when the group becomes onerous",
    explanation = "IFRS 17 requires recognition at the earliest of these three trigger events."
  ),
  q2 = list(
    answer = "When it is received",
    explanation = "IFRS 17 states that if no due date is set, the payment is considered due when received."
  ),
  q3 = list(
    answer = "Before the earlier of coverage start or payment due",
    explanation = "The standard requires a pre-recognition assessment if there's an indication of onerousness."
  ),
  q4 = list(
    answer = "They are recognized as an asset or liability",
    explanation = "IACFs are treated separately until the related group is recognized."
  ),
  q5 = list(
    answer = "When the related group of contracts is recognized",
    explanation = "The asset or liability is derecognized at the point of group recognition."
  ),
  q6 = list(
    answer = "It must be issued by the end of the reporting period",
    explanation = "Only contracts issued by the end of the reporting period are included."
  ),
  q7 = list(
    answer = "The rate must be updated and applied from the start of the reporting period",
    explanation = "The standard requires adjusting the initial discount rate retroactively to the start of the reporting period."
  ),
  q8 = list(
    answer = "They must be recognized immediately",
    explanation = "Onerous groups must be recognized as soon as they become onerous."
  ),
  q9 = list(
    answer = "Only if new contracts are added that change it",
    explanation = "The rate is updated only if new contracts added after the reporting period affect it."
  ),
  q10 = list(
    answer = "It determines when revenue and expenses are recorded",
    explanation = "Proper timing ensures that revenue, risk, and costs are reported accurately."
  )
)

# Module 5 Questions
module5_questions <- list(
  q1 = list(
    title = "1. Which of the following is NOT a component of fulfilment cash flows?",
    choices = c(
      "Future cash flows",
      "Discount rate",
      "Risk adjustment",
      "Insurance acquisition commission bonus pool"
    )
  ),
  q2 = list(
    title = "2. Which cash flows should be included in the measurement of the contract at initial recognition?",
    choices = c(
      "Past claims only",
      "Cash flows related to investment returns",
      "Future premiums and claim payments",
      "Marketing expenses"
    )
  ),
  q3 = list(
    title = "3. If the fulfilment cash flows are negative, what does IFRS 17 require?",
    choices = c(
      "Defer the difference",
      "Recognize a loss immediately",
      "Recognize a CSM",
      "Reduce the asset balance"
    )
  ),
  q4 = list(
    title = "4. What happens to a day-1 gain under IFRS 17?",
    choices = c(
      "Deferred in CSM",
      "Recognized as revenue",
      "Transferred to retained earnings",
      "Recorded as OCI"
    )
  ),
  q5 = list(
    title = "5. Why is discounting applied to future cash flows?",
    choices = c(
      "To increase liabilities",
      "To reflect time value of money",
      "To reduce reporting volatility",
      "To comply with IFRS 9"
    )
  ),
  q6 = list(
    title = "6. Which discount rate is used for initial measurement?",
    choices = c(
      "Zero-coupon rate",
      "Locked-in discount rate",
      "Market average rate",
      "Prime lending rate"
    )
  ),
  q7 = list(
    title = "7. Which cost is not included in initial measurement?",
    choices = c(
      "Direct acquisition costs",
      "Expected claims",
      "Indirect administrative costs",
      "Risk adjustment"
    )
  ),
  q8 = list(
    title = "8. Which cost is typically excluded from fulfilment cash flows?",
    choices = c(
      "Advertising and marketing",
      "Future claims",
      "Premiums",
      "Claim handling costs"
    )
  ),
  q9 = list(
    title = "9. Under which model is no CSM typically recognized?",
    choices = c(
      "GMM",
      "PAA",
      "VFA",
      "Modified GMM"
    )
  ),
  q10 = list(
    title = "10. Which of the following is a valid reason to apply the Premium Allocation Approach (PAA) at initial recognition?",
    choices = c(
      "It results in higher revenue.",
      "The contract has a coverage period of more than one year",
      "The simplification does not significantly differ from GMM results",
      "It avoids recognition of acquisition costs"
    )
  )
)

correct_answers_module5 <- list(
  q1 = list(
    answer = "Insurance acquisition commission bonus pool",
    explanation = "The bonus pool is not part of fulfilment cash flows. The correct components are expected cash flows, discounting, and risk adjustment."
  ),
  q2 = list(
    answer = "Future premiums and claim payments",
    explanation = "Fulfilment cash flows include expected future premiums and claims."
  ),
  q3 = list(
    answer = "Recognize a loss immediately",
    explanation = "Negative fulfilment cash flows indicate an onerous contract; a loss is recognized in profit or loss."
  ),
  q4 = list(
    answer = "Deferred in CSM",
    explanation = "CSM defers day-1 gains and recognizes them over the service period."
  ),
  q5 = list(
    answer = "To reflect time value of money",
    explanation = "Discounting ensures that future cash flows are presented in today's money, reflecting the time value of money."
  ),
  q6 = list(
    answer = "Locked-in discount rate",
    explanation = "The locked-in rate at initial recognition is used to discount fulfilment cash flows and accrete CSM."
  ),
  q7 = list(
    answer = "Indirect administrative costs",
    explanation = "Only directly attributable acquisition costs are included. Indirect costs like general admin are excluded."
  ),
  q8 = list(
    answer = "Advertising and marketing",
    explanation = "General marketing expenses are not part of fulfilment cash flows under IFRS 17."
  ),
  q9 = list(
    answer = "PAA",
    explanation = "PAA does not require a CSM unless the contract is deemed onerous."
  ),
  q10 = list(
    answer = "The simplification does not significantly differ from GMM results",
    explanation = "PAA may be used if it would not materially differ from the GMM measurement. This is especially relevant for short-duration contracts."
  )
)

# Module 6 Questions
module6_questions <- list(
  q1 = list(
    title = "1. What does subsequent measurement refer to under IFRS 17?",
    choices = c(
      "The reassessment of reinsurance cash flows",
      "The update of contract liabilities after initial recognition",
      "Only the measurement of incurred claims",
      "Determining if premiums are received"
    )
  ),
  q2 = list(
    title = "2. An insurance company issues a 4-year term life insurance contract with a total expected Contractual Service Margin (CSM) of $8,000 at initial recognition. The company expects to provide insurance services evenly over the 4 years. How much CSM revenue should be recognized at the end of each year, assuming no changes in estimates or contract modifications?",
    choices = c(
      "$2,000 per year for 4 years",
      "$0 in year 1 and $8,000 in year 4",
      "$4,000 in the first year and $1,333 in each of the following years",
      "$8,000 immediately at contract inception"
    )
  ),
  q3 = list(
    title = "3. How often are fulfilment cash flows updated?",
    choices = c(
      "Once a year",
      "Monthly",
      "At each reporting date",
      "Never after initial recognition"
    )
  ),
  q4 = list(
    title = "4. How are claims incurred shown in financials?",
    choices = c(
      "In CSM",
      "In OCI",
      "In fulfilment cash flows",
      "In profit or loss"
    )
  ),
  q5 = list(
    title = "5. Which is a cause of change in risk adjustment?",
    choices = c(
      "Change in interest rates",
      "Increase in past claims",
      "Changes in uncertainty of future service",
      "Movement in capital reserves"
    )
  ),
  q6 = list(
    title = "6. Which changes are excluded from adjusting the CSM?",
    choices = c(
      "Future service estimates",
      "Time value updates",
      "Risk of lapses",
      "Policyholder behavior assumptions"
    )
  ),
  q7 = list(
    title = "7. Which of the following affects the Liability for Incurred Claims (LIC)?",
    choices = c(
      "Future service premiums",
      "Reinsurance commissions",
      "Claims already incurred",
      "Profit emergence"
    )
  ),
  q8 = list(
    title = "8. What does the Liability for Remaining Coverage (LRC) include?",
    choices = c(
      "CSM + premiums received",
      "Fulfilment cash flows + CSM",
      "Only claims paid",
      "Gross income"
    )
  ),
  q9 = list(
    title = "9. What does LIC capture?",
    choices = c(
      "Claims that may occur in the future",
      "Earned premiums",
      "Deferred acquisition cost",
      "Claims already incurred"
    )
  ),
  q10 = list(
    title = "10. What role does the risk adjustment play in subsequent measurement?",
    choices = c(
      "Reduces cash flows",
      "Defers tax",
      "Adjusts for uncertainty in non-financial risks",
      "Ignores future inflation"
    )
  ),
  q11 = list(
    title = "11. How is ULAE treated in subsequent measurement?",
    choices = c(
      "Expensed in full",
      "Included in LIC and updated",
      "Ignored unless incurred",
      "Deferred to maturity"
    )
  )
)

correct_answers_module6 <- list(
  q1 = list(
    answer = "The update of contract liabilities after initial recognition",
    explanation = "Subsequent measurement involves updating the carrying amounts of insurance liabilities after initial recognition."
  ),
  q2 = list(
    answer = "$2,000 per year for 4 years",
    explanation = "Since the insurance company expects to provide services evenly over the 4-year coverage period and the total CSM is $8,000, the revenue should be recognized on a straight-line basis—$2,000 each year."
  ),
  q3 = list(
    answer = "At each reporting date",
    explanation = "Entities must reassess fulfilment cash flows using current estimates at each reporting period."
  ),
  q4 = list(
    answer = "In profit or loss",
    explanation = "Claims that relate to past service are recognized directly in the profit or loss statement."
  ),
  q5 = list(
    answer = "Changes in uncertainty of future service",
    explanation = "Changes in the level of uncertainty about future cash flows affect the risk adjustment."
  ),
  q6 = list(
    answer = "Time value updates",
    explanation = "Changes due to the passage of time (e.g., interest accretion) do not adjust the CSM — they affect finance income/expense."
  ),
  q7 = list(
    answer = "Claims already incurred",
    explanation = "LIC represents obligations from past events (claims already incurred but not yet paid)."
  ),
  q8 = list(
    answer = "Fulfilment cash flows + CSM",
    explanation = "LRC includes fulfilment cash flows for future coverage and the CSM."
  ),
  q9 = list(
    answer = "Claims already incurred",
    explanation = "LIC reflects the insurer's obligation for incurred claims not yet settled."
  ),
  q10 = list(
    answer = "Adjusts for uncertainty in non-financial risks",
    explanation = "Risk adjustment reflects uncertainty in cash flows and is re-evaluated each period."
  ),
  q11 = list(
    answer = "Included in LIC and updated",
    explanation = "Unallocated Loss Adjustment Expenses (ULAE) are included in LIC and updated regularly."
  )
)

# Module 7 Questions
module7_questions <- list(
  q1 = list(
    title = "1. Which of the following is NOT a required characteristic of the discount rate under IFRS 17?",
    choices = c(
      "Consistency with observable market prices for similar cash flows",
      "Inclusion of illiquidity premiums to reflect insurance contract liquidity",
      "Use of a single fixed discount rate across all types of contracts",
      "Alignment with other assumptions used in valuation to avoid double counting"
    )
  ),
  q2 = list(
    title = "2. Which of the following is a correct interpretation of IFRS 17's guidance on using market data to determine discount rates?",
    choices = c(
      "Discount rates must exclude the effect of market variables that do not impact the insurance contract's cash flows.",
      "Observable market prices should always be used, even if they include factors unrelated to insurance contract cash flows.",
      "Market observable discount rates can be used even if they reflect credit risk not relevant to the insurance liability.",
      "Discount rates should reflect all observable market factors regardless of contract characteristics."
    )
  ),
  q3 = list(
    title = "3. What is the primary distinction between the bottom-up and top-down approaches for deriving discount rates under IFRS 17?",
    choices = c(
      "The bottom-up approach starts from asset returns and adjusts for insurance features",
      "The top-down approach uses a risk-free curve and adds risk premiums",
      "The top-down approach always requires matching the exact liquidity of insurance contracts.",
      "The bottom-up approach starts with a liquid risk-free yield curve and adjusts for illiquidity"
    )
  ),
  q4 = list(
    title = "4. Which statement is TRUE regarding liquidity adjustments in the top-down approach under IFRS 17?",
    choices = c(
      "Liquidity differences between the reference assets and insurance contracts must always be adjusted",
      "No liquidity adjustments are allowed under the top-down approach",
      "Adjustments are made only if the reference portfolio's liquidity differs significantly from that of the insurance contracts",
      "Liquidity risk is already captured in the nominal cash flows, so no adjustments are required"
    )
  ),
  q5_part1 = list(
    title = "5. i) What is the total discount rate to be used under the bottom-up approach?",
    choices = c("A) 3.0%", "B) 2.5%", "C) 3.5%", "D) 2.0%")
  ),
  q5_part2 = list(
    title = "5. ii) What is the present value of the expected cash flows using the appropriate discount rate from Part A?",
    choices = c("A) 3,506.85", "B) 3,477.32", "C) 3,599.25", "D) 3,423.18")
  ),
  q5_part3 = list(
    title = "5. iii) If instead the top-down approach was used and the yield on the reference portfolio is 4.5%, and credit & non-insurance-related risks account for 2%, what would be the equivalent discount rate?",
    choices = c("A) 4.5%", "B) 2.5%", "C) 2.0%", "D) 6.5%")
  ),
  q5_part4 = list(
    title = "5. iv) What is the impact on the present value if the liquidity adjustment increases to 1.0%, keeping the risk-free rate constant?",
    choices = c("A) Present Value increases", "B) Present Value decreases", "C) Present Value stays the same", "D) Present Value becomes negative")
  ),
  q6 = list(
    title = "6. When a group of insurance contracts becomes onerous after initial recognition under IFRS 17, what happens to the Contractual Service Margin (CSM)?",
    choices = c(
      "It is increased to reflect the higher expected losses.",
      "It remains unchanged, as changes are only recognized at initial recognition.",
      "It is set to zero, and a loss component is established to reflect the excess of fulfilment cash flows over the expected inflows.",
      "It is transferred to the Liability for Incurred Claims (LIC)."
    )
  ),
  q7 = list(
    title = "7. Can a loss component (LC) established for an onerous group of contracts under IFRS 17 be reversed in subsequent periods?",
    choices = c(
      "No, once established, a loss component cannot be reversed.",
      "Yes, but only through adjustments to the Risk Adjustment for non-financial risk.",
      "Only if the contracts are derecognized.",
      "Yes, if future changes in fulfilment cash flows indicate that the group is no longer onerous."
    )
  ),
  q8 = list(
    title = "8. In the context of IFRS 17, what does the Liability for Remaining Coverage (LRC) represent when the Contractual Service Margin (CSM) is nil?",
    choices = c(
      "The sum of the fulfilment cash flows and the loss component.",
      "Only the present value of future cash flows without any adjustments.",
      "The Liability for Incurred Claims (LIC) only.",
      "The Risk Adjustment for non-financial risk only."
    )
  ),
  q9 = list(
    title = "9. Which discount rate is used to accrete interest on the CSM?",
    choices = c(
      "The risk-free rate at the reporting date",
      "The weighted average discount rate for incurred claims",
      "The current market interest rate for government bonds",
      "The discount rate at initial recognition of the group of contracts"
    )
  ),
  q10_part1 = list(
    title = "10. i) What is the initial CSM at 1 January 20X1?",
    choices = c("A. $ 200", "B. $ 150", "C. $ 250", "D. $ 100")
  ),
  q10_part2 = list(
    title = "10. ii) What is the CSM balance at 31 December 20X1, before release, assuming a 5% interest rate?",
    choices = c("A. $ 150", "B. $ 157.5", "C. $ 160", "D. $ 155")
  ),
  q10_part3 = list(
    title = "10. iii) If the CSM is released evenly over the 4-year coverage period, what is the CSM balance after release at 31 December 20X1?",
    choices = c("A. $ 118.125", "B. $ 120", "C. $ 130", "D. $ 100")
  ),
  q11 = list(
    title = "11. Which of the following characteristics would lead to a higher risk adjustment according to IFRS 17 principles?",
    choices = c(
      "High-frequency, low-severity risks",
      "Short-duration contracts with predictable claims",
      "Risks with narrow probability distributions",
      "Contracts where little is known about emerging experience"
    )
  ),
  q12 = list(
    title = "12. Which of the following risks is excluded from the IFRS 17 risk adjustment?",
    choices = c(
      "Lapse risk",
      "Expense risk",
      "Financial risk (e.g. interest rate risk)",
      "Morbidity risk"
    )
  ),
  q13 = list(
    title = "13. Two otherwise identical contracts differ only in duration: Contract A is 5 years; Contract B is 15 years. Which will have the higher risk adjustment, and why?",
    choices = c(
      "Contract A, due to faster runoff",
      "Contract B, due to longer exposure to uncertainty",
      "Both will have the same risk adjustment",
      "Contract A, due to the need for more immediate reserves"
    )
  ),
  q14 = list(
    title = "14. Which method is not typically used to quantify the risk adjustment under IFRS 17?",
    choices = c(
      "Cost of capital method",
      "Conditional Tail Expectation (CTE)",
      "Confidence level",
      "Historical premium rate analysis"
    )
  ),
  q15_part1 = list(
    title = "15. (a) What is the value of the liability at the 75% confidence level? Use z = 0.674",
    choices = c(
      "A. $10,506,000",
      "B. $11,011,000",
      "C. $11,674,000",
      "D. $12,350,000"
    )
  ),
  q15_part2 = list(
    title = "15. (b) What is the risk adjustment for non-financial risk at the 75% confidence level?",
    choices = c(
      "A. $674,000",
      "B. $750,000",
      "C. $1,011,000",
      "D. $1,674,000"
    )
  ),
  q15_part3 = list(
    title = "15. (c) If the insurer increases the confidence level to 90%, what is the new risk adjustment? (Use z = 1.282)",
    choices = c(
      "A. $1,282,000",
      "B. $1,500,000",
      "C. $1,922,000",
      "D. $2,282,000"
    )
  )
)

correct_answers_module7 <- list(
  q1 = list(
    answer = "Use of a single fixed discount rate across all types of contracts",
    explanation = "IFRS 17 does not require or recommend a single fixed discount rate for all contracts. Instead, the discount rate should reflect characteristics like liquidity, inflation, and dependency on underlying items."
  ),
  q2 = list(
    answer = "Discount rates must exclude the effect of market variables that do not impact the insurance contract's cash flows.",
    explanation = "IFRS 17 requires that discount rates exclude market variables that don't affect the cash flows of the insurance contract, even if these variables are reflected in market prices."
  ),
  q3 = list(
    answer = "The bottom-up approach starts with a liquid risk-free yield curve and adjusts for illiquidity",
    explanation = "The bottom-up approach starts with a liquid risk-free yield curve, then adjusts it to reflect the characteristics of the insurance contracts."
  ),
  q4 = list(
    answer = "Adjustments are made only if the reference portfolio's liquidity differs significantly from that of the insurance contracts",
    explanation = "IFRS 17 allows flexibility in adjusting for liquidity under the top-down approach, requiring adjustments only if the reference assets' liquidity is not reasonably consistent with that of the insurance contracts."
  ),
  q5_part1 = list(
    answer = "B) 2.5%",
    explanation = "Under the bottom-up approach, the discount rate is calculated as the sum of the risk-free rate and liquidity premium: 2% + 0.5% = 2.5%"
  ),
  q5_part2 = list(
    answer = "A) 3,506.85",
    explanation = "Present value is calculated using a 2.5% discount rate applied to the given cash flows: PV ≈ 975.61 + 1,142.10 + 1,389.14 = 3,506.85"
  ),
  q5_part3 = list(
    answer = "B) 2.5%",
    explanation = "From a 4.5% reference portfolio yield, subtracting 2% non-insurance risk adjustments gives a 2.5% discount rate, aligning with bottom-up."
  ),
  q5_part4 = list(
    answer = "B) Present Value decreases",
    explanation = "An increase in the liquidity adjustment increases the total discount rate, leading to a lower present value of future cash flows."
  ),
  q6 = list(
    answer = "It is set to zero, and a loss component is established to reflect the excess of fulfilment cash flows over the expected inflows.",
    explanation = "If contracts become onerous after recognition, IFRS 17 sets the CSM to zero and establishes a loss component."
  ),
  q7 = list(
    answer = "Yes, if future changes in fulfilment cash flows indicate that the group is no longer onerous.",
    explanation = "IFRS 17 allows reversal of loss components if favourable changes indicate the group is no longer onerous."
  ),
  q8 = list(
    answer = "The sum of the fulfilment cash flows and the loss component.",
    explanation = "When the CSM is nil, the LRC comprises the fulfilment cash flows and the loss component."
  ),
  q9 = list(
    answer = "The discount rate at initial recognition of the group of contracts",
    explanation = "CSM interest accretion is based on the discount rate determined at initial recognition of the group."
  ),
  q10_part1 = list(
    answer = "B. $ 150",
    explanation = "CSM = PV inflows – PV outflows – Risk Adjustment = 1,200 – 1,000 – 50 = 150"
  ),
  q10_part2 = list(
    answer = "B. $ 157.5",
    explanation = "Interest accretion = CSM × 1.05 = 150 × 1.05 = 157.5"
  ),
  q10_part3 = list(
    answer = "A. $ 118.125",
    explanation = "Release = 157.5 ÷ 4 = 39.375; Remaining CSM = 157.5 – 39.375 = 118.125"
  ),
  q11 = list(
    answer = "Contracts where little is known about emerging experience",
    explanation = "Higher uncertainty from limited knowledge leads to a higher risk adjustment under IFRS 17 (B91)."
  ),
  q12 = list(
    answer = "Financial risk (e.g. interest rate risk)",
    explanation = "The risk adjustment only covers non-financial risks. Financial risks are included in discount rate or cash flows."
  ),
  q13 = list(
    answer = "Contract B, due to longer exposure to uncertainty",
    explanation = "Longer duration implies greater uncertainty, requiring higher risk adjustment (B91(b))."
  ),
  q14 = list(
    answer = "Historical premium rate analysis",
    explanation = "Historical premium analysis is not a valid standalone method for risk adjustment; IFRS 17 prefers cost of capital and quantile methods."
  ),
  q15_part1 = list(
    answer = "B. $11,011,000",
    explanation = "Liability = 10,000,000 + 0.674 × 1,500,000 = 11,011,000"
  ),
  q15_part2 = list(
    answer = "C. $1,011,000",
    explanation = "Risk adjustment = Liability at 75% – Expected value = 11,011,000 – 10,000,000"
  ),
  q15_part3 = list(
    answer = "C. $1,922,000",
    explanation = "Risk adjustment = 10,000,000 + 1.282 × 1,500,000 – 10,000,000 = 1,923,000 (rounded to 1,922,000)"
  )
)

# Module 8 Questions
module8_questions <- list(
  q1 = list(
    title = "1. When is a contract classified as an onerous contract under IFRS 17?",
    choices = c(
      "When the contract is expected to lapse early",
      "When the contract has no Contractual Service Margin (CSM)",
      "When the contract is expected to incur a loss",
      "When the contract has no insurance risk"
    )
  ),
  q2 = list(
    title = "2. How is the CSM treated for onerous contracts?",
    choices = c(
      "Deferred",
      "Reversed",
      "Released to profit",
      "Set to zero"
    )
  ),
  q3 = list(
    title = "3. Which component is recognized when a group is onerous at initial recognition?",
    choices = c(
      "Contractual Service Margin",
      "Risk Adjustment",
      "Loss Component",
      "Investment Return"
    )
  ),
  q4 = list(
    title = "4. How is the loss component recognized?",
    choices = c(
      "As an asset",
      "Through OCI",
      "As an adjustment to the CSM",
      "In profit or loss"
    )
  ),
  q5 = list(
    title = "5. What happens if cash flow estimates improve?",
    choices = c(
      "Loss component is reversed first",
      "CSM increases",
      "Risk adjustment decreases",
      "Premiums are restated"
    )
  ),
  q6 = list(
    title = "6. When is a contract classified as onerous?",
    choices = c(
      "When risk adjustment is high",
      "When expected profit is low",
      "When fulfilment cash flows exceed premiums",
      "When lapse rate is high"
    )
  ),
  q7 = list(
    title = "7. What happens to the CSM if a group of contracts becomes onerous after initial recognition?",
    choices = c(
      "It is increased",
      "It is set to zero and loss is recognized",
      "It is locked in",
      "It is recalculated using old assumptions"
    )
  ),
  q8 = list(
    title = "8. Which of the following changes can make a previously profitable contract group onerous?",
    choices = c(
      "Increase in administrative expenses",
      "Drop in discount rates",
      "Revised premium allocation method",
      "Change in accounting policy"
    )
  ),
  q9 = list(
    title = "9. How does the loss component affect future insurance revenue?",
    choices = c(
      "No effect",
      "Increases revenue",
      "It reduces future revenue",
      "It replaces CSM in revenue recognition"
    )
  ),
  q10 = list(
    title = "10. What causes a change in the loss component?",
    choices = c(
      "Increase in discount rate",
      "Change in reinsurance treaty",
      "Adverse claims development",
      "Policyholder death"
    )
  )
)

correct_answers_module8 <- list(
  q1 = list(
    answer = "When the contract is expected to incur a loss",
    explanation = "An onerous contract is one where fulfilment cash flows exceed expected premiums, resulting in a loss."
  ),
  q2 = list(
    answer = "Set to zero",
    explanation = "The CSM is set to zero since no future profits are expected."
  ),
  q3 = list(
    answer = "Loss Component",
    explanation = "The loss component is set up to represent losses on onerous contracts and is recognized immediately in profit or loss."
  ),
  q4 = list(
    answer = "In profit or loss",
    explanation = "The loss component is recognized immediately in profit or loss."
  ),
  q5 = list(
    answer = "Loss component is reversed first",
    explanation = "Improvements first reduce the loss component before adjusting the CSM."
  ),
  q6 = list(
    answer = "When fulfilment cash flows exceed premiums",
    explanation = "A contract is onerous when the fulfilment cash flows exceed the expected inflows (e.g., premiums), indicating a net loss."
  ),
  q7 = list(
    answer = "It is set to zero and loss is recognized",
    explanation = "If contracts become onerous after initial recognition, the CSM is reduced to zero and any further loss is recognized in profit or loss."
  ),
  q8 = list(
    answer = "Increase in administrative expenses",
    explanation = "Increases in expected expenses can raise fulfilment cash flows, potentially making the group onerous."
  ),
  q9 = list(
    answer = "It replaces CSM in revenue recognition",
    explanation = "For onerous groups, the loss component replaces the CSM and is released as insurance revenue as coverage is provided."
  ),
  q10 = list(
    answer = "Adverse claims development",
    explanation = "Any adverse change in fulfilment cash flows increases the loss component."
  )
)

# Module 9 Questions
module9_questions <- list(
  q1 = list(
    title = "1. When is an entity allowed to apply the Premium Allocation Approach (PAA)?",
    choices = c(
      "Only for life insurance contracts",
      "For all investment contracts",
      "If the contract duration is ≤12 months or if results are similar to GMM",
      "For contracts with no risk adjustment"
    )
  ),
  q2 = list(
    title = "2. What does the liability for remaining coverage (LRC) under PAA represent?",
    choices = c(
      "Future claims paid",
      "Present value of premiums",
      "The unearned portion of premiums minus acquisition costs",
      "Incurred claims"
    )
  ),
  q3 = list(
    title = "3. Which of the following requires risk adjustment under PAA?",
    choices = c(
      "Liability for incurred claims",
      "Acquisition cost asset",
      "Liability for remaining coverage",
      "Premium receivable"
    )
  ),
  q4 = list(
    title = "4. What happens if the liability for remaining coverage is lower than fulfilment cash flows?",
    choices = c(
      "Create a contractual service margin",
      "Defer acquisition costs",
      "Recognize a loss",
      "Discount more"
    )
  ),
  q5 = list(
    title = "5. What are fulfilment cash flows made up of?",
    choices = c(
      "Future premiums only",
      "Future claims and profits",
      "Expected future inflows and outflows, discounted, plus risk adjustment",
      "Written premium minus expenses"
    )
  ),
  q6 = list(
    title = "6. How is insurance revenue recognized under PAA?",
    choices = c(
      "All at inception",
      "When claims are paid",
      "Evenly over the coverage period",
      "At contract expiry"
    )
  ),
  q7 = list(
    title = "7. Can insurers offset profitable and onerous contracts within a portfolio under PAA?",
    choices = c(
      "No, grouping rules prevent offsetting",
      "Only with auditor approval",
      "Yes",
      "Only for reinsurance"
    )
  ),
  q8 = list(
    title = "8. What is a key disclosure requirement under IFRS 17 even when using PAA?",
    choices = c(
      "No disclosure required",
      "Confidence level of liabilities",
      "Market value of assets",
      "Tax provision for each contract"
    )
  ),
  q9 = list(
    title = "9. Can PAA be used for reinsurance contracts held?",
    choices = c(
      "Yes, if eligibility criteria are met",
      "No, PAA is only for direct contracts",
      "Yes, but only in life insurance",
      "Only if premiums exceed claims"
    )
  ),
  q10 = list(
    title = "10. What happens when acquisition costs are deferred for an onerous group?",
    choices = c(
      "The loss reduces",
      "It offsets the fulfilment cash flows",
      "It increases the recognized loss",
      "It increases future profits"
    )
  )
)

correct_answers_module9 <- list(
  q1 = list(
    answer = "If the contract duration is ≤12 months or if results are similar to GMM",
    explanation = "PAA can be used if the coverage period is 12 months or less, or if using PAA would yield results that are not materially different from the General Measurement Model (GMM)."
  ),
  q2 = list(
    answer = "The unearned portion of premiums minus acquisition costs",
    explanation = "LRC under PAA reflects the simplified unearned premium approach, adjusted for amortized acquisition costs."
  ),
  q3 = list(
    answer = "Liability for incurred claims",
    explanation = "The risk adjustment under PAA is only applied to the liability for incurred claims, to account for uncertainty in non-financial risk."
  ),
  q4 = list(
    answer = "Recognize a loss",
    explanation = "If fulfilment cash flows exceed the liability for remaining coverage, the contract is deemed onerous and the excess must be recognized as a loss."
  ),
  q5 = list(
    answer = "Expected future inflows and outflows, discounted, plus risk adjustment",
    explanation = "Fulfilment cash flows under IFRS 17 reflect the present value of expected future inflows and outflows, with a risk adjustment for non-financial risk."
  ),
  q6 = list(
    answer = "Evenly over the coverage period",
    explanation = "Under PAA, revenue is recognized as insurance services are provided, typically on a straight-line basis over the coverage period."
  ),
  q7 = list(
    answer = "No, grouping rules prevent offsetting",
    explanation = "IFRS 17 requires separate grouping of onerous and profitable contracts; losses cannot be offset by profitable ones."
  ),
  q8 = list(
    answer = "Confidence level of liabilities",
    explanation = "Disclosure of the confidence level used to determine the risk adjustment is required, even under the simplified PAA model."
  ),
  q9 = list(
    answer = "Yes, if eligibility criteria are met",
    explanation = "PAA can be applied to reinsurance contracts held if the contract meets the same criteria as direct contracts."
  ),
  q10 = list(
    answer = "It increases the recognized loss",
    explanation = "Deferring acquisition costs reduces the LRC, which may increase the difference from fulfilment cash flows, leading to a higher recognized loss."
  )
)

# Module 10 Questions
module10_questions <- list(
  q1 = list(
    title = "1. What is a reinsurance contract held under IFRS 17?",
    choices = c(
      "A contract under which an entity receives compensation for claims from a reinsurer",
      "A contract issued to share profits with partners",
      "Contract issued to insure customers",
      "A contract for investment-linked business"
    )
  ),
  q2 = list(
    title = "2. When should a reinsurance contract held be initially recognized?",
    choices = c(
      "When the reinsurer pays a claim",
      "At the start of the underlying insurance contract",
      "At the earlier of coverage start or when underlying contracts are onerous",
      "At the end of the reporting period"
    )
  ),
  q3 = list(
    title = "3. Can a gain on purchase of reinsurance be recognized immediately?",
    choices = c(
      "Yes, it boosts profit",
      "No, it is included in the CSM",
      "Only if the reinsurer agrees",
      "Yes, under PAA"
    )
  ),
  q4 = list(
    title = "4. Which of the following is NOT included in fulfilment cash flows for reinsurance contracts held?",
    choices = c(
      "Future claims recoveries",
      "Discounting",
      "Reinsurer's risk appetite",
      "Risk adjustment"
    )
  ),
  q5 = list(
    title = "5. What is the impact of reinsurance on the insurer's risk exposure?",
    choices = c(
      "Increases risk",
      "No impact",
      "Transfers and reduces risk",
      "Creates an additional liability"
    )
  ),
  q6 = list(
    title = "6. How are changes in fulfilment cash flows for reinsurance contracts treated?",
    choices = c(
      "Adjust the CSM or go through P&L",
      "Ignore until contract maturity",
      "Expensed as acquisition costs",
      "Deferred indefinitely"
    )
  ),
  q7 = list(
    title = "7. Under the General Model, what happens to the CSM for reinsurance contracts held over time?",
    choices = c(
      "It grows with claims paid",
      "It's released based on services received",
      "It remains constant",
      "It is immediately expensed"
    )
  ),
  q8 = list(
    title = "8. How are reinsurance recoveries presented in the income statement?",
    choices = c(
      "Included in insurance revenue",
      "Included in investment income",
      "Separately from insurance revenue",
      "Net of insurance service expenses"
    )
  ),
  q9 = list(
    title = "9. How are recoveries for past claims treated under reinsurance contracts held?",
    choices = c(
      "Deferred in CSM",
      "Expensed as incurred",
      "Recognized in profit or loss immediately",
      "Deducted from LRC"
    )
  ),
  q10 = list(
    title = "10. What is the impact of a reinsurance CSM being negative?",
    choices = c(
      "It represents a loss",
      "It is a liability",
      "It is not allowed",
      "It's treated as an asset, not a liability"
    )
  )
)

correct_answers_module10 <- list(
  q1 = list(
    answer = "A contract under which an entity receives compensation for claims from a reinsurer",
    explanation = "A reinsurance contract held is one where the insurer (cedant) transfers insurance risk and receives compensation from the reinsurer for claims."
  ),
  q2 = list(
    answer = "At the earlier of coverage start or when underlying contracts are onerous",
    explanation = "Recognition occurs at the earlier of when reinsurance coverage begins or when the reinsurance covers a recognized loss from onerous contracts."
  ),
  q3 = list(
    answer = "No, it is included in the CSM",
    explanation = "Gains on the purchase of reinsurance are deferred within the Contractual Service Margin (CSM) and recognized over the coverage period."
  ),
  q4 = list(
    answer = "Reinsurer's risk appetite",
    explanation = "Fulfilment cash flows include expected recoveries, discounting, and risk adjustment—not subjective elements like reinsurer's risk appetite."
  ),
  q5 = list(
    answer = "Transfers and reduces risk",
    explanation = "Reinsurance helps the insurer reduce and manage their insurance risk by transferring a portion of it to the reinsurer."
  ),
  q6 = list(
    answer = "Adjust the CSM or go through P&L",
    explanation = "Changes in fulfilment cash flows adjust the CSM if they relate to future services, or are recognized in profit or loss otherwise."
  ),
  q7 = list(
    answer = "It's released based on services received",
    explanation = "The CSM for reinsurance contracts held is released over time based on the receipt of reinsurance services."
  ),
  q8 = list(
    answer = "Separately from insurance revenue",
    explanation = "IFRS 17 requires that reinsurance income and expenses be presented separately from insurance revenue and service expenses."
  ),
  q9 = list(
    answer = "Recognized in profit or loss immediately",
    explanation = "Recoveries for past claims are immediately recognized in profit or loss as they relate to events that have already occurred."
  ),
  q10 = list(
    answer = "It's treated as an asset, not a liability",
    explanation = "A negative CSM on a reinsurance contract held represents a net cost to the insurer and is treated as an asset."
  )
)

# Module 11 Questions
module11_questions <- list(
  q1 = list(
    title = "1. Which of the following statements best describes a key difference between insurance contracts with direct participation features and investment contracts with discretionary participation features under IFRS 17?",
    choices = c(
      "Insurance contracts with direct participation features are accounted for under IFRS 9, while investment contracts with discretionary participation features are accounted for under IFRS 17.",
      "Both contract types are accounted for using the Premium Allocation Approach (PAA) under IFRS 17.",
      "Insurance contracts with direct participation features use the Variable Fee Approach (VFA), while investment contracts with discretionary participation features are accounted for under IFRS 17 with minor modifications.",
      "Investment contracts with discretionary participation features involve no discretionary element and must follow IFRS 15."
    )
  ),
  q2 = list(
    title = "2. Which of the following contracts would be classified as an 'investment contract with discretionary participation features' under IFRS 17?",
    choices = c(
      "A savings contract where the insurer retains discretion over bonus payments and the contract does not transfer significant insurance risk.",
      "A unit-linked investment product with guaranteed returns and no discretionary elements.",
      "A life insurance contract that entitles the policyholder to a share of asset returns and includes significant insurance risk.",
      "A pure term life policy with no investment component or discretionary features."
    )
  ),
  q3 = list(
    title = "3. Under IFRS 17, what does the coverage period of a Direct Participation Contract (DPC) include?",
    choices = c(
      "Only the period during which insurance risk is present.",
      "Only the period over which investment returns are credited to the policyholder.",
      "Both the investment and insurance service periods under the contract.",
      "Only the period where fair value of underlying items increases."
    )
  ),
  q4 = list(
    title = "4. Which of the following best defines an 'underlying item' under IFRS 17?",
    choices = c(
      "Any asset owned by the insurer",
      "Any amount guaranteed to the policyholder",
      "Any item that determines amounts payable to the policyholder",
      "A group of insurance contracts with discretionary returns"
    )
  ),
  q5 = list(
    title = "5. According to IFRS 17, what makes a policyholder's participation in a pool of underlying items valid for DPC classification?",
    choices = c(
      "The insurer's intention to share profits",
      "A strong historical pattern of bonus declarations",
      "A legally enforceable right to a share of underlying items",
      "Regulatory expectation that profits be distributed fairly"
    )
  ),
  q6 = list(
    title = "6. Which of the following contracts is most likely NOT to qualify as having a clearly identified pool of underlying items under IFRS 17?",
    choices = c(
      "A unit-linked contract where fund allocation is defined",
      "A with-profits contract tied to a disclosed internal asset pool",
      "A policy linked to an external market index explicitly mentioned in the policy",
      "A universal life contract where the crediting rate is set by the insurer ex post"
    )
  ),
  q7 = list(
    title = "7. When is the assessment of whether an insurance contract qualifies as a Direct Participation Contract (DPC) performed under IFRS 17?",
    choices = c(
      "At initial recognition and not subsequently repeated",
      "At the date of contract modification",
      "At the end of each reporting period",
      "Annually, based on updated assumptions"
    )
  ),
  q8 = list(
    title = "8. Which of the following statements is TRUE about discounting in the measurement of Direct Participation Contracts under IFRS 17?",
    choices = c(
      "DPCs use locked-in discount rates for all adjustments",
      "Adjustments to cash flows not based on underlying items are discounted using current rates",
      "DPCs follow a special discounting method unique to these contracts",
      "Discounting is not applicable to DPCs"
    )
  ),
  q9 = list(
    title = "9. How does the adjustment of the Contractual Service Margin (CSM) for financial risks differ between contracts with and without direct participation features under IFRS 17?",
    choices = c(
      "Contracts with direct participation features adjust the CSM for changes in financial risk using the current interest curve, even if unrelated to future service.",
      "Only contracts without direct participation features adjust the CSM for financial risks using the current discount rate.",
      "For both types of contracts, the CSM is adjusted using the locked-in interest rate.",
      "Neither type of contract adjusts the CSM for financial risks unrelated to underlying items."
    )
  ),
  q10 = list(
    title = "10. What is the appropriate IFRS 17 treatment when a Direct Participation Contract (DPC) is modified such that it no longer meets the definition of a DPC?",
    choices = c(
      "The contract continues to be treated as a DPC until expiry.",
      "The contract is reclassified prospectively without derecognition.",
      "The original contract is derecognised, and a new contract is recognised based on the modified terms.",
      "Only the CSM is adjusted to reflect the modification."
    )
  ),
  q11 = list(
    title = "11. Which of the following best describes an investment contract with discretionary participation features under IFRS 17?",
    choices = c(
      "A financial instrument that guarantees fixed returns and is always classified under IFRS 9.",
      "A contract that gives the investor a right to additional amounts determined solely by market interest rates.",
      "A unit-linked insurance contract with no discretionary elements.",
      "A financial instrument providing the investor with a right to receive significant additional benefits that are contractually discretionary and based on returns or performance."
    )
  )
)

correct_answers_module11 <- list(
  q1 = list(
    answer = "Insurance contracts with direct participation features use the Variable Fee Approach (VFA), while investment contracts with discretionary participation features are accounted for under IFRS 17 with minor modifications.",
    explanation = "IFRS 17 differentiates between DPCs, which use the VFA, and investment contracts with DPFs, which are not insurance contracts but still fall under IFRS 17 with modifications."
  ),
  q2 = list(
    answer = "A savings contract where the insurer retains discretion over bonus payments and the contract does not transfer significant insurance risk.",
    explanation = "Investment contracts with DPFs have no significant insurance risk but offer discretionary returns, placing them within the scope of IFRS 17 (not IFRS 9)."
  ),
  q3 = list(
    answer = "Both the investment and insurance service periods under the contract.",
    explanation = "For DPCs, IFRS 17 states that the coverage period includes both insurance and investment services, unlike other contract types."
  ),
  q4 = list(
    answer = "Any item that determines amounts payable to the policyholder",
    explanation = "Under IFRS 17, underlying items are defined as the basis for determining amounts payable and can include internal or external references."
  ),
  q5 = list(
    answer = "A legally enforceable right to a share of underlying items",
    explanation = "DPC classification under IFRS 17 requires that participation rights in underlying items be contractually or legally enforceable."
  ),
  q6 = list(
    answer = "A universal life contract where the crediting rate is set by the insurer ex post",
    explanation = "Such contracts lack a clearly identified pool of underlying items and therefore fail the DPC requirement under IFRS 17."
  ),
  q7 = list(
    answer = "At initial recognition and not subsequently repeated",
    explanation = "IFRS 17 requires a one-time DPC qualification assessment at initial recognition. It's not updated unless there's a modification leading to derecognition."
  ),
  q8 = list(
    answer = "Adjustments to cash flows not based on underlying items are discounted using current rates",
    explanation = "For DPCs, current interest rates apply to non-underlying item cash flow adjustments, supporting a fair value-based approach."
  ),
  q9 = list(
    answer = "Contracts with direct participation features adjust the CSM for changes in financial risk using the current interest curve, even if unrelated to future service.",
    explanation = "DPCs uniquely adjust the CSM for financial risk using the current interest curve, while other contracts use locked-in rates."
  ),
  q10 = list(
    answer = "The original contract is derecognised, and a new contract is recognised based on the modified terms.",
    explanation = "If a DPC is modified and no longer qualifies, IFRS 17 requires derecognition and recognition of a new contract reflecting the new terms."
  ),
  q11 = list(
    answer = "A financial instrument providing the investor with a right to receive significant additional benefits that are contractually discretionary and based on returns or performance.",
    explanation = "Investment contracts with DPFs provide discretionary, performance-based returns and fall under IFRS 17 if issued by insurers offering insurance contracts."
  )
)

# Module 12 Questions
module12_questions <- list(
  q1 = list(
    title = "1. When is a contract considered modified under IFRS 17?",
    choices = c(
      "When it is extended",
      "When contractual cash flows change",
      "When insurer and policyholder agree on new terms",
      "When premiums change"
    )
  ),
  q2 = list(
    title = "2. What is the first step when assessing a contract modification under IFRS 17?",
    choices = c(
      "Recognize as new contract",
      "Adjust the CSM",
      "Assess whether modification is substantial",
      "Update the risk adjustment"
    )
  ),
  q3 = list(
    title = "3. If a contract modification results in substantially different terms, what is the accounting treatment?",
    choices = c(
      "Adjust liability only",
      "Derecognize old and recognize new contract",
      "Adjust insurance revenue",
      "Disclose in notes only"
    )
  ),
  q4 = list(
    title = "4. What is the impact on CSM if a modification is not substantial?",
    choices = c(
      "It is reversed",
      "It is remeasured",
      "It is released to profit",
      "It is written off"
    )
  ),
  q5 = list(
    title = "5. Under IFRS 17, what causes derecognition of an insurance contract?",
    choices = c(
      "Claims payment",
      "Expiry of coverage",
      "Settlement or cancellation",
      "Change in accounting policy"
    )
  ),
  q6 = list(
    title = "6. Which of the following changes is considered substantial?",
    choices = c(
      "Adding a new coverage type",
      "Change in billing address",
      "Change in payment date",
      "Update to claims contact"
    )
  ),
  q7 = list(
    title = "7. How is the carrying amount of the derecognized contract treated?",
    choices = c(
      "It is capitalized",
      "It is transferred to reserves",
      "It is removed from the balance sheet",
      "It is restated"
    )
  ),
  q8 = list(
    title = "8. How is a new contract initially recognized?",
    choices = c(
      "Based on old values",
      "Using fair value",
      "Using fulfilment cash flows at date of modification",
      "Not recognized separately"
    )
  ),
  q9 = list(
    title = "9. How are derecognised contracts due to full settlement treated?",
    choices = c(
      "CSM is amortized",
      "Recognize gain/loss",
      "Asset revaluation",
      "Insurance revenue restated"
    )
  ),
  q10 = list(
    title = "10. What is the primary difference between substantial and non-substantial modifications?",
    choices = c(
      "Impact on reinsurance",
      "Change in timing of premium",
      "Need for derecognition",
      "Claims experience"
    )
  ),
  q11 = list(
    title = "11. Which of the following is NOT a reason for derecognition?",
    choices = c(
      "Contract lapses",
      "Contract is modified substantially",
      "Policy is cancelled",
      "Policyholder pays premium early"
    )
  ),
  q12 = list(
    title = "12. What is the derecognition criteria under IFRS 17 for insurance contract liabilities?",
    choices = c(
      "Legal cancellation",
      "Transfer to another insurer",
      "Extinguishment of obligation",
      "Policyholder request"
    )
  ),
  q13 = list(
    title = "13. What must be disclosed upon derecognition of a contract?",
    choices = c(
      "Nothing",
      "Reason for derecognition and financial impact",
      "Transition adjustments",
      "Future premiums"
    )
  ),
  q14 = list(
    title = "14. Which modification would not be considered substantial?",
    choices = c(
      "Adding a new benefit",
      "Removing a major risk cover",
      "Changing claim limits significantly",
      "Changing policyholder address"
    )
  )
)

correct_answers_module12 <- list(
  q1 = list(
    answer = "When contractual cash flows change",
    explanation = "A contract is modified when the terms change in a way that affects fulfilment cash flows, not just administrative details."
  ),
  q2 = list(
    answer = "Assess whether modification is substantial",
    explanation = "Before deciding the accounting treatment, the insurer must assess if the modification significantly changes the contract terms."
  ),
  q3 = list(
    answer = "Derecognize old and recognize new contract",
    explanation = "If the modification leads to substantially different terms, the original contract is derecognized, and a new one is recognized."
  ),
  q4 = list(
    answer = "It is remeasured",
    explanation = "If the modification is not substantial, the Contractual Service Margin (CSM) is adjusted (remeasured) without derecognition."
  ),
  q5 = list(
    answer = "Settlement or cancellation",
    explanation = "Derecognition occurs when the insurer's obligation ends, such as through settlement, cancellation, or expiration."
  ),
  q6 = list(
    answer = "Adding a new coverage type",
    explanation = "Adding a new coverage type alters the risk profile, which is considered a substantial modification under IFRS 17."
  ),
  q7 = list(
    answer = "It is removed from the balance sheet",
    explanation = "Upon derecognition, the contract liability is derecognized and removed from the statement of financial position."
  ),
  q8 = list(
    answer = "Using fulfilment cash flows at date of modification",
    explanation = "The new contract is measured based on fulfilment cash flows as at the modification date."
  ),
  q9 = list(
    answer = "Recognize gain/loss",
    explanation = "Derecognition from full settlement leads to recognizing a gain or loss from the difference in cash flows."
  ),
  q10 = list(
    answer = "Need for derecognition",
    explanation = "Substantial modifications require derecognition; non-substantial ones do not."
  ),
  q11 = list(
    answer = "Policyholder pays premium early",
    explanation = "Early premium payment does not end the insurer's contractual obligation and thus is not a derecognition trigger."
  ),
  q12 = list(
    answer = "Extinguishment of obligation",
    explanation = "A contract liability is derecognized when the insurer's obligation to the policyholder is fully extinguished."
  ),
  q13 = list(
    answer = "Reason for derecognition and financial impact",
    explanation = "IFRS 17 requires disclosure of the reasons for derecognition and its impact on the financial statements."
  ),
  q14 = list(
    answer = "Changing policyholder address",
    explanation = "Administrative changes like address updates are not substantial modifications affecting the contract terms."
  )
)

# Module 13 Questions
module13_questions <- list(
  q1 = list(
    title = "1. How are changes in the risk adjustment presented if not disaggregated?",
    choices = c(
      "In other comprehensive income",
      "Fully within the insurance service result",
      "As a deferred liability",
      "As finance income"
    )
  ),
  q2 = list(
    title = "2. How should an entity present a group of contracts with a net obligation (i.e., expected outflows exceed inflows) in the statement of financial position?",
    choices = c(
      "As an asset",
      "As a liability",
      "Under equity",
      "Offset against premiums receivable"
    )
  ),
  q3 = list(
    title = "3. How should an entity present insurance contracts issued in the statement of financial position?",
    choices = c(
      "Only when the contracts are profitable",
      "Combined with acquisition cash flows only",
      "As either assets or liabilities, depending on the net fulfilment cash flows",
      "Net of reinsurance recoverables"
    )
  ),
  q4 = list(
    title = "4. What drives the distinction between insurance revenue and insurance finance income/expenses?",
    choices = c(
      "Time value of money and discount rates",
      "Policy type",
      "Underwriting year",
      "Geographical spread"
    )
  ),
  q5 = list(
    title = "5. What is the appropriate presentation of acquisition costs related to a group of reinsurance contracts held?",
    choices = c(
      "Expensed immediately",
      "Included in insurance service expenses",
      "Reported under administrative expenses",
      "Included in the carrying amount of reinsurance contracts held"
    )
  ),
  q6 = list(
    title = "6. Which of the following would most likely be presented as an insurance liability?",
    choices = c(
      "Deferred acquisition costs",
      "Accrued interest income",
      "Outstanding claims reserves",
      "Expected future premium inflows"
    )
  ),
  q7 = list(
    title = "7. Under IFRS 17, how are insurance contract assets and liabilities presented in the Statement of Financial Position?",
    choices = c(
      "Offset against each other",
      "Presented separately for each group of contracts",
      "Presented net at the entity level",
      "Combined and shown as a single line item"
    )
  ),
  q8 = list(
    title = "8. What is the treatment of a group of onerous contracts in the SFP?",
    choices = c(
      "Recognized as a liability",
      "Included under reinsurance",
      "Recognized as an asset",
      "Deferred to future periods"
    )
  ),
  q9 = list(
    title = "9. Which IFRS 17 paragraph outlines the presentation requirements for the SFP?",
    choices = c(
      "IFRS 17.32",
      "IFRS 17.109",
      "IFRS 17.42",
      "IFRS 17.78"
    )
  ),
  q10 = list(
    title = "10. Which of the following is not shown separately in IFRS 17 SFP presentation?",
    choices = c(
      "Insurance contract liabilities",
      "Insurance contract assets",
      "Deferred acquisition costs",
      "Reinsurance contract assets"
    )
  )
)

correct_answers_module13 <- list(
  q1 = list(
    answer = "Fully within the insurance service result",
    explanation = "If an entity does not choose to disaggregate changes in the risk adjustment, the full change is presented within the insurance service result as per IFRS 17 guidance."
  ),
  q2 = list(
    answer = "As a liability",
    explanation = "Under IFRS 17, if fulfilment cash flows of a group of insurance contracts result in a net obligation, it is presented as a liability in the statement of financial position."
  ),
  q3 = list(
    answer = "As either assets or liabilities, depending on the net fulfilment cash flows",
    explanation = "Insurance contracts are presented based on whether fulfilment cash flows result in a net asset or liability, providing clarity on the insurer's financial position."
  ),
  q4 = list(
    answer = "Time value of money and discount rates",
    explanation = "The distinction arises because revenue reflects service delivery, while finance income/expenses reflect economic effects from interest rates and time value of money."
  ),
  q5 = list(
    answer = "Included in the carrying amount of reinsurance contracts held",
    explanation = "IFRS 17 requires acquisition cash flows related to reinsurance contracts held to be included in the carrying amount of the group, ensuring proper expense matching."
  ),
  q6 = list(
    answer = "Outstanding claims reserves",
    explanation = "These reserves represent obligations still owed by the insurer and are therefore presented as insurance liabilities."
  ),
  q7 = list(
    answer = "Presented separately for each group of contracts",
    explanation = "IFRS 17 requires insurance contract assets and liabilities to be presented separately for each group and prohibits offsetting at the entity level."
  ),
  q8 = list(
    answer = "Recognized as a liability",
    explanation = "Onerous contracts result in expected losses and must be recognized as liabilities in the statement of financial position under IFRS 17."
  ),
  q9 = list(
    answer = "IFRS 17.78",
    explanation = "IFRS 17.78 outlines the requirement for separate presentation of insurance contract assets and liabilities in the statement of financial position."
  ),
  q10 = list(
    answer = "Deferred acquisition costs",
    explanation = "Under IFRS 17, DAC is not presented separately but included within the fulfilment cash flows, reflecting a more integrated liability valuation."
  )
)

# Module 14 Questions
module14_questions <- list(
  q1 = list(
    title = "1. What are the two main components an entity must disaggregate in the statement of financial performance?",
    choices = c(
      "Revenue and Expenses",
      "Insurance Profit and Investment Return",
      "Earned Premium and Unearned Premium",
      "Insurance Service Result and Insurance Finance Income or Expenses"
    )
  ),
  q2 = list(
    title = "2. Which of the following is included in the insurance service result?",
    choices = c(
      "Change in risk adjustment for non-financial risk",
      "Insurance revenue and insurance service expenses",
      "Investment income",
      "Premium refunds"
    )
  ),
  q3 = list(
    title = "3. How should an entity present income or expenses from reinsurance contracts held?",
    choices = c(
      "Separately from insurance contracts issued",
      "Together with insurance contracts issued",
      "Only in other comprehensive income",
      "As a deferred liability"
    )
  ),
  q4 = list(
    title = "4. What should insurance revenue reflect?",
    choices = c(
      "Premiums received",
      "Claims paid",
      "The consideration expected in exchange for coverage and services",
      "Cash flow timing"
    )
  ),
  q5 = list(
    title = "5. Which of the following is NOT included in insurance service expenses?",
    choices = c(
      "Incurred claims",
      "Investment components",
      "Other incurred insurance service expenses",
      "Amounts in paragraph 103(b)"
    )
  ),
  q6 = list(
    title = "6. When disaggregating the change in the risk adjustment for non-financial risk, what is required?",
    choices = c(
      "Mandatory allocation between finance and service result",
      "No allocation is permitted",
      "Optional disaggregation; otherwise, include fully in the insurance service result",
      "Include entirely in finance income"
    )
  ),
  q7 = list(
    title = "7. Which component may NOT be presented in profit or loss?",
    choices = c(
      "Premium information inconsistent with Paragraph 83",
      "Insurance service expenses",
      "Reinsurance income",
      "Risk adjustment"
    )
  ),
  q8 = list(
    title = "8. What options does an entity have for presenting reinsurance contracts held?",
    choices = c(
      "Only as a single net amount",
      "Only as individual line items",
      "Either as a net amount or split into recovered amounts and premium allocations",
      "As part of investment income"
    )
  ),
  q9 = list(
    title = "9. Under the Premium Allocation Approach (PAA), what is insurance revenue generally similar to?",
    choices = c(
      "Earned premium",
      "Written premium",
      "Total premiums received",
      "Premium receivable"
    )
  ),
  q10 = list(
    title = "10. Under the General Measurement Model (GMM), which of the following is NOT part of insurance service revenue?",
    choices = c(
      "Release of the Contractual Service Margin (CSM)",
      "Investment returns",
      "Risk adjustment for non-financial risk",
      "Recovery of acquisition cash flows"
    )
  ),
  q11 = list(
    title = "11. What are 'losses on onerous contracts' classified as under IFRS 17?",
    choices = c(
      "Investment finance expenses",
      "Deferred income",
      "Premium liability",
      "Insurance service expenses"
    )
  ),
  q12 = list(
    title = "12. What does IFRS 17 aim to achieve by separating service result and finance result?",
    choices = c(
      "Enhanced transparency and comparability",
      "Compliance with local GAAP",
      "Tax optimization",
      "Maximizing investment returns"
    )
  ),
  q13 = list(
    title = "13. What is the treatment of reinsurance-related cash flows contingent on claims?",
    choices = c(
      "Deferred revenue",
      "Presented as part of claims recoverable",
      "Included in insurance finance income",
      "Excluded from the financial statements"
    )
  ),
  q14 = list(
    title = "14. What is the effect of the reversal of losses on onerous contracts?",
    choices = c(
      "Increase in insurance finance income",
      "Reduction in insurance service revenue",
      "Decrease in liabilities and increase in insurance service result",
      "Increase in insurance acquisition costs"
    )
  )
)

correct_answers_module14 <- list(
  q1 = list(
    answer = "Insurance Service Result and Insurance Finance Income or Expenses",
    explanation = "IFRS 17 requires disaggregation into Insurance Service Result and Insurance Finance Income or Expenses in the statement of financial performance."
  ),
  q2 = list(
    answer = "Insurance revenue and insurance service expenses",
    explanation = "The insurance service result comprises insurance revenue and insurance service expenses (Paragraph 83)."
  ),
  q3 = list(
    answer = "Separately from insurance contracts issued",
    explanation = "Reinsurance contract results must be presented separately from insurance contracts issued (Paragraph 85)."
  ),
  q4 = list(
    answer = "The consideration expected in exchange for coverage and services",
    explanation = "Insurance revenue should depict the consideration to which the entity expects to be entitled for providing insurance coverage and services (Paragraph 83)."
  ),
  q5 = list(
    answer = "Investment components",
    explanation = "Insurance service expenses exclude investment components (Paragraph 84)."
  ),
  q6 = list(
    answer = "Optional disaggregation; otherwise, include fully in the insurance service result",
    explanation = "If not disaggregated, the entire change is included in the insurance service result (Paragraph 82)."
  ),
  q7 = list(
    answer = "Premium information inconsistent with Paragraph 83",
    explanation = "Premium information should not be presented if inconsistent with how revenue is defined under Paragraph 83."
  ),
  q8 = list(
    answer = "Either as a net amount or split into recovered amounts and premium allocations",
    explanation = "The entity may present a net amount or separate amounts (recoveries and premium allocations) as long as they total the same net amount."
  ),
  q9 = list(
    answer = "Earned premium",
    explanation = "Under PAA, insurance revenue is generally similar to earned premium over the coverage period."
  ),
  q10 = list(
    answer = "Investment returns",
    explanation = "Investment returns are excluded; insurance service revenue includes claims, risk adjustment, CSM release, and acquisition cost recovery."
  ),
  q11 = list(
    answer = "Insurance service expenses",
    explanation = "Losses on onerous contracts are part of insurance service expenses as they relate to future service obligations."
  ),
  q12 = list(
    answer = "Enhanced transparency and comparability",
    explanation = "IFRS 17 aims to enhance transparency and comparability by clearly separating insurance services from financial effects."
  ),
  q13 = list(
    answer = "Presented as part of claims recoverable",
    explanation = "Per paragraph 86(a), such cash flows are part of the expected claims to be reimbursed under the reinsurance contract."
  ),
  q14 = list(
    answer = "Decrease in liabilities and increase in insurance service result",
    explanation = "A reversal of losses on onerous contracts reduces liabilities and increases the insurance service result (Paragraph 84(c))."
  )
)

# Module 15 Questions
module15_questions <- list(
  q1 = list(
    title = "1. What are Insurance Finance Income or Expenses (IFIE)?",
    choices = c(
      "Premiums and claims",
      "Acquisition costs and investment income",
      "Changes in non-financial assumptions",
      "Time value of money and financial risk impacts"
    )
  ),
  q2 = list(
    title = "2. How can IFIE be presented in the statement of financial performance?",
    choices = c(
      "Either fully in P&L or disaggregated between P&L and OCI",
      "Only in Profit or Loss (P&L)",
      "Only in Other Comprehensive Income (OCI)",
      "Only in the notes to the financial statements"
    )
  ),
  q3 = list(
    title = "3. Are there any exceptions to the general treatment of IFIE?",
    choices = c(
      "No exceptions",
      "Yes, for reinsurance contracts",
      "Yes, for insurance contracts with direct participation features and certain assumptions that would adjust CSM but don't",
      "Yes, if the policyholder is a related party"
    )
  ),
  q4 = list(
    title = "4. If the entity chooses to disaggregate IFIE between P&L and OCI, how should the disaggregation be made?",
    choices = c(
      "Based on actual market returns",
      "Using a locked-in discount rate to allocate a portion to P&L",
      "Arbitrarily",
      "Based on revenue recognition patterns"
    )
  ),
  q5 = list(
    title = "5. How should IFIE recorded in OCI be treated when a group of insurance contracts is transferred or derecognized (per paragraph 91)?",
    choices = c(
      "They are reversed in the next period",
      "They are transferred to equity",
      "They are reclassified to P&L if Option 2 under paragraph 88 was applied",
      "They remain in OCI permanently"
    )
  ),
  q6 = list(
    title = "6. How should exchange differences on changes in the carrying amount of groups of insurance contracts be treated?",
    choices = c(
      "Always in OCI",
      "Always in equity",
      "Not recognized",
      "In P&L unless they relate to OCI-recorded IFIE, in which case they go to OCI"
    )
  ),
  q7 = list(
    title = "7. Which component is typically included in the effect of the time value of money under IFIE?",
    choices = c(
      "Expected claims",
      "Acquisition costs",
      "Interest accretion on insurance liabilities",
      "Reinsurance asset recoveries"
    )
  ),
  q8 = list(
    title = "8. Which of the following best describes the treatment of IFIE for contracts with direct participation features?",
    choices = c(
      "Must always be presented in OCI",
      "May be presented to eliminate mismatches with underlying items",
      "Not applicable to participating contracts",
      "Must always be presented in P&L"
    )
  ),
  q9 = list(
    title = "9. If a group of contracts is derecognized and IFIE has been disaggregated under paragraph 89(b), what happens to amounts in OCI?",
    choices = c(
      "Remain in OCI",
      "Transferred to P&L",
      "Reversed",
      "Transferred to retained earnings"
    )
  ),
  q10 = list(
    title = "10. What type of financial risk would be reflected in IFIE?",
    choices = c(
      "Lapse risk",
      "Currency risk",
      "Inflation risk",
      "Equity or interest rate risk impacting discount rates"
    )
  ),
  q11 = list(
    title = "11. Why might an entity choose to disaggregate IFIE between P&L and OCI?",
    choices = c(
      "To smooth earnings volatility",
      "To avoid recognizing claims",
      "To increase policyholder bonuses",
      "To reduce insurance liabilities"
    )
  ),
  q12 = list(
    title = "12. Which paragraph of IFRS 17 allows IFIE disaggregation for non-participating contracts?",
    choices = c(
      "Paragraph 30",
      "Paragraph 88",
      "Paragraph 45",
      "Paragraph 135"
    )
  ),
  q13 = list(
    title = "13. When an entity opts to recognize all IFIE in P&L, the impact on OCI is",
    choices = c(
      "Neutral (no impact)",
      "Positive",
      "Negative",
      "Deferred to future periods"
    )
  ),
  q14 = list(
    title = "14. Which type of insurance contract is most likely to involve disaggregation based on underlying item performance?",
    choices = c(
      "Group term life insurance",
      "Non-participating whole life",
      "Universal life insurance with direct participation features",
      "Reinsurance contracts held"
    )
  )
)

correct_answers_module15 <- list(
  q1 = list(
    answer = "Time value of money and financial risk impacts",
    explanation = "IFIE represent changes in the carrying amount of insurance contracts due to the effect of the time value of money and financial risk, such as interest accretion and changes in discount rates."
  ),
  q2 = list(
    answer = "Either fully in P&L or disaggregated between P&L and OCI",
    explanation = "IFRS 17 allows a policy choice: present all IFIE in P&L or disaggregate them between P&L and OCI, depending on the approach selected."
  ),
  q3 = list(
    answer = "Yes, for insurance contracts with direct participation features and certain assumptions that would adjust CSM but don't",
    explanation = "IFRS 17 excludes from IFIE those changes in financial assumptions that would adjust the contractual service margin (CSM) but do not do so due to application of specific paragraphs (45(b)(ii), etc.)."
  ),
  q4 = list(
    answer = "Using a locked-in discount rate to allocate a portion to P&L",
    explanation = "A systematic allocation using a locked-in discount rate at initial recognition is applied. The portion not recognized in P&L is reported in OCI."
  ),
  q5 = list(
    answer = "They are reclassified to P&L if Option 2 under paragraph 88 was applied",
    explanation = "If the disaggregation under paragraph 88(b) was used, the remaining OCI balance is reclassified to P&L as a reclassification adjustment."
  ),
  q6 = list(
    answer = "In P&L unless they relate to OCI-recorded IFIE, in which case they go to OCI",
    explanation = "Under IAS 21, insurance contracts are monetary items. Exchange differences are included in P&L, except when they relate to amounts in OCI (then they stay in OCI)."
  ),
  q7 = list(
    answer = "Interest accretion on insurance liabilities",
    explanation = "Interest accretion reflects the unwinding of the discount on insurance liabilities, part of the time value of money in IFIE."
  ),
  q8 = list(
    answer = "May be presented to eliminate mismatches with underlying items",
    explanation = "Entities can choose to disaggregate IFIE in a way that eliminates accounting mismatches with underlying items."
  ),
  q9 = list(
    answer = "Remain in OCI",
    explanation = "Under paragraph 91(b), OCI amounts from paragraph 89(b) are not reclassified to P&L."
  ),
  q10 = list(
    answer = "Equity or interest rate risk impacting discount rates",
    explanation = "IFIE includes effects of financial risk, such as interest rate or equity risk, which influence the present value of insurance liabilities."
  ),
  q11 = list(
    answer = "To smooth earnings volatility",
    explanation = "Disaggregation helps reduce volatility in P&L from market-driven movements in discount rates and financial assumptions."
  ),
  q12 = list(
    answer = "Paragraph 88",
    explanation = "Paragraph 88 provides the accounting policy choice for disaggregating IFIE for non-participating contracts."
  ),
  q13 = list(
    answer = "Neutral (no impact)",
    explanation = "If all IFIE are recognized in P&L, there is no effect on OCI."
  ),
  q14 = list(
    answer = "Universal life insurance with direct participation features",
    explanation = "Contracts with direct participation features (e.g., universal life tied to asset performance) often use disaggregation aligned with underlying items."
  )
)

# Process all modules
all_modules <- list(
  list(num = 1, questions = module1_questions, answers = correct_answers_module1),
  list(num = 2, questions = module2_questions, answers = correct_answers_module2),
  list(num = 3, questions = module3_questions, answers = correct_answers_module3),
  list(num = 4, questions = module4_questions, answers = correct_answers_module4),
  list(num = 5, questions = module5_questions, answers = correct_answers_module5),
  list(num = 6, questions = module6_questions, answers = correct_answers_module6),
  list(num = 7, questions = module7_questions, answers = correct_answers_module7),
  list(num = 8, questions = module8_questions, answers = correct_answers_module8),
  list(num = 9, questions = module9_questions, answers = correct_answers_module9),
  list(num = 10, questions = module10_questions, answers = correct_answers_module10),
  list(num = 11, questions = module11_questions, answers = correct_answers_module11),
  list(num = 12, questions = module12_questions, answers = correct_answers_module12),
  list(num = 13, questions = module13_questions, answers = correct_answers_module13),
  list(num = 14, questions = module14_questions, answers = correct_answers_module14),
  list(num = 15, questions = module15_questions, answers = correct_answers_module15)
)

# Insert questions for all modules
for (module in all_modules) {
  module_data <- prepare_module_questions(module$num, module$questions, module$answers)
  
  print(paste("Processing Module", module$num, "with", length(module_data), "questions"))
  
  for (question in module_data) {
    success <- insert_quiz_questions(list(question), admin_token)
    if (success) {
      print(paste("✓ Inserted question", question$question_number, "for", question$module_name))
    } else {
      print(paste("✗ Failed to insert question", question$question_number, "for", question$module_name))
    }
  }
  
  print(paste("Module", module$num, "completed\n"))
}

print("All quiz questions insertion completed!")