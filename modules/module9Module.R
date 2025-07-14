# This module is for the IFRS 17 module 10
IFRS17Module9UI <- function(id) {
  ns <- NS(id)
  logo_bar <- fluidRow(
    class = "logo-bar",                     # you’ll style this in CSS
    column(
      width = 12, style = "border-left: 3px solid #DC5A17;",
      tags$div(
        class = "logo-wrapper d-flex justify-content-between align-items-center",
        # left-hand logo
        tags$img(
          src   = "images/ira_logo_.png", class = "logo logo-afdb"),
        # right-hand logo
        tags$img(src   = "images/kenbright.png",  class = "logo logo-kenbright")
      )
    )
  )
tagList(
  logo_bar,
    div(
      class = "section-header",
      h2("📘 Module 9: Premium Allocation Approach", class = "section-title-top"),
      h3(icon("info-circle"), "Module Objective", class = "section-subheading"),
      p("This module provides a comprehensive overview of the Premium Allocation Approach (PAA) as per IFRS 17 paragraphs 53 to 59."),
      p("The Premium Allocation Approach (PAA) is a simplified method for measuring insurance contract liabilities under IFRS 17."),
      p("It is primarily applied to short-term contracts—usually those with coverage periods of 12 months or less—such as motor, travel, health, and various general insurance policies."),
      p("Conceptually, the PAA resembles the unearned premium reserve (UPR) approach previously used under IFRS 4."),
      p("Contracts must initially be grouped into portfolios that share similar risk characteristics and are managed under the same strategy. Each portfolio is then classified into the following categories:"),
      tags$ul(
        tags$li("Onerous at inception"),
        tags$li("No significant risk of becoming onerous"),
        tags$li("Profitable contracts")
      ),
      p("Contracts issued more than 12 months apart cannot be grouped together.")
    ),

    div(class = "module-section",  
      h3("Eligibility Criteria", class = "section-subtitle"),
      p("According to the standard, PAA can be applied if either of the following conditions is met:"),
      tags$ol(
        tags$li("If the coverage period for each contract within the group is no longer than one year; or"),
        tags$li("If the entity reasonably expects that applying the PAA would result in a measurement of the liability for remaining coverage (LRC) that is not materially different from that produced by the General Measurement Model (GMM).")
      ),
      img(src = "images/paaImage.png", class = "module-image")
     ),

      div(
        h3("Measurement Under PAA", class = "section-subtitle"),
        p("There are two primary components of liability to consider:"),
        
        tags$strong("A. Liability for Remaining Coverage (LRC)"),
        p("This reflects the portion of premiums that pertains to future periods of coverage, adjusted for acquisition costs."),
        tags$ol(type = "a",
          tags$li("Opening LRC balance: The starting balance at the beginning of the period."),
          tags$li("Add: Premium Received: Additional premiums collected during the period."),
          tags$li("Less: Amortization of insurance time value of money and financial risks: Amortized acquisition costs deducted."),
          tags$li("Less: Insurance Revenue: Revenue recognized over time as insurance services are provided.")
        ),
        p("**LRC formula:**  
          LRC = Opening LRC + Premium received – Earned Premium – Change in Deferred Acquisition Costs (DAC)"),
        p("**Where Premium received =** Gross Written Premium (GWP) + Prior premium receivables – Current premium receivables."),
        
        tags$strong("B. Liability for Incurred Claims (LIC)"),
        p("This is measured in a manner similar to the General Measurement Model (GMM):"),
        tags$ol(type = "a",
          tags$li("Present value of future cash flows"),
          tags$li("Risk adjustment for non-financial risk")
        )
      ),


      div(
        h3("Treatment of Acquisition Cash Flows", class = "section-subtitle"),
        p("Entities have the option to either expense acquisition costs immediately when the coverage period is one year or less, or to defer and amortize these costs over the coverage period if it exceeds one year."),
        p("Deferring acquisition costs reduces the Liability for Remaining Coverage (LRC), but this may lead to a higher recognition of losses for onerous contracts."),
        
        h3("Discounting and Risk Adjustment", class = "section-subsubtitle"),
        p("Discounting of the LRC is not required unless there is a significant delay between receipt of premiums and the provision of insurance services."),
        p("Risk adjustment applies solely to the Liability for Incurred Claims (LIC) and not to the LRC."),
        
        h3("Onerous Contracts", class = "section-subsubtitle"),
        p("Even under PAA, entities must assess whether contracts are onerous at initial recognition or subsequently."),
        p("If a contract is deemed onerous, a loss component must be recognized immediately in profit or loss."),
        
        h3("Revenue Recognition", class = "section-subsubtitle"),
        p("Revenue is recognized over the coverage period in line with the pattern of service delivery, typically on a straight-line basis.")
      ),



    div(class = "module-section",
        h3("📋 Comparison of PAA and GMM", class = "section-subsubtitle"),
        img(src = "images/paaTable.png", class = "module-image")
    ),
  
    div(
      h3("Disclosure Requirements", class = "section-subtitle"),
      p("IFRS 17 mandates the following disclosures:"),
      tags$ol(type = "a",
        tags$li("A transparent presentation of revenue, incurred claims, and movements in insurance liabilities."),
        tags$li("Disclosure of confidence levels used in measuring liabilities."),
        tags$li("An option to present changes in discount rates through Other Comprehensive Income (OCI).")
      )
    ),

    div(
      h3("Practical Application Examples", class = "section-subtitle"),
      tags$ul(
        tags$li("Ideal for group life, group credit, and general insurance with short coverage."),
        tags$li("May also be applied to reinsurance contracts held, provided they meet the same eligibility criteria.")
      )
    ),

    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Premium Allocation Approach."),
    ),


    # Quiz Boxes for Module 9
    box(
      title = "1. Under the PAA, which of the following components is NOT included when calculating the Liability for Remaining Coverage (LRC)?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "Opening LRC balance",
        "Risk adjustment for non-financial risk",
        "Premiums received",
        "Insurance revenue recognized"
      ), selected = character(0))
    ),

    box(
      title = "2. Which contracts are generally suitable for the application of PAA?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "Contracts with coverage period exceeding 5 years",
        "Long-term life insurance contracts",
        "Short-duration contracts such as travel insurance",
        "Investment contracts without discretionary participation features"
      ), selected = character(0))
    ),

    box(
      title = "3. What happens if the PAA measurement materially differs from the General Measurement Model (GMM) and the coverage period exceeds 12 months?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "The entity must use GMM",
        "PAA remains applicable",
        "Only LIC is adjusted",
        "Deferred costs are written off"
      ), selected = character(0))
    ),

    box(
      title = "4. When discounting the LRC under PAA, what is the determining factor for applying discounting?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "If the contract is life insurance",
        "If there is a significant financing component",
        "If reinsurance contracts are involved",
        "If premiums are received quarterly"
      ), selected = character(0))
    ),

    box(
      title = "5. Acquisition costs under PAA can be treated in what way for contracts of less than 12 months coverage?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "Must always be deferred",
        "Can be expensed immediately",
        "Must be capitalized over 5 years",
        "Must be refunded to the policyholder"
      ), selected = character(0))
    ),

    box(
      title = "6. In measuring Liability for Incurred Claims (LIC) under PAA, what adjustments are included?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "Only expected claims without risk adjustment",
        "Expected claims discounted with risk adjustment",
        "Future premiums and claims without discounting",
        "Only acquisition costs"
      ), selected = character(0))
    ),

    box(
      title = "7. Which is NOT a required disclosure under IFRS 17 for entities using PAA?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "Revenue recognized",
        "Confidence level of risk adjustment",
        "Discount rate changes in OCI (if applicable)",
        "Fair value of investment properties"
      ), selected = character(0))
    ),

    box(
      title = "8. How are contracts grouped under PAA for measurement purposes?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Based on customer demographics",
        "According to similar risk characteristics and management practices",
        "By geographical region only",
        "By policyholder nationality"
      ), selected = character(0))
    ),

    box(
      title = "9. When assessing onerous contracts under PAA, which of the following is TRUE?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "The assessment is not required under PAA",
        "The test must be done both at inception and subsequently",
        "Onerous contracts can be offset by profitable ones",
        "Only required if acquisition costs are high"
      ), selected = character(0))
    ),

    box(
      title = "10. Which of the following best describes the 'Fulfilment Cash Flows' under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "Past incurred claims",
        "Future expected cash inflows and outflows, discounted and adjusted for risk",
        "Only the expected claims amount",
        "Premiums received but not yet earned"
      ), selected = character(0))
    ),
    

      actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
      br(), 
      br(),
      uiOutput(ns("result")),  

    div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_10"),
          label = tagList(icon("arrow-right"), "Next: Module 10 - Reinsurance Contracts Held"),
          class = "control-button-tavnav"
      )
    ) 
    )
}

# Correct answers and explanations for Module 9
correct_answers_module9 <- list(
  q1 = list(
    answer      = "Risk adjustment for non-financial risk",
    explanation = "Under PAA, risk adjustment applies only to the Liability for Incurred Claims (LIC), not to the Liability for Remaining Coverage (LRC)."
  ),
  q2 = list(
    answer      = "Short-duration contracts such as travel insurance",
    explanation = "The PAA is designed for short-duration insurance contracts, typically 12 months or less (e.g. motor, travel, health)."
  ),
  q3 = list(
    answer      = "The entity must use GMM",
    explanation = "If PAA measurement materially differs from GMM for contracts longer than 12 months, the General Measurement Model must be used."
  ),
  q4 = list(
    answer      = "If there is a significant financing component",
    explanation = "Discounting of the LRC is required only when there is a significant financing component (i.e. material time difference between premium receipt and service delivery)."
  ),
  q5 = list(
    answer      = "Can be expensed immediately",
    explanation = "For contracts with coverage of one year or less, acquisition costs may be expensed immediately or deferred at the insurer’s discretion."
  ),
  q6 = list(
    answer      = "Expected claims discounted with risk adjustment",
    explanation = "LIC under PAA includes the present value of expected future cash flows plus a risk adjustment for non-financial risk."
  ),
  q7 = list(
    answer      = "Fair value of investment properties",
    explanation = "IFRS 17 disclosures focus on insurance liabilities; it does not require disclosure of investment property fair values in the context of PAA."
  ),
  q8 = list(
    answer      = "According to similar risk characteristics and management practices",
    explanation = "Contracts must be grouped into portfolios based on similar risk characteristics and managed under the same strategy."
  ),
  q9 = list(
    answer      = "The test must be done both at inception and subsequently",
    explanation = "IFRS 17 requires entities to assess whether contracts are onerous both at initial recognition and on an ongoing basis."
  ),
  q10 = list(
    answer      = "Future expected cash inflows and outflows, discounted and adjusted for risk",
    explanation = "Fulfilment Cash Flows include future cash flows (inflows and outflows) discounted to present value and adjusted for non-financial risk."
  )
)



IFRS17Module9Server <- (function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Add a reactive value to track when progress is saved
    progress_saved_trigger <- reactiveVal(0)

    score <- reactiveVal(0)

    # Feedback functions
    observeEvent(input$submit, {
        removeModal()
        # 1. List out all your question input IDs
        question_ids <- paste0("q", 1:10)
        
        # 2. Find which ones are missing
        missing <- vapply(question_ids, function(qid) {
          val <- input[[qid]]
          is.null(val) || length(val) == 0 || val == ""
        }, logical(1))
        
        # 3. If any are missing, show a modal and abort
        if (any(missing)) {
          showModal(modalDialog(
            title   = "Please answer all questions",
            HTML(sprintf(
              "You have %d unanswered question(s): %s.<br><br>Please go back and select your answers before submitting.",
              sum(missing),
              paste(which(missing), collapse = ", ")
            )),
            easyClose = TRUE,
            footer    = modalButton("OK")
          ))
          return()
        }
        
        # ———————————
        # 5. All answered: clear any existing modal, then run your scoring code
        removeModal()

        score(0)
        feedback <- list()

    for (qid in names(correct_answers_module9)) {
      correct <- correct_answers_module9[[qid]]$answer
      explanation <- correct_answers_module9[[qid]]$explanation
      user_answer <- input[[qid]]

      if (is.null(user_answer)) {
        feedback[[qid]] <- paste0("⚠️ ", toupper(qid), ": No response recorded.")
        feedbackDanger(qid, "No answer selected.")
        next
      }

      if (user_answer == correct) {
        score(score() + 1)
        feedback[[qid]] <- paste0("✅ ", toupper(qid), ": Correct!")
        feedbackSuccess(qid, "Correct!")
      } else {
        feedback[[qid]] <- paste0("❌ ", toupper(qid), ": Your answer was incorrect. The correct answer is '", correct, "'. Explanation: ", explanation)
        feedbackDanger(qid, paste0("Incorrect! Correct answer is: ", correct, ". ", explanation))
      }
    }

      # ========== NEW PROGRESS SAVING SECTION ==========
      # Save progress for Module 9
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 2 specific calculations
        total_questions <- length(correct_answers_module9)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module9",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 9 Progress Saved!</strong><br>",
                "Score: ", final_score, "/", total_questions, " (", final_percentage, "%)<br>",
                if(final_percentage >= 70) "Great job!" else "Keep practicing!"
              )),
              type = "message",
              duration = 5
            )

            # Trigger progress update
            progress_saved_trigger(progress_saved_trigger() + 1)

          } else {
            showNotification(
              "⚠️ Could not save progress. Please check your connection.",
              type = "warning",
              duration = 4
            )
          }
        }, error = function(e) {
          showNotification(
            "❌ Error saving progress. Please contact support if this persists.",
            type = "error",
            duration = 5
          )
          print(paste("Module 9 progress save error:", e$message))
        })
      } else if (isTRUE(user_data$is_guest)) {
        # Guest mode notification
        showNotification(
          HTML("<strong>ℹ️ Guest Mode</strong><br>
                Your progress is not being saved.<br>
                <a href='#' onclick='location.reload();' style='color: #fff; text-decoration: underline;'>
                Click here to sign up</a>"),
          type = "message",
          duration = 6
        )
      }
      # ========== END PROGRESS SAVING SECTION ========== 


    valid_ids <- paste0("q", 1:10)
    feedback <- lapply(valid_ids, function(qid) {
      if (!is.null(feedback[[qid]])) {
        feedback[[qid]]
      } else {
        paste0("⚠️ ", toupper(qid), ": No response recorded.")
      }
    })
    names(feedback) <- valid_ids

    output$result <- renderUI({
      total_questions <- length(correct_answers_module9)
      percentage       <- round((score() / total_questions) * 100, 1)
      color            <- if (percentage >= 70) "#198754" else "#dc3545"

      tagList(
        div(
          class = "print-area",

          # ——— Certificate Header ———
          div(
            class = "print-title",
            style = "
              text-align:center; 
              padding: 20px;
              border-bottom: 3px solid #0d6efd;
              background-color: #f8f9fa;
              margin-bottom:20px;",
            img(src = "images/ira_logo_.png", style = "height:60px; margin-bottom:10px;"),
            # certificate title
            h1("Certificate of Achievement",
              style = "
                font-family: 'Nunito', sans-serif;
                font-size: 32px;
                margin-bottom: 5px;
                color: #0d6efd;
              "),
                # decorative subtitle
            h4("has successfully completed the IFRS 17 - Combination & Separation of Insurance Contracts",
              style = "
                font-family: 'Nunito', sans-serif;
                font-weight: 400;
                font-style: italic;
                margin-top: 0;
                margin-bottom: 20px;
                color: #343a40;
              "),
            p(format(Sys.Date(), "%B %d, %Y"), 
            style = "
            font-size:14px;
            margin-top: 10px;
            color: #6c757d;
            ")
          ),  # ← comma!

          # ——— Results Summary Card ———
          # ——— Results Summary Card ———
          div(
            class = "print-summary",
            style = "
              background-color: #006AA6;
              padding: 25px;
              border-radius: 10px;
              box-shadow: 0 4px 12px rgba(0,0,0,0.08);
              font-family: Arial, sans-serif;
            ",
            h3(
              "📊 Module 9 Results Summary",
              style = "color:#f5f5f5; font-weight:600; margin-bottom:20px;"
            ),

            HTML(paste0(
              "<hr style='border-top:1px solid #f5f5f5;'>",
              "<p style='font-size:18px; color:#f5f5f5;'><strong>Total Score:</strong> ", score(), " / ", total_questions, "</p>",
              "<p style='font-size:18px; color:#f5f5f5;'><strong>Percentage Score:</strong> <span style='color:#ffffff; font-weight:600;'>", percentage, "%</span></p>"
            )),

            # ——— Detailed Feedback ———
            div(
              style = "margin-top:25px;",
              h4(
                "📘 Detailed Feedback",
                style = "margin-bottom:15px; color:#fff;"
              ),
              tags$ul(
                lapply(feedback, function(msg) {
                  tags$li(style = "margin-bottom:8px; color:#f5f5f5;", HTML(msg))
                })
              )
            )
          )  

        )  

      )

    })  
    })

    # create a reactive for the “Next” click
    to_module_10 <- reactive(input$to_module_10)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_10,
      progress_trigger = progress_saved_trigger
    ))
  })
})