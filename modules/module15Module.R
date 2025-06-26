# This module is for the IFRS 17 module 16
IFRS17Module15UI <- function(id) {
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
      h2("📘 Module 15: Insurance Finance Income or Expenses", class = "section-title-top")
    ),

    div(class = "module-section",
        h3("📖 Introduction"),
        p("This module provides an overview of insurance finance income or expenses as defined under IFRS 17 paragraphs 87 to 92.")    
    ), 

    # Definition section
    div(class = "module-section",
        h3("Definition"),
        p("Insurance finance income or expenses reflect how the value of insurance contract liabilities changes over time due to:"),
        tags$ol( type = "a",
            tags$li("Time value of money – the effect of interest over time."),
            tags$li("Financial risks – changes in interest rates or market movements.")
        ),
        p("These changes affect how much insurers expect to pay in the future.")
    ),
    
    # What's not included section
    div(class = "module-section",
        h3("What's not included?"),
        p("Some changes in assumptions (like interest rates) are not treated as finance income or expense if:"),
        tags$ol( type = "a",
            tags$li("The contract has direct participating features, and"),
            tags$li("The change would normally adjust the Contractual Service Margin (CSM), but IFRS 17 rules prevent that.")
        ),
        p("These are instead reported as insurance service expenses.")
    ),

    div(class = "module-section",
        h3("💡 How to Present Insurance Finance Income or Expense"),
        
        p("Companies have ", strong("two main options"), " for showing these amounts in their financial statements, depending on the type of insurance contract:"),
        
        tags$h4("A. Non-Participating Contracts (no direct link to underlying assets):"),
        tags$ol( type = "a",
            tags$li("Option 1 – Include all insurance finance income or expenses for the period entirely in Profit or Loss (P&L)"),
            tags$li(HTML("Option 2 – Split Between P&amp;L and OCI:"),
                    tags$ol(type = "i",
                        tags$li(HTML("In P&amp;L: Show a steady, expected amount based on a fixed (&ldquo;locked-in&rdquo;) discount rate from when the contract began.")),
                        tags$li("In OCI: Put the difference between actual changes and the expected amount.")
                    )
            )
        ),
        
        tags$h4("B. Participating Contracts (with underlying assets the insurer holds):"),
        tags$ol( type = "a",
            tags$li("Option 1 – Include all insurance finance income or expenses for the period entirely in Profit or Loss (P&L). All finance income/expenses go in the income statement."),
            tags$li(HTML("Option 2 – Match Underlying Items:"),
                    tags$ol(type = "i",
                        tags$li("In P&L: Show an amount that matches the investment returns on the underlying items (to avoid mismatches)."),
                        tags$li("In OCI: The rest of the financial result (the part that doesn’t match the assets).")
                    )
            )
        )
    ),

    div(class = "module-section",
        h3("🌍 Foreign Currency Translation"),
        
        div(class = "currency-translation",
            tags$h4(style = "color:#006AA6;", "Foreign Currency Translation"),
            p("Insurance contracts are treated as ", strong("monetary items"), " under IAS 21 (The Effects of Changes in Foreign Exchange Rates)."),
            p("Any exchange differences (due to currency changes) go to P&L, unless they relate to amounts already in OCI.")
        )
    ),


    div(class = "module-section",
      h3("📊 Option 1"),
      img(src = "images/module_15_option_1.png", 
          alt = "IFRS 17 Module 15 Option 1",
          class = "module-image"
      )
    ),

    div(class = "module-section",
      h3("📊 Option 2"),
      img(src = "images/module_15_option_2.png", 
          alt = "IFRS 17 Module 15 Option 2",
          class = "module-image"
      ),
      p(tags$strong("Exchange differences on insurance contract liabilities (P&L portion)"), 
      " – Arise when converting foreign currency balances into the reporting currency."),
    
      p(tags$strong("Exchange differences on retranslation of foreign operations"), 
      " – Arise when translating the financial results of foreign subsidiaries into the group's reporting currency.")
    ),   

    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Insurance Finance Income"),
    ),

    box(
      title = "1. What are Insurance Finance Income or Expenses (IFIE)?",
      status = NULL , solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "Premiums and claims",
        "Acquisition costs and investment income",
        "Changes in non-financial assumptions",
        "Time value of money and financial risk impacts"
      ), selected = character(0))
    ),

    box(
      title = "2. How can IFIE be presented in the statement of financial performance?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "Either fully in P&L or disaggregated between P&L and OCI",
        "Only in Profit or Loss (P&L)",
        "Only in Other Comprehensive Income (OCI)",
        "Only in the notes to the financial statements"
      ), selected = character(0))
    ),

    box(
      title = "3. Are there any exceptions to the general treatment of IFIE?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "No exceptions",
        "Yes, for reinsurance contracts",
        "Yes, for insurance contracts with direct participation features and certain assumptions that would adjust CSM but don’t",
        "Yes, if the policyholder is a related party"
      ), selected = character(0))
    ),

    box(
      title = "4. If the entity chooses to disaggregate IFIE between P&L and OCI, how should the disaggregation be made?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "Based on actual market returns",
        "Using a locked-in discount rate to allocate a portion to P&L",
        "Arbitrarily",
        "Based on revenue recognition patterns"
      ), selected = character(0))
    ),

    box(
      title = "5. How should IFIE recorded in OCI be treated when a group of insurance contracts is transferred or derecognized (per paragraph 91)?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "They are reversed in the next period",
        "They are transferred to equity",
        "They are reclassified to P&L if Option 2 under paragraph 88 was applied",
        "They remain in OCI permanently"
      ), selected = character(0))
    ),

    box(
      title = "6. How should exchange differences on changes in the carrying amount of groups of insurance contracts be treated?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "Always in OCI",
        "Always in equity",
        "Not recognized",
        "In P&L unless they relate to OCI-recorded IFIE, in which case they go to OCI"
      ), selected = character(0))
    ),

    box(
      title = "7. Which component is typically included in the effect of the time value of money under IFIE?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "Expected claims",
        "Acquisition costs",
        "Interest accretion on insurance liabilities",
        "Reinsurance asset recoveries"
      ), selected = character(0))
    ),

    box(
      title = "8. Which of the following best describes the treatment of IFIE for contracts with direct participation features?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Must always be presented in OCI",
        "May be presented to eliminate mismatches with underlying items",
        "Not applicable to participating contracts",
        "Must always be presented in P&L"
      ), selected = character(0))
    ),

    box(
      title = "9. If a group of contracts is derecognized and IFIE has been disaggregated under paragraph 89(b), what happens to amounts in OCI?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "Remain in OCI",
        "Transferred to P&L",
        "Reversed",
        "Transferred to retained earnings"
      ), selected = character(0))
    ),

    box(
      title = "10. What type of financial risk would be reflected in IFIE?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "Lapse risk",
        "Currency risk",
        "Inflation risk",
        "Equity or interest rate risk impacting discount rates"
      ), selected = character(0))
    ),

    box(
      title = "11. Why might an entity choose to disaggregate IFIE between P&L and OCI?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q11"), label = NULL, choices = c(
        "To smooth earnings volatility",
        "To avoid recognizing claims",
        "To increase policyholder bonuses",
        "To reduce insurance liabilities"
      ), selected = character(0))
    ),

    box(
      title = "12. Which paragraph of IFRS 17 allows IFIE disaggregation for non-participating contracts?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q12"), label = NULL, choices = c(
        "Paragraph 30",
        "Paragraph 88",
        "Paragraph 45",
        "Paragraph 135"
      ), selected = character(0))
    ),

    box(
      title = "13. When an entity opts to recognize all IFIE in P&L, the impact on OCI is",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q13"), label = NULL, choices = c(
        "Neutral (no impact)",
        "Positive",
        "Negative",
        "Deferred to future periods"
      ), selected = character(0))
    ),

    box(
      title = "14. Which type of insurance contract is most likely to involve disaggregation based on underlying item performance?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q14"), label = NULL, choices = c(
        "Group term life insurance",
        "Non-participating whole life",
        "Universal life insurance with direct participation features",
        "Reinsurance contracts held"
      ), selected = character(0))
    ),




    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result"))


    # div(
    #   class = "quiz-nav",
    #   actionButton(
    #       ns("to_case_studies"),
    #       label = tagList(icon("arrow-right"), "Next: Case Studies"),
    #       class = "control-button-tavnav"
    #   )
    # ) 
    )
}


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
    answer = "Yes, for insurance contracts with direct participation features and certain assumptions that would adjust CSM but don’t",
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


IFRS17Module15Server <- (function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Add a reactive value to track when progress is saved
    progress_saved_trigger <- reactiveVal(0)

    score <- reactiveVal(0)

    # Feedback functions
    observeEvent(input$submit, {
        removeModal()
        # 1. List out all your question input IDs
        question_ids <- paste0("q", 1:14)
        
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

    for (qid in names(correct_answers_module15)) {
      correct <- correct_answers_module15[[qid]]$answer
      explanation <- correct_answers_module15[[qid]]$explanation
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
      # Save progress for Module 14
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 14 specific calculations
        total_questions <- length(correct_answers_module15)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module15",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 15 Progress Saved!</strong><br>",
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
          print(paste("Module 15 progress save error:", e$message))
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


    valid_ids <- paste0("q", 1:14)
    feedback <- lapply(valid_ids, function(qid) {
      if (!is.null(feedback[[qid]])) {
        feedback[[qid]]
      } else {
        paste0("⚠️ ", toupper(qid), ": No response recorded.")
      }
    })
    names(feedback) <- valid_ids

    output$result <- renderUI({
      total_questions <- length(correct_answers_module15)
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
              # recipient name
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
              "📊 Module 15 Results Summary",
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

    return(list(
      progress_trigger = progress_saved_trigger
    ))
  })
})