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
        h3("📖 Module Objective", class = "section-subheading"),
        p("This module focuses on insurance finance income or expenses as defined under IFRS 17 paragraphs 87 to 92.")    
    ), 

    # Definition section
    div(class = "module-section",
        h3("Definition", class = "section-subheading"),
        p("Insurance finance income or expenses refer to changes in the carrying amount of insurance contract liabilities (or assets) due to:"),
        tags$ol(type = "a",
            tags$li(
              tags$b("The Time Value of Money (Discounting) –"), 
              "This represents the accretion of interest on insurance contract liabilities, as future cash flows are discounted to their present value. As time progresses, this discount unwinds, impacting the financial results."
            ),
            tags$li(
              tags$b("Changes in Financial Assumptions –"), 
              "This includes the impact of movements in key financial variables, such as:",
              tags$ol(type = "i",
                  tags$li(
                    tags$b("Interest Rates:"), 
                    "Fluctuations in market interest rates directly affect the present value of future cash flows."
                  ),
                  tags$li(
                    tags$b("Inflation:"), 
                    "If inflation is a factor in future cash flows, changes in inflation assumptions will also be reflected here."
                  )
              )
            )
        )
    ),
    div(class = "module-section",
        h3("What’s Excluded from Insurance Finance Income or Expenses?", class = "section-subheading"),
        p("Not all changes in financial assumptions are classified as insurance finance income or expenses. In particular, changes are excluded when:"),
        tags$ol(type = "a",
            tags$li("The contract possesses direct participating features (meaning policyholders share directly in the returns of underlying items held by the insurer)."),
            tags$li("The change would ordinarily adjust the Contractual Service Margin (CSM), but specific IFRS 17 rules prevent this adjustment.")
        ),
        p("In such specific scenarios, these changes are instead recognized as insurance service expenses to ensure appropriate matching.")
    ),

    # A. Non-Participating Contracts
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

    # C. Foreign Currency Translation
    div(class = "module-section",
        h4("Foreign Currency Translation", class = "section-subheading"),
        p("Insurance contracts are treated as ", tags$b("monetary items"), " under IAS 21 (The Effects of Changes in Foreign Exchange Rates)."),
        p("Any exchange differences (due to currency changes) are recognized in P&L, unless they relate to amounts already in OCI.")
    ),


    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Insurance Finance Income"),
    ),

    box(
      title = "1. Insurance finance income or expenses refer to changes in the carrying amount of insurance contract liabilities (or assets) due to which two main factors?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "Premium collection and claims payment.",
        "Time Value of Money (Discounting) and Changes in Financial Assumptions",
        "Investment performance and operational efficiency",
        "Acquisition costs and contract renewals"
      ), selected = character(0))
    ),

    # 2
    box(
      title = "2. A change in market interest rates affects insurance finance income by?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "Modifying service revenue",
        "Adjusting the CSM",
        "Changing the present value of future cash flows",
        "Triggering immediate premium refunds"
      ), selected = character(0))
    ),

    # 3
    box(
      title = "3. When changes in financial assumptions are excluded from IFIE because the contract has direct participating features and IFRS 17 rules prevent CSM adjustment, these changes are recognized in which part of the financial statements?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Directly in equity",
        "In a separate reserve account",
        "As part of investment income",
        "As insurance service expenses"
      ), selected = character(0))
    ),

    # 4
    box(
      title = "4. For non-participating contracts, if an entity chooses to split IFIE between P&L and OCI, the portion recognized in OCI is specifically designed to mitigate volatility arising from which factors?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "Unexpected claims events",
        "Significant fluctuations in current market discount rates",
        "Changes in policyholder lapse rates",
        "Administrative cost overruns"
      ), selected = character(0))
    ),

    # 5
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

    # 6
    box(
      title = "6. According to IAS 21, insurance contracts are classified as?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "Non-monetary items",
        "Monetary items",
        "Equity instruments",
        "Derivatives"
      ), selected = character(0))
    ),

    # 7
    box(
      title = "7. Which of the following best illustrates the time value of money in the context of IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "Claims development",
        "Risk adjustment for non-financial risk",
        "Accretion of interest on discounted future cash flows",
        "Amortization of acquisition costs"
      ), selected = character(0))
    ),

    # 8
    box(
      title = "8. Under IFRS 17, how should an entity handle IFIE if the underlying items are measured at fair value through OCI?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Present IFIE fully in equity",
        "Present IFIE fully in P&L",
        "Present IFIE in OCI to match the measurement of the underlying items",
        "Do not recognize IFIE"
      ), selected = character(0))
    ),

    # 9
    box(
      title = "9. What is the effect on the profit or loss statement when a group of contracts with OCI-disaggregated IFIE is derecognized?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "P&L remains unaffected by OCI balances",
        "P&L shows a spike in IFIE",
        "P&L includes cumulative OCI",
        "P&L shows a reversal of finance expenses"
      ), selected = character(0))
    ),

    # 10
    box(
      title = "10. If an entity is exposed to equity risk, how would this risk typically manifest within IFIE?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "Through changes in policyholder behavior",
        "Through fluctuations in foreign exchange rates",
        "Through unexpected increases in operational expenses",
        "Through its impact on discount rates, which affect the present value of liabilities"
      ), selected = character(0))
    ),

    # 11
    box(
      title = "11. Which of the following is not a valid reason for choosing to disaggregate IFIE between P&L and OCI?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q11"), label = NULL, choices = c(
        "To eliminate the need for tracking changes in financial assumptions",
        "To improve the comparability of financial statements over time",
        "To reflect the impact of market changes in OCI instead of P&L",
        "To match the reporting of liabilities with asset valuation"
      ), selected = character(0))
    ),

    # 12
    box(
      title = "12. Which of the following contract types is most directly affected by the accounting policy choice in IFRS 17 paragraph 88?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q12"), label = NULL, choices = c(
        "Contracts with direct participation features",
        "Non-participating insurance contracts",
        "Reinsurance held",
        "Investment contracts without Direct Participation Features"
      ), selected = character(0))
    ),

    # 13
    box(
      title = "13. If an entity chooses to recognize all IFIE in P&L, why is the impact on OCI considered neutral?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q13"), label = NULL, choices = c(
        "Because all recognition of IFIE occurs within the P&L, leaving no component to affect OCI",
        "Because OCI is only affected by non-cash items",
        "Because IFIE are always considered immaterial for OCI",
        "Because the impact is offset by other comprehensive income items"
      ), selected = character(0))
    ),

    # 14
    box(
      title = "14. Why is disaggregation between profit or loss and OCI particularly relevant for contracts with direct participation features?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q14"), label = NULL, choices = c(
        "To increase reported profit margins",
        "To reflect fair value changes in liabilities linked to market movements",
        "To simplify accounting for non-financial risks",
        "Because they are exempt from IFRS 17 disclosures"
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
    answer    = "Time Value of Money (Discounting) and Changes in Financial Assumptions",
    explanation = "Insurance finance income or expenses reflect the impact of discounting (time value of money) and changes in financial assumptions like interest rates or inflation."
  ),
  q2 = list(
    answer    = "Changing the present value of future cash flows",
    explanation = "Interest rate movements impact the discounting of expected future cash flows, altering the carrying amount."
  ),
  q3 = list(
    answer    = "As insurance service expenses",
    explanation = "When direct participation features prevent CSM adjustment, those changes are recognized as insurance service expenses to ensure appropriate matching."
  ),
  q4 = list(
    answer    = "Significant fluctuations in current market discount rates",
    explanation = "The OCI portion is designed to absorb volatility arising from large changes in market discount rates."
  ),
  q5 = list(
    answer    = "They are reclassified to P&L if Option 2 under paragraph 88 was applied",
    explanation = "Paragraph 91 requires that OCI balances from a split under paragraph 88(b) be recycled into profit or loss upon derecognition."
  ),
  q6 = list(
    answer    = "Monetary items",
    explanation = "IAS 21 treats insurance contracts as monetary items; exchange differences go to P&L unless they pertain to amounts already in OCI."
  ),
  q7 = list(
    answer    = "Accretion of interest on discounted future cash flows",
    explanation = "Accretion of interest represents the unwinding of the discount and exemplifies the time value of money under IFRS 17."
  ),
  q8 = list(
    answer    = "Present IFIE in OCI to match the measurement of the underlying items",
    explanation = "When underlying assets are FVOCI, presenting IFIE in OCI avoids mismatches and aligns with asset measurement."
  ),
  q9 = list(
    answer    = "P&L remains unaffected by OCI balances",
    explanation = "If IFIE was split and OCI held under paragraph 89(b), those OCI amounts are not recycled, so P&L is unaffected on derecognition."
  ),
  q10 = list(
    answer    = "Through its impact on discount rates, which affect the present value of liabilities",
    explanation = "Equity risk can influence the discount rates used to measure liabilities, impacting IFIE."
  ),
  q11 = list(
    answer    = "To eliminate the need for tracking changes in financial assumptions",
    explanation = "This is incorrect—entities must still track all changes in financial assumptions even if they split IFIE."
  ),
  q12 = list(
    answer    = "Non-participating insurance contracts",
    explanation = "Paragraph 88 gives a presentation policy choice specifically for non-participating contracts."
  ),
  q13 = list(
    answer    = "Because all recognition of IFIE occurs within the P&L, leaving no component to affect OCI",
    explanation = "If IFIE is wholly recognized in P&L, there is by design no resulting OCI impact."
  ),
  q14 = list(
    answer    = "To reflect fair value changes in liabilities linked to market movements",
    explanation = "For direct participation contracts, disaggregation allows liabilities to move in OCI alongside asset fair value changes, avoiding profit volatility."
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