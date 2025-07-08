# This module is for the IFRS 17 module 7
IFRS17Module6UI <- function(id) {
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
      h2("📘 Module 6: Subsequent Measurement", class = "section-title-top")
    ),

    div(class = "module-section",  
        h3(icon("balance-scale"), "Module Objective", class = "section-subheading"),
        p("This module outlines the requirements for subsequent measurement of insurance contracts, as detailed in paragraphs 40–52 of IFRS 17."),
        p("It explains how insurers revise the carrying amount of insurance contracts over time, including updates to fulfilment cash flows, changes to the Contractual Service Margin (CSM), and the recognition of insurance-related revenue and expenses.")
    ),

    div(class = "module-section",
        h3("Overview of Subsequent Measurement", class = "section-subheading"),
        p("After initial recognition, insurance contract liabilities must be updated to reflect:"),
        tags$ol(type = "a",
          tags$li("Changes in estimates of future cash flows"),
          tags$li("Release of CSM based on coverage units"),
          tags$li("Changes in discount rates"),
          tags$li("Claims incurred and paid"),
          tags$li("Experience adjustments and risk changes"),
          tags$li("Amortization of insurance acquisition cash flows")
        ),
    ),

    div(class = "module-section",
        h3("Liability for Remaining Coverage (LRC)", class = "section-subheading"),
        p("The LRC reflects the insurer’s obligation to provide coverage in the future. At each reporting date, it is updated for:"),
        tags$ol(type = "a",
          tags$li("Premiums received"),
          tags$li("Release of CSM as services are rendered"),
          tags$li("Changes in fulfilment cash flows relating to future service"),
          tags$li("Adjustments to risk adjustment")
        ),
        p(em("Note: The CSM is adjusted only for changes that relate to future service.")),
        p("The GMM and VFA both measure insurance contract liabilities based on fulfilment cash flows and a contractual service margin; however, VFA applies to contracts with direct participation features and adjusts the CSM to reflect changes in the insurer’s share of underlying items ")
    ),
    div(class = "module-section",
        h3(icon("balance-scale"), "General Measurement Model (GMM)", class = "section-subheading"),
        p("The General Measurement Model (GMM) and the Variable Fee Approach (VFA) both determine insurance contract liabilities using fulfilment cash flows and a Contractual Service Margin (CSM). However, the VFA is specifically designed for contracts with direct participation features and modifies the CSM to capture changes in the insurer’s share of the underlying items."),
        p("The LRC will be adjusted as follows:"),
        tags$ol(type = "a",
          tags$li("Opening LRC balance: Starting point for the period."),
          tags$li("Changes due to claims and expenses"),
          tags$li("Time value of money and financial risks"),
          tags$li("Risk adjustment for non-financial risk"),
          tags$li("Contractual Service Margin (CSM)"),
          tags$li("Loss Component: Optional adjustments for losses in the loss component.")
        ),
    ),
    div(class = "module-section",
        h3("Adjusting the Contractual Service Margin (CSM)", class = "section-subheading"),
        p("The CSM is adjusted for:"),
        tags$ol(type = "a",
          tags$li("Changes in fulfilment cash flows related to future service"),
          tags$li("Accretion of interest using the locked-in rate"),
          tags$li("Release to profit or loss based on coverage units")
        ),
        p("It is not adjusted for:"),
        tags$ol(type = "a",
          tags$li("Changes related to past or current service"),
          tags$li("Experience variances from prior periods")
        ),
        div(class = "info-box scenario-box",
          div(class = "scenario-calc-row",
            # Left column
            div(class = "scenario-col",
              tags$h4("📌 Illustration:"),
              tags$ol( type = "a",
                tags$li("Opening CSM: BWP 200,000"),
                tags$li("Interest accretion: BWP 8,000"),
                tags$li("Adjustment from change in future cash flows: BWP -25,000"),
                tags$li("Release based on service provided: BWP 30,000")
              )
            ),
            # Right column
            div(class = "calc-col",
              tags$h4("🧮 Calculation:"),
              tags$ul( type = "a",
                tags$li("Adjusted CSM = BWP 200,000 + BWP 8,000 - BWP 30,000 = BWP 153,000"),
                tags$li("If the adjustments for changes in expected future cash flows related to future coverage exceed the available CSM (after accounting for interest accretion), the excess is treated as a loss and is immediately recognized in the Profit or Loss statement.")
              )
            )
          )
        )
      ),
      div(class = "module-section",
          h3(icon("exclamation-triangle"), "Treatment of Onerous Contracts", class = "section-subheading"),
          p("Under IFRS 17, a contract becomes onerous when the updated fulfilment cash flows (FCF) exceed the carrying amount of the liability."),
          tags$ol(type = "a",
            tags$li(
              "In subsequent measurement, an entity must:",
              tags$ol(type = "i",
                tags$li("Reduce the CSM to zero if it is insufficient to absorb unfavourable changes in future cash flows."),
                tags$li("Recognise the excess as a loss in the profit or loss statement."),
                tags$li("Establish or increase the loss component of the Liability for Remaining Coverage (LRC).")
              )
            ),
            tags$li(
              "The loss component ensures that future revenue is correctly allocated and tracked against the loss already recognised."
            ),
            tags$li(
              "Once a group of contracts is classified as onerous, this classification remains fixed even if future estimates improve."
            ),
            tags$li(
              "If the expected cash flows improve in subsequent periods:",
              tags$ol(type = "i",
                tags$li("No new CSM is created."),
                tags$li("The loss component is reversed through profit or loss to reflect the improvement.")
              )
            )
          )
      ),
    div(class = "module-section",   
        h3(icon("coins"), "Premium Allocation Approach (PAA)", class = "section-subheading"),   
        p("For contracts measured under the PAA, an entity shall measure the Liability for Remaining Coverage at the end of each subsequent reporting period as follows:"),
        tags$ol( type = "a",
          tags$li("Opening LRC balance"),
          tags$li("Add: Premium Received in the period"),
          tags$li("Less: Insurance acquisition cash flows"),
          tags$li("Add: any amounts relating to the amortization of insurance acquisition cash flows recognised as an expense in the reporting period."),
          tags$li("Add: any adjustment to a financing component."),
          tags$li("Less: Insurance Revenue recognised for coverage provided during the period.")
        ),
        img(src = "images/subsequent_lrc.png", class = "module-image"),
    ),
    div(class = "module-section",
        h3("Liability for Incurred Claims (LIC)", class = "section-subheading"),
        p("The LIC reflects the insurer’s obligation for claims arising from past coverage that have been incurred but not yet paid. Updates to LIC include:"),
        tags$ol( type = "a",
          tags$li("Claims incurred"),
          tags$li("Changes in estimates for reported and unreported claims (e.g., IBNR, OCR)"),
          tags$li("Application of discounting if payment is expected more than 12 months after the reporting date"),
          tags$li("Risk adjustment for non-financial risk"),
          tags$li("Unallocated Loss Adjustment Expenses (ULAE): These are internal costs (not directly linked to individual claims but expected to be incurred in settling claims. ULAE must be estimated, included in the fulfilment cash flows, and discounted if appropriate")
        ),
        img(src = "images/subsequent_lic.png", class = "module-image")
      ),

    div(class = "module-section summary-box",
        h3("🔍 Key Takeaways", class = "section-subheading"),
        tags$ul(
          tags$li("Subsequent measurement ensures that insurance liabilities reflect up-to-date expectations regarding future services and obligations already incurred."),
          tags$li("The Liability for Remaining Coverage (LRC) and the Liability for Incurred Claims (LIC) are regularly updated as new data and experience emerge."),
          tags$li("The Contractual Service Margin (CSM) is essential for deferring and allocating profit recognition over the duration of insurance coverage."),
          tags$li("d)	Ongoing assessments are performed to identify onerous contracts, which can shift between profitable and loss-making over time; however, their classification remains fixed from initial recognition."),
          tags$li("e)	Unallocated Loss Adjustment Expenses (ULAE) must be included in fulfilment cash flows for both claims incurred and the LRC, as they are part of the expected claim-related outflows and should be estimated using robust actuarial methods.")
        )
    ),

    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Subsequent Measurement.", class = "section-subheading"),
    ),


    box(
      title = "1. An insurer initially recognized a group of health insurance contracts with projected cash outflows of BWP 1,200. Six months later, actual claims experience and revised assumptions suggest the outflows will increase to BWP 1,350. What IFRS 17 process does this update represent?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "Premium verification",
        "Subsequent measurement of insurance liabilities",
        "Recognition of new contracts",
        "Adjustment of reinsurance assets"
      ), selected = character(0))
    ),

    box(
      title = "2. A Botswana-based insurer issues a 3-year agricultural insurance contract. Based on expected seasonal risk patterns, the insurer determines that 50% of insurance services will be provided in year 1, 30% in year 2, and 20% in year 3. The Contractual Service Margin (CSM) at initial recognition is BWP 6,000. Assuming no changes in estimates or modifications, how much CSM revenue should be recognized each year?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "BWP 3,000 in year 1, BWP 1,800 in year 2, BWP 1,200 in year 3",
        "BWP 2,000 per year for 3 years",
        "BWP 6,000 immediately at inception",
        "No CSM revenue is recognized under IFRS 17"
      ), selected = character(0))
    ),

    box(
      title = "3. Under IFRS 17, when must an entity revise its estimates of fulfilment cash flows?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Only when there is a change in accounting policy",
        "On a fixed annual schedule",
        "At each financial reporting date",
        "Only during contract inception and termination"
      ), selected = character(0))
    ),

    box(
      title = "4. When an insurer settles a claim for a loss event that has already occurred, how is this transaction reflected under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "As a reduction of the Contractual Service Margin (CSM)",
        "Through Other Comprehensive Income (OCI)",
        "As an adjustment to the fulfilment cash flows for future coverage",
        "As an expense in the profit or loss statement"
      ), selected = character(0))
    ),

    box(
      title = "5. Which is a cause of change in risk adjustment?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "Change in interest rates",
        "Increase in past claims",
        "Changes in uncertainty of future service",
        "Movement in capital reserves"
      ), selected = character(0))
    ),

    box(
      title = "6. Which changes are excluded from adjusting the CSM?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "Future service estimates",
        "Time value updates",
        "Risk of lapses",
        "Policyholder behavior assumptions"
      ), selected = character(0))
    ),

    box(
      title = "7. What component directly impacts the measurement of the Liability for Incurred Claims (LIC) under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "Premiums for future coverage",
        "Reinsurance commission income",
        "Claims that have been incurred but not yet settled",
        "Expected future profit from the contract"
      ), selected = character(0))
    ),

    box(
      title = "8. At initial recognition, a Botswana insurer issues a 2-year home insurance contract. The expected future claims and expenses (fulfilment cash flows) are estimated at BWP 30,000, and the Contractual Service Margin (CSM) is calculated to be BWP 10,000. What is the amount of the Liability for Remaining Coverage (LRC) to be reported on the balance sheet?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "BWP 30,000 only (just the fulfilment cash flows)",
        "BWP 40,000 (BWP 30,000 FCF + BWP 10,000 CSM)",
        "BWP 10,000 only (just the CSM)",
        "BWP 0 (no liability until claims arise)"
      ), selected = character(0))
    ),

    box(
      title = "9. What does LIC capture?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "Claims that may occur in the future",
        "Earned premiums",
        "Deferred acquisition cost",
        "Claims already incurred"
      ), selected = character(0))
    ),

    box(
      title = "10. In the context of IFRS 17, how does the risk adjustment for non-financial risk influence the valuation of insurance contract liabilities during subsequent measurement?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "It directly reduces expected future cash inflows",
        "It creates a deferred tax asset for timing differences",
        "It represents the compensation an entity requires for bearing the uncertainty of non-financial risks and is updated at each reporting date",
        "It excludes the impact of future inflation assumptions"
      ), selected = character(0))
    ),

    box(
      title = "11. An insurer has ongoing claims on a group of insurance contracts and estimates that it will incur BWP 500,000 in salaries and operational costs for its claims handling department over the next reporting period. These costs are not linked to specific claims but are expected to support overall claims management. How should these expected costs be treated under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q11"), label = NULL, choices = c(
        "Expensed immediately in full",
        "Included in the liability for incurred claims (LIC) and updated at each reporting period",
        "Ignored unless individually linked to a claim",
        "Deferred until claim settlement is completed"
      ), selected = character(0))
    ),


    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")), 
    div(
      class = "quiz-nav",
      actionButton(
        ns("to_module_7"),
        label = tagList(icon("arrow-right"), "Next: Module 7 – Discounting, CSM & Risk Adjustment"),
        class = "control-button-tavnav"
      )
    )
  )
}

correct_answers_module6 <- list(
  q1 = list(
    answer     = "Subsequent measurement of insurance liabilities",
    explanation = "This situation illustrates subsequent measurement under IFRS 17: updating insurance liabilities to reflect new experience and assumptions."
  ),
  q2 = list(
    answer     = "BWP 3,000 in year 1, BWP 1,800 in year 2, BWP 1,200 in year 3",
    explanation = "CSM is released based on coverage units (50%, 30%, 20% of BWP 6,000)."
  ),
  q3 = list(
    answer     = "At each financial reporting date",
    explanation = "IFRS 17 requires fulfilment cash flows to be updated at every reporting date using current estimates."
  ),
  q4 = list(
    answer     = "As an expense in the profit or loss statement",
    explanation = "Settled claims are recognized as expenses in profit or loss, reflecting fulfillment of obligations."
  ),
  q5 = list(
    answer     = "Changes in uncertainty of future service",
    explanation = "Risk adjustment changes when the level of uncertainty about future cash flows changes."
  ),
  q6 = list(
    answer     = "Time value updates",
    explanation = "Passage of time (interest accretion) affects finance income/expense, not the CSM."
  ),
  q7 = list(
    answer     = "Claims that have been incurred but not yet settled",
    explanation = "LIC measures the insurer’s obligation for claims already incurred but unpaid."
  ),
  q8 = list(
    answer     = "BWP 40,000 (BWP 30,000 FCF + BWP 10,000 CSM)",
    explanation = "LRC equals fulfilment cash flows for future coverage plus the unearned CSM."
  ),
  q9 = list(
    answer     = "Claims already incurred",
    explanation = "LIC captures obligations for claims that have already occurred."
  ),
  q10 = list(
    answer     = "It represents the compensation an entity requires for bearing the uncertainty of non-financial risks and is updated at each reporting date",
    explanation = "The risk adjustment reflects required compensation for non-financial risk uncertainty and is remeasured each period."
  ),
  q11 = list(
    answer     = "Included in the liability for incurred claims (LIC) and updated at each reporting period",
    explanation = "Unallocated Loss Adjustment Expenses (ULAE) are included in LIC and reassessed each reporting date."
  )
)


IFRS17Module6Server <- (function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

   # Add a reactive value to track when progress is saved
    progress_saved_trigger <- reactiveVal(0)


    score <- reactiveVal(0)

    # Feedback functions
    observeEvent(input$submit, {
        removeModal()
        # 1. List out all your question input IDs
        question_ids <- paste0("q", 1:11)
        
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

    for (qid in names(correct_answers_module6)) {
      correct <- correct_answers_module6[[qid]]$answer
      explanation <- correct_answers_module6[[qid]]$explanation
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
      # Save progress for Module 6
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 6 specific calculations
        total_questions <- length(correct_answers_module6)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module6",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 6 Progress Saved!</strong><br>",
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
          print(paste("Module 6 progress save error:", e$message))
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


    valid_ids <- paste0("q", 1:11)
    feedback <- lapply(valid_ids, function(qid) {
      if (!is.null(feedback[[qid]])) {
        feedback[[qid]]
      } else {
        paste0("⚠️ ", toupper(qid), ": No response recorded.")
      }
    })
    names(feedback) <- valid_ids

    output$result <- renderUI({
      total_questions <- length(correct_answers_module6)
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
              "📊 Module 6 Results Summary",
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
    to_module_7 <- reactive(input$to_module_7)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_7,
      progress_trigger = progress_saved_trigger
    ))

  })
})