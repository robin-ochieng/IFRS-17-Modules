# This module is for the IFRS 17 module 5
IFRS17Module5UI <- function(id) {
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
        h2("📘 Module 5: Measurement on Initial Recognition", class = "section-title-top")
    ),

      div(class = "module-section",
          h3(icon("info-circle"), "Module Overview", class = "section-subheading"),
          p("This module provides an overview of the requirements for measurement on initial recognition of insurance contracts, as set out in paragraphs 32–52 of the IFRS 17 standard.")
      ),

      div(class = "module-section",
          h3("📝 Recognition Criteria", class = "section-subheading"),
          p("An insurance contract is recognized at the earliest of the following:"),
          tags$ul(
            tags$li("The beginning of the coverage period;"),
            tags$li("When the first premium becomes due;"),
            tags$li("When it is determined that a group of contracts will be onerous.")
          )
      ),

        div(class = "module-section image-summary-wrapper",
            h3("📋 Measurement Approaches Overview", class = "section-subheading"),

              img(
                src = "images/measurementApproaches.png",
                alt = "Measurement Approaches Overview",
                class = "module-image"
              )
        ),

        # Components of Measurement at Initial Recognition
        div(class = "module-section",
            h3("📊 Components of Measurement at Initial Recognition", class = "section-subheading"),

            h5("General Measurement Model (GMM)"),
            p("Under the General Measurement Model (GMM), the initial measurement of a group of insurance contracts is the sum of:"),

            tags$ul(
              tags$li(strong("Fulfilment Cash Flows (FCF)")),
              tags$li("Future Cash Flows – Projections of inflows (premiums) and outflows (claims, benefits, and expenses)"),
              tags$li("Discounting – Adjustment to reflect the time value of money and financial risks"),
              tags$li("Risk Adjustment – Adjustment for uncertainties in non-financial assumptions (e.g., lapse rates)")
            )
        ),

        # Contractual Service Margin (CSM)
        div(class = "module-section",
            h3("💼 Contractual Service Margin (CSM)", class = "section-subheading"),

            tags$ul(
              tags$li("Represents the unearned profit that the entity will recognize as it provides insurance contract services over the duration of the contract"),
              tags$li("Ensures that no day-one gain is recognized in profit or loss"),
              tags$li("If the cash fulfilment flows result in a net cost (i.e., a loss), the group of contracts is considered onerous and the loss is recognized immediately in profit or loss (no CSM is recognized)"),
              tags$li("CSM = Present value of expected inflows – (present value of expected outflows + risk adjustment)"),
              tags$li("If the resulting value is negative, the contract is considered onerous and a loss is immediately recognized in profit or loss")
            ),

            h5("Illustration:"),
            tags$ul(
              tags$li("Expected premiums: BWP 3,000"),
              tags$li("Expected claims & expenses: BWP 2,100"),
              tags$li("Risk Adjustment: BWP 150"),
              tags$li("Discounting impact: BWP 250")
            ),

            h5("Calculation:"),
            tags$ol(
              tags$li(strong("Fulfilment Cash Flows (FCF) = BWP 3,000 – BWP 2,100 – BWP 250 – BWP 150 = BWP 500")),
              tags$li(strong("CSM = BWP 500, which is to be released over the coverage period"))
            )
        ),

        # Treatment of Onerous Contracts  
        div(class = "module-section",
            h3("⚠️ Treatment of Onerous Contracts", class = "section-subheading"),
            p("When the total of expected cash outflows and the risk adjustment exceeds the expected cash inflows, the contract is deemed onerous."),
            tags$ul(
              tags$li("In such cases, no Contractual Service Margin (CSM) is recognized, since the CSM cannot be negative."),
              tags$li("Instead, a loss component is recognized to reflect the immediate financial loss."),
              tags$li("This loss is immediately recorded in the profit or loss statement.")
            ),
            h5("Illustration:"),
            tags$ul(
              tags$li("Expected future cash inflows (Premiums): ", strong("BWP 9,000")),
              tags$li("Expected future cash outflows (Claims & expenses): ", strong("BWP 10,000")),
              tags$li("Risk Adjustment for non-financial risk: ", strong("BWP 500")),
              tags$li("Effect of discounting: ", strong("BWP (200)")),
              tags$li("Acquisition costs: ", strong("BWP 300"))
            )
        ),

        # Onerous Loss Calculation  
        div(class = "module-section",
            h3("🧮 Onerous Loss Calculation", class = "section-subheading"),
            tags$ol(
              tags$li(strong("FCF = Expected outflows + Risk Adjustment + Discounting adjustment = 10,000 + 500 – 200 = BWP 10,300")),
              tags$li("Since the fulfilment cash flows (liability) of BWP 10,300 exceed the expected premiums (inflows) of BWP 9,000, this group is onerous."),
              tags$li(strong("Onerous Loss = 10,300 – 9,000 = BWP 1,300")),
              tags$li("The entity recognizes a loss of ", strong("BWP 1,300"), " immediately in profit or loss."),
              tags$li("No Contractual Service Margin (CSM) is recognized."),
              tags$li("The initial liability for the group of contracts is BWP 10,300.")
            )
        ),

        # Key Takeaways  
        div(class = "module-section",
            h3("🔑 Key Takeaways", class = "section-subheading"),
            tags$ul(
              tags$li(
                "A group of insurance contracts must be recognized at the earliest of:",
                tags$ul(
                  tags$li("The start of the coverage period"),
                  tags$li("The due date of the first premium"),
                  tags$li("The date the group becomes onerous")
                )
              ),
              tags$li("Initial measurement includes expected future cash flows, discounting for time value of money, risk adjustment, unallocated loss adjustment expenses (ULAE), and a CSM (under the GMM)."),
              tags$li("CSM ensures profits are deferred and recognized only as insurance services are provided; adjustments to CSM relate only to future service."),
              tags$li("Onerous contracts lead to immediate recognition of a loss, with no CSM created."),
              tags$li("Choosing between GMM and PAA depends on the contract duration and whether the PAA yields results not materially different from GMM."),
              tags$li(
                "Acquisition costs treatment varies by model:",
                tags$ul(
                  tags$li("Under GMM and VFA, included in fulfilment cash flows."),
                  tags$li("Under PAA, may be deferred or expensed immediately based on the entity’s accounting policy.")
                )
              )
            )
        ),

        # Premium Allocation Approach (PAA)  
        div(class = "module-section",
            h3("💡 Premium Allocation Approach (PAA)", class = "section-subheading"),
            p("Per paragraph 53 of IFRS 17, the PAA applies to insurance contracts with a coverage period of one year or less, or where results are not materially different from the GMM."),
            p("At initial recognition:"),
            tags$ul(
              tags$li("Liability is measured as premiums received (or due) less insurance acquisition cash flows."),
              tags$li("Acquisition costs are treated per the entity’s policy—either deferred or expensed immediately in profit or loss.")
            )
        ),
        div(class = "module-section image-summary-wrapper",
            h3("📋 Summary of Measurement at Initial Recognition", class = "section-subheading"),

              img(
                src = "images/summaryofMeasurementonInitialRecognition.png",
                alt = "Summary of Measurement at Initial Recognition",
                class = "module-image"
              )

        ),

        div(class = "module-section",
            h3("📝 Quiz: Answer the following questions to test your understanding of Measurement on Initial Recognition.", class = "section-subheading"),
        ),


        box(
          title = "1. Under IFRS 17, fulfilment cash flows include all cash flows directly attributable to fulfilling the contract. Which of the following would NOT be included?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q1"), label = NULL, choices = c(
            "Probabilistic estimates of future premiums, claims, and benefits expected under the contract terms",
            "The discount rate applied to reflect the time value of money and financial risks not accounted for elsewhere",
            "A risk adjustment reflecting the compensation the entity requires for bearing non-financial uncertainty in cash flows",
            "A commission-based staff bonus pool linked to overall company sales targets for the year"
          ), selected = character(0))
        ),

        box(
          title = "2. Which of the following cash flows should be included when measuring an insurance contract at initial recognition?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q2"), label = NULL, choices = c(
            "Expected future premiums and claims",
            "Only historical claim amounts",
            "Cash flows arising from investment income",
            "Marketing expenses"
          ), selected = character(0))
        ),

        box(
          title = "3. What action does IFRS 17 require if the fulfilment cash flows result in a negative amount?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q3"), label = NULL, choices = c(
            "Postpone recognition of the shortfall",
            "Immediately record a loss",
            "Establish a Contractual Service Margin (CSM)",
            "Adjust the asset balance downward"
          ), selected = character(0))
        ),

        box(
          title = "4. How does IFRS 17 require entities to account for a Day-1 gain on initial recognition of an insurance contract?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q4"), label = NULL, choices = c(
            "Recognized immediately in the statement of profit or loss",
            "Recorded in Other Comprehensive Income (OCI)",
            "Deferred as part of the Contractual Service Margin (CSM) and released over time",
            "Credited directly to retained earnings in equity"
          ), selected = character(0))
        ),

        box(
          title = "5. What is the main reason for applying discounting to future cash flows under IFRS 17?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q5"), label = NULL, choices = c(
            "To inflate the value of liabilities",
            "To account for the time value of money",
            "To minimize fluctuations in financial reporting",
            "To meet IFRS 9 requirements"
          ), selected = character(0))
        ),

        box(
          title = "6. Under IFRS 17, how is the discount rate determined at initial recognition?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q6"), label = NULL, choices = c(
            "Based on a zero-coupon government bond rate only",
            "It is locked in at the date the insurance contract is initially recognized and used consistently for related measurements",
            "Based on an average of short-term and long-term market rates",
            "Derived from the central bank’s prime lending rate at year-end"
          ), selected = character(0))
        ),

        box(
          title = "7. Which of the following costs is excluded from the initial measurement of insurance contract liabilities under IFRS 17?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q7"), label = NULL, choices = c(
            "Acquisition expenses that are directly linked to the contract",
            "Anticipated claims and benefits",
            "Overhead and general administrative expenses",
            "Adjustment for non-financial risk"
          ), selected = character(0))
        ),

        box(
          title = "8. What is the result when the FCF = BWP 1,000 and outflows (including risk and acquisition) = BWP 750?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q8"), label = NULL, choices = c(
            "CSM = BWP 250",
            "CSM = BWP 1,000",
            "Immediate loss of BWP 750",
            "No CSM, contract is onerous"
          ), selected = character(0))
        ),

        box(
          title = "9. An insurer writes six-month travel insurance and applies PAA. No CSM is recorded. Why is this acceptable?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q9"), label = NULL, choices = c(
            "Because CSM only applies to long-term health policies",
            "Because the PAA does not require explicit CSM recognition for non-onerous short-term contracts",
            "Because all insurance contracts under IFRS 17 are measured without CSM",
            "Because travel insurance is exempt from IFRS 17 requirements"
          ), selected = character(0))
        ),

        box(
          title = "10. A one-year motor policy yields very similar liability estimates under PAA and GMM. What justifies using PAA?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q10"), label = NULL, choices = c(
            "PAA always leads to a higher CSM",
            "The contract includes profit-sharing features",
            "The measurement outcome under PAA is not materially different from GMM",
            "PAA automatically excludes all acquisition costs"
          ), selected = character(0))
        ),



        actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
        br(), 
        br(),
        uiOutput(ns("result")),  

        div(class = "quiz-nav",
            actionButton(
              ns("to_module_6"),
              label = tagList(icon("arrow-right"), "Next: Module 6 – Subsequent Measurement"),
              class = "control-button-tavnav"
            )
        )
  )
}

correct_answers_module5 <- list(
  q1 = list(
    answer = "A commission-based staff bonus pool linked to overall company sales targets for the year",
    explanation = "Staff bonus pools tied to general sales targets are not directly attributable to fulfilling the contract and are treated as administrative overhead."
  ),
  q2 = list(
    answer = "Expected future premiums and claims",
    explanation = "Fulfilment cash flows include expected future premiums and claims that are directly related to the contract."
  ),
  q3 = list(
    answer = "Immediately record a loss",
    explanation = "Negative fulfilment cash flows indicate an onerous contract; IFRS 17 requires immediate recognition of the loss in profit or loss."
  ),
  q4 = list(
    answer = "Deferred as part of the Contractual Service Margin (CSM) and released over time",
    explanation = "Day-1 gains are deferred in the CSM and recognized systematically over the coverage period."
  ),
  q5 = list(
    answer = "To account for the time value of money",
    explanation = "Discounting adjusts future cash flows to present value, reflecting the time value of money and financial risks."
  ),
  q6 = list(
    answer = "It is locked in at the date the insurance contract is initially recognized and used consistently for related measurements",
    explanation = "The discount rate is fixed at initial recognition and used both for discounting fulfilment cash flows and unwinding the CSM."
  ),
  q7 = list(
    answer = "Overhead and general administrative expenses",
    explanation = "Only costs directly attributable to contract issuance (e.g., acquisition costs) are included; general Admin costs are excluded."
  ),
  q8 = list(
    answer = "CSM = BWP 250",
    explanation = "CSM = FCF (1,000) – outflows (750) = BWP 250."
  ),
  q9 = list(
    answer = "Because the PAA does not require explicit CSM recognition for non-onerous short-term contracts",
    explanation = "Under PAA, CSM is typically not recognized for non-onerous short-duration contracts."
  ),
  q10 = list(
    answer = "The measurement outcome under PAA is not materially different from GMM",
    explanation = "PAA may be applied when it produces results not materially different from the GMM, simplifying the measurement."
  )
)



IFRS17Module5Server <- (function(id, user_data) {
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

    for (qid in names(correct_answers_module5)) {
      correct <- correct_answers_module5[[qid]]$answer
      explanation <- correct_answers_module5[[qid]]$explanation
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
      # Save progress for Module 5
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 2 specific calculations
        total_questions <- length(correct_answers_module5)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module5",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 5 Progress Saved!</strong><br>",
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
          print(paste("Module 5 progress save error:", e$message))
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
      total_questions <- length(correct_answers_module5)
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
              "📊 Module 5 Results Summary",
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
    to_module_6 <- reactive(input$to_module_6)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_6,
      progress_trigger = progress_saved_trigger
    ))

  })
})