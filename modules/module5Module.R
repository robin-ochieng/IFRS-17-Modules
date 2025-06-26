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
            h3("📖 Introduction"),
            p("This module provides an overview of the requirements for measurement on initial recognition of insurance contracts, as set out in paragraphs 32–52 of the IFRS 17 standard.")
        ),

        div(class = "module-section",
            h3("📌 Recognition Criteria"),
            p("Under IFRS 17, Insurance contracts must be recognized at the earliest of:"),
            tags$ul(
              tags$li("The beginning of the coverage period;"),
              tags$li("The date when the first premium is due; or"),
              tags$li("The date when the group of contracts becomes onerous.")
            )
        ),
        div(class = "module-section",
          h3("📊 Measurement Approaches Overview"),
          div(class = "table-responsive",
            tags$table(class = "measurement-table",
              tags$thead(
                tags$tr(
                  tags$th("Measurement Model"),
                  tags$th("Applicability"),
                  tags$th("Key Features")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(tags$b("General Measurement Model (GMM)")),
                  tags$td("Default model for long-duration contracts"),
                  tags$td("Includes FCF, Discounting, Risk Adjustment, CSM")
                ),
                tags$tr(
                  tags$td(tags$b("Premium Allocation Approach (PAA)")),
                  tags$td("Optional for short-duration contracts"),
                  tags$td("Simplified method, no explicit CSM")
                ),
                tags$tr(
                  tags$td(tags$b("Variable Fee Approach (VFA)")),
                  tags$td("For contracts with direct participation features"),
                  tags$td("CSM adjusts based on underlying asset returns")
                )
              )
            )
          )
        ),

        div(class = "module-section",
          h3("🔍 Components of Measurement at Initial Recognition"),
          p("Under the General Measurement Model (GMM), the initial measurement of a group of insurance contracts is the sum of:"),
          tags$ol(type = "i",
            tags$li(
              strong("Fulfilment Cash Flows (FCF):"),
              tags$ol(type = "a",
                tags$li("Future Cash Flows: Best estimates of expected premiums, claims, benefits, and expenses."),
                tags$li("Discounting: Adjustment to reflect the time value of money and financial risks."),
                tags$li("Risk Adjustment: Compensation for being uncertainty in non-financial risks (e.g. mortality, lapse).")
              )
            ),
            tags$li(
              tagList(
                strong("Contractual Service Margin (CSM): "),
              tags$ol(type = "a",
                tags$li("Represents the unearned profit that the entity will recognize as it provides insurance contract services over the duration of the contract."),
                p("The standard states under paragraph 53 that the PAA is designed for insurance contracts with a coverage period of one year or less, or where it can be demonstrated that using the PAA would not materially differ from applying the General Measurement Model (GMM)."),
                p("At initial recognition:"),
                tags$ol(type = "a",
                  tags$li("The liability is measured as: premiums received (or due) less insurance acquisition cash flows"),
                  tags$li("Acquisition costs are treated based on the entity’s accounting policy—they may either be deferred or expensed immediately in the Profit and Loss.")
                )
              )
              )
            )
          )
        ),



        div(class = "module-section",
            h3("🧮 Contractual Service Margin (CSM)"),
            p("The CSM is the key component that defers recognition of profits until insurance services are provided."),
            p("At initial recognition:"),
            tags$ul(
              tags$li("a)	CSM=Discounted cash inflows -discounted cash outflows-risk adjustment."),
              tags$li("b)	If the result is negative; the contract is onerous, and a loss is recognized immediately in profit or loss.")
            )
        ),

        div(class = "info-box scenario-box",
          div(class = "scenario-calc-row",
            # Left column
            div(class = "scenario-col",
              tags$h4("📌 Illustration:"),
              tags$ul(
                tags$li("Expected premiums: KES 1,200"),
                tags$li("Expected claims & expenses: KES 900"),
                tags$li("Risk Adjustment: KES 50"),
                tags$li("Discounting impact: KES 100")
              )
            ),
            # Right column
            div(class = "calc-col",
              tags$h4("🧮 Calculation:"),
              tags$ul(
                tags$li("Fulfilment Cash Flows = KES 1,200 - KES 900 - KES 100 - KES 50 = KES 150"),
                tags$li(tags$strong("CSM = KES 150"), " which is to be released over the coverage period.")
              )
            )
          )
        ),
     

        div(class = "module-section",
            h3("⚠️ Treatment of Onerous Contracts"),
            p("If the total of expected outflows plus the risk adjustment exceeds the expected inflows, the contract is considered onerous."),
          tags$ol(type = "a",
            tags$li("No CSM is recognized , as the CSM cannot be negative."),
            tags$li("A loss component is established to reflect the immediate loss."),
            tags$li("The loss is recorded in profit or loss.")
          )            
        ),
      
        div(class = "module-section summary-box",
            h3("📌 Key Takeaways"),
            tags$ol(type = "a",
              tags$li("Contracts are recognized when the earliest of:"),
              tags$ol(type = "i",
                tags$li("The beginning of the coverage period"),
                tags$li("The date when the first premium is due; or"),
                tags$li("The date when the group of contracts becomes onerous")
              ),
              tags$li("Measurement includes future cash flows, discounting, risk adjustment, ULAE, and CSM(under GMM)."),
              tags$li("The CSM ensures no upfront profit. and is adjusted only for changes in future service."),
              tags$li("Onerous contracts result in immediate loss recognition."),
              tags$li("Choosing between the GMM and PAA depends on contract duration and PAA eligibility test."),
              tags$li("Acquisition costs are handled differently under GMM and VFA (included in FCFs) and PAA (either deferred or expensed).")
            )
        ),

        div(class = "module-section image-summary-wrapper",
            h3("📋 Summary of Measurement at Initial Recognition"),

              img(
                src = "images/summaryofMeasurementonInitialRecognition.png",
                alt = "Summary of Measurement at Initial Recognition",
                class = "module-image"
              )

        ),

        div(class = "module-section",
            h3("📝 Quiz: Answer the following questions to test your understanding of Measurement on Initial Recognition."),
        ),


        box(
          title = "1. Which of the following is NOT a component of fulfilment cash flows?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q1"), label = NULL, choices = c(
            "Future cash flows",
            "Discount rate",
            "Risk adjustment",
            "Insurance acquisition commission bonus pool"
          ), selected = character(0))
        ),

        box(
          title = "2. Which cash flows should be included in the measurement of the contract at initial recognition?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q2"), label = NULL, choices = c(
            "Past claims only",
            "Cash flows related to investment returns",
            "Future premiums and claim payments",
            "Marketing expenses"
          ), selected = character(0))
        ),

        box(
          title = "3. If the fulfilment cash flows are negative, what does IFRS 17 require?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q3"), label = NULL, choices = c(
            "Defer the difference",
            "Recognize a loss immediately",
            "Recognize a CSM",
            "Reduce the asset balance"
          ), selected = character(0))
        ),

        box(
          title = "4. What happens to a day-1 gain under IFRS 17?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q4"), label = NULL, choices = c(
            "Deferred in CSM",
            "Recognized as revenue",
            "Transferred to retained earnings",
            "Recorded as OCI"
          ), selected = character(0))
        ),

        box(
          title = "5. Why is discounting applied to future cash flows?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q5"), label = NULL, choices = c(
            "To increase liabilities",
            "To reflect time value of money",
            "To reduce reporting volatility",
            "To comply with IFRS 9"
          ), selected = character(0))
        ),

        box(
          title = "6. Which discount rate is used for initial measurement?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q6"), label = NULL, choices = c(
            "Zero-coupon rate",
            "Locked-in discount rate",
            "Market average rate",
            "Prime lending rate"
          ), selected = character(0))
        ),

        box(
          title = "7. Which cost is not included in initial measurement?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q7"), label = NULL, choices = c(
            "Direct acquisition costs",
            "Expected claims",
            "Indirect administrative costs",
            "Risk adjustment"
          ), selected = character(0))
        ),

        box(
          title = "8. Which cost is typically excluded from fulfilment cash flows?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q8"), label = NULL, choices = c(
            "Advertising and marketing",
            "Future claims",
            "Premiums",
            "Claim handling costs"
          ), selected = character(0))
        ),

        box(
          title = "9. Under which model is no CSM typically recognized?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q9"), label = NULL, choices = c(
            "GMM",
            "PAA",
            "VFA",
            "Modified GMM"
          ), selected = character(0))
        ),

        box(
          title = "10. Which of the following is a valid reason to apply the Premium Allocation Approach (PAA) at initial recognition?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q10"), label = NULL, choices = c(
            "It results in higher revenue.",
            "The contract has a coverage period of more than one year",
            "The simplification does not significantly differ from GMM results",
            "It avoids recognition of acquisition costs"
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
    explanation = "Discounting ensures that future cash flows are presented in today’s money, reflecting the time value of money."
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