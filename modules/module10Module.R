# This module is for the IFRS 17 module 11
IFRS17Module10UI <- function(id) {
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
      h2("📘 Module 10: Reinsurance Contracts Held", class = "section-title-top")
    ),
    div(class = "module-section",  
        h3(icon("info-circle"), "Module Objective", class = "section-subtitle"),
        p("This module provides an in-depth overview of the Reinsurance contracts held as per ", strong("IFRS 17"), " paragraphs 60 to 70."),
        
        p("A reinsurance contract held refers to an arrangement where one entity (the cedant) receives compensation from another entity (the reinsurer) for one or more claims arising from insurance contracts that the cedant has issued."),
        
        p("Reinsurance contracts held are measured separately from the underlying insurance contracts issued."),
        
        p("The ", em('"mirroring approach"'), " previously applied under IFRS 4 is no longer used. Under IFRS 17, reinsurance contracts held require an independent assessment."),
        
        p("We account for reinsurance contracts held to:"),
        
        tags$ol(type = "a",
            tags$li("Reflect the risk mitigation effect in financial statements."),
            tags$li("Ensure consistent measurement with the related underlying insurance contracts."),
            tags$li("Recognize gains or losses on risk transfer at initial recognition.")
        )
    ),
    div(class = "module-section",
        h3("📏 Measurement of Reinsurance Contracts Held", class = "section-subtitle"),
        
        h4(style = "color: #7BAF34;", "Initial Recognition"),
        
        p("At initial recognition, the reinsurance contract is measured using the General Measurement Model (GMM) unless the Premium Allocation Approach (PAA) is applied and meets the eligibility criteria."),
        
        p("The measurement components include:"),
        
        tags$ol(type = "a",
            tags$li("Fulfilment cash flows, which are the expected present value of future inflows and outflows related to the reinsurance contract, adjusted for the time value of money and the risk of counterparty default."),
            tags$li("A risk adjustment, which reflects the uncertainty in the amount and timing of the cash flows from the reinsurer."),
            tags$li("A Contractual Service Margin (CSM), which is recognized if the present value of future inflows from the reinsurer exceeds the ceded premiums. This CSM represents the unearned gain and is deferred and recognized as income over the coverage period."),
            tags$li("Loss-recovery component if the underlying contracts are onerous.")
        )
    ),
    div(class = "module-section",
        h3("Subsequent Measurement", class = "section-subtitle"),
        
        p("Following initial recognition, reinsurance contracts held are remeasured at each reporting date. The measurement continues to apply the General Measurement Model (GMM), or the Premium Allocation Approach (PAA) where applicable, and requires updates to the following key components:"),
        
        tags$ol(type = "a",
            tags$li("Fulfilment cash flows"),
            tags$li("Risk adjustment"),
            tags$li("CSM"),
            tags$li("Loss-recovery component"),
            tags$li("Experience adjustments"),
            tags$li("Changes in discount rates")
        ),
        p("If a group of insurance contracts is onerous and there's a related reinsurance contract, the cedant recognizes a recovery asset known as loss-recovery component."),
    ),
    div(class = "module-section",
        h3("Presentation in Financial Statements", class = "section-subtitle"),
        p("Assets and liabilities from reinsurance contracts are presented separately from insurance contracts issued."),
        p("Similarly, income and expenses related to reinsurance are disclosed separately in the statement of profit or loss."),
        p("IFRS 17 requires that all income and expenses from reinsurance be distinctly shown apart from insurance contracts issued.")
    ),
    
    div(class = "module-section",
        h3("Disclosures"),
        p("Entities are required to disclose:"),
        tags$ol(type = "a",
            tags$li("A description of how reinsurance contracts impact amounts reported in the financial statements."),
            tags$li("Key judgments made in applying IFRS 17 to reinsurance contracts."),
            tags$li("Reconciliations of the opening and closing balances of related assets and liabilities.")
        )
    ),

    div(class = "module-section",
        h3("Illustration of reinsurance contracts held."),
        img(src = "images/reinsuranceContractHeld.png", class = "module-image")
    ),      

    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Reinsurance Contracts Held."),
    ),

    box(
      title = "1. What is the purpose of holding a reinsurance contract under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "To increase the insurer’s liability",
        "To transfer and reduce insurance risk",
        "To provide investment returns",
        "To cover policyholder dividends"
      ), selected = character(0))
    ),

    box(
      title = "2. Under IFRS 17, how is the reinsurance contract held measured?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "Together with the underlying insurance contracts",
        "Separately from the underlying insurance contracts",
        "Using the reinsurer’s financial statements",
        "Using IFRS 4 guidelines"
      ), selected = character(0))
    ),

    box(
      title = "3. What replaces the “mirroring approach” from IFRS 4 in IFRS 17 for reinsurance contracts?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Aggregation method",
        "Separate stand-alone measurement",
        "Asset-based approach",
        "Direct debit method"
      ), selected = character(0))
    ),

    box(
      title = "4. What component of measurement reflects the uncertainty in future cash flows from the reinsurer?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "Loss-recovery component",
        "Contractual Service Margin",
        "Risk Adjustment",
        "Discount rate"
      ), selected = character(0))
    ),

    box(
      title = "5. What is the Contractual Service Margin (CSM) for reinsurance contracts held?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "liability for claims incurred",
        "The unearned profit from the reinsurance contract",
        "An expense item",
        "The reinsurer’s fee"
      ), selected = character(0))
    ),

    box(
      title = "6. When is the loss-recovery component recognized?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "When the underlying insurance contracts are profitable",
        "Only when the reinsurer fails to pay",
        "When the underlying insurance contracts are onerous",
        "When premiums are overdue"
      ), selected = character(0))
    ),

    box(
      title = "7. What happens to the CSM of a reinsurance contract held over time?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "It increases automatically",
        "It is released as services are received",
        "It becomes part of equity",
        "It is expensed immediately"
      ), selected = character(0))
    ),

    box(
      title = "8. Which of the following is NOT included in fulfilment cash flows?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Risk adjustment",
        "Discounted expected cash flows",
        "Reinsurer’s profitability forecasts",
        "Credit risk of the reinsurer"
      ), selected = character(0))
    ),

    box(
      title = "9. How should assets and liabilities from reinsurance contracts be presented?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "Net with insurance liabilities",
        "Separately from insurance contracts issued",
        "Offset against claims reserves",
        "As part of investment property"
      ), selected = character(0))
    ),

    box(
      title = "10. How are experience adjustments in reinsurance contracts handled?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "Deferred to the end of the contract",
        "Ignored in measurement",
        "Included in the remeasurement at each reporting date",
        "Treated as financing costs"
      ), selected = character(0))
    ),


    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")),  

    div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_11"),
          label = tagList(icon("arrow-right"), "Next: Module 11 - Investment Contracts with Discretionary Participation Features"),
          class = "control-button-tavnav"
      )
    ) 
    )
}

correct_answers_module10 <- list(
  q1 = list(
    answer = "To transfer and reduce insurance risk",
    explanation = "The main purpose is to transfer and reduce the insurer's risk exposure to claims."
  ),
  q2 = list(
    answer = "Separately from the underlying insurance contracts",
    explanation = "IFRS 17 requires that reinsurance contracts held be measured independently of the underlying insurance contracts."
  ),
  q3 = list(
    answer = "Separate stand-alone measurement",
    explanation = "IFRS 17 eliminates the mirroring approach and mandates separate stand-alone valuation."
  ),
  q4 = list(
    answer = "Risk Adjustment",
    explanation = "The risk adjustment represents uncertainty in the timing and amount of reinsurance cash flows."
  ),
  q5 = list(
    answer = "The unearned profit from the reinsurance contract",
    explanation = "The CSM represents unearned profit, recognized over the period of coverage."
  ),
  q6 = list(
    answer = "When the underlying insurance contracts are onerous",
    explanation = "The loss-recovery component arises when the underlying insurance contracts are loss-making (onerous)."
  ),
  q7 = list(
    answer = "It is released as services are received",
    explanation = "The CSM is gradually released to profit or loss as the reinsurance services are provided."
  ),
  q8 = list(
    answer = "Reinsurer’s profitability forecasts",
    explanation = "The reinsurer’s own profitability forecasts are not part of the cedant’s fulfilment cash flows."
  ),
  q9 = list(
    answer = "Separately from insurance contracts issued",
    explanation = "IFRS 17 requires separate presentation of reinsurance contracts held."
  ),
  q10 = list(
    answer = "Included in the remeasurement at each reporting date",
    explanation = "Experience adjustments affect the remeasurement of reinsurance contracts."
  )
)


IFRS17Module10Server <- (function(id, user_data) {
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

    for (qid in names(correct_answers_module10)) {
      correct <- correct_answers_module10[[qid]]$answer
      explanation <- correct_answers_module10[[qid]]$explanation
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
      # Save progress for Module 10
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 2 specific calculations
        total_questions <- length(correct_answers_module10)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module10",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 10 Progress Saved!</strong><br>",
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
          print(paste("Module 10 progress save error:", e$message))
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
      total_questions <- length(correct_answers_module10)
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
              "📊 Module 10 Results Summary",
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
    to_module_11 <- reactive(input$to_module_11)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_11,
      progress_trigger = progress_saved_trigger
    ))
  })
})