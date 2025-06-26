# This module is for the IFRS 17 module 15
IFRS17Module14UI <- function(id) {
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
      h2("📘 Module 14: Insurance Service Result", class = "section-title-top")
    ),
    div(class = "module-section",
        h3("📖 Introduction"),
        p("This module provides an overview of insurance service result as defined under IFRS 17 paragraphs 83 to 86.")    
    ),  
    div(class = "module-section",
      h3("📊 Presentation in the Statement of Financial Performance"),
      p("As per the standard, insurance companies shall disaggregate (split) their financial results into two main categories parts:"),
      tags$ol(type = "a",
        tags$li("Insurance Service Result"),
        tags$li("Insurance Finance Income or Expenses")
      ),
      p("This breakdown is shown in the statement of profit or loss and other comprehensive income, also called the ",
        strong("statement of financial performance"), "."),
      p("IFRS 17 requires that an entity presents income or expenses from reinsurance contracts held separately from those of insurance contracts issued.")
    ),

    div(class = "module-section",
      p("IFRS 17 requires that an entity presents income or expenses from reinsurance contracts held separately from those of insurance contracts issued."),
      p("The entity may:"),
      tags$ol(
        tags$li("Present a net amount (i.e., the total income or expense from a group of reinsurance contracts held), or"),
        tags$li(
          "Present separately the amounts:",
          tags$ol(type = "i",
            tags$li("Recovered from the reinsurer, and"),
            tags$li("An allocation of the premiums paid to the reinsurer, resulting in the same net effect.")
          )
        )
      )
    ),


    div(class = "module-section",
      h3("📊 Option 1"),
      div(class = "table-responsive",
        tags$table(class = "income-statement-table",
          tags$thead(
            tags$tr(
              tags$th(colspan = 2, "SAMPLE INCOME STATEMENT")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("Insurance Revenue"), tags$td("X")),
            tags$tr(tags$td("Insurance Service Expenses"), tags$td("(X)")),
            tags$tr(tags$td("Net Income/Expense from Reinsurance Contracts Held"), tags$td("X")),
            tags$tr(tags$td(tags$b("Insurance Service Result")), tags$td(tags$b("XX"))),
            tags$tr(tags$td("Insurance Finance Income or Expenses"), tags$td("(X)")),
            tags$tr(tags$td("Net Investment Income"), tags$td("X")),
            tags$tr(tags$td(tags$b("Net Financial Result")), tags$td(tags$b("XX"))),
            tags$tr(tags$td(tags$b("Profit/Loss Before Tax")), tags$td(tags$b("XXXX")))
          )
        )
      )
    ),


    div(class = "module-section",
      h3("📊 Option 2"),
      div(class = "table-responsive",
        tags$table(class = "income-statement-table",
          tags$thead(
            tags$tr(
              tags$th(colspan = 2, "SAMPLE INCOME STATEMENT")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("Insurance Revenue"), tags$td("X")),
            tags$tr(tags$td("Insurance Service Expenses"), tags$td("(X)")),
            tags$tr(tags$td("Insurance Service Result(before reinsurance contracts held)"), tags$td("XX")),
            tags$tr(tags$td("Allocation of reinsurance premium"), tags$td("(Y)")),
            tags$tr(tags$td("Amounts Recoverable from Reinsurers"), tags$td("Y")),
            tags$tr(tags$td("Net Income/Expense from Reinsurance Contract Held"), tags$td("YY")),
            tags$tr(tags$td(tags$b("Insurance Service Result")), tags$td(tags$b("XX"))),
            tags$tr(tags$td("Insurance Finance Income or Expenses"), tags$td("(X)")),
            tags$tr(tags$td("Net Investment Income"), tags$td("X")),
            tags$tr(tags$td("Net Financial Result"), tags$td("XX")),
            tags$tr(tags$td(tags$b("Profit/loss before tax")), tags$td(tags$b("XXXX")))
          )
        )
      )
    ),


    div(class = "module-section",
      h3("🧾 Insurance Service Result", style = "color:#006AA6;"),
      
      p("The Insurance Service Result (before reinsurance contracts held) represents the difference between the ",
        strong("Insurance Service Revenue"), " and the ", strong("Insurance Service Expenses"), "."),
      
      p("Insurance Service revenue reflects the income for providing coverage and other services."),
      
      p("Under the ", strong("Premium Allocation Approach (PAA)"), 
        ", insurance revenue is often similar to the earned premium over the coverage period."),
      
      p("Under the ", strong("General Measurement Model (GMM)"), 
        ", the Insurance service revenue includes:"),
      tags$ol( type = "i",
        tags$li("Expected claims and expenses for the coverage provided in the period"),
        tags$li("Risk adjustment for non-financial risk"),
        tags$li("The release of the contractual service margin"),
        tags$li("Recovery of insurance acquisition cash flows")
      ),
      
      p(strong("Insurance Service Expenses"), " represent the costs related to claims, services, and insurance contracts management. It includes:"),
      tags$ol( type = "i",
        tags$li("Incurred claims and other incurred insurance service expenses"),
        tags$li("Amortization of insurance acquisition cash flows"),
        tags$li("Changes that relate to past service, i.e., changes in fulfilment cash flows relating to the liability for incurred claims"),
        tags$li("Changes that relate to future service, i.e., losses on onerous groups of contracts and reversals of such losses")
      ),
      
      p("Insurance service revenue and insurance service expenses presented in profit or loss shall ",
        strong("exclude any investment components"), ".")
    ),

    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Insurance Service Result."),
    ),

    box(
      title = "1. What are the two main components an entity must disaggregate in the statement of financial performance?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "Revenue and Expenses",
        "Insurance Profit and Investment Return",
        "Earned Premium and Unearned Premium",
        "Insurance Service Result and Insurance Finance Income or Expenses"
      ), selected = character(0))
    ),

    box(
      title = "2. Which of the following is included in the insurance service result?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "Change in risk adjustment for non-financial risk",
        "Insurance revenue and insurance service expenses",
        "Investment income",
        "Premium refunds"
      ), selected = character(0))
    ),

    box(
      title = "3. How should an entity present income or expenses from reinsurance contracts held?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Separately from insurance contracts issued",
        "Together with insurance contracts issued",
        "Only in other comprehensive income",
        "As a deferred liability"
      ), selected = character(0))
    ),

    box(
      title = "4. What should insurance revenue reflect?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "Premiums received",
        "Claims paid",
        "The consideration expected in exchange for coverage and services",
        "Cash flow timing"
      ), selected = character(0))
    ),

    box(
      title = "5. Which of the following is NOT included in insurance service expenses?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "Incurred claims",
        "Investment components",
        "Other incurred insurance service expenses",
        "Amounts in paragraph 103(b)"
      ), selected = character(0))
    ),

    box(
      title = "6. When disaggregating the change in the risk adjustment for non-financial risk, what is required?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "Mandatory allocation between finance and service result",
        "No allocation is permitted",
        "Optional disaggregation; otherwise, include fully in the insurance service result",
        "Include entirely in finance income"
      ), selected = character(0))
    ),

    box(
      title = "7. Which component may NOT be presented in profit or loss?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "Premium information inconsistent with Paragraph 83",
        "Insurance service expenses",
        "Reinsurance income",
        "Risk adjustment"
      ), selected = character(0))
    ),

    box(
      title = "8. What options does an entity have for presenting reinsurance contracts held?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Only as a single net amount",
        "Only as individual line items",
        "Either as a net amount or split into recovered amounts and premium allocations",
        "As part of investment income"
      ), selected = character(0))
    ),

    box(
      title = "9. Under the Premium Allocation Approach (PAA), what is insurance revenue generally similar to?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "Earned premium",
        "Written premium",
        "Total premiums received",
        "Premium receivable"
      ), selected = character(0))
    ),

    box(
      title = "10. Under the General Measurement Model (GMM), which of the following is NOT part of insurance service revenue?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "Release of the Contractual Service Margin (CSM)",
        "Investment returns",
        "Risk adjustment for non-financial risk",
        "Recovery of acquisition cash flows"
      ), selected = character(0))
    ),

    box(
      title = "11. What are “losses on onerous contracts” classified as under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q11"), label = NULL, choices = c(
        "Investment finance expenses",
        "Deferred income",
        "Premium liability",
        "Insurance service expenses"
      ), selected = character(0))
    ),

    box(
      title = "12. What does IFRS 17 aim to achieve by separating service result and finance result?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q12"), label = NULL, choices = c(
        "Enhanced transparency and comparability",
        "Compliance with local GAAP",
        "Tax optimization",
        "Maximizing investment returns"
      ), selected = character(0))
    ),

    box(
      title = "13. What is the treatment of reinsurance-related cash flows contingent on claims?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q13"), label = NULL, choices = c(
        "Deferred revenue",
        "Presented as part of claims recoverable",
        "Included in insurance finance income",
        "Excluded from the financial statements"
      ), selected = character(0))
    ),

    box(
      title = "14. What is the effect of the reversal of losses on onerous contracts?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q14"), label = NULL, choices = c(
        "Increase in insurance finance income",
        "Reduction in insurance service revenue",
        "Decrease in liabilities and increase in insurance service result",
        "Increase in insurance acquisition costs"
      ), selected = character(0))
    ),


    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")),


    div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_15"),
          label = tagList(icon("arrow-right"), "Next: Module 15 - Insurance Finance Income or Expenses"),
          class = "control-button-tavnav"
      )
    ) 
    )
}

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



IFRS17Module14Server <- (function(id, user_data) {
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

    for (qid in names(correct_answers_module14)) {
      correct <- correct_answers_module14[[qid]]$answer
      explanation <- correct_answers_module14[[qid]]$explanation
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
        total_questions <- length(correct_answers_module14)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module14",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 14 Progress Saved!</strong><br>",
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
          print(paste("Module 14 progress save error:", e$message))
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
      total_questions <- length(correct_answers_module14)
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
              "📊 Module 14 Results Summary",
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
    to_module_15 <- reactive(input$to_module_15)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_15,
      progress_trigger = progress_saved_trigger
    ))

  })
})