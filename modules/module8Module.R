# This module is for the IFRS 17 module 9
IFRS17Module8UI <- function(id) {
  ns <- NS(id)
  logo_bar <- fluidRow(
    class = "logo-bar",                     # you’ll style this in CSS
    column(
      width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
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
      h2("📘 Module 8: Onerous Contracts", class = "section-title-top")
    ),
    div(class = "module-section",  
        h3("📖 Introduction"),
        p("This module provides an in-depth overview of onerous contracts, as set out in paragraphs 47-52 of IFRS 17."),
        p("An onerous contract is an insurance contract (or group of contracts) where expected costs (including claims, expenses, and risk margin) exceed the premiums received i.e. cash outflows are greater than cash inflows.")
     ),
    div(class = "module-section",  
        h3("At Initial Recognition"),
        p("A contract (or group of contracts) is onerous at initial recognition if the expected outflows (claims, expenses, risk margin + acquisition costs) exceed inflows (premiums)."),
        p("If contracts are onerous at initial recognition, IFRS 17 requires the insurer to:"),
        tags$ol(
          type = "a",
          tags$li("Group them separately from profitable contracts."),
          tags$li("Recognize loss immediately in the profit and loss."),
          tags$li("Set the Contractual Service Margin (CSM) to zero."),
          tags$li("Recognize full expected cost as a liability."),
          tags$li("Establish a Loss Component to track the loss.")
        )
    ),
    div(class = "module-section",  
        h3("At Subsequent Measurement"),
        p("A group of contracts becomes onerous, or more onerous, after initial recognition if:"),
        tags$ol(
          type = "a",
          tags$li("The expected future costs or claims increase, e.g. worse assumptions or;"),
          tags$li("For participation contracts, the expected investment returns decrease")
        )
    ),
    div(class = "module-section", 
        p("If this happens:"),
        tags$ol(
          type = "a",
          tags$li("Recognize additional loss in P&L"),
          tags$li("Use any remaining CSM to offset the loss"),
          tags$li("Increase or add the Loss Component in the liability")
        )
    ),
    div(class = "module-section", 
        p("The loss component is a part of the liability for remaining coverage that represents the recognized losses in the profit or loss."),
        p("The role of the Loss Component:"),
        tags$ol(
            type = "a",
            tags$li("Holds the loss recognized in the P&L"),
            tags$li(HTML("Is <strong>released gradually</strong> as coverage is provided")),
            tags$li(HTML("Is <strong>reduced</strong> if future assumptions improve"))
        ),
        tags$p(
            tags$strong("Note: "),
            span(style = "color:#006AA6; font-style:italic;",
                "The CSM can only increase after the loss component is cleared"
            )
        ),
        img(src = "images/onerousContracts.png", class = "module-image")
    ),
    div(class = "module-section", 
        p("Changes in assumptions (e.g. future claims, expenses, risk margin) are proportionally allocated between the loss component, and the remaining liability for coverage."),
        p("The aim is to release the loss component by the end of the coverage period."),
        p("The insurer will consider a contract to be onerous if the expected combined ratio for the portfolio exceeds 100%. The combined ratio is calculated as:"),
        
        # Combined Ratio Formula
        div(
            style = "text-align: center; font-style: italic; color: #006AA6; margin-top: 10px;",
            HTML("Combined ratio = <u>Insurance service expenses</u> / <u>Insurance revenue</u> × 100")
        )
    ),
    div(class = "module-section", 
        p("The insurance service expenses include:"),
        tags$ol(
            type = "a",
            tags$li("Incurred claims and other directly attributable expenses"),
            tags$li("Adjustments to the Liability for Incurred Claims (LIC)"),
            tags$li("Losses on onerous contracts"),
            tags$li("Insurance acquisition cash flows amortization")
        ),
        p("The assessment will also include consideration of the combined ratios of previous cohorts, providing a historical perspective on performance.")
    ),

    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Onerous Contracts"),
    ),

      box(
        title = "1. When is a contract classified as an onerous contract under IFRS 17?",
        status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
        radioButtons(ns("q1"), label = NULL, choices = c(
          "When the contract is expected to lapse early",
          "When the contract has no Contractual Service Margin (CSM)",
          "When the contract is expected to incur a loss",
          "When the contract has no insurance risk"
        ), selected = character(0))
      ),

      box(
        title = "2. How is the CSM treated for onerous contracts?",
        status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
        radioButtons(ns("q2"), label = NULL, choices = c(
          "Deferred",
          "Reversed",
          "Released to profit",
          "Set to zero"
        ), selected = character(0))
      ),

      box(
        title = "3. Which component is recognized when a group is onerous at initial recognition?",
        status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
        radioButtons(ns("q3"), label = NULL, choices = c(
          "Contractual Service Margin",
          "Risk Adjustment",
          "Loss Component",
          "Investment Return"
        ), selected = character(0))
      ),

      box(
        title = "4. How is the loss component recognized?",
        status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
        radioButtons(ns("q4"), label = NULL, choices = c(
          "As an asset",
          "Through OCI",
          "As an adjustment to the CSM",
          "In profit or loss"
        ), selected = character(0))
      ),

      box(
        title = "5. What happens if cash flow estimates improve?",
        status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
        radioButtons(ns("q5"), label = NULL, choices = c(
          "Loss component is reversed first",
          "CSM increases",
          "Risk adjustment decreases",
          "Premiums are restated"
        ), selected = character(0))
      ),

      box(
        title = "6. When is a contract classified as onerous?",
        status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
        radioButtons(ns("q6"), label = NULL, choices = c(
          "When risk adjustment is high",
          "When expected profit is low",
          "When fulfilment cash flows exceed premiums",
          "When lapse rate is high"
        ), selected = character(0))
      ),

      box(
        title = "7. What happens to the CSM if a group of contracts becomes onerous after initial recognition?",
        status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
        radioButtons(ns("q7"), label = NULL, choices = c(
          "It is increased",
          "It is set to zero and loss is recognized",
          "It is locked in",
          "It is recalculated using old assumptions"
        ), selected = character(0))
      ),

      box(
        title = "8. Which of the following changes can make a previously profitable contract group onerous?",
        status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
        radioButtons(ns("q8"), label = NULL, choices = c(
          "Increase in administrative expenses",
          "Drop in discount rates",
          "Revised premium allocation method",
          "Change in accounting policy"
        ), selected = character(0))
      ),

      box(
        title = "9. How does the loss component affect future insurance revenue?",
        status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
        radioButtons(ns("q9"), label = NULL, choices = c(
          "No effect",
          "Increases revenue",
          "It reduces future revenue",
          "It replaces CSM in revenue recognition"
        ), selected = character(0))
      ),

      box(
        title = "10. What causes a change in the loss component?",
        status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;", style = "border-left: 3px solid #DC5A17;",
        radioButtons(ns("q10"), label = NULL, choices = c(
          "Increase in discount rate",
          "Change in reinsurance treaty",
          "Adverse claims development",
          "Policyholder death"
        ), selected = character(0))
      ),

      actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
      br(), 
      br(),
      uiOutput(ns("result")),  

    div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_9"),
          label = tagList(icon("arrow-right"), "Next: Module 9 - Premium Allocation Approach"),
          class = "control-button-tavnav"
      )
    ) 
    )
}

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


IFRS17Module8Server <- (function(id, user_data) {
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

    for (qid in names(correct_answers_module8)) {
      correct <- correct_answers_module8[[qid]]$answer
      explanation <- correct_answers_module8[[qid]]$explanation
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
      # Save progress for Module 8
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 2 specific calculations
        total_questions <- length(correct_answers_module8)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module8",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 8 Progress Saved!</strong><br>",
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
          print(paste("Module 8 progress save error:", e$message))
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
      total_questions <- length(correct_answers_module8)
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
              "📊 Results Summary",
              style = "color:#f5f5f5; font-weight:600; margin-bottom:20px;"
            ),

            HTML(paste0(
              "<hr style='border-top:1px solid #f5f5f5;'>",
              "<p style='font-size:18px; color:#f5f5f5;'><strong>Total Score:</strong> ", score(), " / ", total_questions, "</p>",
              "<p style='font-size:18px; color:#f5f5f5;'><strong>Percentage Score:</strong> <span style='color:", color, "; font-weight:600;'>", percentage, "%</span></p>"
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
    to_module_9 <- reactive(input$to_module_9)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_9,
      progress_trigger = progress_saved_trigger
    ))
  })
})