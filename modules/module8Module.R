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
        h3(icon("info-circle"), "Module Objective", class = "section-subheading"),
        p("This module gives a detailed summary of onerous contracts as outlined in ",
          tags$strong("paragraphs 47–52 of the standard"), "."),
        p("An onerous contract refers to an insurance contract (or a group of such contracts)
          where the anticipated total outgoing—such as payments for claims, associated costs,
          and the risk adjustment—are greater than the premiums charged. Meaning, the expected
          cash payments exceed the expected cash receipts.")
    ),

    div(class = "module-section",  
        h3("At Initial Recognition", class = "section-title-top"),
        p("If contracts are onerous at initial recognition, IFRS 17 requires the insurer to:"),
        tags$ol(
          type = "a",
          tags$li("Immediately report the financial loss in the profit and loss statement."),
          tags$li("Group them separately from contracts that are expected to generate profits."),
          tags$li("Record the entire expected cost as a liability on the balance sheet."),
          tags$li("Set the Contractual Service Margin (CSM) to zero, reflecting that no unearned profit exists."),
          tags$li("Create a loss component to monitor and account for the recognized loss over time.")
        )
    ),

    div(class = "module-section",
        h3(icon("stopwatch"), "At Subsequent Measurement", class = "section-subheading"),
        h4("When do contracts become (more) onerous?"),
        p("A group of contracts becomes onerous, or increasingly so, after initial recognition if certain unfavorable developments occur, such as:"),
        tags$ul(
          tags$li("A rise in anticipated future claims or servicing costs, often due to deteriorating assumptions; or"),
          tags$li("In the case of contracts with direct participation features, a decline in the expected investment return.")
        ),
        p("When this occurs, IFRS 17 requires the entity to:"),
        tags$ul(
          tags$li("First, utilize any remaining Contractual Service Margin (CSM) to absorb the loss."),
          tags$li("Record any excess loss directly in the profit and loss account."),
          tags$li("Reflect the loss within the liability for remaining coverage by either increasing or establishing a Loss Component.")
        )
    ),

    div(class = "module-section",
        h3(icon("layer-group"), "Loss Component", class = "section-subheading"),
        p("The Loss Component is a specific element within the liability for remaining coverage that captures losses already recognized in profit or loss."),
        p("Its key purpose is to:"),
        tags$ul(
          tags$li("Track losses that have been recognized in the Profit or Loss statement"),
          tags$li("Ensure losses are gradually released over time as insurance services are provided"),
          tags$li("Reflect the improvement in future assumptions by reducing the Loss Component if expected losses decrease")
        ),
        p(
          tags$em("Point to note: "),
          tags$strong("The CSM cannot be increased until the entire Loss Component has been fully reversed.")
        ),
        img(src = "images/onerousContracts.png", class = "module-image")
    ),

    div(class = "module-section", 
        h3(icon("file-invoice-dollar"), "Assessing Onerous Contracts and Impact of Assumption Changes", class = "section-subheading"),
        p("When assumptions such as future claims, operating costs, or risk margins change, the resulting impact is split proportionally between the loss component and the remaining part of the liability for remaining coverage."),
        p("This approach ensures that the loss component is gradually reduced and fully released by the end of the contract's coverage duration."),
        p("An insurer will classify a group of insurance contracts as onerous if the projected combined ratio for the portfolio goes above 100%. The combined ratio is determined as follows:"),
        
        # Combined Ratio Formula
        div(
            style = "text-align: center; font-style: italic; color: #006AA6; margin-top: 10px;",
            HTML("Combined ratio = <u>Insurance service expenses</u> / <u>Insurance revenue</u> × 100")
        )
    ),

    div(class = "module-section",
        p("The insurance service expenses include:"),
        tags$ul(
          tags$li("Incurred claims and other directly attributable outflows"),
          tags$li("Adjustments made to the Liability for Incurred Claims (LIC)"),
          tags$li("Recognized losses related to onerous contracts"),
          tags$li("Amortization of insurance acquisition cash flows")
        ),
        p("In making this assessment, insurers also review combined ratios from earlier cohorts. 
          This historical view helps provide context and identify trends in portfolio performance.")
    ),





    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Onerous Contracts", class = "section-subheading"),
    ),

    box(
      title = "1. How is an insurance contract identified as onerous at initial recognition?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "If the policy is likely to be cancelled before its coverage ends",
        "If the contract does not include a Contractual Service Margin (CSM)",
        "If the contract is expected to generate a loss for the insurer",
        "If the contract does not transfer significant insurance risk"
      ), selected = character(0))
    ),

    box(
      title = "2. What is required when a contract is identified as onerous at initial recognition?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "Delay recognition until claims are paid",
        "Recognize a loss in profit or loss and continue standard measurement",
        "Group the contract with profitable contracts",
        "Recognize the loss immediately and create a Loss Component"
      ), selected = character(0))
    ),

    box(
      title = "3. What triggers a contract to become onerous after initial recognition?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Reduction in acquisition expenses",
        "A deterioration in future assumptions, such as higher claims or lower returns",
        "A decrease in expected claims",
        "An increase in investment income"
      ), selected = character(0))
    ),

    box(
      title = "4. What happens if a group of insurance contracts becomes more onerous after initial recognition?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "Reverse the previously recognized loss",
        "Ignore the change until coverage ends",
        "Recognize additional loss and increase the Loss Component",
        "Increase the CSM to offset the loss"
      ), selected = character(0))
    ),

    box(
      title = "5. What is the purpose of the Loss Component?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "To track losses recognized in P&L and manage their release over time",
        "To offset unearned premiums",
        "To hold investment income",
        "To reduce insurance acquisition costs"
      ), selected = character(0))
    ),

    box(
      title = "6. What happens to the Contractual Service Margin (CSM) at initial recognition if a contract is classified as onerous?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "It is set to zero, and the full loss is recognized immediately",
        "It is used to offset expected future profits",
        "It is deferred and recognized over the coverage period",
        "It is added to the liability for incurred claims"
      ), selected = character(0))
    ),

    box(
      title = "7. What happens to the Contractual Service Margin (CSM) when a contract becomes onerous?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "It continues to be amortized normally",
        "It is used (to the extent available) to offset the loss before setting up a Loss Component",
        "It is increased to offset the loss",
        "It is ignored and a new CSM is created"
      ), selected = character(0))
    ),

    box(
      title = "8. How is the Loss Component released?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "When investment returns improve",
        "All at once when coverage ends",
        "When a new contract is issued",
        "Over time, insurance services are provided"
      ), selected = character(0))
    ),

    box(
      title = "9. Which of the following statements is true about the CSM and the Loss Component?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "The CSM cannot increase until the Loss Component is fully reversed",
        "The CSM can increase as long as premiums are received",
        "The CSM and the Loss Component can be increased simultaneously",
        "The Loss Component offsets insurance acquisition cash flows"
      ), selected = character(0))
    ),

    box(
      title = "10. What is the combined ratio used for in assessing whether contracts are onerous?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "To measure the profitability of investment income",
        "To determine the adequacy of acquisition costs",
        "To evaluate if expected insurance expenses exceed insurance revenue",
        "To calculate the CSM at initial recognition"
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
    answer = "If the contract is expected to generate a loss for the insurer",
    explanation = "An insurance contract is considered onerous when expected fulfilment cash outflows exceed expected inflows (premiums), resulting in an immediate loss."
  ),
  q2 = list(
    answer = "Recognize the loss immediately and create a Loss Component",
    explanation = "At initial recognition, an onerous group must recognize the loss in profit or loss and establish a Loss Component."
  ),
  q3 = list(
    answer = "A deterioration in future assumptions, such as higher claims or lower returns",
    explanation = "After initial recognition, a contract becomes onerous if adverse changes in assumptions (e.g., higher claims or lower investment returns) worsen fulfilment cash flows."
  ),
  q4 = list(
    answer = "Recognize additional loss and increase the Loss Component",
    explanation = "If a group becomes more onerous, entities must recognize the additional loss in profit or loss and adjust the Loss Component upward."
  ),
  q5 = list(
    answer = "To track losses recognized in P&L and manage their release over time",
    explanation = "The Loss Component captures losses already recognized and ensures they are released gradually as services are provided."
  ),
  q6 = list(
    answer = "It is set to zero, and the full loss is recognized immediately",
    explanation = "When a contract is onerous at inception, the CSM is set to zero because no future profit remains."
  ),
  q7 = list(
    answer = "It is used (to the extent available) to offset the loss before setting up a Loss Component",
    explanation = "When contracts become onerous post-recognition, any remaining CSM offsets the loss before creating or increasing the Loss Component."
  ),
  q8 = list(
    answer = "Over time, insurance services are provided",
    explanation = "The Loss Component is released gradually over the coverage period as the insurer provides services."
  ),
  q9 = list(
    answer = "The CSM cannot increase until the Loss Component is fully reversed",
    explanation = "IFRS 17 prohibits increases in CSM while any Loss Component remains, ensuring losses are fully unwound first."
  ),
  q10 = list(
    answer = "To evaluate if expected insurance expenses exceed insurance revenue",
    explanation = "A combined ratio above 100% indicates expenses exceed revenue, signaling potential onerous contracts."
  )
)


# This is the server logic for the IFRS 17 Module 8
# It handles the quiz submission, scoring, and feedback display.
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
              "📊 Module 8 Results Summary",
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
    to_module_9 <- reactive(input$to_module_9)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_9,
      progress_trigger = progress_saved_trigger
    ))
  })
})