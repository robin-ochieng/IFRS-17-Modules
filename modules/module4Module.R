# This module is for the IFRS 17 module 4
IFRS17Module4UI <- function(id) {
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
            h2("📘 Module 4: Recognition of Insurance Contracts", class = "section-title-top")
        ),

        div(class = "module-section",
            h3("📖 Introduction"),
            p("This module provides an overview of how insurance contracts are recognized as per IFRS 17 paragraph 25 to paragraph 28.")
        ),

        div(class = "module-section",
            h3("📌 Recognition in IFRS 17"),
            p("IFRS 17 requires insurers to recognize groups of insurance contracts in a timely and structured manner to ensure accurate financial reporting, especially in reflecting risk and acquisition costs."),
            p("Below is a summary of the key recognition principles, including when and how a group of contracts should be accounted for.")
        ),

        div(class = "module-section image-timeline-wrapper",
            h3("1. Timing of Initial Recognition", class = "section-subheading"),
            h4("IFRS 17: Initial Recognition Timeline", class = "timeline-title"),
            p("Recognition occurs at the earliest of the following:"),
            div(
                class = "timeline-image-container",
                img(
                    src = "images/initialRecognitionTimeline.png",  # make sure you save the image to this path
                    alt = "IFRS 17 Initial Recognition Timeline",
                    class = "timeline-image"
                )
            )
        ),

        div(class = "module-section",
            h3("2. Identification of Onerous Contracts before Recognition", class = "section-subheading"),
            p("An entity must assess if any contracts are onerous before the earlier of:"),
            tags$ul(
              tags$li("The beginning of the coverage period, or"),
              tags$li("The due date of the first payment.")
            )
        ),

        div(class = "module-section",
            h3("3. Insurance Acquisition Cash Flows (IACFs)", class = "section-subheading"),
            p("IACFs refer to costs related to setting up insurance contracts, such as broker or agent fees."),
            p("Any IACFs paid or received before group recognition are recorded as an asset or liability unless used up immediately."),
            p("These amounts stay on the books temporarily. Once the group of contracts starts, this money becomes part of the group’s total cost and therefore, the asset or liability can then be derecognized.")
        ),

        div(class = "module-section",
            h3("4. Contracts Included in a Group", class = "section-subheading"),
            p("The group should only include contracts issued by the end of the reporting period."),
            p("When the insurer records a group of insurance contracts, they need to figure out the interest rate they'll use to adjust future cash flows to today's value."),
            p("They also need to estimate how much insurance services they’ll provide during the period, so they can spread the revenue fairly over time."),
            p("New contracts may be added to the group after the reporting period, but they are only added in the period they are issued. In the event this happens, the initial discount rate may change, which must then be updated and applied from the start of the reporting period.")
        ),

        div(class = "module-section",
            h3("📝 Quiz: Answer the following questions to test your understanding of Recognition of Insurance Contracts."),
        ),        

        box(
          title = "1. When must a group of insurance contracts be recognized under IFRS 17?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q1"), label = NULL, choices = c(
            "At the end of the reporting period",
            "When the last payment is received",
            "When the policyholder signs the contract",
            "At the earliest of the coverage period start, first payment due, or when the group becomes onerous"
          ), selected = character(0))
        ),

        box(
          title = "2. If there is no contractual due date for the first payment, when is it considered due?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q2"), label = NULL, choices = c(
            "At the end of the month",
            "When it is received",
            "After coverage starts",
            "When billed"
          ), selected = character(0))
        ),

        box(
          title = "3. When should an insurer assess if a contract is onerous?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q3"), label = NULL, choices = c(
            "After recognition",
            "Before the earlier of coverage start or payment due",
            "At the end of the financial year",
            "Only when a loss is reported"
          ), selected = character(0))
        ),

        box(
          title = "4. What is the treatment if IACFs are not immediately expensed?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q4"), label = NULL, choices = c(
            "They are recognized as an asset or liability",
            "They are deferred revenue",
            "They are added to the CSM",
            "They are amortized over the contract term"
          ), selected = character(0))
        ),

        box(
          title = "5. When is the acquisition asset or liability removed from the books?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q5"), label = NULL, choices = c(
            "When the last premium is received",
            "When the policyholder cancels",
            "When the related group of contracts is recognized",
            "At the year-end"
          ), selected = character(0))
        ),

        box(
          title = "6. What is the condition for including a contract in a group?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q6"), label = NULL, choices = c(
            "It must be active",
            "It must be issued by the end of the reporting period",
            "It must be profitable",
            "It must be short-term"
          ), selected = character(0))
        ),

        box(
          title = "7. What happens if new contracts added to a group affect the discount rate?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q7"), label = NULL, choices = c(
            "The rate must be updated and applied from the start of the reporting period",
            "Nothing changes",
            "It only applies to new contracts",
            "The group must be split"
          ), selected = character(0))
        ),

        box(
          title = "8. Which of the following is TRUE regarding onerous contracts?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q8"), label = NULL, choices = c(
            "They must be recognized immediately",
            "They are ignored under IFRS 17",
            "They are grouped with profitable contracts",
            "They are only assessed annually"
          ), selected = character(0))
        ),

        box(
          title = "9. How often can the discount rate be changed for a group?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q9"), label = NULL, choices = c(
            "Monthly",
            "Only if new contracts are added that change it",
            "Once a year",
            "Never"
          ), selected = character(0))
        ),

        box(
          title = "10. Why is the initial recognition timing important under IFRS 17?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q10"), label = NULL, choices = c(
            "It determines when revenue and expenses are recorded",
            "It helps identify reinsurers",
            "It is used to calculate tax",
            "It helps with customer satisfaction"
          ), selected = character(0))
        ),



        actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
        br(), 
        br(),
        uiOutput(ns("result")),  

        div(
          class = "quiz-nav",
          actionButton(
              ns("to_module_5"),
              label = tagList(icon("arrow-right"), "Next: Module 5 - Measurement on Initial Recognition"),
              class = "control-button-tavnav"
          )
        )  
    )
}

correct_answers_module4 <- list( 
  q1 = list(
    answer = "At the earliest of the coverage period start, first payment due, or when the group becomes onerous",
    explanation = "IFRS 17 requires recognition at the earliest of these three trigger events."
  ),
  q2 = list(
    answer = "When it is received",
    explanation = "IFRS 17 states that if no due date is set, the payment is considered due when received."
  ),
  q3 = list(
    answer = "Before the earlier of coverage start or payment due",
    explanation = "The standard requires a pre-recognition assessment if there's an indication of onerousness."
  ),
  q4 = list(
    answer = "They are recognized as an asset or liability",
    explanation = "IACFs are treated separately until the related group is recognized."
  ),
  q5 = list(
    answer = "When the related group of contracts is recognized",
    explanation = "The asset or liability is derecognized at the point of group recognition."
  ),
  q6 = list(
    answer = "It must be issued by the end of the reporting period",
    explanation = "Only contracts issued by the end of the reporting period are included."
  ),
  q7 = list(
    answer = "The rate must be updated and applied from the start of the reporting period",
    explanation = "The standard requires adjusting the initial discount rate retroactively to the start of the reporting period."
  ),
  q8 = list(
    answer = "They must be recognized immediately",
    explanation = "Onerous groups must be recognized as soon as they become onerous."
  ),
  q9 = list(
    answer = "Only if new contracts are added that change it",
    explanation = "The rate is updated only if new contracts added after the reporting period affect it."
  ),
  q10 = list(
    answer = "It determines when revenue and expenses are recorded",
    explanation = "Proper timing ensures that revenue, risk, and costs are reported accurately."
  )
)




IFRS17Module4Server <- (function(id, user_data) {
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

    for (qid in names(correct_answers_module4)) {
      correct <- correct_answers_module4[[qid]]$answer
      explanation <- correct_answers_module4[[qid]]$explanation
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
      # Save progress for Module 4
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 2 specific calculations
        total_questions <- length(correct_answers_module4)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module4",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 4 Progress Saved!</strong><br>",
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
          print(paste("Module 4 progress save error:", e$message))
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
      total_questions <- length(correct_answers_module4)
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
              "📊 Module 4 Results Summary",
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
    to_module_5 <- reactive(input$to_module_5)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_5,
      progress_trigger = progress_saved_trigger
    ))
  })
})