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
            h2("📘 Module 4: Recognition", class = "section-title-top")
        ),

        div(class = "module-section",
            h3(icon("info-circle"), "Module Overview", class = "section-subheading"),
            p("This module explains how insurance contracts are initially recognized under IFRS 17, specifically referring to the guidance found in paragraphs 25 to 28."),
            p("The module outlines the timing, criteria, and related considerations that insurers must follow when bringing groups of contracts onto the financial statements.")
        ),


        div(class = "module-section",
            h3("Recognition in IFRS 17", class = "section-subheading"),
            p("IFRS 17 establishes clear rules for when and how insurance contracts should be recognized. The goal is to provide a reliable representation of financial performance, particularly in relation to insurance risk and the cost of acquiring contracts."),
            p("Below is a summary of the key recognition principles, including when and how a group of contracts should be accounted for.")
          ),
        div(class = "module-section image-timeline-wrapper",
            h5("1. Initial Recognition"),
            p("A group of insurance contracts must be recognized at the earliest of the following three events:"),
            tags$ul(
              tags$li("The beginning of the coverage period for any contract in the group"),
              tags$li("The date when the first payment from a policyholder becomes due"),
              tags$li("When the group becomes onerous (i.e., expected to generate a loss), if this occurs before the other two events")
            ),
            div(
                class = "timeline-image-container",
                img(
                    src = "images/initialRecognitionTimeline.png",  # make sure you save the image to this path
                    alt = "IFRS 17 Initial Recognition Timeline",
                    class = "timeline-image"
                )
            ),
           p("This ensures timely reporting, particularly when contracts start to impact the entity’s financial position.")           
        ),


        div(class = "module-section",
            h5("2. Assessing Onerous Contracts before Recognition"),
            p("Before officially recognizing a group, insurers are required to check whether any contracts within the group are onerous."),
            p("This assessment must be performed before the earlier of:"),
            tags$ul(
              tags$li("The start date of the coverage period, or"),
              tags$li("The due date of the first premium payment")
            ),
            p("Any contract expected to result in a loss at inception must be identified as onerous and recognized immediately."),
            p("This early recognition ensures that losses are reported promptly, preventing their deferral to future reporting periods.")
        ),

        div(class = "module-section",
            h5("3. Insurance Acquisition Cash Flows (IACFs)"),
            p("Insurance acquisition cash flows refer to the costs associated with acquiring insurance contracts — such as commissions to brokers or agents."),
            p("When these acquisition costs are paid or received before the group of contracts is formally recognized, they are initially recorded as an asset or liability, depending on the nature of the cash flow."),
            p("These amounts are only temporary on the balance sheet and are to be reclassified once the group is recognized."),
            p("Once they are recognized, the acquisition costs are added to the group’s measurement as part of the fulfilment cash flows, and the initial asset or liability is derecognized."),
            p("This approach ensures that acquisition costs are properly matched to the group of contracts they relate to.")
        ),

        div(class = "module-section",
            h5("4. Contracts included in a group"),
            p("Only contracts that are issued on or before the reporting date can be included in a recognized group."),
            
            p("When recognizing the group, entities must also:"),
            tags$ol(type = "a",
              tags$li("Determine the discount rate that will be applied to adjust expected future cash flows to present value"),
              tags$li("Estimate the pattern of insurance service delivery, so that revenue can be appropriately allocated over the coverage period")
            ),
            
            p("Although contracts issued after the reporting date cannot be included retroactively, they may be added in a future period, but only in the period they are actually issued."),
            
            p("If this occurs, the insurer must:"),
            tags$ol(type = "a",
              tags$li("Reassess and update the discount rate used"),
              tags$li("Apply the revised rate retrospectively from the start of the reporting period in which the new contracts are added")
            )
        ),


        div(class = "module-section",
            h3("📝 Quiz: Answer the following questions to test your understanding of Recognition of Insurance Contracts.", class = "section-subheading"),
        ),        

        # UI boxes for Module 4 questions
        box(
          title = "1. When must a group of insurance contracts be recognized at the latest?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q1"), label = NULL, choices = c(
            "When the group becomes onerous, if earlier than other conditions",
            "When the financial statements are finalized",
            "After the coverage period ends",
            "At the insurer’s discretion"
          ), selected = character(0))
        ),

        box(
          title = "2. Which is not one of the conditions triggering initial recognition of a group of contracts?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q2"), label = NULL, choices = c(
            "Beginning of coverage period",
            "First premium payment due",
            "End of coverage period",
            "When the group is onerous"
          ), selected = character(0))
        ),

        box(
          title = "3. What happens if a contract is identified as onerous at inception?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q3"), label = NULL, choices = c(
            "Recognition can be delayed",
            "It must be recognized immediately",
            "The contract is canceled",
            "No special accounting is needed"
          ), selected = character(0))
        ),

        box(
          title = "4. What are Insurance Acquisition Cash Flows (IACFs)?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q4"), label = NULL, choices = c(
            "Costs of acquiring insurance contracts, e.g., commissions",
            "Premiums paid by policyholders",
            "Future claim payments",
            "Reinsurance recoveries"
          ), selected = character(0))
        ),

        box(
          title = "5. How are IACFs treated before group recognition?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q5"), label = NULL, choices = c(
            "Expensed immediately",
            "Ignored until contract matures",
            "Added to premiums",
            "Recorded as asset or liability"
          ), selected = character(0))
        ),

        box(
          title = "6. What happens to IACFs once the group is recognized?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q6"), label = NULL, choices = c(
            "They remain on the balance sheet",
            "They are derecognized and included in fulfilment cash flows",
            "They are converted to revenue",
            "They are written off"
          ), selected = character(0))
        ),

        box(
          title = "7. Which contracts can be included in a group for reporting purposes?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q7"), label = NULL, choices = c(
            "Only those issued after the reporting date",
            "All contracts, regardless of issuance date",
            "Only those issued before or on the reporting date",
            "Only those with premiums already received"
          ), selected = character(0))
        ),

        box(
          title = "8. Why is the discount rate important in recognition?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q8"), label = NULL, choices = c(
            "It determines the insurer’s profit",
            "It calculates taxes",
            "It sets premium levels",
            "It adjusts expected cash flows to present value"
          ), selected = character(0))
        ),

        box(
          title = "9. If contracts are issued after the reporting date, when are they added to a group?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q9"), label = NULL, choices = c(
            "Only in the period they are issued",
            "Retroactively to the prior period",
            "When premiums are fully paid",
            "At the insurer’s discretion"
          ), selected = character(0))
        ),

        box(
          title = "10. What must happen if new contracts are added to a group after reporting?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q10"), label = NULL, choices = c(
            "Recognize them as separate contracts",
            "No changes required",
            "Reassess discount rate and apply retrospectively from start of period",
            "Ignore the change"
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


# Correct answers and explanations for Module 4
correct_answers_module4 <- list(
  q1 = list(
    answer     = "When the group becomes onerous, if earlier than other conditions",
    explanation= "Recognition occurs at the earliest of coverage start, first payment due, or when the group becomes onerous."
  ),
  q2 = list(
    answer     = "End of coverage period",
    explanation= "Initial recognition cannot occur after the coverage period ends."
  ),
  q3 = list(
    answer     = "It must be recognized immediately",
    explanation= "Onerous contracts must be recognized immediately to avoid deferring losses."
  ),
  q4 = list(
    answer     = "Costs of acquiring insurance contracts, e.g., commissions",
    explanation= "IACFs cover acquisition costs such as broker or agent commissions."
  ),
  q5 = list(
    answer     = "Recorded as asset or liability",
    explanation= "Before group recognition, IACFs are temporarily recorded on the balance sheet."
  ),
  q6 = list(
    answer     = "They are derecognized and included in fulfilment cash flows",
    explanation= "On recognition, acquisition cash flows are added to fulfilment cash flows and removed as an asset/liability."
  ),
  q7 = list(
    answer     = "Only those issued before or on the reporting date",
    explanation= "Only contracts issued up to the reporting date can be included."
  ),
  q8 = list(
    answer     = "It adjusts expected cash flows to present value",
    explanation= "The discount rate ensures future cash flows are stated at present value."
  ),
  q9 = list(
    answer     = "Only in the period they are issued",
    explanation= "New contracts can only be added in the period they are actually issued."
  ),
  q10 = list(
    answer     = "Reassess discount rate and apply retrospectively from start of period",
    explanation= "If new contracts are added after reporting, the discount rate must be updated retrospectively."
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
    to_module_5 <- reactive(input$to_module_5)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_5,
      progress_trigger = progress_saved_trigger
    ))
  })
})