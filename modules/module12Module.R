# This module is for the IFRS 17 module 13
IFRS17Module12UI <- function(id) {
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
      h2("📘 Module 12: Modification and Decognition of insurance contracts", class = "section-title-top")
    ),

    div(class = "module-section",
        h3("📖 Introduction", class = "section-subtitle"),
        p("This module provides a **detailed overview of the requirements for modifying and derecognizing insurance contracts**, as per IFRS 17 (paragraphs 72–77)."),
        p("Modification refers to a change in the contractual terms of an insurance contract after it has been initially recognized. Such changes may result either from a mutual agreement between the insurer and the policyholder or as a result of changes in regulation.")
    ),


    div(class = "module-section",
        h3("🔄 When a Contract is Derecognized", class = "section-subtitle"),
        p("According to IFRS 17, an insurance contract must be derecognized and replaced with a new contract only if one or more of the following conditions are met:"),
        tags$ol(type = "a",
          tags$li("The modified contract now contains distinct non-insurance components, such as separate goods or services."),
          tags$li("The contract is no longer eligible to be measured using the PAA."),
          tags$li("The contract either acquires or loses the variable fee features."),
          tags$li("The contract included modification terms when the contract was initially issued."),
          tags$li("The contract no longer falls within the scope of IFRS 17."),
          tags$li("There are substantial changes to the contract boundary."),
          tags$li("The contract must be transferred to a different group —for example, a new cohort, portfolio, or due to it becoming onerous.")
        ),
        p("Changes that do not meet the specified conditions are treated as updates to expected future cash flows and are accounted for in line with IFRS 17 subsequent measurement requirements."),
        p(strong("When is a contract derecognized?")),
        tags$ol(type = "a",
          tags$li("Any of the above derecognition criteria are met; or"),
          tags$li("The insurer has fully discharged, cancelled, or the obligations to the policyholder have expired; or")
          # add further criteria here if needed
        )
    ),

    div(class = "module-section",
        h3("📋 Accounting Treatment for Derecognition", class = "section-subtitle"),
        
        ## a) Derecognition within a Group
        h4("a) Derecognition of a Contract Within a Group"),
        p("When a contract is removed from a group (e.g., due to expiry or cancellation), the insurer must:"),
        tags$ul(
          tags$li("Eliminate the contract’s portion of the present value of future cash flows and the associated risk adjustment."),
          tags$li("Recalculate coverage units and revise profit recognized, based on the updated number of contracts in the group."),
          tags$li("Reflect the change in fulfilment cash flows by updating the Contractual Service Margin (CSM), where applicable.")
        ),
        
        ## b) Derecognition due to Modification or Transfer
        h4("b) Derecognition Due to Modification or Transfer"),
        p("If derecognition results from a modification or a transfer to another entity:"),
        tags$ul(
          tags$li(strong("The CSM must be updated to reflect the difference between:"), 
            tags$ul(
              tags$li("The change in the carrying amount of the contract, and"),
              tags$li("Either the premium set by a third party (for a transfer), or"),
              tags$li("The premium the insurer would have required if issuing the modified contract as new.")
            )
          ),
          tags$li("Account for the modified contract as if this premium had been received on the modification date.")
        ),
        img(src = "images/modificationaandDerecognition.png", alt = "Modification and Derecognition", class = "module-image")
    ),

    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Modification and Decognition of Insurance Contracts."),
    ),

    box(
      title = "1. Under what conditions should a modified insurance contract be derecognized and replaced with a new contract?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "If the policyholder changes their payment frequency",
        "If there are substantial changes to the contract terms affecting scope, boundary, or group classification",
        "If the policyholder changes their preferred mode of communication",
        "If the contract experiences delays in premium payment"
      ), selected = character(0))
    ),

    box(
      title = "2. What must be done if a modified contract is recognized as a new contract?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "Recognize it as if issued on the date of modification",
        "Use the same measurement model as the old one",
        "Record a retrospective adjustment",
        "Keep the original contract in the books"
      ), selected = character(0))
    ),

    box(
      title = "3. What is the correct accounting treatment when a contract is derecognized due to modification?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "The existing balances are carried forward to the new contract",
        "Only the Contractual Service Margin (CSM) is retained",
        "The original contract balances (CSM, LRC, LIC) are removed and the modified contract is recognized as new",
        "No changes are made in accounting"
      ), selected = character(0))
    ),

    box(
      title = "4. What happens if a change in an insurance contract does not meet the derecognition criteria?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "The change is ignored for accounting purposes",
        "The contract is still derecognized as a precaution",
        "The change is reflected as a change in cash flow estimates",
        "The insurer must issue a new contract"
      ), selected = character(0))
    ),

    box(
      title = "5. Which of the following changes would NOT trigger derecognition of an insurance contract?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "The policyholder switches to a different communication language",
        "The contract boundary is significantly changed",
        "The contract is modified to include distinct non-insurance components",
        "The contract is moved to a different group due to becoming onerous"
      ), selected = character(0))
    ),

    box(
      title = "6. Which condition would lead to derecognition of a contract even without modification?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "Policyholder requests fewer policy documents",
        "Insurer no longer has obligations to the policyholder",
        "Policyholders update their address",
        "The insurer upgrades internal systems"
      ), selected = character(0))
    ),

    box(
      title = "7. What does Paragraph 72 of IFRS 17 NOT include as a reason for derecognition?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "Exclusion from IFRS 17 scope",
        "Change in contract boundary",
        "Movement to a different group",
        "Change in payment currency"
      ), selected = character(0))
    ),

    box(
      title = "8. What must an insurer do when removing a contract from a group due to expiry or cancellation?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Recalculate coverage units and revise profit",
        "Transfer the contract to a suspense account",
        "Add a new policy to replace it",
        "Record a gain immediately"
      ), selected = character(0))
    ),

    box(
      title = "9. When derecognizing a contract due to modification, how should the adjustment to the CSM be calculated?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "Based on the claims already paid",
        "As the difference between the old carrying amount and the premium for the modified or transferred contract",
        "Using the historical premium received",
        "Based on market value of the policy"
      ), selected = character(0))
    ),

    box(
      title = "10. What is the key difference in accounting when a contract modification requires derecognition versus when it does not?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "The risk adjustment is recalculated in both cases",
        "The insurer retains the original fulfilment cash flows in both cases",
        "In derecognition, the original contract is removed; in non-derecognition, only future cash flows are updated",
        "Both cases require reporting to the insurance supervisor"
      ), selected = character(0))
    ),



    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")), 

      div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_13"),
          label = tagList(icon("arrow-right"), "Next: Module 13 - Presentation in the Statement of Financial Position"),
          class = "control-button-tavnav"
      )
    )     
    )
}

correct_answers_module12 <- list(
  q1 = list(
    answer = "If there are substantial changes to the contract terms affecting scope, boundary, or group classification",
    explanation = "A modified contract must be derecognized and replaced only when changes are substantial under IFRS 17."
  ),
  q2 = list(
    answer = "Recognize it as if issued on the date of modification",
    explanation = "The new contract is recognized as if issued on the modification date, per IFRS 17 rules."
  ),
  q3 = list(
    answer = "The original contract balances (CSM, LRC, LIC) are removed and the modified contract is recognized as new",
    explanation = "Upon derecognition due to modification, all original balances are removed and the modified contract is treated as new."
  ),
  q4 = list(
    answer = "The change is reflected as a change in cash flow estimates",
    explanation = "If derecognition criteria are not met, modifications are accounted for as updates to future cash flows."
  ),
  q5 = list(
    answer = "The policyholder switches to a different communication language",
    explanation = "Administrative changes like language preference do not trigger derecognition."
  ),
  q6 = list(
    answer = "Insurer no longer has obligations to the policyholder",
    explanation = "A contract is derecognized when the insurer’s obligations end, such as upon settlement or expiry."
  ),
  q7 = list(
    answer = "Change in payment currency",
    explanation = "IFRS 17 paragraph 72 does not list currency changes as a derecognition criterion."
  ),
  q8 = list(
    answer = "Recalculate coverage units and revise profit",
    explanation = "When removing a contract from a group, coverage units and profit recognized must be updated."
  ),
  q9 = list(
    answer = "As the difference between the old carrying amount and the premium for the modified or transferred contract",
    explanation = "CSM adjustments on derecognition are based on the difference between carrying amount and new premium."
  ),
  q10 = list(
    answer = "In derecognition, the original contract is removed; in non-derecognition, only future cash flows are updated",
    explanation = "Substantial modifications lead to removal of the original contract; non-substantial ones only update cash flows."
  )
)



IFRS17Module12Server <- (function(id, user_data) {
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

    for (qid in names(correct_answers_module12)) {
      correct <- correct_answers_module12[[qid]]$answer
      explanation <- correct_answers_module12[[qid]]$explanation
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
      # Save progress for Module 12
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 2 specific calculations
        total_questions <- length(correct_answers_module12)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module12",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 12 Progress Saved!</strong><br>",
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
          print(paste("Module 12 progress save error:", e$message))
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
      total_questions <- length(correct_answers_module12)
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
              "📊 Module 12 Results Summary",
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
    to_module_13 <- reactive(input$to_module_13)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_13,
      progress_trigger = progress_saved_trigger
    ))
  })
})