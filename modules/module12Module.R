# This module is for the IFRS 17 module 13
IFRS17Module12UI <- function(id) {
  ns <- NS(id)
  logo_bar <- fluidRow(
    class = "logo-bar",                     # you’ll style this in CSS
    column(
      width = 12,
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
        h3("📖 Introduction"),
        p("This module provides an in-depth overview of the modification and derecognition requirements for insurance contracts, as set out in paragraphs 72–77 of IFRS 17."),
        p("Modification is a change in the terms of an insurance contract after it has been initially recognized. These changes may result from mutual agreement between the insurer and policyholder, or as a result of changes in regulation.")
    ),  


    div(class = "module-section",
        h3("📉 When a Contract is Derecognized"),
        p("Under IFRS 17, an insurer must ", strong("derecognize the original contract and recognize a new one"), " if and only if:"),
        tags$ol(type = "a",
          tags$li("Modification terms were included at inception."),
          tags$li("The contract is no longer within IFRS 17 scope."),
          tags$li("The contract boundary changes significantly."),
          tags$li("The contract moves to a different group (e.g., new cohort or portfolio, or becomes onerous)."),
          tags$li("The modified contract now includes distinct non-insurance components (e.g., goods/services)."),
          tags$li("It no longer qualifies for the Premium Allocation Approach (PAA)."),
          tags$li("It gains or loses direct participation features.")
        ),
        p("If none of the above apply, treat the change as an update to expected cash flows. These changes are accounted for using normal subsequent measurement rules under IFRS 17."),
        p(strong("A contract is derecognized if:")),
        tags$ol(type = "a",
          tags$li("The insurer has no further obligation to the policyholder (e.g., it expired, was cancelled or fully settled), or"),
          tags$li("The modification criteria for derecognition (above) are met.")
        )
    ),

    div(class = "module-section",
        h3("💼 Accounting Treatment for Derecognition"),

        tags$ol(type = "a",
            tags$li(
                strong("Derecognition of a contract from within a group"),
                tags$ul(
                    tags$li("Remove the contract’s share of present value of future cash flows and risk adjustment."),
                    tags$li("Adjust the Contractual Service Margin (CSM), to reflect the resulting change in fulfilment cash flows, where applicable."),
                    tags$li("Update the coverage units and profit recognition based on the revised number of units.")
                )
            ),
            tags$li(
                strong("Derecognition from Modification or Transfer"),
                tags$ul(
                    tags$li("Adjust the CSM based on the difference between:"),
                    tags$ul(
                        tags$li("The change in carrying amount, and"),
                        tags$li("The premium charged by a third party (if transferred), or"),
                        tags$li("The premium the insurer would have charged for the modified contract")
                    )
                )
            )
        ),

        tags$ul(
            tags$li("Recognize the new contract as if that calculated premium had been received on the modification date.")
        ),
        img(src = "images/modificationaandDerecognition.png", alt = "Modification and Derecognition", class = "module-image")
    ),

    box(
      title = "Answer the following questions to test your understanding of Modification and Decognition of insurance contracts.",
      status = "white", solidHeader = TRUE, width = 12,
      p("Please enter your name."),
      textInput(ns("participant_name"), "Enter your Name:")
    ),

    box(
      title = "1. When is a contract considered modified under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q1"), label = NULL, choices = c(
        "When it is extended",
        "When contractual cash flows change",
        "When insurer and policyholder agree on new terms",
        "When premiums change"
      ), selected = character(0))
    ),

    box(
      title = "2. What is the first step when assessing a contract modification under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q2"), label = NULL, choices = c(
        "Recognize as new contract",
        "Adjust the CSM",
        "Assess whether modification is substantial",
        "Update the risk adjustment"
      ), selected = character(0))
    ),

    box(
      title = "3. If a contract modification results in substantially different terms, what is the accounting treatment?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Adjust liability only",
        "Derecognize old and recognize new contract",
        "Adjust insurance revenue",
        "Disclose in notes only"
      ), selected = character(0))
    ),

    box(
      title = "4. What is the impact on CSM if a modification is not substantial?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q4"), label = NULL, choices = c(
        "It is reversed",
        "It is remeasured",
        "It is released to profit",
        "It is written off"
      ), selected = character(0))
    ),

    box(
      title = "5. Under IFRS 17, what causes derecognition of an insurance contract?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q5"), label = NULL, choices = c(
        "Claims payment",
        "Expiry of coverage",
        "Settlement or cancellation",
        "Change in accounting policy"
      ), selected = character(0))
    ),

    box(
      title = "6. Which of the following changes is considered substantial?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q6"), label = NULL, choices = c(
        "Adding a new coverage type",
        "Change in billing address",
        "Change in payment date",
        "Update to claims contact"
      ), selected = character(0))
    ),

    box(
      title = "7. How is the carrying amount of the derecognized contract treated?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q7"), label = NULL, choices = c(
        "It is capitalized",
        "It is transferred to reserves",
        "It is removed from the balance sheet",
        "It is restated"
      ), selected = character(0))
    ),

    box(
      title = "8. How is a new contract initially recognized?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Based on old values",
        "Using fair value",
        "Using fulfilment cash flows at date of modification",
        "Not recognized separately"
      ), selected = character(0))
    ),

    box(
      title = "9. How are derecognised contracts due to full settlement treated?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q9"), label = NULL, choices = c(
        "CSM is amortized",
        "Recognize gain/loss",
        "Asset revaluation",
        "Insurance revenue restated"
      ), selected = character(0))
    ),

    box(
      title = "10. What is the primary difference between substantial and non-substantial modifications?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q10"), label = NULL, choices = c(
        "Impact on reinsurance",
        "Change in timing of premium",
        "Need for derecognition",
        "Claims experience"
      ), selected = character(0))
    ),

    box(
      title = "11. Which of the following is NOT a reason for derecognition?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q11"), label = NULL, choices = c(
        "Contract lapses",
        "Contract is modified substantially",
        "Policy is cancelled",
        "Policyholder pays premium early"
      ), selected = character(0))
    ),

    box(
      title = "12. What is the derecognition criteria under IFRS 17 for insurance contract liabilities?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q12"), label = NULL, choices = c(
        "Legal cancellation",
        "Transfer to another insurer",
        "Extinguishment of obligation",
        "Policyholder request"
      ), selected = character(0))
    ),

    box(
      title = "13. What must be disclosed upon derecognition of a contract?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q13"), label = NULL, choices = c(
        "Nothing",
        "Reason for derecognition and financial impact",
        "Transition adjustments",
        "Future premiums"
      ), selected = character(0))
    ),

    box(
      title = "14. Which modification would not be considered substantial?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q14"), label = NULL, choices = c(
        "Adding a new benefit",
        "Removing a major risk cover",
        "Changing claim limits significantly",
        "Changing policyholder address"
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
    answer = "When contractual cash flows change",
    explanation = "A contract is modified when the terms change in a way that affects fulfilment cash flows, not just administrative details."
  ),
  q2 = list(
    answer = "Assess whether modification is substantial",
    explanation = "Before deciding the accounting treatment, the insurer must assess if the modification significantly changes the contract terms."
  ),
  q3 = list(
    answer = "Derecognize old and recognize new contract",
    explanation = "If the modification leads to substantially different terms, the original contract is derecognized, and a new one is recognized."
  ),
  q4 = list(
    answer = "It is remeasured",
    explanation = "If the modification is not substantial, the Contractual Service Margin (CSM) is adjusted (remeasured) without derecognition."
  ),
  q5 = list(
    answer = "Settlement or cancellation",
    explanation = "Derecognition occurs when the insurer’s obligation ends, such as through settlement, cancellation, or expiration."
  ),
  q6 = list(
    answer = "Adding a new coverage type",
    explanation = "Adding a new coverage type alters the risk profile, which is considered a substantial modification under IFRS 17."
  ),
  q7 = list(
    answer = "It is removed from the balance sheet",
    explanation = "Upon derecognition, the contract liability is derecognized and removed from the statement of financial position."
  ),
  q8 = list(
    answer = "Using fulfilment cash flows at date of modification",
    explanation = "The new contract is measured based on fulfilment cash flows as at the modification date."
  ),
  q9 = list(
    answer = "Recognize gain/loss",
    explanation = "Derecognition from full settlement leads to recognizing a gain or loss from the difference in cash flows."
  ),
  q10 = list(
    answer = "Need for derecognition",
    explanation = "Substantial modifications require derecognition; non-substantial ones do not."
  ),
  q11 = list(
    answer = "Policyholder pays premium early",
    explanation = "Early premium payment does not end the insurer’s contractual obligation and thus is not a derecognition trigger."
  ),
  q12 = list(
    answer = "Extinguishment of obligation",
    explanation = "A contract liability is derecognized when the insurer’s obligation to the policyholder is fully extinguished."
  ),
  q13 = list(
    answer = "Reason for derecognition and financial impact",
    explanation = "IFRS 17 requires disclosure of the reasons for derecognition and its impact on the financial statements."
  ),
  q14 = list(
    answer = "Changing policyholder address",
    explanation = "Administrative changes like address updates are not substantial modifications affecting the contract terms."
  )
)



IFRS17Module12Server <- (function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    


    # Quiz result output
    final_name <- reactiveVal("")

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
        
        # 4. (Optional) also check name
        if (is.null(input$participant_name) || input$participant_name == "") {
          showModal(modalDialog(
            title   = "Participant Name Required",
            "Please enter your name before you submit the quiz.",
            easyClose = TRUE,
            footer    = modalButton("OK")
          ))
          return()
        }
        
        # ———————————
        # 5. All answered: clear any existing modal, then run your scoring code
        removeModal()

        final_name(input$participant_name)


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
      total_questions <- length(correct_answers_module12)
      percentage       <- round((score() / total_questions) * 100, 1)
      name             <- isolate(input$participant_name)
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
          h2(isolate(input$participant_name),
            style = "
              font-family: 'Nunito', sans-serif;
              font-size: 28px;
              margin: 0;
              color: #198754;
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
              "📊 Results Summary",
              style = "color:#0d6efd; font-weight:600; margin-bottom:20px;"
            ),

            HTML(paste0(
              "<p style='font-size:17px;'><strong>👤 Participant:</strong> ", name, "</p>",
              "<hr style='border-top:1px solid #dee2e6;'>",
              "<p style='font-size:18px;'><strong>Total Score:</strong> ", score(), " / ", total_questions, "</p>",
              "<p style='font-size:18px;'><strong>Percentage Score:</strong> <span style='color:", color, "; font-weight:600;'>", percentage, "%</span></p>"
            )),

            # ——— Detailed Feedback ———
            div(
              style = "margin-top:25px;",
              h4(
                "📘 Detailed Feedback",
                style = "margin-bottom:15px; color:#343a40;"
              ),
              tags$ul(
                lapply(feedback, function(msg) {
                  tags$li(style = "margin-bottom:8px;", HTML(msg))
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

    # return it so the app can observe it
    to_module_13
  })
})