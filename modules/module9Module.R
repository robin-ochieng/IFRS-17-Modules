# This module is for the IFRS 17 module 10
IFRS17Module9UI <- function(id) {
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
      h2("📘 Module 9: Premium Allocation Approach", class = "section-title-top")
    ),
    div(class = "module-section",  
        h3("📖 Introduction"),
        p("This module provides an in-depth overview of the Premium Allocation Approach (PAA) as per IFRS 17 paragraphs 53 to 59."),
        p("The Premium Allocation Approach (PAA) is a simplified measurement model for insurance contract liabilities under IFRS 17."),
        p("It is typically used for short-duration contracts (generally 12 months or less) such as motor, travel, health, and other general insurance products.")
     ),
    div(class = "module-section",  
        p("PAA is similar in concept to the unearned premium reserve (UPR) approach under IFRS 4."),
        p("Contracts must be grouped into portfolios with similar risk and management practices"),
        p("Then further split into:"),
        tags$ol(
            type = "a",
            tags$li("Onerous at inception"),
            tags$li("No significant risk of becoming onerous"),
            tags$li("Profitable contracts")
        ),
        p("Contracts issued more than 12 months apart cannot be grouped together.")        
     ),

    div(class = "module-section",
        h3("📋 Eligibility Criteria"),
        p("According to the standard, PAA can be applied under these two conditions:"),
        tags$ol(
            type = "a",
            tags$li("If the coverage period of each contract in the group is one year or less, or"),
            tags$li("If the entity reasonably expects that using the PAA would produce a measurement of the liability for remaining coverage (LRC) that does not differ materially from the General Measurement Model (GMM).")
        ),
        img(src = "images/paaImage.png", class = "module-image")
    ),
  
    div(class = "module-section",
        h3("📐 Measurement Under PAA"),
        p("There are two components:"),
        
        tags$ol(
            # 1. Liability for Remaining Coverage
            tags$li(
                strong("Liability for Remaining Coverage (LRC)"),
                tags$ol(
                    type = "a",
                    tags$li("Opening LRC balance: Starting point for the period."),
                    tags$li("Add: Premium Received – Additional premiums collected during the period."),
                    tags$li("Less: Amortization of Insurance Time value of money and financial risks: Amortized acquisition costs deducted."),
                    tags$li("Less: Insurance Revenue – Revenue recognized over time as insurance services are provided.")
                ),
                tags$ul(
                    tags$li("LRC = Opening LRC + Premium received – Earned Premium – Change in DAC"),
                    tags$li("Premium received = GWP + Prior premium receivables – Current premium receivables")
                )
            ),
            
            # 2. Liability for Incurred Claims
            tags$li(
                strong("Liability for Incurred Claims (LIC)"),
                tags$ol(
                    type = "a",
                    tags$li("Measured similarly to GMM:"),
                    tags$ul(
                        tags$li("Present value of future cash flows"),
                        tags$li("Risk adjustment for non-financial risk")
                    )
                )
            )
        )
    ),

    div(class = "module-section",
        h3("💰 Acquisition Cashflow Treatment"),
        p("Entities can choose to ",
          tags$a(href = "#", "expense"), 
          " acquisition costs immediately if the coverage period of contracts is one year or less."),
        p("Otherwise, acquisition costs can be deferred and amortized over the coverage period."),
        p("Deferring acquisition costs reduces LRC but may increase loss recognition for onerous contracts.")
    ),

    div(class = "module-section",
        h3("📉 Discounting and Risk Adjustment"),
        p("Discounting of LRC is not required unless there is a significant time lag between receiving the premium and providing services."),
        p("Risk adjustment is only applicable to the liability for claims incurred, not LRC.")
    ),

    div(class = "module-section",
        h3("⚠️ Onerous Contracts"),
        p("Even under PAA, entities must assess whether contracts are onerous at initial recognition or subsequently."),
        p("If onerous, a loss component must be recognized immediately in P&L.")
    ),

    div(class = "module-section",
        h3("💵 Revenue Recognition"),
        p("Revenue is recognized over the coverage period in line with the pattern of transfer of services (typically straight-line).")
    ),

    div(class = "module-section",
        h3("⚖️ Comparison of PAA and GMM"),
        tags$div(class = "table-responsive",
        tags$table(class = "comparison-table",
            tags$thead(
                tags$tr(
                    tags$th("Feature"),
                    tags$th("PAA"),
                    tags$th("GMM")
                )
            ),
            tags$tbody(
                tags$tr(
                    tags$td("Complexity"),
                    tags$td("Low"),
                    tags$td("High")
                ),
                tags$tr(
                    tags$td("Intended for"),
                    tags$td("Short-term contracts"),
                    tags$td("All types")
                ),
                tags$tr(
                    tags$td("Discounting"),
                    tags$td("Usually not required for LRC"),
                    tags$td("Required")
                ),
                tags$tr(
                    tags$td("Risk Adjustment"),
                    tags$td("Only for incurred claims"),
                    tags$td("Required for all liabilities")
                ),
                tags$tr(
                    tags$td("Onerous contract test"),
                    tags$td("Required"),
                    tags$td("Required")
                )
            )
        )
      )
    ),


    div(class = "module-section",
        tags$h4("📑 Disclosure Requirements"),
        p("IFRS 17 requires:"),
        tags$ol(
            type = "a",
            tags$li("Clear presentation of revenue, incurred claims, and movements in liabilities."),
            tags$li("Disclosure of confidence levels used in measuring liabilities."),
            tags$li("OCI option for presenting changes in discount rates.")
        )
    ),

    div(class = "module-section",
        h3("🧪  Practical Application Examples"),
        p("Ideal for group life, group credit, and general insurance with short coverage."),
        p("Can also apply to reinsurance contracts held, provided the same eligibility rules are met.")
    ),

      box(
        title = "Answer the following questions to test your understanding of remium Allocation Approach.",
        status = "white", solidHeader = TRUE, width = 12,
        p("Please enter your name."),
        textInput(ns("participant_name"), "Enter your Name:")
      ),

      box(
        title = "1. When is an entity allowed to apply the Premium Allocation Approach (PAA)?",
        status = "white", solidHeader = TRUE, width = 12,
        radioButtons(ns("q1"), label = NULL, choices = c(
          "Only for life insurance contracts",
          "For all investment contracts",
          "If the contract duration is ≤12 months or if results are similar to GMM",
          "For contracts with no risk adjustment"
        ), selected = character(0))
      ),

      box(
        title = "2. What does the liability for remaining coverage (LRC) under PAA represent?",
        status = "white", solidHeader = TRUE, width = 12,
        radioButtons(ns("q2"), label = NULL, choices = c(
          "Future claims paid",
          "Present value of premiums",
          "The unearned portion of premiums minus acquisition costs",
          "Incurred claims"
        ), selected = character(0))
      ),

      box(
        title = "3. Which of the following requires risk adjustment under PAA?",
        status = "white", solidHeader = TRUE, width = 12,
        radioButtons(ns("q3"), label = NULL, choices = c(
          "Liability for incurred claims",
          "Acquisition cost asset",
          "Liability for remaining coverage",
          "Premium receivable"
        ), selected = character(0))
      ),

      box(
        title = "4. What happens if the liability for remaining coverage is lower than fulfilment cash flows?",
        status = "white", solidHeader = TRUE, width = 12,
        radioButtons(ns("q4"), label = NULL, choices = c(
          "Create a contractual service margin",
          "Defer acquisition costs",
          "Recognize a loss",
          "Discount more"
        ), selected = character(0))
      ),

      box(
        title = "5. What are fulfilment cash flows made up of?",
        status = "white", solidHeader = TRUE, width = 12,
        radioButtons(ns("q5"), label = NULL, choices = c(
          "Future premiums only",
          "Future claims and profits",
          "Expected future inflows and outflows, discounted, plus risk adjustment",
          "Written premium minus expenses"
        ), selected = character(0))
      ),

      box(
        title = "6. How is insurance revenue recognized under PAA?",
        status = "white", solidHeader = TRUE, width = 12,
        radioButtons(ns("q6"), label = NULL, choices = c(
          "All at inception",
          "When claims are paid",
          "Evenly over the coverage period",
          "At contract expiry"
        ), selected = character(0))
      ),

      box(
        title = "7. Can insurers offset profitable and onerous contracts within a portfolio under PAA?",
        status = "white", solidHeader = TRUE, width = 12,
        radioButtons(ns("q7"), label = NULL, choices = c(
          "No, grouping rules prevent offsetting",
          "Only with auditor approval",
          "Yes",
          "Only for reinsurance"
        ), selected = character(0))
      ),

      box(
        title = "8. What is a key disclosure requirement under IFRS 17 even when using PAA?",
        status = "white", solidHeader = TRUE, width = 12,
        radioButtons(ns("q8"), label = NULL, choices = c(
          "No disclosure required",
          "Confidence level of liabilities",
          "Market value of assets",
          "Tax provision for each contract"
        ), selected = character(0))
      ),

      box(
        title = "9. Can PAA be used for reinsurance contracts held?",
        status = "white", solidHeader = TRUE, width = 12,
        radioButtons(ns("q9"), label = NULL, choices = c(
          "Yes, if eligibility criteria are met",
          "No, PAA is only for direct contracts",
          "Yes, but only in life insurance",
          "Only if premiums exceed claims"
        ), selected = character(0))
      ),

      box(
        title = "10. What happens when acquisition costs are deferred for an onerous group?",
        status = "white", solidHeader = TRUE, width = 12,
        radioButtons(ns("q10"), label = NULL, choices = c(
          "The loss reduces",
          "It offsets the fulfilment cash flows",
          "It increases the recognized loss",
          "It increases future profits"
        ), selected = character(0))
      ),
    

      actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
      br(), 
      br(),
      uiOutput(ns("result")),  

    div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_10"),
          label = tagList(icon("arrow-right"), "Next: Module 10 - Reinsurance Contracts Held"),
          class = "control-button-tavnav"
      )
    ) 
    )
}

  correct_answers_module9 <- list(
    q1 = list(
      answer = "If the contract duration is ≤12 months or if results are similar to GMM",
      explanation = "PAA can be used if the coverage period is 12 months or less, or if using PAA would yield results that are not materially different from the General Measurement Model (GMM)."
    ),
    q2 = list(
      answer = "The unearned portion of premiums minus acquisition costs",
      explanation = "LRC under PAA reflects the simplified unearned premium approach, adjusted for amortized acquisition costs."
    ),
    q3 = list(
      answer = "Liability for incurred claims",
      explanation = "The risk adjustment under PAA is only applied to the liability for incurred claims, to account for uncertainty in non-financial risk."
    ),
    q4 = list(
      answer = "Recognize a loss",
      explanation = "If fulfilment cash flows exceed the liability for remaining coverage, the contract is deemed onerous and the excess must be recognized as a loss."
    ),
    q5 = list(
      answer = "Expected future inflows and outflows, discounted, plus risk adjustment",
      explanation = "Fulfilment cash flows under IFRS 17 reflect the present value of expected future inflows and outflows, with a risk adjustment for non-financial risk."
    ),
    q6 = list(
      answer = "Evenly over the coverage period",
      explanation = "Under PAA, revenue is recognized as insurance services are provided, typically on a straight-line basis over the coverage period."
    ),
    q7 = list(
      answer = "No, grouping rules prevent offsetting",
      explanation = "IFRS 17 requires separate grouping of onerous and profitable contracts; losses cannot be offset by profitable ones."
    ),
    q8 = list(
      answer = "Confidence level of liabilities",
      explanation = "Disclosure of the confidence level used to determine the risk adjustment is required, even under the simplified PAA model."
    ),
    q9 = list(
      answer = "Yes, if eligibility criteria are met",
      explanation = "PAA can be applied to reinsurance contracts held if the contract meets the same criteria as direct contracts."
    ),
    q10 = list(
      answer = "It increases the recognized loss",
      explanation = "Deferring acquisition costs reduces the LRC, which may increase the difference from fulfilment cash flows, leading to a higher recognized loss."
    )
  )


IFRS17Module9Server <- (function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Quiz result output
    final_name <- reactiveVal("")

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

    for (qid in names(correct_answers_module9)) {
      correct <- correct_answers_module9[[qid]]$answer
      explanation <- correct_answers_module9[[qid]]$explanation
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
      total_questions <- length(correct_answers_module9)
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
    to_module_10 <- reactive(input$to_module_10)
    # return it so the app can observe it
    to_module_10
  })
})