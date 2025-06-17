# This module is for the IFRS 17 module 11
IFRS17Module10UI <- function(id) {
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
      h2("📘 Module 10: Reinsurance Contracts Held", class = "section-title-top")
    ),
    div(class = "module-section",  
        h3("📖 Introduction"),
        p("This module provides an in-depth overview of the Reinsurance contracts held as per ", strong("IFRS 17"), " paragraphs 60 to 70."),
        
        p("Reinsurance contract held refers to a contract under which one entity (the cedant) receives compensation from another entity (the reinsurer) for one or more claims arising from insurance contracts it has issued."),
        
        p("Reinsurance contracts held are measured independently from the underlying insurance contracts issued."),
        
        p("The ", em('"mirroring approach"'), " used under IFRS 4 is no longer applied. Instead, IFRS 17 requires a stand-alone assessment of reinsurance contracts held."),
        
        p("We account for reinsurance contracts held to:"),
        
        tags$ul(
            tags$li("a) Reflect the risk mitigation effect in financial statements."),
            tags$li("b) Show consistent measurement with underlying insurance contracts."),
            tags$li("c) Recognize gains or losses on risk transfer at initial recognition.")
        )
    ),
    div(class = "module-section",
        h3("📏 Measurement of Reinsurance Contracts Held"),
        
        h4(style = "color: #7BAF34;", "Initial Recognition"),
        
        p("At initial recognition, the reinsurance contract is measured using the General Measurement Model (GMM) unless the Premium Allocation Approach (PAA) is applied and meets the eligibility criteria."),
        
        p("The measurement components include:"),
        
        tags$ul(
            tags$li("a) Fulfilment cash flows, which are the expected present value of future inflows and outflows related to the reinsurance contract, adjusted for the time value of money and the risk of counterparty default."),
            tags$li("b) A risk adjustment, which reflects the uncertainty in the amount and timing of the cash flows from the reinsurer."),
            tags$li("c) A Contractual Service Margin (CSM), which is established if the present value of future inflows from the reinsurer exceeds the ceded premiums. This CSM represents the unearned gain and is deferred and recognized as income over the coverage period."),
            tags$li("d) Loss-recovery component if the underlying contracts are onerous.")
        )
    ),
    div(class = "module-section",
        h4(style = "color: #7BAF34;", "Subsequent Measurement"),
        
        p("After initial recognition, reinsurance contracts held are remeasured at each reporting date. The measurement continues to follow the GMM (or PAA if applicable), which requires updating the key components which include:"),
        
        tags$ul(
            tags$li("a) Fulfilment cash flows"),
            tags$li("b) Risk adjustment"),
            tags$li("c) CSM"),
            tags$li("d) Loss-recovery component"),
            tags$li("e) Experience adjustments"),
            tags$li("f) Changes in discount rates")
        ),
        p("If a group of insurance contracts is onerous and there's a related reinsurance contract, the cedant recognizes a recovery asset known as loss-recovery component."),
    ),
    div(class = "module-section",
        h3("Presentation in Financial Statements"),
        p("Assets and liabilities from reinsurance contracts are presented separately from insurance contracts issued."),
        p("Revenue and expenses from reinsurance are also presented separately in the statement of profit or loss."),
        p("Income and expenses from reinsurance are shown separately from insurance contracts issued per IFRS 17 disclosure requirements.")
    ),
    div(class = "module-section",
        h3("Disclosures"),
        p("Entities must disclose:"),
        tags$ul(
            tags$li("a) An explanation of how reinsurance contracts affect the amounts in financial statements"),
            tags$li("b) Significant judgments made in applying IFRS 17 to reinsurance."),
            tags$li("c) Reconciliations of opening and closing balances of assets and liabilities.")
        )
    ),
    div(class = "module-section",
        h3("Disclosures"),
        img(src = "images/reinsuranceContractHeld.png", class = "module-image")
    ),  

    box(
      title = "Answer the following questions to test your understanding of Reinsurance Contracts Held.",
      status = "white", solidHeader = TRUE, width = 12,
      p("Please enter your name."),
      textInput(ns("participant_name"), "Enter your Name:")
    ),

    box(
      title = "1. What is a reinsurance contract held under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q1"), label = NULL, choices = c(
        "A contract under which an entity receives compensation for claims from a reinsurer",
        "A contract issued to share profits with partners",
        "Contract issued to insure customers",
        "A contract for investment-linked business"
      ), selected = character(0))
    ),

    box(
      title = "2. When should a reinsurance contract held be initially recognized?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q2"), label = NULL, choices = c(
        "When the reinsurer pays a claim",
        "At the start of the underlying insurance contract",
        "At the earlier of coverage start or when underlying contracts are onerous",
        "At the end of the reporting period"
      ), selected = character(0))
    ),

    box(
      title = "3. Can a gain on purchase of reinsurance be recognized immediately?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Yes, it boosts profit",
        "No, it is included in the CSM",
        "Only if the reinsurer agrees",
        "Yes, under PAA"
      ), selected = character(0))
    ),

    box(
      title = "4. Which of the following is NOT included in fulfilment cash flows for reinsurance contracts held?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q4"), label = NULL, choices = c(
        "Future claims recoveries",
        "Discounting",
        "Reinsurer’s risk appetite",
        "Risk adjustment"
      ), selected = character(0))
    ),

    box(
      title = "5. What is the impact of reinsurance on the insurer’s risk exposure?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q5"), label = NULL, choices = c(
        "Increases risk",
        "No impact",
        "Transfers and reduces risk",
        "Creates an additional liability"
      ), selected = character(0))
    ),

    box(
      title = "6. How are changes in fulfilment cash flows for reinsurance contracts treated?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q6"), label = NULL, choices = c(
        "Adjust the CSM or go through P&L",
        "Ignore until contract maturity",
        "Expensed as acquisition costs",
        "Deferred indefinitely"
      ), selected = character(0))
    ),

    box(
      title = "7. Under the General Model, what happens to the CSM for reinsurance contracts held over time?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q7"), label = NULL, choices = c(
        "It grows with claims paid",
        "It’s released based on services received",
        "It remains constant",
        "It is immediately expensed"
      ), selected = character(0))
    ),

    box(
      title = "8. How are reinsurance recoveries presented in the income statement?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Included in insurance revenue",
        "Included in investment income",
        "Separately from insurance revenue",
        "Net of insurance service expenses"
      ), selected = character(0))
    ),

    box(
      title = "9. How are recoveries for past claims treated under reinsurance contracts held?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q9"), label = NULL, choices = c(
        "Deferred in CSM",
        "Expensed as incurred",
        "Recognized in profit or loss immediately",
        "Deducted from LRC"
      ), selected = character(0))
    ),

    box(
      title = "10. What is the impact of a reinsurance CSM being negative?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q10"), label = NULL, choices = c(
        "It represents a loss",
        "It is a liability",
        "It is not allowed",
        "It’s treated as an asset, not a liability"
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
    answer = "A contract under which an entity receives compensation for claims from a reinsurer",
    explanation = "A reinsurance contract held is one where the insurer (cedant) transfers insurance risk and receives compensation from the reinsurer for claims."
  ),
  q2 = list(
    answer = "At the earlier of coverage start or when underlying contracts are onerous",
    explanation = "Recognition occurs at the earlier of when reinsurance coverage begins or when the reinsurance covers a recognized loss from onerous contracts."
  ),
  q3 = list(
    answer = "No, it is included in the CSM",
    explanation = "Gains on the purchase of reinsurance are deferred within the Contractual Service Margin (CSM) and recognized over the coverage period."
  ),
  q4 = list(
    answer = "Reinsurer’s risk appetite",
    explanation = "Fulfilment cash flows include expected recoveries, discounting, and risk adjustment—not subjective elements like reinsurer's risk appetite."
  ),
  q5 = list(
    answer = "Transfers and reduces risk",
    explanation = "Reinsurance helps the insurer reduce and manage their insurance risk by transferring a portion of it to the reinsurer."
  ),
  q6 = list(
    answer = "Adjust the CSM or go through P&L",
    explanation = "Changes in fulfilment cash flows adjust the CSM if they relate to future services, or are recognized in profit or loss otherwise."
  ),
  q7 = list(
    answer = "It’s released based on services received",
    explanation = "The CSM for reinsurance contracts held is released over time based on the receipt of reinsurance services."
  ),
  q8 = list(
    answer = "Separately from insurance revenue",
    explanation = "IFRS 17 requires that reinsurance income and expenses be presented separately from insurance revenue and service expenses."
  ),
  q9 = list(
    answer = "Recognized in profit or loss immediately",
    explanation = "Recoveries for past claims are immediately recognized in profit or loss as they relate to events that have already occurred."
  ),
  q10 = list(
    answer = "It’s treated as an asset, not a liability",
    explanation = "A negative CSM on a reinsurance contract held represents a net cost to the insurer and is treated as an asset."
  )
)



IFRS17Module10Server <- (function(id, user_data) {
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
    to_module_11 <- reactive(input$to_module_11)
    # return it so the app can observe it
    to_module_11
  })
})