# This module is for the IFRS 17 module 7
IFRS17Module6UI <- function(id) {
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
      h2("📘 Module 6: Subsequent Measurement", class = "section-title-top")
    ),

    div(class = "module-section",  
        h3("📖 Introduction"),
        p("This module provides an overview of the subsequent measurement requirements for insurance contracts, as set out in paragraphs 40–52 of IFRS 17."),
        p("It covers how entities update the carrying amount of insurance contracts over time, including updates to fulfilment cash flows, adjustments to the contractual service margin (CSM), and the recognition of insurance revenue and expenses.")
     ),
    div(class = "module-section",
        h3("Overview of Subsequent Measurement"),
        p("After initial recognition, insurance contract liabilities must be updated to reflect:"),
        tags$ol(type = "a",
          tags$li("Changes in estimates of future cash flows"),
          tags$li("Release of CSM based on coverage units"),
          tags$li("Changes in discount rates"),
          tags$li("Claims incurred and paid"),
          tags$li("Experience adjustments and risk changes"),
          tags$li("Amortization of insurance acquisition cash flows")
        ),
    ),

    div(class = "module-section",
        h3("Liability for Remaining Coverage (LRC)"),
        p("The LRC reflects the insurer’s obligation to provide coverage in the future. At each reporting date, it is updated for:"),
        tags$ol(type = "a",
          tags$li("Premiums received"),
          tags$li("Release of CSM as services are rendered"),
          tags$li("Changes in fulfilment cash flows relating to future service"),
          tags$li("Adjustments to risk adjustment")
        ),
        p(em("Note: The CSM is adjusted only for changes that relate to future service.")),
        p("The GMM and VFA both measure insurance contract liabilities based on fulfilment cash flows and a contractual service margin; however, VFA applies to contracts with direct participation features and adjusts the CSM to reflect changes in the insurer’s share of underlying items ")
    ),
    div(class = "module-section",
        p("The LRC will be adjusted as follows:"),
        tags$ol(type = "a",
          tags$li("Opening LRC balance: Starting point for the period."),
          tags$li("Changes due to claims and expenses"),
          tags$li("Time value of money and financial risks"),
          tags$li("Risk adjustment for non-financial risk"),
          tags$li("Contractual Service Margin (CSM)"),
          tags$li("Loss Component: Optional adjustments for losses in the loss component.")
        ),
    ),
    div(class = "module-section",
        h3("Adjusting the Contractual Service Margin (CSM)"),
        p("The CSM is adjusted for:"),
        tags$ol(type = "a",
          tags$li("Changes in fulfilment cash flows related to future service"),
          tags$li("Accretion of interest using the locked-in rate"),
          tags$li("Release to profit or loss based on coverage units")
        ),
        p("It is not adjusted for:"),
        tags$ol(type = "a",
          tags$li("Changes related to past or current service"),
          tags$li("Experience variances from prior periods")
        ),
        div(class = "info-box scenario-box",
          div(class = "scenario-calc-row",
            # Left column
            div(class = "scenario-col",
              tags$h4("📌 Illustration:"),
              tags$ol( type = "a",
                tags$li("Opening CSM: KES 150,000"),
                tags$li("Interest accretion: KES 5,000"),
                tags$li("Adjustment from change in future cash flows: KES -30,000"),
                tags$li("Release based on service provided: KES 20,000")
              )
            ),
            # Right column
            div(class = "calc-col",
              tags$h4("🧮 Calculation:"),
              tags$ul( type = "a",
                tags$li("Adjusted CSM = 150,000 + 5,000 - 30,000 - 20,000 = KES 105,000"),
                tags$li("If the change in future cash flow estimates and adjustment relating to future coverage exceeded the CSM after allowance of interest accretion and the excess amounts above CSM are recognised as losses in the Profit or Loss.")
              )
            )
          )
        )
      ),
    div(class = "module-section",      
        p("For contracts measured under the PAA, an entity shall measure the Liability for Remaining Coverage at the end of each subsequent reporting period as follows:"),
        tags$ol( type = "a",
          tags$li("Opening LRC balance"),
          tags$li("Add: Premium Received in the period"),
          tags$li("Less: Insurance acquisition cash flows"),
          tags$li("Add: any amounts relating to the amortization of insurance acquisition cash flows recognised as an expense in the reporting period."),
          tags$li("Add: any adjustment to a financing component."),
          tags$li("Less: Insurance Revenue recognised for coverage provided during the period.")
        ),
        img(src = "images/subsequent_lrc.png", class = "module-image"),
    ),
    div(class = "module-section",
        h3("Liability for Incurred Claims (LIC)"),
        p("The LIC reflects the insurer’s obligation for claims arising from past coverage that have been incurred but not yet paid. Updates to LIC include:"),
        tags$ol( type = "a",
          tags$li("Claims incurred"),
          tags$li("Changes in estimates for reported and unreported claims (e.g., IBNR, OCR)"),
          tags$li("Application of discounting if payment is expected more than 12 months after the reporting date"),
          tags$li("Risk adjustment for non-financial risk"),
          tags$li("Unallocated Loss Adjustment Expenses (ULAE): These are internal costs (not directly linked to individual claims but expected to be incurred in settling claims. ULAE must be estimated, included in the fulfilment cash flows, and discounted if appropriate")
        ),
        img(src = "images/subsequent_lic.png", class = "module-image")
      ),

    div(class = "module-section summary-box",
        h3("🔍 Key Takeaways"),
        tags$ul(
          tags$li("Subsequent measurement ensures that liabilities reflect the current expectations of future service and incurred obligations."),
          tags$li("The LRC and LIC are updated continuously as new information becomes available."),
          tags$li("The CSM plays a critical role in spreading profit recognition over the service period."),
          tags$li("Onerous contract assessments continue throughout the life of the contract and may change from profitable to onerous, or vice versa; however, contracts are not reclassified after initial recognition."),
          tags$li("ULAE should be included in the fulfilment cash flows for incurred claims and for the LRC, as it forms part of the claim-related cash flows and should be estimated using sound actuarial techniques.")
        )
    ),
    box(
      title = "Answer the following questions to test your understanding of Subsequent Measurement.",
      status = "white", solidHeader = TRUE, width = 12,
      p("Please enter your name."),
      textInput(ns("participant_name"), "Enter your Name:")
    ),

    box(
      title = "1. What does subsequent measurement refer to under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q1"), label = NULL, choices = c(
        "The reassessment of reinsurance cash flows",
        "The update of contract liabilities after initial recognition",
        "Only the measurement of incurred claims",
        "Determining if premiums are received"
      ), selected = character(0))
    ),

    box(
      title = "2. An insurance company issues a 4-year term life insurance contract with a total expected Contractual Service Margin (CSM) of $8,000 at initial recognition. The company expects to provide insurance services evenly over the 4 years. How much CSM revenue should be recognized at the end of each year, assuming no changes in estimates or contract modifications?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q2"), label = NULL, choices = c(
        "$2,000 per year for 4 years",
        "$0 in year 1 and $8,000 in year 4",
        "$4,000 in the first year and $1,333 in each of the following years",
        "$8,000 immediately at contract inception"
      ), selected = character(0))
    ),

    box(
      title = "3. How often are fulfilment cash flows updated?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Once a year",
        "Monthly",
        "At each reporting date",
        "Never after initial recognition"
      ), selected = character(0))
    ),

    box(
      title = "4. How are claims incurred shown in financials?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q4"), label = NULL, choices = c(
        "In CSM",
        "In OCI",
        "In fulfilment cash flows",
        "In profit or loss"
      ), selected = character(0))
    ),

    box(
      title = "5. Which is a cause of change in risk adjustment?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q5"), label = NULL, choices = c(
        "Change in interest rates",
        "Increase in past claims",
        "Changes in uncertainty of future service",
        "Movement in capital reserves"
      ), selected = character(0))
    ),

    box(
      title = "6. Which changes are excluded from adjusting the CSM?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q6"), label = NULL, choices = c(
        "Future service estimates",
        "Time value updates",
        "Risk of lapses",
        "Policyholder behavior assumptions"
      ), selected = character(0))
    ),

    box(
      title = "7. Which of the following affects the Liability for Incurred Claims (LIC)?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q7"), label = NULL, choices = c(
        "Future service premiums",
        "Reinsurance commissions",
        "Claims already incurred",
        "Profit emergence"
      ), selected = character(0))
    ),

    box(
      title = "8. What does the Liability for Remaining Coverage (LRC) include?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q8"), label = NULL, choices = c(
        "CSM + premiums received",
        "Fulfilment cash flows + CSM",
        "Only claims paid",
        "Gross income"
      ), selected = character(0))
    ),

    box(
      title = "9. What does LIC capture?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q9"), label = NULL, choices = c(
        "Claims that may occur in the future",
        "Earned premiums",
        "Deferred acquisition cost",
        "Claims already incurred"
      ), selected = character(0))
    ),

    box(
      title = "10. What role does the risk adjustment play in subsequent measurement?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q10"), label = NULL, choices = c(
        "Reduces cash flows",
        "Defers tax",
        "Adjusts for uncertainty in non-financial risks",
        "Ignores future inflation"
      ), selected = character(0))
    ),

    box(
      title = "11. How is ULAE treated in subsequent measurement?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q11"), label = NULL, choices = c(
        "Expensed in full",
        "Included in LIC and updated",
        "Ignored unless incurred",
        "Deferred to maturity"
      ), selected = character(0))
    ),


    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")), 
    div(
      class = "quiz-nav",
      actionButton(
        ns("to_module_7"),
        label = tagList(icon("arrow-right"), "Next: Module 7 – Discounting, CSM & Risk Adjustment"),
        class = "control-button-tavnav"
      )
    )
  )
}

correct_answers_module6 <- list( 
  q1 = list(
    answer = "The update of contract liabilities after initial recognition",
    explanation = "Subsequent measurement involves updating the carrying amounts of insurance liabilities after initial recognition."
  ),
  q2 = list(
    answer = "$2,000 per year for 4 years",
    explanation = "Since the insurance company expects to provide services evenly over the 4-year coverage period and the total CSM is $8,000, the revenue should be recognized on a straight-line basis—$2,000 each year."
  ),
  q3 = list(
    answer = "At each reporting date",
    explanation = "Entities must reassess fulfilment cash flows using current estimates at each reporting period."
  ),
  q4 = list(
    answer = "In profit or loss",
    explanation = "Claims that relate to past service are recognized directly in the profit or loss statement."
  ),
  q5 = list(
    answer = "Changes in uncertainty of future service",
    explanation = "Changes in the level of uncertainty about future cash flows affect the risk adjustment."
  ),
  q6 = list(
    answer = "Time value updates",
    explanation = "Changes due to the passage of time (e.g., interest accretion) do not adjust the CSM — they affect finance income/expense."
  ),
  q7 = list(
    answer = "Claims already incurred",
    explanation = "LIC represents obligations from past events (claims already incurred but not yet paid)."
  ),
  q8 = list(
    answer = "Fulfilment cash flows + CSM",
    explanation = "LRC includes fulfilment cash flows for future coverage and the CSM."
  ),
  q9 = list(
    answer = "Claims already incurred",
    explanation = "LIC reflects the insurer’s obligation for incurred claims not yet settled."
  ),
  q10 = list(
    answer = "Adjusts for uncertainty in non-financial risks",
    explanation = "Risk adjustment reflects uncertainty in cash flows and is re-evaluated each period."
  ),
  q11 = list(
    answer = "Included in LIC and updated",
    explanation = "Unallocated Loss Adjustment Expenses (ULAE) are included in LIC and updated regularly."
  )
)


IFRS17Module6Server <- (function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Quiz result output
    final_name <- reactiveVal("")

    score <- reactiveVal(0)

    # Feedback functions
    observeEvent(input$submit, {
        removeModal()
        # 1. List out all your question input IDs
        question_ids <- paste0("q", 1:11)
        
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

    for (qid in names(correct_answers_module6)) {
      correct <- correct_answers_module6[[qid]]$answer
      explanation <- correct_answers_module6[[qid]]$explanation
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
        
    valid_ids <- paste0("q", 1:11)
    feedback <- lapply(valid_ids, function(qid) {
      if (!is.null(feedback[[qid]])) {
        feedback[[qid]]
      } else {
        paste0("⚠️ ", toupper(qid), ": No response recorded.")
      }
    })
    names(feedback) <- valid_ids

    output$result <- renderUI({
      total_questions <- length(correct_answers_module6)
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
    to_module_7 <- reactive(input$to_module_7)
    # return it so the app can observe it
    to_module_7
  })
})