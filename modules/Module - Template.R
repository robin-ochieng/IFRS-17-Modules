# modules/quizModule.R

# ---- UI ----
IFRS17Module14UI<- function(id) {
  ns <- NS(id)
  tagList(
    div(
    class = "module-content-wrapper",
    
    div(
        class = "module-content-header",
        h3("📘 Overview: Presentation in the Statement of Financial Position", class = "section-heading"),
        p("The table below summarizes key presentation differences between IFRS 4 and IFRS 17 across various financial reporting aspects.")
    ),

    div(
        class = "comparison-table-wrapper",
        DT::dataTableOutput(ns("ifrs_table"))
    ),

    div(
        class = "module-content-footer",
        h4("🔍 Key Takeaways"),
        tags$ul(
        tags$li("IFRS 17 requires separation of insurance assets and liabilities to enhance transparency."),
        tags$li("DAC is absorbed into fulfilment cash flows rather than presented as a separate asset."),
        tags$li("Onerous contracts are now explicitly recognised and disclosed."),
        tags$li("Offsetting of receivables and liabilities is not allowed."),
        tags$li("Risk adjustments and CSM significantly alter liability presentation.")
        )
    )
    ),


    tags$div(
        class = "section-header",
        h2("📘 Module 14 – Presentation in the Statement of Financial Position", class = "section-title-top"),
        p("Answer the following questions to test your understanding of Presentation in the Statement of Financial Position.",
          class = "section-subtitle")
    ),
    box(
        title = "Participant Information",
        status = "white", solidHeader = TRUE, width = 12,
        p("Please enter your name."),
        textInput(ns("participant_name"), "Enter your Name:")
    ),


    box(
      title = "1.  Can insurance contracts issued be presented net of reinsurance recoverables?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q1"), label = NULL, choices = c(
        "Yes, if on the same risk", "Only for short-term policies", "No, they must be presented separately", "Yes, under materiality exemption"), selected = character(0))
    ),
    
    box(
      title = "2.  How are changes in the risk adjustment presented if not disaggregated?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q2"), label = NULL, choices = c("In other comprehensive income", "Fully within the insurance service result", "As a deferred liability", "As finance income"), selected = character(0))
    ),
    
    box(
      title = "3.  How are insurance finance income or expenses presented?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q3"), label = NULL, choices = c("As underwriting losses", "Under investment income", "As the effect of time value of money on cash flows", "As administrative costs"), selected = character(0))
    ),
    
    box(
      title = "4. How often should an entity reassess the classification of contracts as assets or liabilities?",
      status = "white", solidHeader = TRUE, width = 12,
      radioButtons(ns("q4"), label = NULL, choices = c("At each reporting date", "When claims exceed expectations", "Only at initial recognition", "Annually"), selected = character(0))
    ),

    box(
    title = "5.  How should an entity present a group of contracts with a net obligation in the statement of financial position?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q5"), label = NULL,
                choices = c("As an asset", "As a liability", "Under equity", "Offset against premiums receivable"), selected = character(0))
    ),   

    box(
    title = "6. How should an entity present insurance contracts issued in the statement of financial position?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q6"), label = NULL,
                choices = c("Only when the contracts are profitable", "Combined with acquisition cash flows only", "As either assets or liabilities, depending on the net fulfilment cash flows", "Net of reinsurance recoverables"), selected = character(0))
    ),

    box(
    title = "7.  If reinsurance recoverables exceed expected reinsurance premium outflows, how is it presented?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q7"), label = NULL,
                choices = c(
                    "Disclosed in notes only",
                    "Offset against claims",
                    "As a liability",
                    "As an asset"), selected = character(0))
    ),

    box(
    title = "8.  What are the main components of the insurance service result in the income statement?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q8"), label = NULL,
                choices = c(
                    "Insurance revenue and insurance service expenses",
                    "Premiums and claims",
                    "Revenue and reinsurance recoveries",
                    "Insurance finance income and expense"), selected = character(0))
    ),

    box(
    title = "9.  What drives the distinction between insurance revenue and insurance finance income/expenses?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q9"), label = NULL,
                choices = c(
                    "Time value of money and discount rates", 
                    "Policy type", 
                    "Underwriting year", 
                    "Geographical spread"), selected = character(0))
    ),

    box(
    title = "10.  What is the appropriate presentation of acquisition costs related to a group of reinsurance contracts held?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q10"), label = NULL,
                choices = c(
                    "Expensed immediately",
                    "Included in insurance service expenses",
                    "Reported under administrative expenses",
                    "Included in the carrying amount of reinsurance contracts held "), selected = character(0))
    ),

    box(
    title = "11.  When are reinsurance contracts held presented as assets or liabilities?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q11"), label = NULL,
                choices = c(
                    "Separate line items as assets or liabilities",
                    "A single balance in the notes",
                    "Net amounts with insurance contracts",
                    "Offsetting items within insurance contract liabilities"), selected = character(0))
    ),

    box(
    title = "12.  When does an insurance contract group transition from an asset to a liability?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q12"), label = NULL,
                choices = c(
                    "When premiums are not received",
                    "When expected future outflows exceed inflows ",
                    "When it is reinsured",
                    "At contract expiry"), selected = character(0))
    ),

    box(
    title = "13.   When reinsurance income is earned, where should it be presented in the statement of financial performance?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q13"), label = NULL,
                choices = c(
                    "Offset against insurance service expenses", 
                    "Within insurance revenue",
                    "Within finance income", 
                    "Separately from insurance contracts issued"), selected = character(0))
    ),

    # Question 14
    box(
    title = "14. Which of the following would most likely be presented as an insurance liability?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q14"), label = NULL,
                choices = c(
                    "Deferred acquisition costs",
                    "Accrued interest income",
                    "Outstanding claims reserves",
                    "Expected future premium inflows"
                ),
                selected = character(0)
    )
    ),

    # Question 15
    box(
    title = "15. Under IFRS 17, how are insurance contract assets and liabilities presented in the Statement of Financial Position?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q15"), label = NULL,
                choices = c(
                    "Offset against each other",
                    "Presented separately for each group of contracts",
                    "Presented net at the entity level",
                    "Combined and shown as a single line item"
                ),
                selected = character(0)
    )
    ),

    # Question 16
    box(
    title = "16. What is the treatment of a group of onerous contracts in the Statement of Financial Position?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q16"), label = NULL,
                choices = c(
                    "Recognized as a liability",
                    "Included under reinsurance",
                    "Recognized as an asset",
                    "Deferred to future periods"
                ),
                selected = character(0)
    )
    ),

    # Question 17
    box(
    title = "17. Which IFRS 17 paragraph outlines the presentation requirements for the Statement of Financial Position?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q17"), label = NULL,
                choices = c(
                    "IFRS 17.32",
                    "IFRS 17.109",
                    "IFRS 17.42",
                    "IFRS 17.78"
                ),
                selected = character(0)
    )
    ),

    # Question 18
    box(
    title = "18. Which of the following is *not* shown separately in IFRS 17 SFP presentation?",
    status = "white", solidHeader = TRUE, width = 12,
    radioButtons(ns("q18"), label = NULL,
                choices = c(
                    "Insurance contract liabilities",
                    "Insurance contract assets",
                    "Deferred acquisition costs",
                    "Reinsurance contract assets"
                ),
                selected = character(0)
    )
    ),

    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")),
    div(
      class = "quiz-nav",
      actionButton(
          ns("to_case_studies"),
          label = tagList(icon("arrow-right"), "Next: Case Studies"),
          class = "control-button-tavnav"
      )
    )   
  )
}

# ---- Server ----
IFRS17Module14Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # bring ns into scope
    ns <- session$ns

    output$ifrs_table <- DT::renderDataTable({
    data.frame(
        Aspect = c(
        "Presentation of Insurance Contracts", "Level of Aggregation", "Contractual Service Margin (CSM)",
        "Acquisition Costs (DAC)", "Onerous Contracts", "Risk Adjustment", "Reinsurance Contracts",
        "Transparency & Comparability", "Measurement Basis", "Offsetting Allowed?"
        ),
        `IFRS 4` = c(
        "Often presented as net positions; lacks transparency",
        "Portfolio/product level; inconsistent",
        "Not recognised; profits front-loaded or deferred",
        "Capitalised as DAC asset separately",
        "No explicit requirement",
        "Not required",
        "Inconsistently presented or netted",
        "Low comparability due to GAAP diversity",
        "Historic assumptions, static reserves",
        "Offsetting common, obscures gross exposures"
        ),
        `IFRS 17` = c(
        "Separates contract assets and liabilities",
        "Grouped by issue year and profitability",
        "Recognised and amortised as unearned profit",
        "Included in fulfilment cash flows, affects CSM",
        "Identified at inception, loss shown as liability",
        "Required for non-financial risk",
        "Presented separately as asset or liability",
        "High comparability via uniform structure",
        "Current estimates updated every reporting date",
        "Prohibited to enhance clarity"
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
    }, options = list(dom = 't', paging = FALSE), rownames = FALSE)


    final_name <- reactiveVal("")

    score <- reactiveVal(0)

    # Feedback functions
    observeEvent(input$submit, {
        removeModal()
        # 1. List out all your question input IDs
        question_ids <- paste0("q", 1:18)
        
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


      # Correct answers
      # Q1
      if (input$q1 == "No, they must be presented separately") {
        score(score() + 1)
        feedback[[1]] <- "✅ Q1: Correct!"
        feedbackSuccess("q1", "Correct!")
      } else {
        feedback[[1]] <- "❌ Q1: Correct answer is 'No, they must be presented separately'. IFRS 17 prohibits offsetting between insurance and reinsurance contracts to provide transparency and prevent misleading financial presentation."
        feedbackDanger("q1", "Incorrect! Correct answer is 'No, they must be presented separately'. IFRS 17 prohibits offsetting between insurance and reinsurance contracts to provide transparency and prevent misleading financial presentation.")
      }
       

        # Q2
      if (input$q2 == "Fully within the insurance service result") {
        score(score() + 1)
        feedback[[2]] <- "✅ Q2: Correct!"
        feedbackSuccess("q2", "Correct!")
      } else {
        feedback[[2]] <- "❌ Q2: Correct answer is 'Fully within the insurance service result'. If an entity does not choose to disaggregate changes in the risk adjustment, the full change is presented within the insurance service result as per IFRS 17 guidance."
        feedbackDanger("q2", "Incorrect! Correct answer is 'Fully within the insurance service result'. If an entity does not choose to disaggregate changes in the risk adjustment, the full change is presented within the insurance service result as per IFRS 17 guidance.")
      }
       
      
        # Q3
      if (input$q3 == "As the effect of time value of money on cash flows") {
        score(score() + 1)
        feedback[[3]] <- "✅ Q3: Correct!"
        feedbackSuccess("q3", "Correct!")
      } else {
        feedback[[3]] <- "❌ Q3: Correct answer is 'As the effect of time value of money on cash flows'. Insurance finance income or expenses reflect the impact of discounting and time value on expected future cash flows."
        feedbackDanger("q3", "Incorrect! Correct answer is 'As the effect of time value of money on cash flows'. Insurance finance income or expenses reflect the impact of discounting and time value on expected future cash flows.")
      }
       
      
        # Q4
      if (input$q4 == "At each reporting date") {
        score(score() + 1)
        feedback[[4]] <- "✅ Q4: Correct!"
        feedbackSuccess("q4", "Correct!")
      } else {
        feedback[[4]] <- "❌ Q4: Correct answer is 'At each reporting date'. IFRS 17 requires periodic reassessment at every reporting date to ensure accuracy in reflecting financial obligations or entitlements.."
        feedbackDanger("q4", "Incorrect! Correct answer is 'At each reporting date'. IFRS 17 requires periodic reassessment at every reporting date to ensure accuracy in reflecting financial obligations or entitlements..")
      }
       


        # Section 2
        # Q5
        if (input$q5 == "As a liability") {
            score(score() + 1)
            feedback[[5]] <- "✅ Q5: Correct!"
            feedbackSuccess("q5", "Correct!")
        } else {
            feedback[[5]] <- "❌ Q5: Correct answer is 'As a liability'. Under IFRS 17, if the fulfilment cash flows of a group of insurance contracts result in a net obligation (i.e., expected outflows exceed inflows), the group is presented as a liability in the statement of financial position."
            feedbackDanger("q5", "Incorrect! Correct answer is 'As a liabilityt'. Under IFRS 17, if the fulfilment cash flows of a group of insurance contracts result in a net obligation (i.e., expected outflows exceed inflows), the group is presented as a liability in the statement of financial position.")
        }
       

        # Q6
        if (input$q6 == "As either assets or liabilities, depending on the net fulfilment cash flows") {
            score(score() + 1)
            feedback[[6]] <- "✅ Q6: Correct!"
            feedbackSuccess("q6", "Correct!")
        } else {
            feedback[[6]] <- "❌ Q6: Correct answer is 'As either assets or liabilities, depending on the net fulfilment cash flows'. Insurance contracts are presented based on whether the net fulfilment cash flows result in an asset or liability. This provides clarity on the insurerâ€™s financial position."
            feedbackDanger("q6", "Incorrect! Correct answer is 'As either assets or liabilities, depending on the net fulfilment cash flows'. Insurance contracts are presented based on whether the net fulfilment cash flows result in an asset or liability. This provides clarity on the insurerâ€™s financial position.")
        }
        

            # Q7
        if (input$q7 == "As an asset") {
            score(score() + 1)
            feedback[[7]] <- "✅ Q7: Correct!"
            feedbackSuccess("q7", "Correct!")
        } else {
            feedback[[7]] <- "❌ Q7: Correct answer is 'As an asset'. Where recoverables exceed premium outflows, reinsurance contracts held result in a net asset, which must be presented accordingly.."
            feedbackDanger("q7", "Incorrect! Correct answer is 'As an asset'. Where recoverables exceed premium outflows, reinsurance contracts held result in a net asset, which must be presented accordingly..")
        }
        

            # Q8
        if (input$q8 == "Insurance revenue and insurance service expenses ") {
            score(score() + 1)
            feedback[[8]] <- "✅ Q8: Correct!"
            feedbackSuccess("q8", "Correct!")
        } else {
            feedback[[8]] <- "❌ Q8: Correct answer is 'Insurance revenue and insurance service expenses '. The insurance service result includes revenue and expenses directly related to the provision of insurance services."
            feedbackDanger("q8", "Incorrect! Correct answer is 'Insurance revenue and insurance service expenses '. The insurance service result includes revenue and expenses directly related to the provision of insurance services.")
        }
        

            # Q9    
        if (input$q9 == "Time value of money and discount rates") {
            score(score() + 1)
            feedback[[9]] <- "✅ Q9: Correct!"
            feedbackSuccess("q9", "Correct!")
        } else {
            feedback[[9]] <- "❌ Q9: Correct answer is 'Time value of money and discount rates'. Revenue reflects service delivery, while finance components reflect economic effects from interest and time."
            feedbackDanger("q9", "Incorrect! Correct answer is 'Time value of money and discount rates'. Revenue reflects service delivery, while finance components reflect economic effects from interest and time.")
        }
        

            # Q10
        if (input$q10 == "Included in the carrying amount of reinsurance contracts held ") {
            score(score() + 1)
            feedback[[10]] <- "✅ Q10: Correct!"
            feedbackSuccess("q10", "Correct!")
        } else {
            feedback[[10]] <- "❌ Q10: Correct answer is 'Included in the carrying amount of reinsurance contracts held '. IFRS 17 requires acquisition cash flows related to reinsurance contracts held to be included in the carrying amount of the related group of reinsurance contracts, ensuring proper matching of expenses and benefits."
            feedbackDanger("q10", "Incorrect! Correct answer is 'Included in the carrying amount of reinsurance contracts held '. IFRS 17 requires acquisition cash flows related to reinsurance contracts held to be included in the carrying amount of the related group of reinsurance contracts, ensuring proper matching of expenses and benefits.")
        }
        

            # Q11
        if (input$q11 == "Separate line items as assets or liabilities") {
            score(score() + 1)
            feedback[[11]] <- "✅ Q11: Correct!"
            feedbackSuccess("q11", "Correct!")
        } else {
            feedback[[11]] <- "❌ Q11: Correct answer is 'Separate line items as assets or liabilities'. Reinsurance contracts held must be shown separately as assets or liabilities to reflect their distinct nature from direct insurance contracts."
            feedbackDanger("q11", "Incorrect! Correct answer is 'Separate line items as assets or liabilities'. Reinsurance contracts held must be shown separately as assets or liabilities to reflect their distinct nature from direct insurance contracts.")
        }
        

            # Q12
        if (input$q12 == "When expected future outflows exceed inflows ") {
            score(score() + 1)
            feedback[[12]] <- "✅ Q12: Correct!"
            feedbackSuccess("q12", "Correct!")
        } else {
            feedback[[12]] <- "❌ Q12: Correct answer is 'When expected future outflows exceed inflows '. A group of insurance contracts is classified as a liability when its fulfilment cash flows reflect a net obligationâ€”i.e., expected outflows exceed expected inflows."
            feedbackDanger("q12", "Incorrect! Correct answer is 'When expected future outflows exceed inflows '. A group of insurance contracts is classified as a liability when its fulfilment cash flows reflect a net obligationâ€”i.e., expected outflows exceed expected inflows.")
        }
        

            # Q13
        if (input$q13 == "Separately from insurance contracts issued") {
            score(score() + 1)
            feedback[[13]] <- "✅ Q13: Correct!"
            feedbackSuccess("q13", "Correct!")
        } else {
            feedback[[13]] <- "❌ Q13: Correct answer is 'Separately from insurance contracts issued'. IFRS 17 mandates that income and expenses from reinsurance contracts held must be presented separately from those arising from insurance contracts issued, ensuring clarity in financial reporting."
            feedbackDanger("q13", "Incorrect! Correct answer is 'Separately from insurance contracts issued'. IFRS 17 mandates that income and expenses from reinsurance contracts held must be presented separately from those arising from insurance contracts issued, ensuring clarity in financial reporting.")
        }
        

            # Q14
        if (input$q14 == "Outstanding claims reserves") {
            score(score() + 1)
            feedback[[14]] <- "✅ Q14: Correct!"
            feedbackSuccess("q14", "Correct!")
        } else {
            feedback[[14]] <- "❌ Q14: Correct answer is 'Outstanding claims reserves'. Outstanding claims reserves represent obligations the insurer owes and are presented as part of insurance contract liabilities."
            feedbackDanger("q14", "Incorrect! Correct answer is 'Outstanding claims reserves'. Outstanding claims reserves represent obligations the insurer owes and are presented as part of insurance contract liabilities.")
        }
        

            # Q15
        if (input$q15 == "Presented separately for each group of contracts") {
            score(score() + 1)
            feedback[[15]] <- "✅ Q14: Correct!"
            feedbackSuccess("q15", "Correct!")
        } else {
            feedback[[15]] <- "❌ Q14: Correct answer is 'Presented separately for each group of contracts'. IFRS 17 requires insurance contract assets and liabilities to be presented separately for each group of contracts, not netted at the entity level."
            feedbackDanger("q15", "Incorrect! Correct answer is 'Presented separately for each group of contracts'. IFRS 17 requires insurance contract assets and liabilities to be presented separately for each group of contracts, not netted at the entity level.")
        }
        
            # Q16
        if (input$q16 == "Recognized as a liability") {
            score(score() + 1)
            feedback[[16]] <- "✅ Q14: Correct!"
            feedbackSuccess("q16", "Correct!")
        } else {
            feedback[[16]] <- "❌ Q14: Correct answer is 'Recognized as a liability'. Onerous contracts result in a loss and are therefore recognized as a liability."
            feedbackDanger("q16", "Incorrect! Correct answer is Recognized as a liability'. Onerous contracts result in a loss and are therefore recognized as a liability.")
        }
        
            # Q17           
        if (input$q17 == "IFRS 17.78") {
            score(score() + 1)
            feedback[[17]] <- "✅ Q14: Correct!"
            feedbackSuccess("q17", "Correct!")
        } else {
            feedback[[17]] <- "❌ Q14: Correct answer is 'IFRS 17.78'. IFRS 17.78 requires separate presentation of insurance contract assets and liabilities."
            feedbackDanger("q17", "Incorrect! Correct answer is 'IFRS 17.78'. IFRS 17.78 requires separate presentation of insurance contract assets and liabilities.")
        }
        
            # Q18
        if (input$q18 == "Deferred acquisition costs") {
            score(score() + 1)
            feedback[[18]] <- "✅ Q14: Correct!"
            feedbackSuccess("q18", "Correct!")
        } else {
            feedback[[18]] <- "❌ Q14: Correct answer is 'Deferred acquisition costs'. IFRS 17 does not require DAC to be shown separately — it's included in the measurement of fulfilment cash flows."
            feedbackDanger("q18", "Incorrect! Correct answer is 'Deferred acquisition costs'. IFRS 17 does not require DAC to be shown separately — it's included in the measurement of fulfilment cash flows.")
        }
        


    output$result <- renderUI({
      total_questions <- 18
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
            h4("has successfully completed the IFRS 17 - Presentation in the Statement of Financial Position Quiz",
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
              background-color:rgb(172, 167, 167);
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
            ),  # ← comma!

            # ——— Print Button ———
            div(
              style = "text-align:center; margin-top:30px;",
              actionButton(
                ns("print_certificate"),
                "Print Results as PDF",
                icon  = icon("print"),
                class = "control-button-tavnav no-print"
              )
            )

          )  

        )  

      )

    })  
    })

    observeEvent(input$print_certificate, {
      runjs('
        var cert = document.querySelector(".print-area");
        if (!cert) {
          alert("Nothing to print – make sure you have submitted the quiz first.");
        } else {
          // open a blank window
          var w = window.open("", "_blank", "width=800,height=600");
          // build a print-only style to hide .no-print
          var head = `
            <head>
              <title>Participation Certificate</title>
              <style>
                body { margin:20px; font-family:Arial,sans-serif; }
                @media print { .no-print { display: none; } }
              </style>
            </head>`;
          // grab the certificate HTML
          var body = "<body>" + cert.outerHTML +
                    // wrap your button in a no-print div
                    "<div class=\\"no-print\\" style=\\"text-align:center; margin-top:30px;\\">" +
                      "<button onclick=\\"window.print()\\">Print Certificate as PDF</button>" +
                    "</div></body>";
          // write it all
          w.document.write("<!doctype html><html>" + head + body + "</html>");
          w.document.close();
          w.onload = function() {
            w.focus();
            w.print();
          };
        }
      ')
    })

   

    # create a reactive for the “Next” click
    to_case_studies <- reactive(input$to_case_studies)

    # return it so the app can observe it
    to_case_studies

  })
}
