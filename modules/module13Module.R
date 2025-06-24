# modules/quizModule.R

# ---- UI ----
IFRS17Module13UI<- function(id) {
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
      h2("📘 Module 13: Presentation in the Statement of Financial Position", class = "section-title-top")
    ),
 
    div(class = "module-section",
        h3("📖 Introduction"),
        p("This module provides an overview of the key changes in the presentation of the statement of financial position under IFRS 17 compared to IFRS 4. This is covered under paragraphs 78 – 82."),
        p("IFRS 17 requires insurance companies to present insurance and reinsurance contracts separately in their financial statements. In the balance sheet (statement of financial position), insurers must clearly show:"),
        tags$ol(type = "a",
          tags$li("Insurance contracts they issued that are assets."),
          tags$li("Insurance contracts they issued that are liabilities."),
          tags$li("Reinsurance contracts they hold that are assets."),
          tags$li("Reinsurance contracts they hold that are liabilities.")
        ),        
    ),  

      #Balance Sheet: liabilities and acquisition costs
      div(class = "module-section",
        p("If a group of insurance contracts is expected to pay out more than it will receive (i.e., expected outflows are greater than inflows), it must be shown as a liability. The same applies to onerous contracts, which are expected to make a loss, and to outstanding claims, which are amounts the insurer still owes."),
        p("Acquisition costs, like commissions paid to agents, should be included in the total value of the contracts on the balance sheet. These costs are not shown separately and including them gives a fuller picture of the insurer’s financial position.")
      ),

      #Income statement, risk adjustment, and review
      div(class = "module-section",
        p("In the income statement (statement of profit or loss), two main things must be shown:"),
        tags$ul(
          tags$li("Insurance service result – income and expenses from providing insurance services, not including investment-related amounts."),
          tags$li("Insurance finance income or expenses – the effect of interest rates and time on expected future cash flows.")
        ),
        p("An insurer is not required to disaggregate the change in the risk adjustment for non-financial risk between the insurance service result and insurance finance income or expenses. If the insurer does not make such a disaggregation, it shall include the entire change in the risk adjustment for non-financial risk as part of the insurance service result."),
        p("IFRS 17 also requires insurers to review their figures at each reporting date to ensure they still reflect their true financial situation.")
      ),
      div(
        class = "module-section financial-aspects",
        h4("📊 IFRS 4 vs IFRS 17 across various financial reporting aspects", class = "diagram-title"),
        p("The table below summarizes key presentation differences between IFRS 4 and IFRS 17 across various financial reporting aspects."),
        div(class = "table-responsive",
          tags$table(class = "financial-aspects-table",
            tags$thead(
              tags$tr(
                tags$th("Aspect"),
                tags$th("IFRS 4"),
                tags$th("IFRS 17")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Presentation on Insurance Contracts"),
                tags$td("Insurers often presented a net position (e.g., premiums receivable net of claims payable), leading to a lack of transparency in the true financial exposure of the insurer. There were no specific rules on separate presentation."),
                tags$td("Requires a clear split between insurance contract assets and insurance contract liabilities. This ensures greater clarity on whether an entity is in a net asset or net liability position for each group of contracts.")
              ),
              tags$tr(
                tags$td("Level of Aggregation"),
                tags$td("Presentation was typically at a portfolio or product line level, with limited guidance on how to group contracts. This resulted in inconsistent practices."),
                tags$td("Contracts must be grouped by issue year and expected profitability into three categories: (i) onerous, (ii) no significant possibility of becoming onerous, and (iii) remaining contracts. The presentation is at the group of contracts level.")
              ),
              tags$tr(
                tags$td("Contractual Service Margin (CSM)"),
                tags$td("CSM was not recognised. Profits were often front-loaded or deferred through unearned premium reserves or DAC."),
                tags$td("The CSM is a core liability component under IFRS 17. It represents the unearned profit and is amortised over the coverage period, ensuring profit is recognised as services are provided.")
              ),
              tags$tr(
                tags$td("Acquisition Costs (DAC)"),
                tags$td("Acquisition costs could be capitalised and presented as a Deferred Acquisition Cost asset, separate from insurance liabilities."),
                tags$td("These costs are included in fulfilment cash flows, which affect the CSM. DAC is no longer a separate balance sheet item, aligning recognition more closely with the insurance liability.")
              ),
              tags$tr(
                tags$td("Onerous Contracts"),
                tags$td("There was no explicit requirement to identify or account separately for onerous contracts, allowing insurers to smooth losses."),
                tags$td("Onerous groups must be identified at initial recognition. Any loss is recognised immediately in profit or loss and the group is shown as a liability in the SFP.")
              ),
              tags$tr(
                tags$td("Risk Adjustment"),
                tags$td("Not required. There was no standardised way to reflect uncertainty or risk in the liability valuation."),
                tags$td("A risk adjustment for non-financial risk must be included in the measurement of insurance liabilities. This adds a buffer for the uncertainty in fulfilment cash flows.")
              ),
              tags$tr(
                tags$td("Reinsurance Contracts"),
                tags$td("Often netted against underlying insurance contracts or presented inconsistently across companies."),
                tags$td("Reinsurance contracts are presented separately as assets or liabilities and measured independently from underlying insurance contracts, improving transparency.")
              ),
              tags$tr(
                tags$td("Transparency & Comparability"),
                tags$td("Low comparability across insurers due to different local GAAPs and inconsistent application. Diverse presentations reduced the usefulness of financial statements."),
                tags$td("High comparability. IFRS 17 enforces a uniform structure, with disclosures enhancing comparability across insurers and jurisdictions.")
              ),
              tags$tr(
                tags$td("Measurement Basis"),
                tags$td("Frequently relied on historic assumptions, such as unadjusted premium cash flows and static reserves. Assumptions were not updated regularly."),
                tags$td("Based on current estimates of future cash flows, updated at each reporting date. This includes changes in assumptions and discounting, improving relevance and reliability.")
              ),
              tags$tr(
                tags$td("Offsetting Allowed?"),
                tags$td("Offsetting was common (e.g., premiums receivable net of claims payable), reducing the visibility of gross exposures."),
                tags$td("Offsetting is prohibited. This rule enhances balance sheet clarity.")
              )
            )
          )
        )
      ),

      div(
        class = "financial-reporting-diagram",
        h4("📊 IFRS 4 vs IFRS 17 Balance Sheet Transition", class = "diagram-title"),
        p("The diagram below shows the transition between the IFRS 4 Balance Sheet and the IFRS 17 Balance Sheet."),
        div(class = "balance-sheet-table-wrapper table-responsive",
          tags$table(class = "balance-sheet-table",
            tags$thead(
              tags$tr(
                tags$th("IFRS 4", colspan = 1, style = "background-color:#DC5A17; color:#fff;"),
                tags$th("IFRS 17", colspan = 1, style = "background-color:#006AA6; color:#fff;")
              )
            ),
            tags$tbody(
              # Assets
              tags$tr(tags$td(tags$b("ASSETS")), tags$td(tags$b("ASSETS"))),
              tags$tr(tags$td("Reinsurance Contract Assets (1)"), tags$td("Insurance contract assets (2), (3)")),
              tags$tr(tags$td("Deferred acquisition costs (2)"), tags$td("Reinsurance contract assets (1), (4)")),
              tags$tr(tags$td("Insurance receivable (3)"), tags$td("Other assets (5)")),
              tags$tr(tags$td("Reinsurance receivables (4)"), tags$td("")),
              tags$tr(tags$td("Other assets (5)"), tags$td("")),
              tags$tr(tags$td(tags$b("Total Assets xx"), colspan = 1), tags$td(tags$b("Total Assets xx"))),

              # Equity
              tags$tr(tags$td(tags$b("EQUITY")), tags$td(tags$b("EQUITY"))),
              tags$tr(tags$td("Share capital (1)"), tags$td("Share capital (1)")),
              tags$tr(tags$td("Statutory reserves (2)"), tags$td("Statutory reserves (2)")),
              tags$tr(tags$td("General reserves (3)"), tags$td("General reserves (3)")),
              tags$tr(tags$td("Retained earnings (4)"), tags$td("Retained earnings (4)")),
              tags$tr(tags$td(tags$b("Total Equity xx")), tags$td(tags$b("Total Equity xx"))),

              # Liabilities
              tags$tr(tags$td(tags$b("LIABILITIES")), tags$td(tags$b("LIABILITIES"))),
              tags$tr(tags$td("Insurance contract liabilities (1)"), tags$td("Insurance contract liabilities (1), (4)")),
              tags$tr(tags$td("Deferred Commission Income (2)"), tags$td("Reinsurance contract liabilities (2), (3), (5)")),
              tags$tr(tags$td("Reinsurance deposits retained (3)"), tags$td("Other Liabilities (6)")),
              tags$tr(tags$td("Payable arising out of insurance arrangements (4)"), tags$td("")),
              tags$tr(tags$td("Payable arising out of reinsurance arrangements (5)"), tags$td("")),
              tags$tr(tags$td("Other Liabilities (6)"), tags$td("")),
              tags$tr(tags$td(tags$b("Total Liabilities xx")), tags$td(tags$b("Total Liabilities xx")))
            )
          )
        )
      ),

    div(class = "module-section summary-box",
        h3("🔍 Key Takeaways"),
        tags$ul(
          tags$li("IFRS 17 requires separation of insurance assets and liabilities to enhance transparency."),
          tags$li("DAC is absorbed into fulfilment cash flows rather than presented as a separate asset."),
          tags$li("Onerous contracts are now explicitly recognised and disclosed."),
          tags$li("Offsetting of receivables and liabilities is not allowed."),
          tags$li("Risk adjustments and CSM significantly alter liability presentation.")
        )
    ),

    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Presentation in the Statement of Financial Position.")
    ),

    box(
      title = "1. How are changes in the risk adjustment presented if not disaggregated?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "In other comprehensive income",
        "Fully within the insurance service result",
        "As a deferred liability",
        "As finance income"
      ), selected = character(0))
    ),

    box(
      title = "2. How should an entity present a group of contracts with a net obligation (i.e., expected outflows exceed inflows) in the statement of financial position?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "As an asset",
        "As a liability",
        "Under equity",
        "Offset against premiums receivable"
      ), selected = character(0))
    ),

    box(
      title = "3. How should an entity present insurance contracts issued in the statement of financial position?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Only when the contracts are profitable",
        "Combined with acquisition cash flows only",
        "As either assets or liabilities, depending on the net fulfilment cash flows",
        "Net of reinsurance recoverables"
      ), selected = character(0))
    ),

    box(
      title = "4. What drives the distinction between insurance revenue and insurance finance income/expenses?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "Time value of money and discount rates",
        "Policy type",
        "Underwriting year",
        "Geographical spread"
      ), selected = character(0))
    ),

    box(
      title = "5. What is the appropriate presentation of acquisition costs related to a group of reinsurance contracts held?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "Expensed immediately",
        "Included in insurance service expenses",
        "Reported under administrative expenses",
        "Included in the carrying amount of reinsurance contracts held"
      ), selected = character(0))
    ),

    box(
      title = "6. Which of the following would most likely be presented as an insurance liability?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "Deferred acquisition costs",
        "Accrued interest income",
        "Outstanding claims reserves",
        "Expected future premium inflows"
      ), selected = character(0))
    ),

    box(
      title = "7. Under IFRS 17, how are insurance contract assets and liabilities presented in the Statement of Financial Position?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "Offset against each other",
        "Presented separately for each group of contracts",
        "Presented net at the entity level",
        "Combined and shown as a single line item"
      ), selected = character(0))
    ),

    box(
      title = "8. What is the treatment of a group of onerous contracts in the SFP?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Recognized as a liability",
        "Included under reinsurance",
        "Recognized as an asset",
        "Deferred to future periods"
      ), selected = character(0))
    ),

    box(
      title = "9. Which IFRS 17 paragraph outlines the presentation requirements for the SFP?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "IFRS 17.32",
        "IFRS 17.109",
        "IFRS 17.42",
        "IFRS 17.78"
      ), selected = character(0))
    ),

    box(
      title = "10. Which of the following is not shown separately in IFRS 17 SFP presentation?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "Insurance contract liabilities",
        "Insurance contract assets",
        "Deferred acquisition costs",
        "Reinsurance contract assets"
      ), selected = character(0))
    ),



    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")),
    div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_14"),
          label = tagList(icon("arrow-right"), "Next: Module 14 - Insurance Service Result"),
          class = "control-button-tavnav"
      )
    )   
  )
}


correct_answers_module13 <- list( 
  q1 = list(
    answer = "Fully within the insurance service result",
    explanation = "If an entity does not choose to disaggregate changes in the risk adjustment, the full change is presented within the insurance service result as per IFRS 17 guidance."
  ),
  q2 = list(
    answer = "As a liability",
    explanation = "Under IFRS 17, if fulfilment cash flows of a group of insurance contracts result in a net obligation, it is presented as a liability in the statement of financial position."
  ),
  q3 = list(
    answer = "As either assets or liabilities, depending on the net fulfilment cash flows",
    explanation = "Insurance contracts are presented based on whether fulfilment cash flows result in a net asset or liability, providing clarity on the insurer’s financial position."
  ),
  q4 = list(
    answer = "Time value of money and discount rates",
    explanation = "The distinction arises because revenue reflects service delivery, while finance income/expenses reflect economic effects from interest rates and time value of money."
  ),
  q5 = list(
    answer = "Included in the carrying amount of reinsurance contracts held",
    explanation = "IFRS 17 requires acquisition cash flows related to reinsurance contracts held to be included in the carrying amount of the group, ensuring proper expense matching."
  ),
  q6 = list(
    answer = "Outstanding claims reserves",
    explanation = "These reserves represent obligations still owed by the insurer and are therefore presented as insurance liabilities."
  ),
  q7 = list(
    answer = "Presented separately for each group of contracts",
    explanation = "IFRS 17 requires insurance contract assets and liabilities to be presented separately for each group and prohibits offsetting at the entity level."
  ),
  q8 = list(
    answer = "Recognized as a liability",
    explanation = "Onerous contracts result in expected losses and must be recognized as liabilities in the statement of financial position under IFRS 17."
  ),
  q9 = list(
    answer = "IFRS 17.78",
    explanation = "IFRS 17.78 outlines the requirement for separate presentation of insurance contract assets and liabilities in the statement of financial position."
  ),
  q10 = list(
    answer = "Deferred acquisition costs",
    explanation = "Under IFRS 17, DAC is not presented separately but included within the fulfilment cash flows, reflecting a more integrated liability valuation."
  )
)


# ---- Server ----
IFRS17Module13Server <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    # bring ns into scope
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

    for (qid in names(correct_answers_module13)) {
      correct <- correct_answers_module13[[qid]]$answer
      explanation <- correct_answers_module13[[qid]]$explanation
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
      # Save progress for Module 13
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 2 specific calculations
        total_questions <- length(correct_answers_module13)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module13",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 13 Progress Saved!</strong><br>",
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
          print(paste("Module 13 progress save error:", e$message))
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
      total_questions <- length(correct_answers_module13)
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
    to_module_14 <- reactive(input$to_module_14)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_14,
      progress_trigger = progress_saved_trigger
    ))
  })
}
