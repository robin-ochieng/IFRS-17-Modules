# modules/module2Module.R
IFRS17Module2UI <- function(id) {
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
        h2("📘 Module 2: Combination & Separation of Insurance Contracts", class = "section-title-top")
    ),

    div(class = "module-section",
        h3(icon("info-circle"), "Overview", class = "section-subheading"),
        
        p("Paragraphs 9 to 13 of IFRS 17 set out when insurance contracts should be combined or separated, so that financial reporting reflects the economic substance of the arrangements rather than merely their legal form."),
        
        h4("Key areas covered include:"),
        tags$ul(
          tags$li(strong("Combination of Contracts:"), " criteria for combining contracts designed to achieve an overall commercial effect—e.g., interdependent pricing or one contract offsetting another’s risk"),
          tags$li(strong("Pricing Dependencies:"), " recognition that interdependent pricing structures may require treating multiple contracts as a single arrangement"),
          tags$li(strong("Separation of Components:"), " identification and separation of components within a contract that would fall under other IFRS standards if issued independently"),
          tags$li(strong("Cross-Standard Application:"), " application of IFRS 9 and IFRS 15 to embedded derivatives and non-insurance services within insurance contracts")
        ),
        
        h4("Related standards:"),
        tags$ul(
          tags$li(strong("IFRS 9:"), " classification, measurement and recognition of financial assets and liabilities"),
          tags$li(strong("IFRS 15:"), " framework for recognizing revenue from contracts involving the transfer of goods or services")
        )
    ),


    div(class = "module-section",
        h3("Embedded Derivatives", class = "section-subheading"),
        
        p("Embedded derivatives within insurance contracts must be evaluated under IFRS 9 to determine whether they require separation."),
        p("These features modify cash flows based on external variables—such as interest rates, equity prices or inflation indices—and, while not standalone instruments, are integral to the insurance contract."),
        p("They must be accounted for separately if their economic characteristics are not closely related to the underlying insurance risk.")
    ),

    div(class = "module-section",
        h3("Investment Components and their Treatment Under IFRS 17 and IFRS 9", class = "section-subheading"),
        
        p("An investment component embedded within an insurance contract must first be assessed under IFRS 17 to see if it is distinct."),
        p("If it is distinct, the component is separated and accounted for under IFRS 9—unless it qualifies as a discretionary participation feature, in which case IFRS 17 continues to apply.")
    ),

    div(class = "module-section",
        h3("What Is a Distinct Investment Component?", class = "section-subheading"),
        
        p("A component is considered distinct if:"),
        tags$ul(
          tags$li("It is not highly interrelated with the insurance component of the contract"),
          tags$li("A contract with similar terms could be sold separately in the same market")
        ),
        
        p("In such cases, the contractual amount is payable to the policyholder even if the insured event does not occur—for example, in savings or investment-linked benefits. These cash flows are not contingent on an insured event and thus fall under the scope of IFRS 9.")
    ),

    div(class = "module-section",
        h3("When Is an Investment Component Not Distinct?", class = "section-subheading"),
        
        p("An investment component is not distinct—and therefore remains within the scope of IFRS 17—when it is highly interrelated with the insurance component. This occurs when:"),
        tags$ul(
          tags$li("The investment component cannot be separated from the insurance coverage without altering the terms of the contract"),
          tags$li("The policyholder cannot benefit from the investment component independently of the insurance coverage"),
          tags$li("The cash flows of the investment component are significantly influenced by the insurance component, making them not separately identifiable")
        ),
        p("In Summary"),
        div(class = "table-responsive",
            tags$table(class = "comparison-table",
                tags$thead(
                    tags$tr(
                        tags$th("Condition"),
                        tags$th("Treatment")
                    )
                ),
                tags$tbody(
                    tags$tr(
                        tags$td("Investment component is distinct and separable"),
                        tags$td("Account under IFRS 9")
                    ),
                    tags$tr(
                        tags$td("Investment component qualifies as a discretionary participation feature"),
                        tags$td("Account under IFRS 17")
                    ),
                    tags$tr(
                        tags$td("Investment component is not distinct (highly interrelated)"),
                        tags$td("Remain within IFRS 17")
                    )
                )
            )
        )
    ),
            
    div(class = "module-section",
        h3("Non-insurance Services", class = "section-subheading"),
        
        p("When an insurance contract includes additional services—such as wellness subscriptions or health-related benefits—these must be assessed for separability under IFRS 17."),
        p("If the services are distinct, meaning they can be provided independently of the insurance coverage, they must be accounted for under IFRS 15. This standard also governs the allocation of consideration between insurance and non-insurance components, ensuring that financial statements reflect the economic substance of the arrangement rather than its legal bundling.")
    ),

    div(class = "module-section",
        h3("How Pricing Dependencies Influence Contract Treatment Under IFRS 17", class = "section-subheading"),
        
        p("Under IFRS 17, the decision to combine or separate insurance contracts hinges on their economic interdependence and commercial intent. The presence or absence of pricing and risk dependencies plays a critical role in determining the appropriate accounting treatment."),
        
        tags$h4("When Contracts Should Be Combined"),
        tags$ul(
          tags$li(strong("Interdependent Pricing:"), " multiple contracts priced together or structured as a bundled package are economically linked and may require combined reporting"),
          tags$li(strong("Risk Neutralization:"), " one contract offsets or eliminates the financial exposure of another, so the contracts are viewed as interrelated and should be accounted for together"),
          tags$li(strong("Unified Commercial Effect:"), " contracts designed to function collectively to achieve a specific financial or risk outcome must be reported as a single unit to reflect true economic substance")
        ),
        p(em("\"Commercial effect\" refers to the overall economic impact of a group of contracts that are structured to work in tandem.")),
        
        tags$h4("When Contracts Should Be Separated"),
        tags$ul(
          tags$li(strong("Independent Pricing:"), " each contract is priced on its own without reference to the others and is considered distinct for reporting purposes"),
          tags$li(strong("Standalone Risk Profiles:"), " contracts carrying separate and unrelated insurance risks should be treated independently"),
          tags$li(strong("Unrelated Policyholder Benefits:"), " contracts serving different purposes and not interacting financially or operationally should remain separate in financial reporting")
        )
    ),

    div(class = "module-section",
        h3("Components Requiring Separation Under IFRS 17", class = "section-subheading"),
        p("IFRS 17 mandates separation of components when they meet distinct-ness criteria and would otherwise fall under a different IFRS standard:"),
        div(class = "table-responsive",
            tags$table(class = "comparison-table",
                tags$thead(
                    tags$tr(
                        tags$th("Component"),
                        tags$th("Applicable Standard"),
                        tags$th("Condition for Separation")
                    )
                ),
                tags$tbody(
                    tags$tr(
                        tags$td("Embedded Derivatives"),
                        tags$td("IFRS 9"),
                        tags$td("Not closely related to the insurance risk")
                    ),
                    tags$tr(
                        tags$td("Investment Components"),
                        tags$td("IFRS 9"),
                        tags$td("Distinct and not contingent on an insured event")
                    ),
                    tags$tr(
                        tags$td("Non-Insurance Services"),
                        tags$td("IFRS 15"),
                        tags$td("Provide distinct goods or services deliverable independently")
                    )
                )
            )
        )
    ),    

    div(class = "module-section",
        h3("IFRS 4 vs IFRS 17", class = "section-subheading"),
        p("The table below shows how IFRS 4 and IFRS 17 treat the combination and separation of insurance contracts:"),
        div(class = "table-responsive",
            tags$table(class = "comparison-table",
                tags$thead(
                    tags$tr(
                        tags$th("Aspect"),
                        tags$th("IFRS 4"),
                        tags$th("IFRS 17")
                    )
                ),
                tags$tbody(
                    tags$tr(
                        tags$td("Combination of Insurance Contracts"),
                        tags$td("No specific guidance: insurers followed local policies"),
                        tags$td("Requires contracts to be combined if they achieve an overall commercial effect")
                    ),
                    tags$tr(
                        tags$td("Embedded Derivatives"),
                        tags$td("Limited guidance: insurers could apply IFRS 9 in some cases"),
                        tags$td("Must be separated and assessed under IFRS 9 if not closely related to insurance risk")
                    ),
                    tags$tr(
                        tags$td("Investment Components"),
                        tags$td("Generally included within insurance contracts"),
                        tags$td("Must be separated and accounted for under IFRS 9 if they are distinct")
                    ),
                    tags$tr(
                        tags$td("Non-Insurance Services"),
                        tags$td("Often bundled within insurance contracts"),
                        tags$td("Must be separated and accounted for under IFRS 15 if they provide distinct goods or services")
                    )
                )
            )
        )
    ),






    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Combination & Separation of Insurance Contracts."),
    ),


    box(
      title = "1. An insurance company issues two contracts simultaneously to the same policyholder. Contract X provides insurance coverage, while Contract Y fully offsets the financial risk associated with Contract X. Under IFRS 17, how should the insurer account for these contracts?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "Account for the contracts as a single arrangement due to their combined commercial substance",
        "Present each contract separately, treating them as distinct insurance arrangements",
        "Recognize only Contract X, as it represents the initial insurance obligation",
        "Apply IFRS 9 to both contracts and include them only in the financial disclosures"
      ), selected = character(0))
    ),

    box(
      title = "2. An insurer structures a package of interrelated insurance contracts for a corporate client. The package includes policies that provide coverage and others that hedge specific risks, with pricing designed to reflect their interdependence. Under IFRS 17, how should the insurer account for these bundled contracts?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "Assess and report each contract separately, regardless of their interrelated features",
        "Treat the bundled contracts as a single contract if they collectively represent a unified commercial arrangement",
        "Separate the contracts based solely on differences in contract duration",
        "Report each contract based strictly on its legal form, not its economic substance"
      ), selected = character(0))
    ),

    box(
      title = "3. Which of the following situations would not require combining contracts under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Two insurance contracts issued at the same time to the same policyholder, with pricing structured to work together",
        "A reinsurance contract that fully offsets the risk of an insurance policy issued by the same insurer",
        "An insurance contract and a separate investment product sold independently with no pricing or risk interdependence",
        "A life insurance policy bundled with a rider that nullifies the main policy’s coverage",
        "A life insurance contract and a rider that cancels all coverage in the main policy"
      ), selected = character(0))
    ),

    box(
      title = "4. A life insurer offers a bundled product that includes both insurance coverage and an investment feature. The investment component is capable of generating returns independently and could be sold separately in the same market. How should this arrangement be treated under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "Account for the entire package as a single insurance contract",
        "Apply IFRS 9 to the full contract since it includes an investment element",
        "Separate the investment component only if it exceeds 50% of the total premiums",
        "Separate the investment component if it is distinct and can be sold independently"
      ), selected = character(0))
    ),

    box(
      title = "5. An insurer issues two separate policies to a corporate client—one covering property damage and the other covering business interruption losses tied to that property. The premiums are interdependent and structured as a bundled solution. How should the insurer account for these contracts under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "Always report the contracts separately regardless of structure",
        "Combine the contracts if their pricing is interdependent and they form a unified risk solution",
        "Account for both contracts under IFRS 9 due to their financial nature",
        "Combine the contracts only if the policyholder explicitly requests it"
      ), selected = character(0))
    ),

    box(
      title = "6. An insurance contract includes an embedded derivative that modifies cash flows based on a financial index. According to IFRS 17, how should this embedded derivative be treated?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "It must always remain part of the insurance contract",
        "It should be ignored unless the insurer requests separation",
        "It must be reported only in the contract disclosures",
        "It must be separated and accounted for under IFRS 9 if required"
      ), selected = character(0))
    ),

    box(
      title = "7. An insurer issues a contract that includes both insurance coverage and an investment feature. The investment component is capable of being sold independently in the same market. How should the investment component be treated under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "It should remain embedded in the insurance contract",
        "It should be accounted for under IFRS 15",
        "It must be separated only if it is distinct",
        "It should be reported only if the policyholder requests separate treatment"
      ), selected = character(0))
    ),

    box(
      title = "8. An insurer offers a health insurance policy that also includes access to wellness services such as gym memberships and nutritional consultations. These services are provided as an add-on to the insurance coverage. Under IFRS 17, how should the insurer account for the wellness component?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Include it within the insurance contract and account for it under IFRS 17",
        "Separate it and apply IFRS 15 if it qualifies as a distinct non-insurance service",
        "Reclassify it as an investment component and apply IFRS 9",
        "Only disclose it in the notes to the financial statements without separate recognition"
      ), selected = character(0))
    ),


    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")),

    div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_3"),
          label = tagList(icon("arrow-right"), "Next: Module 3 – Level of Aggregation"),
          class = "control-button-tavnav"
      )
    )
)
}

correct_answers_module2 <- list(
  q1 = list(
    answer      = "Account for the contracts as a single arrangement due to their combined commercial substance",
    explanation = "Since Contract Y nullifies the financial exposure of Contract X, the two together do not transfer significant insurance risk—a key criterion for IFRS 17 applicability—so they must be considered as one arrangement to reflect their economic substance."
  ),
  q2 = list(
    answer      = "Treat the bundled contracts as a single contract if they collectively represent a unified commercial arrangement",
    explanation = "IFRS 17 requires insurers to consider the economic substance of interrelated contracts and account for them as one if designed to achieve a unified commercial effect."
  ),
  q3 = list(
    answer      = "An insurance contract and a separate investment product sold independently with no pricing or risk interdependence",
    explanation = "Under IFRS 17 paragraph 9, contracts combine only if structured to achieve an overall commercial effect, such as when:\n• One contract negates another’s obligations\n• Contracts are priced as a single risk\n• The lapse or maturity of one affects the other\n\nIn the standalone investment case, cash flows are:\n• Sold separately\n• Not priced together\n• Not interdependent in risk or cash flows\n\nHence, they do not meet the combination criteria and should be accounted for independently."
  ),
  q4 = list(
    answer      = "Separate the investment component if it is distinct and can be sold independently",
    explanation = "According to IFRS 17 paragraph 11(b) and B31–B32, an investment component must be separated if it is distinct—i.e., not highly interrelated with the insurance component and capable of being sold on its own—and then accounted for under IFRS 9."
  ),
  q5 = list(
    answer      = "Combine the contracts if their pricing is interdependent and they form a unified risk solution",
    explanation = "Per IFRS 17 paragraph B24, contracts should be combined when:\n• Issued to the same or related counterparty,\n• Designed to achieve an overall commercial effect,\n• Pricing or risk structures are interdependent.\n\nHere, property damage and business interruption policies share purpose and pricing and are economically interrelated, so they must be combined."
  ),
  q6 = list(
    answer      = "It must be separated and accounted for under IFRS 9 if required",
    explanation = "IFRS 17 paragraph 11(a) and IFRS 9 section 4.3.3 require separating an embedded derivative when:\n• Its risks and economic characteristics are not closely related to the host contract,\n• A separate instrument with identical terms would qualify as a derivative,\n• The host contract is not measured at fair value through profit or loss.\n\nSince this feature alters cash flows based on a financial index, it likely introduces unrelated financial risk and must be separated under IFRS 9."
  ),
  q7 = list(
    answer      = "It must be separated only if it is distinct",
    explanation = "IFRS 17 paragraph 11(b) states an entity shall separate an investment component from the host contract if—and only if—that component is distinct (i.e., not highly interrelated and capable of independent sale)."
  ),
  q8 = list(
    answer      = "Separate it and apply IFRS 15 if it qualifies as a distinct non-insurance service",
    explanation = "According to IFRS 17 paragraph 12, when a contract includes non-insurance goods or services (e.g., wellness programs), the insurer must:\n• Separate the component if it is distinct,\n• Account for it under IFRS 15 (Revenue from Contracts with Customers).\n\nWellness services are non-insurance components and, if sold separately in the market, qualify for separation and IFRS 15 treatment."
  )
)


IFRS17Module2Server <- (function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Add a reactive value to track when progress is saved
    progress_saved_trigger <- reactiveVal(0)

    score <- reactiveVal(0)

    # Feedback functions
    observeEvent(input$submit, {
        removeModal()
        # 1. List out all your question input IDs
        question_ids <- paste0("q", 1:8)
        
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

    for (qid in names(correct_answers_module2)) {
      correct <- correct_answers_module2[[qid]]$answer
      explanation <- correct_answers_module2[[qid]]$explanation
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
      # Save progress for Module 2
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 2 specific calculations
        total_questions <- length(correct_answers_module2)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module2",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 2 Progress Saved!</strong><br>",
                "Score: ", final_score, "/", total_questions, " (", final_percentage, "%)<br>",
                if(final_percentage >= 70) "Great job!" else "Keep practicing!"
              )),
              type = "message",
              duration = 5
            )

            # Trigger the reactive to indicate progress was saved
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
          print(paste("Module 2 progress save error:", e$message))
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
    valid_ids <- paste0("q", 1:8)
    feedback <- lapply(valid_ids, function(qid) {
      if (!is.null(feedback[[qid]])) {
        feedback[[qid]]
      } else {
        paste0("⚠️ ", toupper(qid), ": No response recorded.")
      }
    })
    names(feedback) <- valid_ids

    output$result <- renderUI({
      total_questions <- length(correct_answers_module2)
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
              "📊 Module 2 Results Summary",
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
    to_module_3 <- reactive(input$to_module_3)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_3,
      progress_trigger = progress_saved_trigger
    ))
  })
})