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
        h3("📖 Introduction"),
        
        p("IFRS 17 under paragraphs 9 to 13 has provided requirements for combination and separation of insurance contracts."),
        
        p("These paragraphs in the standard provide guidance on:"),
        tags$ul(
            tags$li("When insurance contracts should be combined under IFRS 17"),
            tags$li("Recognition of how pricing dependencies affect contract treatment"),
            tags$li("Identification of when components within an insurance contract must be separated under IFRS 17"),
            tags$li("How IFRS 9 and IFRS 15 apply to embedded derivatives and non-insurance services")
        )
     ),

    div(class = "module-section",  
        p("IFRS 9 is an International Financial Reporting Standard (IFRS) that governs the accounting treatment of financial instruments. It took effect on January 1, 2018, replacing IAS 39."),
        
        p("IFRS 15 is the International Financial Reporting Standard that governs revenue recognition from contracts with customers. It took effect on January 1, 2018, replacing IAS 18 (Revenue) and IAS 11 (Construction Contracts)."),
        
        p("IFRS 17 requires insurers to separate certain components within an insurance contract when they would fall under other IFRS standards if treated separately."),
        
        p("This applies to:"),
        tags$ul(
            tags$li("embedded derivatives,"),
            tags$li("investment components, and"),
            tags$li("non-insurance services.")
        )
    ), 

    div(class = "module-section",
        h3("Embedded Derivatives"),
        
        p("They must be assessed under IFRS 9 to determine whether they should be separated. An embedded derivative is any financial instrument within an insurance contract that alters the cash flows based on external factors, such as interest rates, stock prices, or inflation indices. These derivatives are not separate contracts but are embedded within the insurance agreement itself.")
    ),

    div(class = "module-section",
        h3("Investment Components"),
        
        p("They are separated only if they are distinct. Distinct contracts mean contracts that can exist independently i.e not highly interrelated with the insurance component of the contract and contracts with similar terms can be sold independently in the marketplace. The contractual amount under a distinct investment component is payable to the policyholder even if the insured event does not occur. In this case IFRS 9 applies unless the component qualifies for discretionary participation features under IFRS 17."),
        
        p("Investment components here refer to any cash flow within an insurance contract that is not contingent on an insured event (e.g., savings or investment-linked benefits)."),
        
    ),
            
    div(class = "module-section",
        p("The investment component is not considered distinct when it is highly interrelated with the insurance component of the contract. In this case it is accounted for under IFRS 17 rather than IFRS 9. Such instances include when:"),
        
        tags$ul(
            tags$li("it cannot be separated from the insurance component without affecting the terms of the contract."),
            tags$li("the policyholder cannot benefit from the investment component independently of the insurance coverage."),
            tags$li("the cash flows of the investment component are significantly affected by the insurance component, meaning they are not separately identifiable.")
        )
    ),

    div(class = "module-section",
        h3("Non-insurance Services"),
        
        p("The non-insurance services such as wellness subscriptions within medical insurance policies must be separated and accounted for under IFRS 15 if they provide distinct goods or services beyond the insurance contract. IFRS 15 also guides how cash inflows and outflows are allocated between insurance and non-insurance components, ensuring financial reporting reflects the economic reality rather than just legal form.")
    ),

    div(class = "module-section",
        h3("How do pricing dependencies affect contract treatment?"),
        
        p("Contracts should be combined when the following characteristics are observed:"),
        tags$ol(
            tags$li("Interdependent pricing – if multiple contracts are priced together or structured as a package, they may need to be combined for financial reporting."),
            tags$li("Risk neutralization – if one contract eliminates the financial exposure of another, IFRS 17 may require treating them as a single arrangement."),
            tags$li("Overall commercial effect – contracts designed to work together to achieve a specific financial outcome must be reported as a unit. Commercial effect means the economic impact of a series of insurance contracts put together.")
        ),
        
        p("Contracts should be separated when the following characteristics are observed:"),
        tags$ol(
            tags$li("Distinct pricing – if contracts have independent pricing and do not rely on each other, they can be accounted for separately."),
            tags$li("Standalone risk profiles – if each contract carries its own risk without affecting the other, separation is appropriate."),
            tags$li("Different policyholder benefits – if contracts serve different purposes and do not interact financially, they remain separate.")
        )
    ),

    div(class = "module-section image-timeline-wrapper",
          h3("Decision Process for Separating Components", class = "section-subheading"),
          div(
              class = "timeline-image-container",
              img(
                  src = "images/separatingComponents.png",  # make sure you save the image to this path
                  alt = "Decision Process for Separating Components",
                  class = "timeline-image"
              )
          )
    ),  
    div(class = "module-section",
      h3("IFRS 4 vs IFRS 17", class = "section-subheading"),
      p("The table below shows how IFRS 4 and IFRS 17 treat the combination and separation of insurance contracts."),
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
      title = "1. An insurer enters into two separate contracts with the same policyholder at the same time. Contract A provides insurance coverage, while Contract B negates the financial exposure of Contract A entirely. According to IFRS 17, how should the insurer report these contracts?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "Treat the contracts as a single arrangement because they achieve an overall commercial effect",
        "Report both contracts separately as independent arrangements",
        "Recognize only Contract A since it was issued first",
        "Disclose both contracts but report them under IFRS 9"
      ), selected = character(0))
    ),

    box(
      title = "2. An insurer bundles multiple policies for a corporate client into a package with interdependent pricing. Some policies provide coverage, while others hedge specific risks associated with the insured entity. Under IFRS 17, how should these contracts be accounted for?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "Each contract must be evaluated individually regardless of interdependencies",
        "The bundled contracts should be treated as a single unit if they collectively achieve an overall commercial effect",
        "Contracts should be separated since they have different durations",
        "Each contract should be reported based on legal form rather than economic substance"
      ), selected = character(0))
    ),

    box(
      title = "3. Which of the following scenarios would not require the combination of contracts under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Two insurance contracts issued simultaneously to the same policyholder, with pricing designed to work together",
        "A reinsurance contract that fully offsets the risk of an insurance policy issued by the same insurer",
        "An insurance contract and an investment product sold separately with no dependency in pricing or risk",
        "A life insurance contract and a rider that cancels all coverage in the main policy"
      ), selected = character(0))
    ),

    box(
      title = "4. A life insurer offers a package where a main policy includes both insurance coverage and an investment component. The investment feature provides financial returns that could exist independently without the insurance portion. How should the insurer treat this arrangement under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "Recognize it as a single insurance contract",
        "Treat the entire contract under IFRS 9",
        "Combine the investment component only if it exceeds 50% of total premiums",
        "Separate the investment component if it can be sold independently"
      ), selected = character(0))
    ),

    box(
      title = "5. An insurer issues two separate policies to the same corporate client—one covering property damage and another covering business interruption losses linked to that property. The premiums are interdependent and structured as a bundle to provide a cohesive risk solution. What is the appropriate IFRS 17 treatment?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "The contracts should always be separated",
        "The contracts should be combined if pricing is interdependent",
        "The contracts must be accounted for under IFRS 9",
        "The contracts should be combined only if policyholders request it"
      ), selected = character(0))
    ),

    box(
      title = "6. An insurance contract includes an embedded derivative feature that alters cash flows based on a financial index. According to IFRS 17, how should this embedded derivative be accounted for?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "It must always remain part of the insurance contract",
        "It should be ignored unless the insurer requests separation",
        "It must be reported only in the contract disclosures",
        "It must be separated and accounted for under IFRS 9 if required"
      ), selected = character(0))
    ),

    box(
      title = "7. An insurer offers a contract that includes both insurance coverage and an investment component that can exist independently in the market. How should the investment component be treated under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "It should remain embedded in the insurance contract",
        "It should be accounted for under IFRS 15",
        "It must be separated only if it is distinct",
        "It should be reported only if the policyholder requests separate treatment"
      ), selected = character(0))
    ),

    box(
      title = "8. An insurance contract includes health coverage and an add-on subscription service for wellness programs, such as gym memberships and nutrition consultations. How should this non-insurance component be accounted for under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "It must remain part of the insurance contract under IFRS 17",
        "It should be accounted for separately using IFRS 15 if distinct",
        "It should be reclassified as an investment component under IFRS 9",
        "It must only be disclosed in the insurer’s financial statements"
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
    answer = "Treat the contracts as a single arrangement because they achieve an overall commercial effect",
    explanation = "When contracts are designed to achieve an overall commercial effect (such as one negating the obligations of another), IFRS 17 requires treating them as a single arrangement to reflect the economic substance."
  ),
  
  q2 = list(
    answer = "The bundled contracts should be treated as a single unit if they collectively achieve an overall commercial effect",
    explanation = "IFRS 17 mandates that contracts designed to work together as a package with shared pricing or risk mitigation should be combined to reflect their true economic impact."
  ),
  
  q3 = list(
    answer = "An insurance contract and an investment product sold separately with no dependency in pricing or risk",
    explanation = "If contracts have no interdependent pricing or risk structure, they do not need to be combined under IFRS 17. Separation is appropriate in such cases."
  ),
  
  q4 = list(
    answer = "Separate the investment component if it can be sold independently",
    explanation = "IFRS 17 requires separating investment components if they can function independently, ensuring accurate financial reporting."
  ),
  
  q5 = list(
    answer = "The contracts should be combined if pricing is interdependent",
    explanation = "IFRS 17 requires combining contracts that are designed to function together commercially, particularly if pricing reflects mutual risk dependencies."
  ),
  
  q6 = list(
    answer = "It must be separated and accounted for under IFRS 9 if required",
    explanation = "IFRS 17 directs insurers to apply IFRS 9 to determine whether an embedded derivative should be separated and how it should be accounted for."
  ),
  
  q7 = list(
    answer = "It must be separated only if it is distinct",
    explanation = "Investment components should be separated if they are distinct, meaning they can function independently. IFRS 9 applies unless the component qualifies for discretionary participation features under IFRS 17."
  ),
  
  q8 = list(
    answer = "It should be accounted for separately using IFRS 15 if distinct",
    explanation = "IFRS 17 requires separating non-insurance services if they provide distinct goods or services. The insurer must apply IFRS 15 to allocate cash flows and account for them separately."
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