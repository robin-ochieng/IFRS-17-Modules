# modules/module3Module.R
# This module is for the IFRS 17 module 4
IFRS17Module3UI <- function(id) {
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
          h2("📘 Module 3: Level of Aggregation", class = "section-title-top")
      ),

        div(class = "module-section",
            h3("📖 Introduction"),
            p("This module provides an overview on how insurance contracts are grouped for measurement and reporting as per IFRS 17 paragraph 14 to paragraph 24.")
        ),
        
        div(class = "module-section",
            h3("What is Aggregation in IFRS 17?"),
            p("Under IFRS 17, aggregation refers to the grouping of insurance contracts that share similar risks and are managed together. This helps ensure accurate loss recognition and profit timing in financial reporting.")
        ),
        
        div(class = "module-section",
            h3(class = "step-title", "Step 1: Grouping by Portfolio"),
            p("A portfolio consists of insurance contracts that share:"),
            tags$ol(
                tags$li("Similar characteristics – e.g., motor insurance, term life products"),
                tags$li("Similar management structure")
            )
        ),
        
        div(class = "module-section",
            h3(class = "step-title", "Step 2: Subdividing Portfolios into Groups"),
            p("Once portfolios are defined, each must further be split into at least three distinct groups:"),
            tags$ol(
                tags$li(strong("Onerous Contracts at Initial Recognition – "), "These contracts are expected to generate a loss from the beginning"),
                tags$li(strong("Contracts with no significant risk of becoming onerous – "), "Expected to remain profitable or at least break even"),
                tags$li(strong("Remaining Contracts – "), "Not considered loss-making upfront, but might become onerous over time")
            )
        ),
        
        div(class = "module-section",
            h3(class = "step-title", "Step 3: Breakdown of Cohorts"),
            p("After subdivision, contracts shall be grouped into cohorts based on issue dates. All contracts issued within the same calendar year shall form a single cohort.")
        ),
        
        div(class = "module-section",
            h3("The Grouping Process"),
            img(src = "images/portfolioContracts.png", 
                alt = "IFRS 17 Level of Aggregation Diagram", 
                class = "aggregation-image")
        ),

        div(class = "module-section",
            h3("🔍 Assessing Onerous Contracts under Premium Allocation Approach", class = "subheading-highlight"),
            
            p("Under the Premium Allocation Approach (PAA), it is assumed that no contracts in a portfolio are onerous at initial recognition ",
              strong("unless facts and circumstance indicate otherwise.")),
            
            p("Contracts that are not onerous initially must also be assessed on whether they might become onerous later, by evaluating ",
              strong("potential changes in facts and circumstances.")),
            
            p("For contracts which do not apply the premium allocation approach, ",
              a(href = "#", "the contracts"),
              " that are not onerous at initial recognition must be assessed whether they have any significant probability of becoming onerous."),
            
            p("This shall be based on:"),
            
            tags$ul(
              tags$li("Likelihood of changes in assumptions which may result in the contract becoming onerous"),
              tags$li(HTML('Information provided by an entity’s internal <a href="#">reporting</a>.'))
            )
        ),
        div(class = "module-section",            
            h3("📌 Grouping Timeframe and Consistency", class = "step-title"),
            
            p("While grouping, contracts issued more than one year apart must not be included in the same group."),
            
            p("Additionally, once contract groupings are determined at initial recognition, they must remain fixed; the composition of groups cannot be reassessed or changed later.")
        ),

        div(class = "module-section",
            h3("📝 Quiz: Answer the following questions to test your understanding of Level of Aggregation."),
        ),


        box(
          title = "1. What is the main purpose of aggregation under IFRS 17?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q1"), label = NULL, choices = c(
            "To reduce the number of contracts reported",
            "To ensure accurate timing of profit and loss recognition",
            "To make contract management easier",
            "To avoid having to assess individual contracts"
          ), selected = character(0))
        ),

        box(
          title = "2. Under IFRS 17, contracts grouped into the same portfolio must share:",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q2"), label = NULL, choices = c(
            "The same inception date",
            "The same profit margin",
            "The same policyholder",
            "Similar risk characteristics and management structure"
          ), selected = character(0))
        ),

        box(
          title = "3. How far apart can contract issuance dates be within the same group?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q3"), label = NULL, choices = c(
            "Any number of years",
            "Two years",
            "Not more than one year",
            "Three years if risk is similar"
          ), selected = character(0))
        ),

        box(
          title = "4. What is the first step in the aggregation process under IFRS 17?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q4"), label = NULL, choices = c(
            "Grouping by issuance year",
            "Subdividing portfolios",
            "Grouping by portfolio",
            "Assessing profitability"
          ), selected = character(0))
        ),

        box(
          title = "5. Under IFRS 17, why are insurers not allowed to reassess contract groups after initial recognition?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q5"), label = NULL, choices = c(
            "To maintain consistency and transparency in reporting",
            "To reduce workload",
            "To allow for more flexibility later",
            "Because contracts cannot change after issuance"
          ), selected = character(0))
        ),

        box(
          title = "6. How does IFRS 17 recommend handling groups of contracts under the Premium Allocation Approach (PAA)?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q6"), label = NULL, choices = c(
            "Assume they are always profitable",
            "Assume all contracts are onerous",
            "Group them based on product type only",
            "Assume none are onerous at initial recognition unless facts suggest otherwise"
          ), selected = character(0))
        ),

        box(
          title = "7. What additional check must be done for policies eligible for the General Measurement Model (GMM)?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q7"), label = NULL, choices = c(
            "Verification of market premium rates",
            "Sensitivity testing and internal report reviews",
            "Reinsurance matching",
            "Underwriter interviews"
          ), selected = character(0))
        ),

        box(
          title = "8. Which of the following best describes a 'portfolio' under IFRS 17?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q8"), label = NULL, choices = c(
            "A collection of policies sold by one agent",
            "Contracts grouped based on risk and management similarity",
            "Contracts grouped by coverage period",
            "All insurance contracts issued in one year"
          ), selected = character(0))
        ),

        box(
          title = "9. What should an entity use to assess whether a contract might become onerous later?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q9"), label = NULL, choices = c(
            "Market interest rates",
            "Past claims history only",
            "Likelihood of changes in applicable facts and circumstances",
            "Broker recommendations"
          ), selected = character(0))
        ),

        box(
          title = "10. What happens if a contract becomes onerous after initial recognition?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q10"), label = NULL, choices = c(
            "The group composition remains unchanged",
            "It is moved to the 'onerous' group retroactively",
            "The contract is cancelled",
            "A new group is created"
          ), selected = character(0))
        ),


    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")),

    div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_4"),
          label = tagList(icon("arrow-right"), "Next: Module 4 – Recognition of Insurance Revenue"),
          class = "control-button-tavnav"
      )
    )  
)
}

correct_answers_module3 <- list( 
  q1 = list(
    answer = "To ensure accurate timing of profit and loss recognition",
    explanation = "Aggregation helps ensure that profits and losses are recognized accurately and consistently in financial reporting."
  ),
  q2 = list(
    answer = "Similar risk characteristics and management structure",
    explanation = "Portfolios are formed based on risk similarity and being managed together."
  ),
  q3 = list(
    answer = "Not more than one year",
    explanation = "IFRS 17 requires that all contracts in a group are issued no more than one year apart."
  ),
  q4 = list(
    answer = "Grouping by portfolio",
    explanation = "Aggregation begins by forming portfolios based on similar risks and management structures."
  ),
  q5 = list(
    answer = "To maintain consistency and transparency in reporting",
    explanation = "Fixing the groupings at initial recognition supports consistent, unbiased financial reporting over time."
  ),
  q6 = list(
    answer = "Assume none are onerous at initial recognition unless facts suggest otherwise",
    explanation = "IFRS 17 allows insurers applying the PAA to assume contracts are not onerous at initial recognition, unless evidence indicates otherwise."
  ),
  q7 = list(
    answer = "Sensitivity testing and internal report reviews",
    explanation = "Sensitivity testing and internal reporting are used to confirm profitability assumptions for GMM-eligible contracts."
  ),
  q8 = list(
    answer = "Contracts grouped based on risk and management similarity",
    explanation = "A portfolio consists of contracts that have similar risk characteristics and are managed together."
  ),
  q9 = list(
    answer = "Likelihood of changes in applicable facts and circumstances",
    explanation = "Entities must consider whether new or changing circumstances might render a contract onerous in the future."
  ),
  q10 = list(
    answer = "The group composition remains unchanged",
    explanation = "Group compositions are fixed at initial recognition, even if a contract’s status changes later."
  )
)



IFRS17Module3Server <- (function(id, user_data) {
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

    for (qid in names(correct_answers_module3)) {
      correct <- correct_answers_module3[[qid]]$answer
      explanation <- correct_answers_module3[[qid]]$explanation
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
      # Save progress for Module 3
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 2 specific calculations
        total_questions <- length(correct_answers_module3)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module3",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 3 Progress Saved!</strong><br>",
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
          print(paste("Module 3 progress save error:", e$message))
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
      total_questions <- length(correct_answers_module3)
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
              "📊 Module 3 Results Summary",
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
    to_module_4 <- reactive(input$to_module_4)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_4,
      progress_trigger = progress_saved_trigger
    ))

  })
})



