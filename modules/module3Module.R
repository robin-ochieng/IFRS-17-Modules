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
            h3("📖 Introduction", class = "section-subheading"),
            p("This module introduces the principles behind how insurance contracts are grouped for measurement and reporting purposes in accordance with IFRS 17.")
        ),
        
        div(class = "module-section",
            h3("What is Aggregation in IFRS 17?", class = "section-subheading"),
            p("In IFRS 17, aggregation involves organizing insurance contracts that exhibit similar risk profiles and are managed collectively. This structure supports appropriate timing of profit recognition and early identification of losses."),
            p("Under IFRS 17, aggregation refers to the grouping of insurance contracts that share similar risks and are managed together. This helps ensure accurate loss recognition and profit timing in financial reporting.")
        ),

        div(class = "module-section",
            p("The grouping of these contracts follows the process below:"),
            img(src = "images/groupingContractsProcess.png", 
                alt = "Grouping of Insurance Contracts Process", 
                class = "groupingContractsProcess-image")
        ),        
        div(class = "module-section",
            h3("Use of Premium Allocation Approach (PAA)", class = "section-subheading"),
            p("When applying the PAA, it is generally assumed that contracts are not onerous at initial recognition unless evidence suggests otherwise."),
            p("Insurers must regularly reassess these assumptions and evaluate whether changing circumstances or risks could make contracts onerous over time.")
        ),

        div(class = "module-section",
            h3("Contracts Outside the Premium Allocation Approach", class = "section-subheading"),
            p("For contracts not using the PAA, even those not considered onerous at initial recognition must be assessed for the risk of becoming onerous. This should be based on:"),
            tags$ol(type = "a",
              tags$li("Possible changes in assumptions that could lead to a loss"),
              tags$li("Information derived from the entity’s internal reporting systems")
            ),
            h3("Consistency and Grouping Duration", class = "section-subheading"),
            p("Contracts issued more than 12 months apart must not be included in the same group."),
            p("Once contracts are grouped at initial recognition, the composition of the group is fixed and cannot be changed later.")
        ),        

        div(class = "module-section",
            h3("📝 Quiz: Answer the following questions to test your understanding of Level of Aggregation."),
        ),


        box(
          title = "1. What is the primary objective of aggregation under IFRS 17?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q1"), label = NULL, choices = c(
            "Maximize premium income",
            "Organize insurance contracts for taxation purposes",
            "Group contracts with similar risk profiles for financial reporting",
            "Simplify customer service operations"
          ), selected = character(0))
        ),

        box(
          title = "2. According to IFRS 17, contracts in a portfolio must:",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q2"), label = NULL, choices = c(
            "Share common features and be under the same management strategy",
            "Have similar geographical locations",
            "Be managed by the same investor",
            "Be issued by the same insurance agent"
          ), selected = character(0))
        ),

        box(
          title = "3. How are insurance contracts first grouped under IFRS 17?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q3"), label = NULL, choices = c(
            "By insurer location",
            "Into portfolios",
            "By premium size",
            "By policyholder's age"
          ), selected = character(0))
        ),

        box(
          title = "4. What is the purpose of creating annual cohorts?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q4"), label = NULL, choices = c(
            "Divide grouped contracts based on the year they were issued",
            "Simplify reinsurance procedures",
            "Classify contracts by region",
            "Separate high-risk and low-risk contracts"
          ), selected = character(0))
        ),

        box(
          title = "5. Contracts issued in the same calendar year:",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q5"), label = NULL, choices = c(
            "Must be grouped in one portfolio",
            "Form a distinct cohort",
            "Must be re-evaluated monthly",
            "Cannot be grouped under IFRS 17"
          ), selected = character(0))
        ),

        box(
          title = "6. What assumption is made when applying the Premium Allocation Approach (PAA)?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q6"), label = NULL, choices = c(
            "Contracts are always onerous",
            "All contracts are short-term",
            "Contracts must be reinsured",
            "Contracts are not onerous at initial recognition"
          ), selected = character(0))
        ),

        box(
          title = "7. What must be assessed for contracts outside the PAA?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q7"), label = NULL, choices = c(
            "Tax compliance",
            "Policyholder credit score",
            "Risk of becoming onerous",
            "Employee performance"
          ), selected = character(0))
        ),

        box(
          title = "8. What could trigger a contract to be considered onerous later?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q8"), label = NULL, choices = c(
            "Change in marketing team",
            "Changes in assumptions leading to losses",
            "Policyholder complaints",
            "Reduced customer engagement"
          ), selected = character(0))
        ),

        box(
          title = "9. Which of the following supports assessment of onerous contracts?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q9"), label = NULL, choices = c(
            "Customer feedback surveys",
            "Competitor sales reports",
            "Social media data",
            "Internal reporting systems"
          ), selected = character(0))
        ),

        box(
          title = "10. What is stated about grouping duration under IFRS 17?",
          status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
          radioButtons(ns("q10"), label = NULL, choices = c(
            "Contracts can be grouped at any time",
            "Contracts from different years can be combined",
            "Contracts issued over 12 months apart must not be in the same group",
            "Contracts should be grouped every quarter"
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
    answer     = "Group contracts with similar risk profiles for financial reporting",
    explanation = "IFRS 17 aggregation aims to group contracts with similar risks to ensure accurate loss recognition and profit timing."
  ),
  q2 = list(
    answer     = "Share common features and be under the same management strategy",
    explanation = "Portfolios include contracts with similar features that are managed together under a consistent strategy."
  ),
  q3 = list(
    answer     = "Into portfolios",
    explanation = "The first step in grouping under IFRS 17 is establishing portfolios of contracts."
  ),
  q4 = list(
    answer     = "Divide grouped contracts based on the year they were issued",
    explanation = "Annual cohorts ensure that contracts from different years are not grouped together."
  ),
  q5 = list(
    answer     = "Form a distinct cohort",
    explanation = "Contracts issued in the same calendar year form a distinct annual cohort under IFRS 17."
  ),
  q6 = list(
    answer     = "Contracts are not onerous at initial recognition",
    explanation = "The PAA allows insurers to assume contracts are non-onerous at initial recognition unless evidence suggests otherwise."
  ),
  q7 = list(
    answer     = "Risk of becoming onerous",
    explanation = "Contracts outside the PAA must be assessed for the risk of becoming onerous over time."
  ),
  q8 = list(
    answer     = "Changes in assumptions leading to losses",
    explanation = "New or changing assumptions that indicate potential losses may trigger a contract to be classified as onerous."
  ),
  q9 = list(
    answer     = "Internal reporting systems",
    explanation = "Internal systems provide the data used to assess whether contracts may become onerous."
  ),
  q10 = list(
    answer     = "Contracts issued over 12 months apart must not be in the same group",
    explanation = "IFRS 17 requires that contracts more than 12 months apart are not grouped together."
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



