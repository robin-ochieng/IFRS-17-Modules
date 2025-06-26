# modules/module1Module.R

# ---- UI ----
IFRS17Module1UI <- function(id) {
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
        h2("📘 Module 1 – Introduction & Scope of IFRS 17", class = "section-title-top")
    ),
    div(class = "module-section",
          h3("🎯 Module Objective"),
          p("This module aims to provide an overview of the introduction and scope of IFRS 17, as outlined in Paragraphs 1–8 of the Standard.")
    ),    
    div(class = "module-section",
      h3("📖 Introduction"),
      p("International Accounting Standards Board (IASB) introduced IFRS 17 Standard in May 2017 to replace IFRS 4."),
      p(HTML("The Standard sets out comprehensive requirements for the <strong>recognition, measurement, presentation, and disclosure</strong> of insurance contracts. It aims to ensure that an entity provides relevant information that faithfully represents those contracts.")),
    ),
    div(class = "module-section",
      p(HTML("In March 2020, IASB extended the implementation deadline from <strong>1<sup>st</sup> January 2022</strong> to <strong>1<sup>st</sup> January 2023</strong>.")),
      p(HTML("Insurers adopted the IFRS 17 reporting standard with the transition date being <strong>1<sup>st</sup> January 2022</strong>, and date of initial application being <strong>1<sup>st</sup> January 2023</strong>. Companies were expected to carry out testing and parallel runs of IFRS 17 processes and systems.")),
      p(HTML("IFRS 17 became effective on <strong>1<sup>st</sup> January 2023</strong>, and full compliance in financial reporting was required."))
    ),    
    div(class = "module-section image-timeline-wrapper",
          h3("🗓️ IFRS 17 Timeline", class = "section-subheading"),
          div(
              class = "timeline-image-container",
              img(
                  src = "images/ifrs17timeline.png",  # make sure you save the image to this path
                  alt = "IFRS 17 Timeline",
                  class = "timeline-image"
              )
          )
    ),
    div(class = "module-section",
      h3("🌐 Scope of IFRS 17"),
      p("An insurer is required to apply the Standard to the following:"),
      tags$ul(
        tags$li("Insurance contracts, including ", strong("reinsurance contracts"), ", it issues"),
        tags$li("Reinsurance contracts it holds; and"),
        tags$li(
          strong("Investment contracts with discretionary participation features"),
          " it issues, provided the entity also issues insurance contracts."
        )
      ),
      p("Discretionary participation features give policyholders the right to receive additional benefits at the discretion of the insurer. ",
        em("For example, bonuses added to participating life insurance policies based on the insurer’s investment performance.")
      ),
      p("Under IFRS 17, an insurance contract must involve an uncertain future event and significant insurance risk."),
      p("A key requirement is the transfer of risk, where the insurer must compensate the policyholder if the insured event has a negative impact on them.")
    ),

    div(class = "module-section image-timeline-wrapper",
      h3("🌐 Contracts outside the scope of IFRS 17", class = "section-subheading"),
      tags$div(class = "table-responsive",
      tags$table(class = "ifrs-table-module1",
        tags$thead(
          tags$tr(
            tags$th("Excluded Contract Type"),
            tags$th("Description"),
            tags$th("Treated Under")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Product warranties"),
            tags$td("Warranties provided by a manufacturer, dealer, or retailer linked to a sale"),
            tags$td("IFRS 15")
          ),
          tags$tr(
            tags$td("Employee benefits"),
            tags$td("Employer obligations under benefit plans for example pensions"),
            tags$td("IAS 19, IFRS 2, IAS 26")
          ),
          tags$tr(
            tags$td("Use of non-financial items"),
            tags$td("Rights/obligations based on future use of non-financial items, such as license fees, lease contingents"),
            tags$td("IFRS 15, IFRS 16, IAS 38")
          ),
          tags$tr(
            tags$td("Residual value guarantees"),
            tags$td("Often part of leasing or sales contracts"),
            tags$td("IFRS 15, IFRS 16")
          ),
          tags$tr(
            tags$td("Financial guarantee contracts"),
            tags$td("Unless the issuer opts to treat them as insurance"),
            tags$td("IFRS 9, IFRS 7, IAS 32")
          ),
          tags$tr(
            tags$td("Business Combinations"),
            tags$td("Contingent consideration receivable/payable"),
            tags$td("IFRS 3")
          ),
          tags$tr(
            tags$td("Policyholder Contracts"),
            tags$td("Insurance contracts where the entity is the policyholder, unless it's reinsurance held"),
            tags$td("Excluded from IFRS 17")
          )
        )
      )
     ),

      # Highlighted note below table
      p(em("*Description of various standards have been provided in the List of IFRS & IAS Standards"),
        style = "font-style: italic; color: #DC5A17; margin-top: 10px;")   
    ),

    div(class = "module-section",
    p("Some contracts may meet the definition of insurance contracts but are mainly meant to provide services for a fixed fee. An entity can apply IFRS 15 instead of IFRS 17 to these contracts if the conditions below exist:"),
      tags$ul(
        tags$li("The price set is not based on the risk of each individual customer."),
        tags$li("The contract provides compensation in the form of services, not cash payments."),
        tags$li("The insurance risk arises primarily from the customer's use of services, and not uncertainty about service cost.")
      )  
    ),

    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Introduction & Scope of IFRS 17"),
    ),


    box(
      title = "1. What is the primary objective of IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "To standardize insurance accounting globally",
        "To replace IFRS 16",
        "To define financial instruments",
        "To measure investment property"
      ), selected = character(0))
    ),

    box(
      title = "2. What does IFRS 17 replace?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "IAS 37",
        "IFRS 4",
        "IFRS 9",
        "IAS 40"
      ), selected = character(0))
    ),

    box(
      title = "3. What was the official date of initial application for IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "1st January 2022",
        "31st December 2022",
        "1st January 2023",
        "1st January 2021"
      ), selected = character(0))
    ),

    box(
      title = "4. IFRS 17 applies to?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "All insurance entities only",
        "Any entity issuing insurance contracts",
        "Reinsurers only",
        "Investment banks only"
      ), selected = character(0))
    ),

    box(
      title = "5. How does IFRS 17 define an insurance contract?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "Contract transferring insurance risk",
        "Contract transferring investment risk",
        "Contract transferring liquidity risk",
        "Contract for investment advice"
      ), selected = character(0))
    ),

    box(
      title = "6. How does IFRS 17 define 'insurance risk'?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "The risk of policyholder default",
        "The risk of future investment losses",
        "The risk transferred from the policyholder to the insurer due to uncertain future events",
        "Exchange rate risk"
      ), selected = character(0))
    ),

    box(
      title = "7. Which of the following contracts falls under the scope of IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "Product warranty issued by a retailer",
        "Lease contract under IFRS 16",
        "Financial guarantee contract under IFRS 9",
        "Reinsurance contract held by an insurer"
      ), selected = character(0))
    ),

    box(
      title = "8. Which contracts are only within IFRS 17 if the issuer also issues insurance contracts?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Leases",
        "Derivatives",
        "Term Deposits",
        "Investment contracts with discretionary participation features"
      ), selected = character(0))
    ),

    box(
      title = "9. Are product warranties issued by a retailer within IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "Yes, always",
        "No, they fall under IAS 37",
        "Only for 12-month terms",
        "Yes, if embedded in insurance"
      ), selected = character(0))
    ),

    box(
      title = "10. What type of contract is explicitly excluded from IFRS 17 scope?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "Group life insurance",
        "Reinsurance contracts",
        "Insurance-linked investments",
        "Financial guarantees (under IFRS 9)"
      ), selected = character(0))
    ),

    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")),
    
    div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_2"),
          label = tagList(icon("arrow-right"), "Next: Module 2 - Combination & Separation of Insurance Contracts"),
          class = "control-button-tavnav"
      )
    )   
  )
}

correct_answers_module1 <- list( 
  q1 = list(
    answer = "To standardize insurance accounting globally",
    explanation = "IFRS 17 aims to create a consistent accounting framework for insurance contracts to improve transparency and comparability."
  ),
  q2 = list(
    answer = "IFRS 4",
    explanation = "IFRS 17 replaced IFRS 4, which was an interim standard."
  ),
  q3 = list(
    answer = "1st January 2023",
    explanation = "The initial application date for IFRS 17 was 1st January 2023."
  ),
  q4 = list(
    answer = "Any entity issuing insurance contracts",
    explanation = "This reflects IFRS 17's scope, which applies to any entity that issues insurance contracts."
  ),
  q5 = list(
    answer = "Contract transferring insurance risk",
    explanation = "This captures the essential element of IFRS 17: transferring insurance risk from policyholder to insurer."
  ),
  q6 = list(
    answer = "The risk transferred from the policyholder to the insurer due to uncertain future events",
    explanation = "Insurance risk under IFRS 17 involves uncertainty about future events that may trigger insurer payment."
  ),
  q7 = list(
    answer = "Reinsurance contract held by an insurer",
    explanation = "Reinsurance contracts held are explicitly included under IFRS 17's scope."
  ),
  q8 = list(
    answer = "Investment contracts with discretionary participation features",
    explanation = "These contracts are only within the scope of IFRS 17 if issued by entities that also issue insurance contracts."
  ),
  q9 = list(
    answer = "No, they fall under IAS 37",
    explanation = "Retail product warranties are covered by IAS 37, not IFRS 17."
  ),
  q10 = list(
    answer = "Financial guarantees (under IFRS 9)",
    explanation = "Financial guarantee contracts are usually treated under IFRS 9 unless specifically designated as insurance."
  )
)



# ---- Server ----
IFRS17Module1Server <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {
   
    # bring ns into scope
    ns <- session$ns

    # Add a reactive value to track when progress is saved
    progress_saved_trigger <- reactiveVal(0)

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
        
        
        # ———————————
        # 5. All answered: clear any existing modal, then run your scoring code
        removeModal()


        score(0)
        feedback <- list()

    for (qid in names(correct_answers_module1)) {
      correct <- correct_answers_module1[[qid]]$answer
      explanation <- correct_answers_module1[[qid]]$explanation
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
        total_questions <- length(correct_answers_module1)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module1",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 1 Progress Saved!</strong><br>",
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
          print(paste("Module 1 progress save error:", e$message))
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
      total_questions <- length(correct_answers_module1)
      percentage       <- round((score() / total_questions) * 100, 1)
      color            <- if (percentage >= 70) "#fff" else "#fff"

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
            h4("has successfully completed the IFRS 17 - Introduction & Scope of IFRS17 module",
              style = "
                font-family: 'Nunito', sans-serif;
                font-weight: 400;
                font-style: italic;
                margin-top: 0;
                margin-bottom: 20px;
                color: #343a40;
              "),
              # recipient name
            style = "
              font-family: 'Nunito', sans-serif;
              font-size: 28px;
              margin: 0;
              color: #fff;
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
              "📊 Module 1 Results Summary",
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

    })  
    })

    # create a reactive for the “Next” click
    to_module_2 <- reactive(input$to_module_2)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_2,
      progress_trigger = progress_saved_trigger
    ))

  })
}
