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
          h3(icon("info-circle"), "Module Overview", class = "section-subheading"),
          p(HTML("This module gives an overview of the introduction and scope of IFRS 17, as outlined in <strong>IFRS 17 Insurance Contracts</strong> of the Standard."))
    ),    
    div(class = "module-section",
      h3("📖 Introduction", class = "section-subheading"),
      p(HTML("The International Accounting Standards Board (IASB) introduced IFRS 17 Standard in May 2017 to replace IFRS 4, which had served as an interim standard. IFRS 17 establishes a consistent and detailed framework for the <strong>recognition, measurement, presentation, and disclosure</strong> of insurance contracts, aiming to enhance transparency and comparability in financial reporting.")),
      p(HTML("Initially set to become effective on <strong>1 January 2022</strong>, the standard’s implementation was deferred in March 2020 to allow insurers more time to prepare. The revised effective date was <strong>1 January 2023</strong>, with <strong>1 January 2022</strong> designated as the transition date. During this period, insurers were expected to conduct parallel runs and system testing to ensure a smooth shift to the new standard.")),
      p(HTML("IFRS 17 became effective on <strong>1 January 2023</strong>, where full compliance was required for all entities issuing insurance contracts."))
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
      h3("🌐 Scope of IFRS 17", class = "section-subheading"),
      p("An insurer must apply the Standard to the following:"),
      tags$ul(
        tags$li("Insurance contracts, including ", strong("reinsurance contracts"), ", it issues"),
        tags$li("Reinsurance contracts it holds; and"),
        tags$li(
          strong("Investment contracts with discretionary participation features"),
          " it issues, provided the entity also issues insurance contracts."
        )
      )),
      div(class = "module-section",
        h3("🛡️ Insurance contracts under IFRS 17", class = "section-subheading"),
        p("A contract is considered an insurance contract under IFRS 17 if:"),
        tags$ul(
          tags$li("It involves an ", strong("uncertain future event"), " (the insured event)."),
          tags$li("It transfers ", strong("significant insurance risk"), " which requires the insurer to compensate the policyholder for adverse impacts.")
        )
      ),

      div(class = "module-section",
        h3("🌐 Contracts outside the scope of IFRS 17", class = "section-subheading"),
        p("Certain contracts, although they may appear similar to insurance in nature, fall outside the scope of IFRS 17 and are addressed under other accounting standards:"),
        tags$ol(type = "i",
          tags$li(HTML("<strong>Product warranties</strong>, typically provided by manufacturers or retailers as part of a sale, are not treated as insurance contracts and are accounted for under <strong>IFRS 15</strong>.")),
          tags$li(HTML("<strong>Employee benefit obligations</strong>, such as pensions or share-based payments, are governed by <strong>IAS 19</strong>, <strong>IFRS 2</strong>, and <strong>IAS 26</strong>, reflecting their role as employer-employee arrangements rather than insurance.")),
          tags$li(HTML("<strong>Contracts involving the right to use non-financial assets</strong> (e.g. licenses or leased equipment) are excluded and instead addressed under <strong>IFRS 15</strong>, <strong>IFRS 16</strong>, or <strong>IAS 38</strong>, depending on their nature.")),
          tags$li(HTML("<strong>Residual value guarantees</strong>, often embedded within leasing or sales arrangements, are accounted for under <strong>IFRS 15</strong> or <strong>IFRS 16</strong>, as they do not meet the definition of insurance risk.")),
          tags$li(HTML("<strong>Financial guarantee contracts</strong> typically fall under <strong>IFRS 9</strong> (and <strong>IFRS 7</strong> for disclosure), unless the issuer irrevocably elects to treat them as insurance under IFRS 17.")),
          tags$li(HTML("In <strong>business combinations</strong>, contingent consideration that becomes payable or receivable is scoped under <strong>IFRS 3</strong>.")),
          tags$li(HTML("Contracts where the entity is the <strong>policyholder</strong>, such as when insurance is purchased rather than issued, do not fall within IFRS 17, unless they represent <strong>reinsurance held</strong>, which is explicitly included in the standard."))
        )
      ),


      div(class = "module-section",
        h3("🔧 Service-based contracts accounted for under IFRS 15", class = "section-subheading"),
        p(HTML("Some contracts, while technically insurance contracts, are primarily meant to provide services for a fixed fee, and may be accounted for under <strong>IFRS 15</strong> if the conditions below exist:")),
        tags$ol(type = "a",
          tags$li("The pricing does not vary based on the individual risk profile of the customer."),
          tags$li("Compensation provided is in the form of services, not cash payments."),
          tags$li("The insurance risk arises mainly from the customer’s use of services, rather than the uncertainty about service costs.")
        )
      ),


    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Introduction & Scope of IFRS 17"),
    ),


    box(
      title = "1. Which of the following best describes the purpose of IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "To prescribe lease accounting principles for lessors and lessees",
        "To standardize accounting for insurance contracts across entities and jurisdictions",
        "To classify and measure financial assets",
        "To consolidate group insurance financials"
      ), selected = character(0))
    ),

    box(
      title = "2. What is the transition date under IFRS 17 for most insurers adopting the standard in 2023?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "31 December 2022",
        "1 January 2022",
        "1 January 2023",
        "31 December 2023"
      ), selected = character(0))
    ),

    box(
      title = "3. Which of the following is not considered an insurance contract under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "A health insurance policy",
        "A product warranty issued by a manufacturer",
        "A life insurance contract",
        "A reinsurance contract issued"
      ), selected = character(0))
    ),

    box(
      title = "4. Under IFRS 17, what characteristic must a contract have to be within scope?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "Investment guarantees",
        "Transfer of insurance risk",
        "Premium collection",
        "Asset management services"
      ), selected = character(0))
    ),

    box(
      title = "5. Under what conditions might a contract that appears to transfer insurance risk be accounted for under IFRS 15 instead of IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "When the contract includes a financial guarantee",
        "When the issuer is not a licensed insurer",
        "When compensation is provided through services, pricing is fixed, and risk depends on service use",
        "When the contract involves pooling of investment returns"
      ), selected = character(0))
    ),

    box(
      title = "6. Which of the following would trigger the application of IFRS 17 to an investment contract?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "It includes an embedded derivative",
        "It carries no guaranteed benefits",
        "It has discretionary participation features and is issued by an insurer",
        "It is backed by financial instruments"
      ), selected = character(0))
    ),

    box(
      title = "7. Which of the following would likely be excluded from IFRS 17 scope, even if issued by an insurer?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "Group annuity",
        "Property cover with risk pooling",
        "Credit card insurance",
        "Service contract with no significant insurance risk"
      ), selected = character(0))
    ),

    box(
      title = "8. Which contract is explicitly included in IFRS 17 scope?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "Reinsurance contracts held",
        "Employee benefit obligations",
        "Derivatives on mortality rates",
        "Financial guarantee contracts under IFRS 9"
      ), selected = character(0))
    ),

    box(
      title = "9. What determines whether a contract qualifies as an insurance contract under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "Whether it's regulated by an insurance authority",
        "The presence of an underwriting process",
        "The existence of a pool of assets",
        "Transfer of risk due to uncertain future insured events"
      ), selected = character(0))
    ),

    box(
      title = "10. Which of the following scenarios would be clearly outside the scope of IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "A reinsurer assuming risk from a direct insurer",
        "A government grant for primary healthcare services",
        "An insurer issuing a policy with both investment and insurance components",
        "A life insurance company issuing participating contracts"
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
    answer = "To standardize accounting for insurance contracts across entities and jurisdictions",
    explanation = "IFRS 17 was developed to bring consistency and comparability to insurance contract accounting worldwide."
  ),
  q2 = list(
    answer = "1 January 2022",
    explanation = "The transition date under IFRS 17 for comparative figures was 1 January 2022."
  ),
  q3 = list(
    answer = "A product warranty issued by a manufacturer",
    explanation = "Product warranties issued directly by manufacturers are accounted for under IAS 37, not IFRS 17."
  ),
  q4 = list(
    answer = "Transfer of insurance risk",
    explanation = "A contract must transfer insurance risk from the policyholder to the issuer to be within IFRS 17 scope."
  ),
  q5 = list(
    answer = "When compensation is provided through services, pricing is fixed, and risk depends on service use",
    explanation = "IFRS 15 may apply if the contract is service-based, pricing is not risk-adjusted, and benefits are delivered through services rather than cash, even if some insurance-like risk exists."
  ),
  q6 = list(
    answer = "It has discretionary participation features and is issued by an insurer",
    explanation = "Investment contracts with discretionary participation features fall under IFRS 17 only if issued by entities that also issue insurance contracts."
  ),
  q7 = list(
    answer = "Service contract with no significant insurance risk",
    explanation = "Contracts that do not transfer significant insurance risk are excluded from IFRS 17 scope, even if issued by an insurer."
  ),
  q8 = list(
    answer = "Reinsurance contracts held",
    explanation = "Reinsurance contracts held involve risk transfer between insurers and are explicitly within IFRS 17 scope."
  ),
  q9 = list(
    answer = "Transfer of risk due to uncertain future insured events",
    explanation = "The defining characteristic of an insurance contract under IFRS 17 is the transfer of insurance risk due to uncertain future events."
  ),
  q10 = list(
    answer = "A government grant for primary healthcare services",
    explanation = "Government grants for non-insurance services (like healthcare subsidies) do not meet the definition of an insurance contract under IFRS 17 and typically fall under IAS 20 or IPSAS frameworks."
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
