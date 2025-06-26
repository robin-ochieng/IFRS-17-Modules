# This module is for the IFRS 17 module 12
IFRS17Module11UI <- function(id) {
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
      h2("📘 Module 11: Investment Contracts with Discretionary Participation Features", class = "section-title-top")
    ),

    div(class = "module-section",
        h3("📖 Introduction"),
        p("This module explores the treatment of investment contracts with discretionary participation features (DPF) under IFRS 17, focusing on their recognition, measurement, and presentation."),
        img(src = "images/module11Introduction.png", class = "module-image")
    ),  

    div(class = "module-section",
        p("Under IFRS 17, the following refences cover on Investment Contracts with Discretionary Participation Features:"),
        p("The underlying items are items that determine some of the amounts payable to a policyholder. These can include a reference portfolio, net assets, or even external indices—not limited to physical assets. IFRS 17 requires the pool of underlying items to be fixed and clearly identified—retrospective changes to the pool violate this requirement. The module also discusses the treatment of investment contracts with DPF, which are contracts that provide a return linked to the performance of underlying items, such as mutual funds or unit-linked insurance products.")
    ),

    div(class = "module-section",
        p("These contracts may include both insurance and non-insurance components, and their measurement often involves significant judgement due to the discretionary nature of certain cash flows. The Variable Fee Approach (VFA) may be applicable for eligible contracts, where the entity’s fee is considered to vary with the underlying asset returns. This module also outlines the conditions required for applying the VFA, and sets the stage for understanding how these unique contract types are integrated into IFRS 17's overall framework"),
        p("The underlying items are items that determine some of the amounts payable to a policyholder. These can include a reference portfolio, net assets, or even external indices—not limited to physical assets. IFRS 17 requires the pool of underlying items to be fixed and clearly identified—retrospective changes to the pool violate this requirement. The module also discusses the treatment of investment contracts with DPF, which are contracts that provide a return linked to the performance of underlying items, such as mutual funds or unit-linked insurance products.")
    ),

    div(class = "module-section",
      p("Under IFRS 17, the following references cover Investment Contracts with Discretionary Participation Features:"),
      tags$div(class = "table-responsive",
      tags$table(class = "table-dpf-reference",
        tags$thead(
          tags$tr(
            tags$th("Reference"),
            tags$th("Section"),
            tags$th("Content Summary")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Para 3(c)"),
            tags$td("Scope"),
            tags$td("IFRS 17 applies to investment contracts with DPFs if issued by insurers (i.e., entities also issuing insurance contracts).")
          ),
          tags$tr(
            tags$td("Appendix A"),
            tags$td("Definitions"),
            tags$td("Defines investment contracts with DPFs and sets three key criteria: discretion, significance, and performance linkage.")
          ),
          tags$tr(class = "dpf-highlight",
            tags$td("Para 71"),
            tags$td("Recognition & Measurement"),
            tags$td(HTML("Modifications for DPFs:<br>
                          - Recognize when the entity becomes party to the contract<br>
                          - Contract boundary: based on substantive investment services<br>
                          - CSM allocated to reflect investment service (not insurance coverage)"))
          ),
          tags$tr(class = "dpf-highlight",
            tags$td("BC83–BC86"),
            tags$td("Basis for Conclusions"),
            tags$td(HTML("Justifies including investment contracts with DPFs under IFRS 17:<br>
                          - Economic substance is similar to insurance contracts<br>
                          - Ensures consistent accounting treatment across products"))
          ),
          tags$tr(
            tags$td("Para 88–89"),
            tags$td("Contractual Service Margin (CSM)"),
            tags$td("Explains systematic allocation of CSM for DPF contracts based on the transfer of investment service")
          ),
          tags$tr(
            tags$td("Para B73–B75"),
            tags$td("Implementation Guidance"),
            tags$td("Provides guidance on investment services and how to allocate CSM accordingly")
          )
        )
      )
    )
    ),


    div(
      class = "module-section",
      h3("What is an Investment Contract with Discretionary Participation Features (DPF)?"),
      p("An Investment Contract with DPF is a financial contract that:"),
      tags$ul(
        tags$li("Does not transfer significant insurance risk,"),
        tags$li(
          HTML("But gives the policyholder a contractual right to receive additional benefits that are:"),
          tags$ol(
            tags$li("Significant,"),
            tags$li("Discretionary (the issuer has some freedom in determining them),"),
            tags$li("And based on the performance of a pool of underlying items (like assets, other contracts, or profits of the entity).")
          )
        )
      ),
      p("IFRS 17 requires that such contracts be accounted for, but with some modifications, only if the issuer also issues insurance contracts.")
    ),
    div(
      class = "module-section",
      p("Examples of Investment Contract with Discretionary Participation Features:"),
      tags$ol(
        tags$li(
          tags$strong("With-profits savings policies (no insurance risk)"),
          tags$br(),
          tags$em("Bonuses based on fund performance, no life cover.")
        ),
        tags$li(
          tags$strong("Deferred annuities (investment only)"),
          tags$br(),
          tags$em("Accumulates value, bonuses discretionary, no guarantees.")
        ),
        tags$li(
          tags$strong("Participating in deposit contracts"),
          tags$br(),
          tags$em("Base return + discretionary top-up from profits.")
        ),
        tags$li(
          tags$strong("Group savings plans"),
          tags$br(),
          tags$em("Employees share in pooled investment performance; bonuses are discretionary.")
        )
      )
    ),
    div(
      class = "module-section",
      h3("Direct Participation Contracts and Investment Contracts with DPFs"),
      tags$div(class = "table-responsive",
      tags$table(class = "styled-ifrs-table",
        tags$thead(
          tags$tr(
            tags$th(""),
            tags$th("Direct Participation Contracts"),
            tags$th("Investment Contracts with DPFs")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$th(class = "highlight-header", "Definition"),
            tags$td("A type of insurance contract where the policyholder shares in the performance of clearly identified underlying items."),
            tags$td("A financial contract (not insurance) that gives the holder the right to discretionary returns based on performance of a pool of assets.")
          ),
          tags$tr(
            tags$th(class = "highlight-header", "Standard"),
            tags$td("Fully within the scope of IFRS 17, using the Variable Fee Approach (VFA)."),
            tags$td("Also, within the scope of IFRS 17 (with modifications), but only if the issuer also issues insurance contracts.")
          ),
          tags$tr(
            tags$th(class = "highlight-header", "Purpose"),
            tags$td("Combines insurance coverage with investment return sharing."),
            tags$td("Provides investment-like benefits with discretionary bonuses, but no insurance risk.")
          )
        )
      )
    )
    ),

    div(
      class = "module-section",
      h3("Key Characteristics Comparison"),
      tags$div(class = "table-responsive",
      tags$table(class = "styled-ifrs-table",
        tags$thead(
          tags$tr(
            tags$th("Feature"),
            tags$th("Direct Participation Contracts (DPCs)"),
            tags$th("Investment Contracts with DPFs")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$th(class = "highlight-brown", "Insurance Risk Present?"),
            tags$td("✔ Yes"),
            tags$td("✘ No")
          ),
          tags$tr(
            tags$th(class = "highlight-brown", "Participation in Underlying Items?"),
            tags$td("✔ Yes (clearly identified)"),
            tags$td("✔ Yes")
          ),
          tags$tr(
            tags$th(class = "highlight-brown", "Discretionary Participation?"),
            tags$td("✔ Yes"),
            tags$td("✔ Yes")
          ),
          tags$tr(
            tags$th(class = "highlight-brown", "Measurement Approach"),
            tags$td("Variable Fee Approach (VFA) under IFRS 17"),
            tags$td("General Model with modifications under IFRS 17")
          ),
          tags$tr(
            tags$th(class = "highlight-brown", "Eligibility Criteria"),
            tags$td(HTML("Must meet 3 conditions:<br>1. Share in underlying items<br>2. Substantial share of fair value returns<br>3. Payments vary with fair value changes")),
            tags$td("Must provide discretionary returns + issued by insurer that also issues insurance contracts")
          ),
          tags$tr(
            tags$th(class = "highlight-brown", "Discounting"),
            tags$td("Uses current rates for cash flows not based on underlying items"),
            tags$td("Similar, but less focus on variability in fees")
          ),
          tags$tr(
            tags$th(class = "highlight-brown", "CSM Adjustment for Financial Risk?"),
            tags$td("✔ Yes, using current interest rates"),
            tags$td(HTML("✘ No, uses locked-in rate unless modified"))
          )
        )
      )
      )
    ),

    div(
      class = "module-section",
      h3("Key Characteristics Comparison"),

    ),


    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Investment Contracts with Discretionary Participation Features.")
    ),

    box(
      title = "1. Which of the following statements best describes a key difference between insurance contracts with direct participation features and investment contracts with discretionary participation features under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL, choices = c(
        "Insurance contracts with direct participation features are accounted for under IFRS 9, while investment contracts with discretionary participation features are accounted for under IFRS 17.",
        "Both contract types are accounted for using the Premium Allocation Approach (PAA) under IFRS 17.",
        "Insurance contracts with direct participation features use the Variable Fee Approach (VFA), while investment contracts with discretionary participation features are accounted for under IFRS 17 with minor modifications.",
        "Investment contracts with discretionary participation features involve no discretionary element and must follow IFRS 15."
      ), selected = character(0))
    ),

    box(
      title = "2. Which of the following contracts would be classified as an “investment contract with discretionary participation features” under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL, choices = c(
        "A savings contract where the insurer retains discretion over bonus payments and the contract does not transfer significant insurance risk.",
        "A unit-linked investment product with guaranteed returns and no discretionary elements.",
        "A life insurance contract that entitles the policyholder to a share of asset returns and includes significant insurance risk.",
        "A pure term life policy with no investment component or discretionary features."
      ), selected = character(0))
    ),

    box(
      title = "3. Under IFRS 17, what does the coverage period of a Direct Participation Contract (DPC) include?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL, choices = c(
        "Only the period during which insurance risk is present.",
        "Only the period over which investment returns are credited to the policyholder.",
        "Both the investment and insurance service periods under the contract.",
        "Only the period where fair value of underlying items increases."
      ), selected = character(0))
    ),

    box(
      title = "4. Which of the following best defines an \"underlying item\" under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL, choices = c(
        "Any asset owned by the insurer",
        "Any amount guaranteed to the policyholder",
        "Any item that determines amounts payable to the policyholder",
        "A group of insurance contracts with discretionary returns"
      ), selected = character(0))
    ),

    box(
      title = "5. According to IFRS 17, what makes a policyholder’s participation in a pool of underlying items valid for DPC classification?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL, choices = c(
        "The insurer’s intention to share profits",
        "A strong historical pattern of bonus declarations",
        "A legally enforceable right to a share of underlying items",
        "Regulatory expectation that profits be distributed fairly"
      ), selected = character(0))
    ),

    box(
      title = "6. Which of the following contracts is most likely NOT to qualify as having a clearly identified pool of underlying items under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL, choices = c(
        "A unit-linked contract where fund allocation is defined",
        "A with-profits contract tied to a disclosed internal asset pool",
        "A policy linked to an external market index explicitly mentioned in the policy",
        "A universal life contract where the crediting rate is set by the insurer ex post"
      ), selected = character(0))
    ),

    box(
      title = "7. When is the assessment of whether an insurance contract qualifies as a Direct Participation Contract (DPC) performed under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL, choices = c(
        "At initial recognition and not subsequently repeated",
        "At the date of contract modification",
        "At the end of each reporting period",
        "Annually, based on updated assumptions"
      ), selected = character(0))
    ),

    box(
      title = "8. Which of the following statements is TRUE about discounting in the measurement of Direct Participation Contracts under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL, choices = c(
        "DPCs use locked-in discount rates for all adjustments",
        "Adjustments to cash flows not based on underlying items are discounted using current rates",
        "DPCs follow a special discounting method unique to these contracts",
        "Discounting is not applicable to DPCs"
      ), selected = character(0))
    ),

    box(
      title = "9. How does the adjustment of the Contractual Service Margin (CSM) for financial risks differ between contracts with and without direct participation features under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL, choices = c(
        "Contracts with direct participation features adjust the CSM for changes in financial risk using the current interest curve, even if unrelated to future service.",
        "Only contracts without direct participation features adjust the CSM for financial risks using the current discount rate.",
        "For both types of contracts, the CSM is adjusted using the locked-in interest rate.",
        "Neither type of contract adjusts the CSM for financial risks unrelated to underlying items."
      ), selected = character(0))
    ),

    box(
      title = "10. What is the appropriate IFRS 17 treatment when a Direct Participation Contract (DPC) is modified such that it no longer meets the definition of a DPC?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL, choices = c(
        "The contract continues to be treated as a DPC until expiry.",
        "The contract is reclassified prospectively without derecognition.",
        "The original contract is derecognised, and a new contract is recognised based on the modified terms.",
        "Only the CSM is adjusted to reflect the modification."
      ), selected = character(0))
    ),

    box(
      title = "11. Which of the following best describes an investment contract with discretionary participation features under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q11"), label = NULL, choices = c(
        "A financial instrument that guarantees fixed returns and is always classified under IFRS 9.",
        "A contract that gives the investor a right to additional amounts determined solely by market interest rates.",
        "A unit-linked insurance contract with no discretionary elements.",
        "A financial instrument providing the investor with a right to receive significant additional benefits that are contractually discretionary and based on returns or performance."
      ), selected = character(0))
    ),

    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")), 

    div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_12"),
          label = tagList(icon("arrow-right"), "Next: Module 12 - Modification and Decognition of insurance contracts"),
          class = "control-button-tavnav"
      )
    ) 
    )
}

correct_answers_module11 <- list(
  q1 = list(
    answer = "Insurance contracts with direct participation features use the Variable Fee Approach (VFA), while investment contracts with discretionary participation features are accounted for under IFRS 17 with minor modifications.",
    explanation = "IFRS 17 differentiates between DPCs, which use the VFA, and investment contracts with DPFs, which are not insurance contracts but still fall under IFRS 17 with modifications."
  ),
  q2 = list(
    answer = "A savings contract where the insurer retains discretion over bonus payments and the contract does not transfer significant insurance risk.",
    explanation = "Investment contracts with DPFs have no significant insurance risk but offer discretionary returns, placing them within the scope of IFRS 17 (not IFRS 9)."
  ),
  q3 = list(
    answer = "Both the investment and insurance service periods under the contract.",
    explanation = "For DPCs, IFRS 17 states that the coverage period includes both insurance and investment services, unlike other contract types."
  ),
  q4 = list(
    answer = "Any item that determines amounts payable to the policyholder",
    explanation = "Under IFRS 17, underlying items are defined as the basis for determining amounts payable and can include internal or external references."
  ),
  q5 = list(
    answer = "A legally enforceable right to a share of underlying items",
    explanation = "DPC classification under IFRS 17 requires that participation rights in underlying items be contractually or legally enforceable."
  ),
  q6 = list(
    answer = "A universal life contract where the crediting rate is set by the insurer ex post",
    explanation = "Such contracts lack a clearly identified pool of underlying items and therefore fail the DPC requirement under IFRS 17."
  ),
  q7 = list(
    answer = "At initial recognition and not subsequently repeated",
    explanation = "IFRS 17 requires a one-time DPC qualification assessment at initial recognition. It’s not updated unless there's a modification leading to derecognition."
  ),
  q8 = list(
    answer = "Adjustments to cash flows not based on underlying items are discounted using current rates",
    explanation = "For DPCs, current interest rates apply to non-underlying item cash flow adjustments, supporting a fair value-based approach."
  ),
  q9 = list(
    answer = "Contracts with direct participation features adjust the CSM for changes in financial risk using the current interest curve, even if unrelated to future service.",
    explanation = "DPCs uniquely adjust the CSM for financial risk using the current interest curve, while other contracts use locked-in rates."
  ),
  q10 = list(
    answer = "The original contract is derecognised, and a new contract is recognised based on the modified terms.",
    explanation = "If a DPC is modified and no longer qualifies, IFRS 17 requires derecognition and recognition of a new contract reflecting the new terms."
  ),
  q11 = list(
    answer = "A financial instrument providing the investor with a right to receive significant additional benefits that are contractually discretionary and based on returns or performance.",
    explanation = "Investment contracts with DPFs provide discretionary, performance-based returns and fall under IFRS 17 if issued by insurers offering insurance contracts."
  )
)



IFRS17Module11Server <- (function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Add a reactive value to track when progress is saved
    progress_saved_trigger <- reactiveVal(0)

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
        
        
        # ———————————
        # 5. All answered: clear any existing modal, then run your scoring code
        removeModal()

        score(0)
        feedback <- list()

    for (qid in names(correct_answers_module11)) {
      correct <- correct_answers_module11[[qid]]$answer
      explanation <- correct_answers_module11[[qid]]$explanation
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
      # Save progress for Module 11
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 2 specific calculations
        total_questions <- length(correct_answers_module11)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module11",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 11 Progress Saved!</strong><br>",
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
          print(paste("Module 11 progress save error:", e$message))
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
      total_questions <- length(correct_answers_module11)
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
              "📊 Module 11 Results Summary",
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
    to_module_12 <- reactive(input$to_module_12)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_12,
      progress_trigger = progress_saved_trigger
    ))
  })
})