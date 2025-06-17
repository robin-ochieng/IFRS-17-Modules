IFRS17CaseStudiesUI <- function(id) {
  ns <- NS(id)

  logo_bar <- fluidRow(
    class = "logo-bar",
    column(
      width = 12,
      tags$div(
        class = "logo-wrapper d-flex justify-content-between align-items-center",
        tags$img(src = "images/ira_logo_.png", class = "logo logo-afdb")
      )
    )
  )

    tagList(
    # logo_bar,

    tags$div(
        class = "section-header",
        h2("📂 Case Study 1: Wakanda Case Study")
    ),


    box(
    title = "Case Study: GoodInsurer in Wakanda",
    status = "white", solidHeader = TRUE, width = 12,
    p("In the virtual world of Wakanda, there are two companies called GoodInsurer and BadInsurer. The government of
    Wakanda has managed to keep interest rates at zero percent (0%) for all bonds. The people are flourishing and
    very happy. The Currency of Wakanda is Wsh."),
    p("GoodInsurer has launched a 5-Year endowment life insurance product which pays either a death benefit or a
    maturity benefit of Wsh 1,000,000. The cost of the product is a premium of Wsh 400,000 per annum."),
    p("The product is exclusively sold to retired footballers, athletes & other professionals who retire at age 40 years."),
    p("The Government Actuary of Wakanda, Mr Challa, is mandated to sets the mortality rates and has said that each
    policyholder who is age 40-45 years should contribute Wsh 200,000 per year to make the product sustainable."),
    p("In addition, based on the risk appetite of the Shareholders of GoodInsurer, the Risk Adjustment for Non-Financial
    was set at Wsh 50,000. The Board now feels that they are 85% sure that the cost per policyholder will not exceed
    Wsh 250,000 per year (Wsh 200,000 as the cost of claims (as the estimate provided by the Government Actuary) +
    Wsh 50,000 for the Risk Adjustment Margin to give the investors comfort that their profits are protected). They
    expect to release Wsh 10,000 from the Risk Adjustment Margin every per year if there are no significant claims)."),
    p("GoodInsurer uses internal staff to sell its product. The entire department earns a salary of Wsh 1 billion per year. In
    addition, Management has told the Board that expenses that can be attributable to this product are Wsh 2 billion
    while other overheads which cannot be attributable to this product are Wsh 3 billion."),
    p("Based on the sales projection, each product will be allocated Wsh 50,000 in the first year only for acquisition costs
    (commission payable) and Wsh 100,000 every year in attributable expenses."),
    p("The Board asked what the theoretical non-attributable expenses of Wsh 3 billion would mean for the product and
    they were informed that this would be Wsh 200,000 per product."),
    p("GoodInsurer sold 10,000 policies in the first year and sent the usual end of year report to the Insurance Regulatory
    Authority of Wakanda (IRAW).")
    ),
    br(),
    p("Answer the following questions based on this case study:"),

    box(
    title = "15. What is the Contractual Service Margin (CSM) expected to be seen in the accounts for this product by the IRAW for GoodInsurer? (6 marks)",
    status = "white", solidHeader = TRUE, width = 12,
    textAreaInput(ns("q15"), label = NULL, rows = 2, placeholder = "e.g., Wsh 9B")
    ),

    box(
    title = "16. If no new policy was sold after the first year, what is the CSM expected in the second year? (4 marks)",
    status = "white", solidHeader = TRUE, width = 12,
    textAreaInput(ns("q16"), label = NULL, rows = 2, placeholder = "eg., Wsh 3.9B")
    ),

    box(
    title = "17. In Year 3, the shareholders of GoodInsurer feel that the footballers are buying very fast
                cars and may have more claims. There is no evidence yet that the claims will increase. The
                Board of GoodInsurer held a meeting and approved the Risk Adjustment to be held at a
                higher confidence level of 95%. This means that the Original Risk Margin would have been
                Wsh 100,000 instead of Wsh 50,000. Calculate the CSM for Year 3? (6 marks)",
    status = "white", solidHeader = TRUE, width = 12,
    textAreaInput(ns("q17"), label = NULL, rows = 2, placeholder = "e.g., Wsh 1.19B")
    ),

    box(
    title = "18. What is the Insurance Finance Expenses expected in Year 1 to Year 5? (5 marks)",
    status = "white", solidHeader = TRUE, width = 12,
    textAreaInput(ns("q18"), label = NULL, rows = 2, placeholder = "e.g., Wsh 1")
    ),

    box(
    title = "19. BadInsurer decided to undercut and sell the same product at 50% of the premium.
                Calculate the Loss Component expected to be held on the insurer? (5 marks)",
    status = "white", solidHeader = TRUE, width = 12,
    textAreaInput(ns("q19"), label = NULL, rows = 2, placeholder = "e.g., Wsh 9B"),


    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result"))   
    ),



    tags$div(
        class = "section-header",
        h2("📂 Case Study 2: Grouping Contracts")
    ),
    box(
    title = "Case Study: Grouping Contracts",
    status = "white", solidHeader = TRUE, width = 12,
    div(class = "case-outer-wrapper",
        div(class = "ifrs17-case-container",
            p(class = "case-description",
              "This case study demonstrates how insurance contracts are grouped under IFRS 17 based on expected profitability. Contracts issued within a similar timeframe are grouped for consistent measurement and reporting."
            ),
            DTOutput(ns("case_table")),

            div(class = "intro-note",
                "Grouping is critical in IFRS 17 as it influences how contracts are measured and reported. Profitability segmentation ensures that losses are recognized early and profits are allocated over time."
            )
        )
    )) 
    
 
  )
}

IFRS17CaseStudiesServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Initialize reactive values for scores and feedback
    score <- reactiveVal(0)
    section3_score <- reactiveVal(0)
    feedback <- reactiveVal(list())

    observeEvent(input$submit, {  # Assuming a submit button

      temp_feedback <- list()
      temp_score <- 0
      temp_section_score <- 0

      # Q1 (originally Q15)
      if (tolower(trimws(input$q15)) %in% c("wsh 4b", "4b")) {
        temp_score <- temp_score + 1
        temp_section_score <- temp_section_score + 1
        temp_feedback <- c(temp_feedback, "✅ Q1: Correct!")
      } else {
        temp_feedback <- c(temp_feedback, "❌ Q1: Incorrect! Correct answer is 'Wsh 4B'.")
      }

      # Q2 (originally Q16)
      if (tolower(trimws(input$q16)) %in% c("wsh 3.2b", "3.2b")) {
        temp_score <- temp_score + 1
        temp_section_score <- temp_section_score + 1
        temp_feedback <- c(temp_feedback, "✅ Q2: Correct!")
      } else {
        temp_feedback <- c(temp_feedback, "❌ Q2: Incorrect! Correct answer is 'Wsh 3.2B'.")
      }

      # Q3 (originally Q17)
      if (tolower(trimws(input$q17)) %in% c("wsh 1.47b", "1.47b")) {
        temp_score <- temp_score + 1
        temp_section_score <- temp_section_score + 1
        temp_feedback <- c(temp_feedback, "✅ Q3: Correct!")
      } else {
        temp_feedback <- c(temp_feedback, "❌ Q3: Incorrect! Correct answer is 'Wsh 1.47B'.")
      }

      # Q4 (originally Q18)
      if (tolower(trimws(input$q18)) %in% c("wsh 0", "0")) {
        temp_score <- temp_score + 1
        temp_section_score <- temp_section_score + 1
        temp_feedback <- c(temp_feedback, "✅ Q4: Correct!")
      } else {
        temp_feedback <- c(temp_feedback, "❌ Q4: Incorrect! Correct answer is 'Wsh 0'.")
      }

      # Q5 (originally Q19)
      if (tolower(trimws(input$q19)) %in% c("wsh 6b", "6b")) {
        temp_score <- temp_score + 1
        temp_section_score <- temp_section_score + 1
        temp_feedback <- c(temp_feedback, "✅ Q5: Correct!")
      } else {
        temp_feedback <- c(temp_feedback, "❌ Q5: Incorrect! Correct answer is 'Wsh 6B'.")
      }

      # Update reactive values
      score(temp_score)
      section3_score(temp_section_score)
      feedback(temp_feedback)

    # Render results UI
    output$result <- renderUI({
      req(score())  # Ensure score calculation before rendering
      total_questions <- 5  # Updated to 5 as we have 5 questions now
      percentage <- round((score() / total_questions) * 100, 1)
      
      color <- if (percentage >= 70) "#198754" else "#dc3545"

      tagList(
        div(style = "background-color:#ffffff; padding:25px; border-radius:10px; box-shadow:0 4px 12px rgba(0,0,0,0.08); font-family:Arial, sans-serif;",
            h3("📊 Results Summary", style = "color:#0d6efd; font-weight:600; margin-bottom:20px;"),

            HTML(paste0(
              "<p style='font-size:18px;'><strong>Total Score:</strong> ", score(), " / ", total_questions, "</p>",
              "<p style='font-size:18px;'><strong>Percentage Score:</strong> <span style='color:", color, "; font-weight:600;'>", percentage, "%</span></p>"
            )),

            div(style = "margin-top:25px;",
                h4("📘 Detailed Feedback", style = "margin-bottom:15px; color:#343a40;"),
                tags$ul(
                  lapply(feedback(), function(msg) {
                    tags$li(style = "margin-bottom:8px;", HTML(msg))
                  })
                )
            )
        )
      )
    })

    })



    # Render DT Table
    case_data <- data.frame(
      ContractID = paste0("C", 1:5),
      IssueDate = Sys.Date() - 1:5,
      ExpectedProfit = c(TRUE, TRUE, FALSE, TRUE, FALSE),
      Notes = c(
        "This contract is expected to generate a profit and is grouped accordingly.",
        "Consistent profit expectations allow grouping within the profitable cohort.",
        "This contract is loss-making and must be grouped separately for immediate loss recognition.",
        "Profitable grouping applies, subject to timing alignment.",
        "This contract triggers onerous contract treatment under IFRS 17."
      )
    )

    # Render the table with expandable rows using JS
    output$case_table <- renderDT({
      datatable(
        case_data[, 1:3],  # show only main columns
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          ),
          rowCallback = JS(
            "function(row, data, index) {
               $(row).css('cursor', 'pointer');
               $(row).off('click').on('click', function() {
                 var table = $('#', this).closest('table').DataTable();
                 var tr = $(this).closest('tr');
                 var row = table.row(tr);
                 if (row.child.isShown()) {
                   row.child.hide();
                   tr.removeClass('shown');
                 } else {
                   row.child('<div style=\"padding:10px;\">' + 
                             '<b>Notes:</b> ' + data[3] + '</div>').show();
                   tr.addClass('shown');
                 }
               });
             }"
          )
        ),
        callback = JS("table.on('click', 'tr', function() { table.rows().deselect(); });"),
        escape = FALSE,
        selection = "none",
        rownames = FALSE
      )
    })


  })
}

