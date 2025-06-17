IFRS17MeasurementUI <- function(id) {
  ns <- NS(id)
  logo_bar <- fluidRow(
    class = "logo-bar",                     # you’ll style this in CSS
    column(
      width = 12,
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
        h2("🔍 IFRS 17 Foundations and Standards Overview", class = "section-title-top")
    ),
      div(class = "module-section",
          h2("📖 IFRS 17 Glossary", class = "section-title"),
        tags$div(class = "table-responsive",
          tags$table(class = "glossary-table",
            tags$thead(
              tags$tr(
                tags$th("TERM"),
                tags$th("DEFINITION")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Contractual Service Margin"),
                tags$td("A component of the carrying amount of the asset or liability for a group of insurance contracts representing the unearned profit the entity will recognise as it provides insurance contract services under the insurance contracts in the group.")
              ),
              tags$tr(
                tags$td("Coverage Period"),
                tags$td("The period during which the entity provides insurance contract services. This period includes the insurance contract services that relate to all premiums within the boundary of the insurance contract.")
              ),
              tags$tr(
                tags$td("Experience Adjustment"),
                tags$td(HTML("A difference between:
                  <ol type='a'>
                    <li>a)	for premium receipts (and any related cash flows such as insurance acquisition cash flows and insurance premium taxes) – the estimate at the beginning of the period of the amounts expected in the period and the actual cash flows in the period; or</li>
                    <li>b)	for insurance service expenses (excluding insurance acquisition expenses)—the estimate at the beginning of the period of the amounts expected to be incurred in the period and the actual amounts incurred in the period.</li>
                  </ol>"))
              ),
              tags$tr(
                tags$td("Financial Risk"),
                tags$td("The risk of a possible future change in one or more of a specified interest rate, financial instrument price, commodity price, currency exchange rate, index of prices or rates, credit rating or credit index or other variable, provided in the case of a non-financial variable that the variable is not specific to a party to the contract.")
              ),
              tags$tr(
                tags$td("Fulfilment Cash Flows"),
                tags$td("An explicit, unbiased and probability-weighted estimate (ie expected value) of the present value of the future cash outflows minus the present value of the future cash inflows that will arise as the entity fulfils insurance contracts, including a risk adjustment for non-financial risk.")
              ),
              tags$tr(
                tags$td("Group of Insurance Contracts"),
                tags$td(HTML("A set of insurance contracts resulting from the division of a portfolio of insurance contracts into, at a minimum, contracts issued within a period of no longer than one year and that, at initial recognition:
                  <ol type='a'>
                    <li>are onerous, if any;</li>
                    <li>have no significant possibility of becoming onerous subsequently, if any; or</li>
                    <li>do not fall into either (a) or (b), if any.</li>
                  </ol>"))
              ),
              tags$tr(
                tags$td("Insurance Acquisition Cash Flows"),
                tags$td("Cash flows arising from the costs of selling, underwriting and starting a group of insurance contracts (issued or expected to be issued) that are directly attributable to the portfolio of insurance contracts to which the group belongs. Such cash flows include cash flows that are not directly attributable to individual contracts or groups of insurance contracts within the portfolio.")
              ),
              tags$tr(
                tags$td("Insurance Contract"),
                tags$td("A contract under which one party (the issuer) accepts significant insurance risk from another party (the policyholder) by agreeing to compensate the policyholder if a specified uncertain future event (the insured event) adversely affects the policyholder.")
              ),
              tags$tr(
                tags$td("Insurance Contract Services"),
                tags$td(HTML("The following services that an entity provides to a policyholder of an insurance contract:
                  <ol type='a'>
                    <li>coverage for an insured event(insurance coverage);</li>
                    <li>for insurance contracts without direct participation features, the generation of an investment return for the policyholder, if applicable (investment-return service); and</li>
                    <li>c)	for insurance contracts with direct participation features, the management of underlying items on behalf of the policyholder (investment-related service).</li>
                  </ol>"))
              ),
              tags$tr(
                tags$td("Insurance Contract with Direct Participation Features"),
                tags$td(HTML("An insurance contract for which, at inception:
                  <ol type='a'>
                    <li>the contractual terms specify that the policyholder participates in a share of a clearly identified pool of underlying items;</li>
                    <li>the entity expects to pay to the policyholder an amount equal to a substantial share of the fair value returns on the underlying items; and</li>
                    <li>the entity expects a substantial proportion of any change in the amounts to be paid to the policyholder to vary with the change in fair value of the underlying items.</li>
                  </ol>"))
              ),
              tags$tr(
                tags$td("Insurance Contract without Direct Participation Features"),
                tags$td("An insurance contract that is not an insurance contract with direct participation features.")
              ),
              tags$tr(
                tags$td("Insurance Risk"),
                tags$td("Risk, other than financial risk, transferred from the holder of a contract to the issuer.")
              ),
              tags$tr(
                tags$td("Insured Event"),
                tags$td("An uncertain future event covered by an insurance contract that creates insurance risk.")
              ),
              tags$tr(
                tags$td("Investment Component"),
                tags$td("The amounts that an insurance contract requires the entity to repay to a policyholder in all circumstances, regardless of whether an insured event occurs.")
              ),
              tags$tr(
                tags$td("Investment Contract with Discretionary Participation Features"),
                tags$td(HTML("A financial instrument that provides a particular investor with the contractual right to receive, as a supplement to an amount not subject to the discretion of the issuer, additional amounts:
                  <ol type='a'>
                    <li>that are expected to be a significant portion of the total contractual benefits;</li>
                    <li>the timing or amount of which are contractually at the discretion of the issuer; and</li>
                    <li>that are contractually based on:
                      <ol type='i'>
                        <li>returns on a specified pool of contracts or a specified type of contract;</li>
                        <li>realised/unrealised investment returns returns on a specified pool of assets held by the issuer; or</li>
                        <li>profit or loss of the entity or fund that issues the contract.</li>
                      </ol>
                    </li>
                  </ol>"))
              ),
              tags$tr(
                tags$td("Liability for Incurred Claims"),
                tags$td(HTML("An entity’s obligation to:
                  <ol type='a'>
                    <li>investigate and pay valid claims for insured events that have already occurred, including events that have occurred but for which claims have not been reported, and other incurred insurance expenses; and</li>
                    <li>pay amounts that are not included in (a) and that relate to:
                      <ol type='i'>
                        <li>insurance contract services that have already been provided; or</li>
                        <li>any investment components or other amounts that are not related to the provision of insurance contract services and that are not in the liability for remaining coverage</li>
                      </ol>
                    </li>
                  </ol>"))
              ),
              tags$tr(
                tags$td("Liability for Remaining Coverage"),
                tags$td(HTML("An entity’s obligation to:
                  <ol type='a'>
                    <li>investigate and pay valid claims under existing insurance contracts for insured events that have not yet occurred (ie the obligation that relates to the unexpired portion of the insurance coverage); and</li>
                    <li>pay amounts under existing insurance contracts that are not included in (a) and that relate to:
                      <ol type='i'>
                        <li>insurance contract services not yet provided (ie the obligations that relate to future provision of insurance contract services); or</li>
                        <li>any investment components or other amounts that are not related to the provision of insurance contract services and that have not been transferred to the liability for incurred claims</li>
                      </ol>        
                    </li>
                  </ol>"))
              ),
              tags$tr(
                tags$td("Policyholder"),
                tags$td("A party that has a right to compensation under an insurance contract if an insured event occurs.")
              ),
              tags$tr(
                tags$td("Portfolio of Insurance Contracts"),
                tags$td("Insurance contracts subject to similar risks and managed together.")
              ),
              tags$tr(
                tags$td("Reinsurance Contract"),
                tags$td("An insurance contract issued by one entity (the reinsurer) to compensate another entity for claims arising from one or more insurance contracts issued by that other entity (underlying contracts).")
              ),
              tags$tr(
                tags$td("Risk Adjustment for Non-Financial Risk"),
                tags$td("The compensation an entity requires for bearing the uncertainty about the amount and timing of the cash flows that arises from non-financial risk as the entity fulfils insurance contracts.")
              ),
              tags$tr(
                tags$td("Underlying Items"),
                tags$td("Items that determine some of the amounts payable to a policyholder. Underlying items can comprise any items; for example, a reference portfolio of assets, the net assets of the entity, or a specified subset of the net assets of the entity.")
              )
            )
          )
        )  
      ),

    div(class = "module-section",
        h2("⚖️ IFRS Standards", class = "section-title"),
      tags$div(class = "table-responsive",  
        tags$table(class = "ifrs-table",
          tags$thead(
            tags$tr(
              tags$th("STANDARD"),
              tags$th("NAME"),
              tags$th("DESCRIPTION")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("IFRS 1"),
              tags$td("First-time Adoption of International Financial Reporting Standards"),
              tags$td("Provides guidance for entities adopting IFRS for the first time.")
            ),
            tags$tr(
              tags$td("IFRS 2"),
              tags$td("Share-based Payment"),
              tags$td("Deals with the accounting for share-based payment transactions.")
            ),
            tags$tr(
              tags$td("IFRS 3"),
              tags$td("Business Combinations"),
              tags$td("Prescribes the accounting treatment for business combinations.")
            ),
            tags$tr(
              tags$td("IFRS 4"),
              tags$td("Insurance Contracts"),
              tags$td("Interim standard for insurance contracts prior to IFRS 17.")
            ),
            tags$tr(
              tags$td("IFRS 5"),
              tags$td("Non-current Assets Held for Sale and Discontinued Operations"),
              tags$td("Covers accounting for assets held for sale and discontinued operations.")
            ),
            tags$tr(
              tags$td("IFRS 6"),
              tags$td("Exploration for and Evaluation of Mineral Resources"),
              tags$td("Accounting for exploration and evaluation of mineral resources.")
            ),
            tags$tr(
              tags$td("IFRS 7"),
              tags$td("Financial Instruments: Disclosures"),
              tags$td("Requires disclosures relating to financial instruments.")
            ),
            tags$tr(
              tags$td("IFRS 8"),
              tags$td("Operating Segments"),
              tags$td("Requires disclosure of financial performance by operating segments.")
            ),
            tags$tr(
              tags$td("IFRS 9"),
              tags$td("Financial Instruments"),
              tags$td("Deals with classification, measurement, and impairment of financial instruments.")
            ),
            tags$tr(
              tags$td("IFRS 10"),
              tags$td("Consolidated Financial Statements"),
              tags$td("Provides requirements for consolidated financial statements.")
            ),
            tags$tr(
              tags$td("IFRS 11"),
              tags$td("Joint Arrangements"),
              tags$td("Describes accounting for joint ventures and joint operations.")
            ),
            tags$tr(
              tags$td("IFRS 12"),
              tags$td("Disclosure of Interests in Other Entities"),
              tags$td("Outlines disclosure requirements for interests in subsidiaries, joint arrangements, associates, and unconsolidated structured entities.")
            ),
            tags$tr(
              tags$td("IFRS 13"),
              tags$td("Fair Value Measurement"),
              tags$td("Defines fair value and sets out a framework for measuring fair value.")
            ),
            tags$tr(
              tags$td("IFRS 14"),
              tags$td("Regulatory Deferral Accounts"),
              tags$td("Permits first-time adopters to continue recognizing regulatory deferral account balances.")
            ),
            tags$tr(
              tags$td("IFRS 15"),
              tags$td("Revenue from Contracts with Customers"),
              tags$td("Provides a comprehensive framework for revenue recognition.")
            ),
            tags$tr(
              tags$td("IFRS 16"),
              tags$td("Leases"),
              tags$td("Outlines accounting for leases by lessees and lessors.")
            ),
            tags$tr(
              tags$td("IFRS 17"),
              tags$td("Insurance Contracts"),
              tags$td("Provides comprehensive guidance on the recognition, measurement, presentation, and disclosure of insurance contracts.")
            )
          )
        )
      )  
    ),



        

      div(class = "module-section",
          h2("📘 IAS Standards", class = "section-title"),
        tags$div(class = "table-responsive",
          tags$table(class = "ias-table",
            tags$thead(
              tags$tr(
                tags$th("STANDARD"),
                tags$th("NAME"),
                tags$th("DESCRIPTION")
              )
            ),
            tags$tbody(
              tags$tr(tags$td("IAS 1"), tags$td("Presentation of Financial Statements"), tags$td("Sets out overall requirements for financial statements, including structure and content.")),
              tags$tr(tags$td("IAS 2"), tags$td("Inventories"), tags$td("Provides guidance on the accounting treatment for inventories.")),
              tags$tr(tags$td("IAS 7"), tags$td("Statement of Cash Flows"), tags$td("Prescribes how to present information about changes in cash and cash equivalents.")),
              tags$tr(tags$td("IAS 8"), tags$td("Accounting Policies, Changes in Accounting Estimates and Errors"), tags$td("Provides criteria for selecting and changing accounting policies.")),
              tags$tr(tags$td("IAS 10"), tags$td("Events After the Reporting Period"), tags$td("Deals with events occurring after the reporting period.")),
              tags$tr(tags$td("IAS 12"), tags$td("Income Taxes"), tags$td("Prescribes the accounting treatment for current and deferred tax.")),
              tags$tr(tags$td("IAS 16"), tags$td("Property, Plant and Equipment"), tags$td("Provides guidance on accounting for tangible fixed assets.")),
              tags$tr(tags$td("IAS 19"), tags$td("Employee Benefits"), tags$td("Covers accounting for all employee benefits, including pensions.")),
              tags$tr(tags$td("IAS 20"), tags$td("Accounting for Government Grants and Disclosure of Government Assistance"), tags$td("Deals with accounting for, and disclosure of, government grants.")),
              tags$tr(tags$td("IAS 21"), tags$td("The Effects of Changes in Foreign Exchange Rates"), tags$td("Prescribes how to account for foreign currency transactions and operations.")),
              tags$tr(tags$td("IAS 23"), tags$td("Borrowing Costs"), tags$td("Prescribes the accounting for borrowing costs.")),
              tags$tr(tags$td("IAS 24"), tags$td("Related Party Disclosures"), tags$td("Requires disclosure of related party relationships and transactions.")),
              tags$tr(tags$td("IAS 26"), tags$td("Accounting and Reporting by Retirement Benefit Plans"), tags$td("Specifies the reporting requirements for retirement benefit plans.")),
              tags$tr(tags$td("IAS 27"), tags$td("Separate Financial Statements"), tags$td("Deals with accounting for investments in subsidiaries, jointly controlled entities and associates in separate financial statements.")),
              tags$tr(tags$td("IAS 28"), tags$td("Investments in Associates and Joint Ventures"), tags$td("Prescribes the accounting for investments in associates and joint ventures.")),
              tags$tr(tags$td("IAS 29"), tags$td("Financial Reporting in Hyperinflationary Economies"), tags$td("Provides guidance on financial reporting in hyperinflationary environments.")),
              tags$tr(tags$td("IAS 32"), tags$td("Financial Instruments: Presentation"), tags$td("Establishes principles for presenting financial instruments.")),
              tags$tr(tags$td("IAS 33"), tags$td("Earnings per Share"), tags$td("Prescribes principles for determining and presenting earnings per share.")),
              tags$tr(tags$td("IAS 34"), tags$td("Interim Financial Reporting"), tags$td("Prescribes the content of an interim financial report.")),
              tags$tr(tags$td("IAS 36"), tags$td("Impairment of Assets"), tags$td("Prescribes the procedures to ensure assets are not carried at more than their recoverable amount.")),
              tags$tr(tags$td("IAS 37"), tags$td("Provisions, Contingent Liabilities and Contingent Assets"), tags$td("Addresses accounting for provisions and contingencies.")),
              tags$tr(tags$td("IAS 38"), tags$td("Intangible Assets"), tags$td("Prescribes the accounting for intangible assets not dealt with in other standards.")),
              tags$tr(tags$td("IAS 40"), tags$td("Investment Property"), tags$td("Covers the accounting for investment property.")),
              tags$tr(tags$td("IAS 41"), tags$td("Agriculture"), tags$td("Deals with the accounting for agricultural activity."))
            )
          )
        )
      ),

    div(class = "module-section",
            h2("📊 Measurement Models", class = "section-title"),

            p(class = "model-intro",
              "IFRS 17 prescribes different measurement models depending on the nature of the insurance contract. Select a model below to explore its key characteristics and visual behavior."
            ),

            div(class = "radio-wrapper",
                radioButtons(ns("model_select"), label = "Choose a model:",
                  choices = c(
                    "Building Block Approach (BBA)",
                    "Premium Allocation Approach (PAA)",
                    "Variable Fee Approach (VFA)"
                  ),
                  selected = "Building Block Approach (BBA)"
                ),

            div(class = "model-description",
                textOutput(ns("model_explanation"))
            ),

            div(plotOutput(ns("model_plot")), class = "plot-wrapper")
            )
    ),

          div(
            class = "intro-nav",
            actionButton(
              ns("to_module_1"),
              label = tagList(icon("arrow-right"), "Next: Module 1 - Introduction to IFRS 17"),
              class = "control-button-tavnav"
            )
          )
  )
}

IFRS17MeasurementServer <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {

    output$model_plot <- renderPlot({
      req(input$model_select)

      model <- input$model_select

      # Simulated logic for each model
      plot_data <- switch(model,
        "Building Block Approach (BBA)" = data.frame(x = 1:10, y = c(100, 95, 90, 86, 83, 80, 78, 76, 75, 74)),
        "Premium Allocation Approach (PAA)" = data.frame(x = 1:10, y = c(100, 98, 95, 92, 88, 84, 80, 76, 72, 68)),
        "Variable Fee Approach (VFA)" = data.frame(x = 1:10, y = c(100, 97, 103, 98, 105, 102, 99, 95, 92, 90))
      )

      color <- switch(model,
        "Building Block Approach (BBA)" = "#007bff",
        "Premium Allocation Approach (PAA)" = "#28a745",
        "Variable Fee Approach (VFA)" = "#6f42c1"
      )

      ggplot(plot_data, aes(x, y)) +
        geom_line(color = color, linewidth = 1.8) +
        geom_point(color = color, size = 3) +
        labs(
          title = paste("Illustrative Liability Trend:", model),
          x = "Time (Period)",
          y = "Liability Value"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, color = "#333"),
          axis.title = element_text(color = "#555")
        )
    })

  output$model_explanation <- renderText({
    req(input$model_select)

    switch(input$model_select,
      "Building Block Approach (BBA)" = 
        "The BBA is a general model used for long-term contracts. It accounts for expected future cash flows, risk adjustment, and contractual service margin.",
      
      "Premium Allocation Approach (PAA)" = 
        "The PAA is a simplified model typically used for short-duration contracts. It approximates the BBA and resembles unearned premium approaches.",

      "Variable Fee Approach (VFA)" = 
        "The VFA is designed for contracts with direct participation features. It reflects an insurer’s share in underlying asset returns and adjusts profit recognition accordingly."
    )
  })

    # create a reactive for the “Next” click
    to_module_1 <- reactive(input$to_module_1)

    # return it so the app can observe it
    to_module_1

  })
}
