# This module is for the IFRS 17 module 7
IFRS17Module7UI <- function(id) {
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
      h2("📘 Module 7: Discounting CSM and Risk Adjustment", class = "section-title-top")
    ),
   div(class = "module-section",  
        h3("📖 Introduction"),
        p("This module provides an overview of Discounting, Contractual Service Margin (CSM) and Risk Adjustment (RA) under IFRS 17.")
     ),
    div(class = "module-section",  
      h3("📘 Introduction"),
      p("On initial recognition, an entity must measure a group of insurance contracts as the total of:"),
      tags$ul(
        tags$li("the fulfilment cash flows (\"FCF\"), which comprise:"),
        tags$ol(
          tags$li("estimates of future cash flows;"),
          tags$li("an adjustment to reflect the time value of money (\"TVM\") and the financial risks associated with the future cash flows, and a risk adjustment for non-financial risk."),
          tags$li("the contractual service margin (\"CSM\").")
        )
      ),
      img(src = "images/fulfilmentCashFlows.png", alt = "Fulfilment Cash Flows", class = "module-image")
    ),
    div(class = "module-section",
      h3("💡 Discounting"),
      p("This sub-module introduces the fundamental concept of discounting under IFRS 17, emphasizing the importance of adjusting future cash flows to reflect the time value of money. Discounting ensures that the value of expected future payments and receipts are expressed in today’s terms, allowing for accurate liability measurement."),
      p("Under IFRS 17 standards Paragraph 36 covers discounting as a part of fulfilment cash flow and Paragraph 38–39 covers the characteristics discounting must reflect, and paragraph 44(b) and 47 cover the interest accretion on the CSM using locked-in rates while appendix B72 to B85 cover detailed guidance on determining discount rates and yield curves.")
    ),
    div(class = "module-section",
      h4(span(style = "color: #006AA6;", "What is Discounting?")),
      p("Discounting is the process of converting future cash flows into present values. It's based on the idea that money today is worth more than money in the future due to the time value of money."),
      p("The discount rate should reflect characteristics like liquidity, inflation, and dependency on underlying items.")
    ),
    div(class = "module-section",
      h4(span(style = "color: #006AA6;", "Why Are Future Cash Flows Discounted in IFRS 17?")),
      p("To reflect the time value of money and financial risks not already captured in the estimates."),
      h5(span(style = "color: #AF851D;", "Purpose:")),
      tags$ul(
        tags$li("Ensure liabilities are shown realistically on the balance sheet."),
        tags$li("Helps compare cash flows that occur at different times."),
        tags$li("Adjusts for liquidity and financial risk.")
      )
    ),
    div(class = "module-section",
      h3("📉 Discounting Approaches"),
      p("IFRS 17 has provided two approaches for the determination of discount rates for insurers as follows:"),

      # Bottom-Up Approach
      tags$h4(span(style = "color:#006AA6;", "a) Bottom-Up Approach")),
      tags$strong("How It Works:"),
      tags$ol(
        tags$li("Start with a risk-free yield curve (e.g., government bonds)"),
        tags$li("Add a liquidity premium if the insurance contracts are illiquid")
      ),

      # Top-Down Approach
      tags$h4(span(style = "color:#006AA6;", "b) Top-down Approach")),
      tags$strong("How It Works:"),
      tags$ol(
        tags$li("Start with the total yield of a reference asset portfolio"),
        tags$li("Eliminates components not relevant to insurance (e.g., credit risk, other market risks)")
      ),

      p("IFRS 17 requires that discount rates exclude market variables that don’t affect the cash flows of the insurance contract, even if these variables are reflected in market prices. This ensures accuracy in aligning economic reality with liability measurement."),
      img(src = "images/discountingApproaches.png", alt = "Discounting Approaches", class = "module-image")
    ),
    div(class = "module-section",
      h3("📉 Key Discounting Concepts in IFRS 17"),
      tags$div(class = "table-responsive",
      tags$table(class = "table key-table", style = "width:100%",
        tags$thead(
          tags$tr(
            tags$th("Concept"),
            tags$th("Explanation")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Time Value of Money"),
            tags$td("Money today is more valuable than money tomorrow.")
          ),
          tags$tr(
            tags$td("Discount Rate"),
            tags$td("The rate used to bring future cash flows to the present value.")
          ),
          tags$tr(
            tags$td("Bottom-Up Approach"),
            tags$td("Start with a risk-free rate, then adjust for liquidity.")
          ),
          tags$tr(
            tags$td("Top-Down Approach"),
            tags$td("Start with a portfolio yield, then remove credit and other risks not relevant to the insurance contract.")
          ),
          tags$tr(
            tags$td("Liquidity Premium"),
            tags$td("Adjustment made to a liquid risk-free yield curve to reflect differences between liquidity characteristics of the financial instruments that underlie the risk-free rates and insurance contracts.")
          ),
          tags$tr(
            tags$td("Market Consistency"),
            tags$td("Use observable market data only if it reflects the characteristics of the liability.")
          )
        )
      )
    )
    ),

    div(class = "module-section",
      h3("📉 Contractual Service Margin"),
      p("This sub-module provides a comprehensive overview of how the CSM is calculated, adjusted, and released."),
      p("Under IFRS 17, the following tables show the paragraphs and appendix that cover CSM:"),
      tags$div(class = "table-responsive",
      tags$table(class = "table key-table", style = "width:100%",
        tags$thead(
          tags$tr(
            tags$th("Paragraph"),
            tags$th("What It Covers")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("17.38–39"),
            tags$td("Fulfilment cash flows (FCF), basis for CSM")
          ),
          tags$tr(
            tags$td("17.43"),
            tags$td("No gain at initial recognition — CSM absorbs positive FCF")
          ),
          tags$tr(
            tags$td("17.44–45"),
            tags$td("CSM changes: interest, release, updates to cash flows")
          ),
          tags$tr(
            tags$td("17.46"),
            tags$td("Onerous contracts: CSM set to zero, loss component created")
          ),
          tags$tr(
            tags$td("17.47"),
            tags$td("Use of locked-in discount rate for interest accretion")
          ),
          tags$tr(
            tags$td("B94–B96"),
            tags$td("Allocation of CSM to coverage periods")
          ),
          tags$tr(
            tags$td("B97–B100"),
            tags$td("Adjustments for changes in estimates or derecognition")
          )
        )
      )
    )
    ),

  div(class = "module-section",
    h3("What is the Contractual Service Margin (CSM)?"),
    p("CSM represents the unearned profit an insurance company expects to earn as it provides coverage over the life of an insurance contract. It is part of the Liability for Remaining Coverage (LRC)."),
    p(tags$b("You can think of the CSM like this:"), style = "margin-top:15px;"),
    p(em("Imagine selling a 4-year gym membership today. The full fee is paid upfront, but you haven’t provided the service yet. The profit you’re expecting to make is spread over the 4 years – that’s your CSM."))
  ),

  div(class = "module-section",
    h3("CSM at initial recognition"),
    p("When a group of insurance contracts is first recognised (usually at the point of inception), IFRS 17 requires calculating a “Contractual Service Margin (CSM)” if the contract is expected to be profitable."),
    img(src = "images/csmatInitialRecognition.png", alt = "CSM at Initial Recognition", class = "module-image"),
    p(em("If the result is positive → this becomes the initial CSM (unearned profit). If the result is negative → the contract is onerous and no CSM is recognised. Instead, a loss component is created."))
  ),

  div(class = "module-section",
    h4(style = "color:#94B43B; font-weight:600;", "CSM at Subsequent Measurement"),
    p("Once the initial CSM is set up, it’s not static. It gets adjusted over time to reflect:"),
    tags$ol(
      tags$li(
        tags$b("Interest Accretion:"), 
        " CSM increases over time using the discount rate at initial recognition."
      ),
      tags$li(
        tags$b("Profit Release (reduction):"),
        " CSM is released as revenue as the insurer provides coverage. Usually done evenly unless another pattern better reflects the service."
      ),
      tags$li(
        tags$b("Adjustments for Changes in Estimates:"),
        " If assumptions about future cash flows change (e.g. fewer claims expected), the CSM is adjusted, but only if the contract is not onerous."
      ),
      tags$li(
        tags$b("Onerous Contracts:"),
        " If the group becomes loss-making: CSM is set to zero. A loss component is recognised for the shortfall."
      ),
      p("A loss component can be reversed in subsequent periods if there are favorable changes in the fulfilment cash flows related to future service, indicating that the group of contracts is no longer onerous.")
    )
  ),

    div(class = "module-section",
      h3("Risk Adjustment"),
      p("This sub-module looks at Risk Adjustment, which is one of the key components in measuring insurance contract liabilities."),
      p("IFRS 17 under paragraphs 37 covers risk adjustment as part of fulfilment cash flows and Paragraph 44 covers the disclosure of method and confidence level while appendix B86 to B92 covers the principles, methods and factors affecting risk adjustment."),
      p(
        tags$em(style = "color:#b6a600; font-weight:600;", "What is Risk Adjustment in IFRS 17?")
      ),
      p("The Risk Adjustment for non-financial risk represents the compensation an insurer requires for the uncertainty about the amount and timing of future cash flows from insurance contracts — due to non-financial risks like mortality, morbidity, lapse, and expense risks."),
      p("While the fulfilment cash flows reflect expected values, the risk adjustment accounts for the inherent variability and uncertainty in those expectations. It ensures that liabilities are not just a neutral best estimate but also include a margin for risk—aligned with the insurer’s own risk appetite and risk-bearing capacity.")
    ), 

    div(class = "module-section",
      h4(style = "color:#94B43B; font-weight:600;", "Factors That Influence the Risk Adjustment"),
      p("Under IFRS 17, the Risk Adjustment (RA) is influenced by the degree of uncertainty in non-financial risks. These include:"),

      # 1. Degree of Uncertainty
      tags$p(tags$strong(style = "color:#73A500;", "1. Degree of Uncertainty")),
      p("More uncertainty is equal to Higher RA"),
      p("This includes uncertainty about:"),
      tags$ul(
        tags$li("Timing of cash flows"),
        tags$li("Amount of future claims"),
        tags$li("Claims development")
      ),

      # 2. Type of Risk
      tags$p(tags$strong(style = "color:#73A500;", "2. Type of Risk")),
      p("Different non-financial risks contribute differently:"),
      tags$ul(
        tags$li("Morbidity/Mortality Risk: Variability in health/death rates."),
        tags$li("Lapse Risk: Uncertainty in policyholder behavior."),
        tags$li("Expense Risk: Changes in administration costs.")
      ),

      # 3. Contract Duration
      tags$p(tags$strong(style = "color:#73A500;", "3. Contract Duration")),
      p("Longer duration = Higher RA"),
      tags$em("Why? Because longer exposure → more uncertainty → more compensation needed."),

      # 4. Quality of Data / Knowledge of Risks
      tags$p(tags$strong(style = "color:#73A500;", "4. Quality of Data / Knowledge of Risks")),
      p("If the insurer has limited data or is less confident about assumptions, the RA is higher."),
      tags$em("Less knowledge = higher compensation required for bearing risk."),

      # 5. Diversification and Risk Pooling
      tags$p(tags$strong(style = "color:#73A500;", "5. Diversification and Risk Pooling")),
      p("A larger and more diversified portfolio tends to have lower RA per contract."),
      p("Risks that can be pooled or offset across contracts reduce overall uncertainty."),

      # 6. Reinsurance
      tags$p(tags$strong(style = "color:#73A500;", "6. Reinsurance")),
      p("If risk is ceded to a reinsurer, the RA for the cedant is lower."),
      p("The reinsurer still carries RA for their accepted portion."),
    
        #7. Risk Appetite and Confidence Level
      tags$p(tags$strong(style = "color:#73A500;", "7. Risk Appetite and Confidence Level")),
      p("A more risk-averse insurer will choose a higher confidence level, leading to a higher RA."),
      img(src = "images/riskAdjustmentFactors.png", alt = "Risk Adjustment Factors", class = "module-image")
    ),

    div(class = "module-section",
      h4(style = "color:#94B43B; font-weight:600;", "Methods of determining Risk Adjustment"),
      p("IFRS 17 does not prescribe a specific method — but it requires the RA to:"),
      tags$ul(
        tags$li("Reflect compensation for non-financial risk uncertainty"),
        tags$li("Be explicitly and separately disclosed"),
        tags$li("Be consistent with how the entity would assess its own risk preferences")
      ),
      img(src = "images/riskAdjustmentMethods.png", alt = "Risk Adjustment Methods", class = "module-image")
      ),
    div(class = "module-section",
      h3("The 3 most common approaches:"),
      
      tags$ol(
        tags$li(
          tags$span(style = "font-style:italic; font-weight:600;", 
                    "Confidence Level Approach (Quantile Method)"),
          p("This method sets the RA so that the present value of future cash flows covers obligations with a given confidence level (e.g., 75%, 90%).")
        ),
        
        tags$li(
          tags$span(style = "font-style:italic; font-weight:600;", 
                    "Cost of Capital Method"),
          p("Calculates RA based on the cost of holding capital to support non-financial risks over time."),
          tags$div(
            tags$strong("Key Components:"),
            tags$ul(
              tags$li("Required capital amount (e.g. 99.5% VaR)"),
              tags$li("Holding period (e.g. contract lifetime)"),
              tags$li("Cost of capital rate (e.g. 6%)")
            )
          )
        ),
        
        tags$li(
          tags$span(style = "font-style:italic; font-weight:600;", 
                    "Conditional Tail Expectation (CTE)"),
          p("Also referred to as the tail value at risk; it reflects the average of all worse outcomes beyond a certain threshold.")
        ),
        img(src = "images/riskAdjustmentApproaches.jpeg", alt = "Risk Adjustment Approaches", class = "module-image")
      )
    ),

    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Discounting, CSM & Risk Adjustment."),
    ),

    box(
      title = "1. Which of the following is NOT a required characteristic of the discount rate under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL,
                  choices = c(
                    "Consistency with observable market prices for similar cash flows",
                    "Inclusion of illiquidity premiums to reflect insurance contract liquidity",
                    "Use of a single fixed discount rate across all types of contracts",
                    "Alignment with other assumptions used in valuation to avoid double counting"),
                  selected = character(0))
    ),

    box(
      title = "2. Which of the following is a correct interpretation of IFRS 17's guidance on using market data to determine discount rates?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL,
                  choices = c(
                    "Discount rates must exclude the effect of market variables that do not impact the insurance contract's cash flows.",
                    "Observable market prices should always be used, even if they include factors unrelated to insurance contract cash flows.",
                    "Market observable discount rates can be used even if they reflect credit risk not relevant to the insurance liability.",
                    "Discount rates should reflect all observable market factors regardless of contract characteristics."),
                  selected = character(0))
    ),

    box(
      title = "3. What is the primary distinction between the bottom-up and top-down approaches for deriving discount rates under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL,
                  choices = c(
                    "The bottom-up approach starts from asset returns and adjusts for insurance features",
                    "The top-down approach uses a risk-free curve and adds risk premiums",
                    "The top-down approach always requires matching the exact liquidity of insurance contracts.",
                    "The bottom-up approach starts with a liquid risk-free yield curve and adjusts for illiquidity"),
                  selected = character(0))
    ),

    box(
      title = "4. Which statement is TRUE regarding liquidity adjustments in the top-down approach under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL,
                  choices = c(
                    "Liquidity differences between the reference assets and insurance contracts must always be adjusted",
                    "No liquidity adjustments are allowed under the top-down approach",
                    "Adjustments are made only if the reference portfolio’s liquidity differs significantly from that of the insurance contracts",
                    "Liquidity risk is already captured in the nominal cash flows, so no adjustments are required"),
                  selected = character(0))
    ),

        p(strong("5. An insurance contract is expected to pay the following future cash flows at the end of each year for the next 3 years:")),
        div(style = "display: flex; justify-content: center; margin-top: 20px;",
          tags$table(class = "table table-bordered", style = "width: 300px; text-align: center;",
            tags$thead(
              tags$tr(
                tags$th("Year", style = "background-color: #DC5A17; color: white;"),
                tags$th("Expected Cash Flow", style = "background-color: #DC5A17; color: white;")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("1", style = "background-color: #006AA6; color: white;"),
                tags$td("$1,000")
              ),
              tags$tr(
                tags$td("2", style = "background-color: #006AA6; color: white;"),
                tags$td("$1,200")
              ),
              tags$tr(
                tags$td("3", style = "background-color: #006AA6; color: white;"),
                tags$td("$1,500")
              )
            )
          )
        ),
      p("Assume the following:", style = "margin-left: 10px;"),
      tags$ol(type = "a",
        tags$li("The risk-free interest rate is 2% per annum, compounded annually."),
        tags$li("The liquidity adjustment for the insurance contract is 0.5% per annum."),
        tags$li("Use the bottom-up approach to calculate discount rates."),
        tags$li("Discounting is done from end-of-year values to present value at time 0.")
      ),
    box(
      title = " i) What is the total discount rate to be used under the bottom-up approach?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5_part1"), label = NULL,
                  choices = c("A) 3.0%", "B) 2.5%", "C) 3.5%", "D) 2.0%"),
                  selected = character(0))
    ),

    box(
      title = "5. ii) What is the present value of the expected cash flows using the appropriate discount rate from Part A?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5_part2"), label = NULL,
                  choices = c("A) 3,506.85", "B) 3,477.32", "C) 3,599.25", "D) 3,423.18"),
                  selected = character(0))
    ),

    box(
      title = "5. iii) If instead the top-down approach was used and the yield on the reference portfolio is 4.5%, and credit & non-insurance-related risks account for 2%, what would be the equivalent discount rate?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5_part3"), label = NULL,
                  choices = c("A) 4.5%", "B) 2.5%", "C) 2.0%", "D) 6.5%"),
                  selected = character(0))
    ),

    box(
      title = "5. iv) What is the impact on the present value if the liquidity adjustment increases to 1.0%, keeping the risk-free rate constant?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5_part4"), label = NULL,
                  choices = c("A) Present Value increases", "B) Present Value decreases", "C) Present Value stays the same", "D) Present Value becomes negative"),
                  selected = character(0))
    ),

    box(
      title = "6. When a group of insurance contracts becomes onerous after initial recognition under IFRS 17, what happens to the Contractual Service Margin (CSM)?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6"), label = NULL,
                  choices = c(
                    "It is increased to reflect the higher expected losses.",
                    "It remains unchanged, as changes are only recognized at initial recognition.",
                    "It is set to zero, and a loss component is established to reflect the excess of fulfilment cash flows over the expected inflows.",
                    "It is transferred to the Liability for Incurred Claims (LIC)."),
                  selected = character(0))
    ),

    box(
      title = "7. Can a loss component (LC) established for an onerous group of contracts under IFRS 17 be reversed in subsequent periods?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL,
                  choices = c(
                    "No, once established, a loss component cannot be reversed.",
                    "Yes, but only through adjustments to the Risk Adjustment for non-financial risk.",
                    "Only if the contracts are derecognized.",
                    "Yes, if future changes in fulfilment cash flows indicate that the group is no longer onerous."),
                  selected = character(0))
    ),

    box(
      title = "8. In the context of IFRS 17, what does the Liability for Remaining Coverage (LRC) represent when the Contractual Service Margin (CSM) is nil?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL,
                  choices = c(
                    "The sum of the fulfilment cash flows and the loss component.",
                    "Only the present value of future cash flows without any adjustments.",
                    "The Liability for Incurred Claims (LIC) only.",
                    "The Risk Adjustment for non-financial risk only."),
                  selected = character(0))
    ),

    box(
      title = "9. Which discount rate is used to accrete interest on the CSM?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL,
                  choices = c(
                    "The risk-free rate at the reporting date",
                    "The weighted average discount rate for incurred claims",
                    "The current market interest rate for government bonds",
                    "The discount rate at initial recognition of the group of contracts"),
                  selected = character(0))
    ),
      p("10. An insurance company issues a group of insurance contracts on 1 January 20X1. The following information is available:"),
      tags$ol(type = "a",
        tags$li("Present Value of Future Cash Inflows: $ 1,200."),
        tags$li("Present Value of Future Cash Outflows (including acquisition costs): $ 1,000"),
        tags$li("Risk Adjustment for Non-Financial Risk: $ 50"),
        tags$li("Discount rate at initial recognition: 5% annually, compounded yearly."),
        tags$li("Coverage period: 4 years, with coverage services evenly provided over time.")
      ),
    box(
      title = "i) What is the initial CSM at 1 January 20X1?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10_part1"), label = NULL,
                  choices = c("A. $ 200", "B. $ 150", "C. $ 250", "D. $ 100"),
                  selected = character(0))
    ),

    box(
      title = "10. ii) What is the CSM balance at 31 December 20X1, before release, assuming a 5% interest rate?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10_part2"), label = NULL,
                  choices = c("A. $ 150", "B. $ 157.5", "C. $ 160", "D. $ 155"),
                  selected = character(0))
    ),

    box(
      title = "10. iii) If the CSM is released evenly over the 4-year coverage period, what is the CSM balance after release at 31 December 20X1?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10_part3"), label = NULL,
                  choices = c("A. $ 118.125", "B. $ 120", "C. $ 130", "D. $ 100"),
                  selected = character(0))
    ),

    box(
      title = "11. Which of the following characteristics would lead to a higher risk adjustment according to IFRS 17 principles?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q11"), label = NULL,
                  choices = c(
                    "High-frequency, low-severity risks",
                    "Short-duration contracts with predictable claims",
                    "Risks with narrow probability distributions",
                    "Contracts where little is known about emerging experience"),
                  selected = character(0))
    ),

    box(
      title = "12. Which of the following risks is excluded from the IFRS 17 risk adjustment?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q12"), label = NULL,
                  choices = c(
                    "Lapse risk",
                    "Expense risk",
                    "Financial risk (e.g. interest rate risk)",
                    "Morbidity risk"),
                  selected = character(0))
    ),

    box(
      title = "13. Two otherwise identical contracts differ only in duration: Contract A is 5 years; Contract B is 15 years. Which will have the higher risk adjustment, and why?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q13"), label = NULL,
                  choices = c(
                    "Contract A, due to faster runoff",
                    "Contract B, due to longer exposure to uncertainty",
                    "Both will have the same risk adjustment",
                    "Contract A, due to the need for more immediate reserves"),
                  selected = character(0))
    ),

    box(
      title = "14. Which method is not typically used to quantify the risk adjustment under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q14"), label = NULL,
                  choices = c(
                    "Cost of capital method",
                    "Conditional Tail Expectation (CTE)",
                    "Confidence level",
                    "Historical premium rate analysis"),
                  selected = character(0))
    ),
      p("15.	An insurer estimates that the present value of future cash outflows from a group of insurance contracts follows a normal distribution with:"),
      tags$ol(type = "i",
        tags$li("Mean (Expected Value): $10,000,000"),
        tags$li("Standard Deviation: $1,500,000"),
        tags$li("The insurer uses a confidence level approach to determine the risk adjustment for non-financial risk.")
      ),
    box(
      title = "(a) What is the value of the liability at the 75% confidence level? Use z = 0.674",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q15_part1"), label = NULL,
                  choices = c(
                    "A. $10,506,000",
                    "B. $11,011,000",
                    "C. $11,674,000",
                    "D. $12,350,000"),
                  selected = character(0))
    ),

    box(
      title = "15. (b) What is the risk adjustment for non-financial risk at the 75% confidence level?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q15_part2"), label = NULL,
                  choices = c(
                    "A. $674,000",
                    "B. $750,000",
                    "C. $1,011,000",
                    "D. $1,674,000"),
                  selected = character(0))
    ),

    box(
      title = "15. (c) If the insurer increases the confidence level to 90%, what is the new risk adjustment? (Use z = 1.282)",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q15_part3"), label = NULL,
                  choices = c(
                    "A. $1,282,000",
                    "B. $1,500,000",
                    "C. $1,922,000",
                    "D. $2,282,000"),
                  selected = character(0))
    ),

    actionButton(ns("submit"), "Submit Quiz", icon = icon("check"), class = "btn-primary control-button-submit" ),
    br(), 
    br(),
    uiOutput(ns("result")),
    div(
      class = "quiz-nav",
      actionButton(
          ns("to_module_8"),
          label = tagList(icon("arrow-right"), "Next: Module 8 - Onerous Contracts"),
          class = "control-button-tavnav"
      )
    ) 
    )
}

correct_answers_module7 <- list(
  q1 = list(
    answer = "Use of a single fixed discount rate across all types of contracts",
    explanation = "IFRS 17 does not require or recommend a single fixed discount rate for all contracts. Instead, the discount rate should reflect characteristics like liquidity, inflation, and dependency on underlying items."
  ),
  q2 = list(
    answer = "Discount rates must exclude the effect of market variables that do not impact the insurance contract's cash flows.",
    explanation = "IFRS 17 requires that discount rates exclude market variables that don’t affect the cash flows of the insurance contract, even if these variables are reflected in market prices."
  ),
  q3 = list(
    answer = "The bottom-up approach starts with a liquid risk-free yield curve and adjusts for illiquidity",
    explanation = "The bottom-up approach starts with a liquid risk-free yield curve, then adjusts it to reflect the characteristics of the insurance contracts."
  ),
  q4 = list(
    answer = "Adjustments are made only if the reference portfolio’s liquidity differs significantly from that of the insurance contracts",
    explanation = "IFRS 17 allows flexibility in adjusting for liquidity under the top-down approach, requiring adjustments only if the reference assets’ liquidity is not reasonably consistent with that of the insurance contracts."
  ),
  q5_part1 = list(
    answer = "B) 2.5%",
    explanation = "Under the bottom-up approach, the discount rate is calculated as the sum of the risk-free rate and liquidity premium: 2% + 0.5% = 2.5%"
  ),
  q5_part2 = list(
    answer = "A) 3,506.85",
    explanation = "Present value is calculated using a 2.5% discount rate applied to the given cash flows: PV ≈ 975.61 + 1,142.10 + 1,389.14 = 3,506.85"
  ),
  q5_part3 = list(
    answer = "B) 2.5%",
    explanation = "From a 4.5% reference portfolio yield, subtracting 2% non-insurance risk adjustments gives a 2.5% discount rate, aligning with bottom-up."
  ),
  q5_part4 = list(
    answer = "B) Present Value decreases",
    explanation = "An increase in the liquidity adjustment increases the total discount rate, leading to a lower present value of future cash flows."
  ),
  q6 = list(
    answer = "It is set to zero, and a loss component is established to reflect the excess of fulfilment cash flows over the expected inflows.",
    explanation = "If contracts become onerous after recognition, IFRS 17 sets the CSM to zero and establishes a loss component."
  ),
  q7 = list(
    answer = "Yes, if future changes in fulfilment cash flows indicate that the group is no longer onerous.",
    explanation = "IFRS 17 allows reversal of loss components if favourable changes indicate the group is no longer onerous."
  ),
  q8 = list(
    answer = "The sum of the fulfilment cash flows and the loss component.",
    explanation = "When the CSM is nil, the LRC comprises the fulfilment cash flows and the loss component."
  ),
  q9 = list(
    answer = "The discount rate at initial recognition of the group of contracts",
    explanation = "CSM interest accretion is based on the discount rate determined at initial recognition of the group."
  ),
  q10_part1 = list(
    answer = "B. $ 150",
    explanation = "CSM = PV inflows – PV outflows – Risk Adjustment = 1,200 – 1,000 – 50 = 150"
  ),
  q10_part2 = list(
    answer = "B. $ 157.5",
    explanation = "Interest accretion = CSM × 1.05 = 150 × 1.05 = 157.5"
  ),
  q10_part3 = list(
    answer = "A. $ 118.125",
    explanation = "Release = 157.5 ÷ 4 = 39.375; Remaining CSM = 157.5 – 39.375 = 118.125"
  ),
  q11 = list(
    answer = "Contracts where little is known about emerging experience",
    explanation = "Higher uncertainty from limited knowledge leads to a higher risk adjustment under IFRS 17 (B91)."
  ),
  q12 = list(
    answer = "Financial risk (e.g. interest rate risk)",
    explanation = "The risk adjustment only covers non-financial risks. Financial risks are included in discount rate or cash flows."
  ),
  q13 = list(
    answer = "Contract B, due to longer exposure to uncertainty",
    explanation = "Longer duration implies greater uncertainty, requiring higher risk adjustment (B91(b))."
  ),
  q14 = list(
    answer = "Historical premium rate analysis",
    explanation = "Historical premium analysis is not a valid standalone method for risk adjustment; IFRS 17 prefers cost of capital and quantile methods."
  ),
  q15_part1 = list(
    answer = "B. $11,011,000",
    explanation = "Liability = 10,000,000 + 0.674 × 1,500,000 = 11,011,000"
  ),
  q15_part2 = list(
    answer = "C. $1,011,000",
    explanation = "Risk adjustment = Liability at 75% – Expected value = 11,011,000 – 10,000,000"
  ),
  q15_part3 = list(
    answer = "C. $1,922,000",
    explanation = "Risk adjustment = 10,000,000 + 1.282 × 1,500,000 – 10,000,000 = 1,923,000 (rounded to 1,922,000)"
  )
)


IFRS17Module7Server <- (function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Add a reactive value to track when progress is saved
    progress_saved_trigger <- reactiveVal(0)

    score <- reactiveVal(0)

    # Feedback functions
    observeEvent(input$submit, {
        removeModal()
        # 1. List out all your question input IDs
        question_ids <- c(
          paste0("q", 1:4),
          paste0("q5_part", 1:4),
          paste0("q", 6:9),
          paste0("q10_part", 1:3),
          paste0("q", 11:14),
          paste0("q15_part", 1:3)
        )

        
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

    for (qid in names(correct_answers_module7)) {
      correct <- correct_answers_module7[[qid]]$answer
      explanation <- correct_answers_module7[[qid]]$explanation
      user_answer <- input[[qid]]

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
      # Save progress for Module 7
      if (!is.null(user_data) && isTRUE(user_data$is_authenticated) && !isTRUE(user_data$is_guest)) {
        # Module 2 specific calculations
        total_questions <- length(correct_answers_module7)
        final_score <- score()
        final_percentage <- round((final_score / total_questions) * 100, 1)
        
        # Save to database
        tryCatch({
          progress_saved <- save_user_progress(
            user_id = user_data$user_id,
            module_name = "module7",  # Module 2 identifier
            score = final_score,
            percentage = final_percentage,
            completed_at = Sys.time(),
            token = user_data$token
          )
          
          if (progress_saved) {
            # Success notification with score
            showNotification(
              HTML(paste0(
                "<strong>✅ Module 7 Progress Saved!</strong><br>",
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
          print(paste("Module 7 progress save error:", e$message))
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



    output$result <- renderUI({
      total_questions <- length(correct_answers_module7)
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
            h4("has successfully completed the IFRS 17 - Discounting, CSM & Risk Adjustment Quiz",
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
              "📊 Module 7 Results Summary",
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

    # observeEvent(input$print_certificate, {
    #   runjs('
    #     var cert = document.querySelector(".print-area");
    #     if (!cert) {
    #       alert("Nothing to print – make sure you have submitted the quiz first.");
    #     } else {
    #       // open a blank window
    #       var w = window.open("", "_blank", "width=800,height=600");
    #       // build a print-only style to hide .no-print
    #       var head = `
    #         <head>
    #           <title>Participation Certificate</title>
    #           <style>
    #             body { margin:20px; font-family:Arial,sans-serif; }
    #             @media print { .no-print { display: none; } }
    #           </style>
    #         </head>`;
    #       // grab the certificate HTML
    #       var body = "<body>" + cert.outerHTML +
    #                 // wrap your button in a no-print div
    #                 "<div class=\\"no-print\\" style=\\"text-align:center; margin-top:30px;\\">" +
    #                   "<button onclick=\\"window.print()\\">Print Certificate as PDF</button>" +
    #                 "</div></body>";
    #       // write it all
    #       w.document.write("<!doctype html><html>" + head + body + "</html>");
    #       w.document.close();
    #       w.onload = function() {
    #         w.focus();
    #         w.print();
    #       };
    #     }
    #   ')
    # })

    # create a reactive for the “Next” click
    to_module_8 <- reactive(input$to_module_8)

    # Return both the navigation trigger and the progress update trigger
    return(list(
      navigate = to_module_8,
      progress_trigger = progress_saved_trigger
    ))

  })
})