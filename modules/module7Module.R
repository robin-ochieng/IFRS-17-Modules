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
        h3(icon("info-circle"), "Module Objective", class = "section-title-top"),
        p("This module provides an overview of Discounting, Contractual Service Margin (CSM) and Risk Adjustment (RA) under IFRS 17.")
     ),
    div(class = "module-section",
      h3("Fulfilment Cash flows", class = "section-title-top"),
      p("At initial recognition, an entity is required to measure a group of insurance contracts as the sum of:"),
      tags$ul(
        tags$li(
          strong("Fulfilment Cash Flows (FCF), which include:"),
          tags$ul(
            tags$li("Estimates of future cash inflows and outflows."),
            tags$li("A discount adjustment to reflect the time value of money and financial risks tied to those cash flows, plus a risk adjustment for non-financial uncertainty.")
          )
        ),
        tags$li(
          strong("Contractual Service Margin (CSM), representing the unearned profit from the contracts.")
        ),
        ,
      img(src = "images/fulfilmentCashFlows.png", alt = "Fulfilment Cash Flows", class = "module-image")
      )
    ),
    div(class = "module-section",
      h3("Discounting", class = "section-title-top"),
      p("This sub-module introduces the key principle of discounting in IFRS 17, highlighting the need to adjust future cash flows to account for the time value of money. This process ensures that projected future payments and receipts are presented in today’s terms, leading to a more accurate valuation of insurance liabilities."),
      p("Specifically:"),
      tags$ol(
        tags$li("Paragraph 36 includes discounting as part of the fulfilment cash flows."),
        tags$li("Paragraphs 38–39 outline the key characteristics that discount rates must reflect."),
        tags$li("Paragraphs 44(b) and 47 explain how interest is accreted on the Contractual Service Margin (CSM) using the original discount rate set at contract inception."),
        tags$li("Appendix B72–B85 provides detailed guidance on selecting appropriate discount rates and constructing yield curves.")
      ),
      h4("What is Discounting?", class = "section-subtitle"),
      p("Discounting is the process of converting future cash flows into present values based on the principle that a sum of money held now is more valuable than the same amount received later — this is known as the time value of money."),
      p("The discount rate used should capture features such as liquidity, inflation expectations, and how the cash flows are influenced by any underlying items.")
    ),
    div(class = "module-section",
      h3("Discounting Approaches", class = "section-title-top"),
      p("IFRS 17 outlines two methods insurers can use to determine appropriate discount rates:"),
      
      h4("a) Bottom-Up Approach", class = "section-subtitle"),
      p(strong("How It Works:")),
      tags$ul(
        tags$li("Begins with a risk-free yield curve (such as government bond rates)"),
        tags$li("Adds a liquidity premium if the insurance contracts are not easily tradable (i.e., illiquid)")
      ),
      
      h4("b) Top-Down Approach", class = "section-subtitle"),
      p(strong("How It Works:")),
      tags$ol(
        tags$li("Start with the total yield of a reference asset portfolio"),
        tags$li("Deduct elements unrelated to insurance contract cash flows (e.g., credit risk, other market risks)")
      ),
      
      p("IFRS 17 requires that discount rates exclude any market variables that don’t impact the insurance cash flow, even if such factors are reflected in asset prices. This helps ensure the liability measurement reflects the true economics of the contract."),
      img(src = "images/discountingApproaches.png", alt = "Discounting Approaches", class = "module-image")
    ),
    div(class = "module-section",
      h3("Contractual Service Margin", class = "section-title-top"),
      p("This sub-module gives a clear summary of how the Contractual Service Margin (CSM) is calculated, adjusted over time, and released into profit or loss under IFRS 17."),
      p("CSM represents the unearned profit an insurance company expects to earn as it provides coverage over the life of an insurance contract. It is part of the Liability for Remaining Coverage (LRC)."),
      p("Under IFRS 17, the following paragraphs and appendices cover CSM:"),
      tags$ol(
        tags$li("Paragraphs 38–39 – Define fulfilment cash flows, which form the basis for measuring the CSM."),
        tags$li("Paragraph 43 – States that no gain is recognised at initial recognition; any positive fulfilment cash flows are absorbed into the CSM."),
        tags$li(
          "Paragraphs 44–45 – Explain how the CSM is adjusted over time, including:",
          tags$ul(
            tags$li("Interest accretion"),
            tags$li("Release of the CSM as service is provided"),
            tags$li("Changes in estimates related to future service")
          )
        ),
        tags$li("Paragraph 46 – Describes treatment for onerous contracts, where the CSM is set to zero and a loss component is established."),
        tags$li("Paragraph 47 – Requires that interest on the CSM be accreted using the locked-in discount rate from initial recognition."),
        tags$li("Appendix B94–B96 – Provide guidance on allocating the CSM across coverage periods in a way that reflects the insurance services provided."),
        tags$li("Appendix B97–B100 – Detail how to adjust the CSM when there are changes in estimates or when contracts are derecognized.")
      )
    ),
    div(class = "module-section",
      h3("CSM at Initial Recognition", class = "section-title-top"),
      p("At the time a group of insurance contracts is initially recognised—typically when the contracts begin—IFRS 17 requires the calculation of a Contractual Service Margin (CSM), provided the contracts are expected to generate a profit."),
      # Placeholder for the CSM formula diagram
      img(
        src = "images/csmatInitialRecognition.png",
        alt = "CSM Initial Recognition Formula",
        class = "module-image"
      ),
      p(em("If the result is positive → this becomes the initial CSM (unearned profit). If the result is negative → the contract is onerous and no CSM is recognised. Instead, a loss component is created.")),
      
      h3("CSM at Subsequent Measurement", class = "section-title-top"),
      p("After the initial Contractual Service Margin (CSM) is established, it is regularly updated to reflect changes over time. These updates include:"),
      tags$ul(
        tags$li(
          strong("Interest Accretion – "),
          "The CSM increases using the discount rate locked in at initial recognition, reflecting the passage of time."
        ),
        tags$li(
          strong("Release of Profit – "),
          "As the insurer delivers coverage, a portion of the CSM is recognised as revenue. This is typically spread evenly unless another method better represents the service provided."
        ),
        tags$li(
          strong("Changes in Future Estimates – "),
          "If expectations about future cash flows improve (e.g., fewer claims), the CSM is adjusted—but only if the group is not onerous."
        ),
        tags$li(
          strong("Onerous Contracts – "),
          "If a group becomes loss-making, the CSM is reduced to zero, and a loss component is recorded to reflect the deficit."
        )
      ),
      p("This loss component can be reversed in later periods if future cash flows improve and the contracts are no longer considered onerous.")
    ),
    div(class = "module-section",
      h3("Risk Adjustment", class = "section-title-top"),
      p("This sub-module focuses on the Risk Adjustment, a critical element in calculating insurance contract liabilities under IFRS 17."),
      tags$ul(
        tags$li("Paragraph 37 identifies the Risk Adjustment as a part of the fulfilment cash flows."),
        tags$li("Paragraph 44 requires disclosure of the methods used and the confidence level (or equivalent)."),
        tags$li("Appendix B86–B92 provides detailed guidance on its principles, methodologies, and influencing factors.")
      ),
      
      h4("What is Risk Adjustment in IFRS 17?", class = "section-subtitle"),
      p("The Risk Adjustment for non-financial risk represents the amount an insurer charges for bearing the uncertainty around timing and amount of future cash flows, stemming from risks such as mortality, morbidity, lapse, and expenses."),
      p("While expected cash flows capture the average outcome, the risk adjustment reflects variability and uncertainty around those expectations. It ensures liabilities include not just a neutral estimate, but also a margin that aligns with the insurer’s risk appetite and capacity to bear risk."),
      
      h4("Factors That Influence the Risk Adjustment", class = "section-subtitle"),
      p("The Risk Adjustment for non-financial risk reflects the level of compensation the insurer requires for bearing the uncertainty around cash flows. According to IFRS 17 Appendix B88–B91, key influencing factors include:"),
      tags$ul(
        tags$li(
          strong("Degree of Uncertainty — "),
          "Greater uncertainty in the timing or amount of future cash flows leads to a higher risk adjustment."
        ),
        tags$li(
          strong("Duration of Contracts — "),
          "Longer coverage periods increase the risk exposure over time, resulting in a higher risk adjustment due to more future uncertainty."
        ),
        tags$li(
          strong("Amount of Claims — "),
          "Higher potential variability in future claim amounts contributes to a greater need for compensation."
        ),
        tags$li(
          strong("Policyholder Characteristics & Options — "),
          "Contracts with features such as lapse options, guarantees, or uncertain policyholder behaviour increase complexity and raise risk adjustment."
        ),
        tags$li(
          strong("Quality of Data and Estimates — "),
          "Less credible or incomplete data increases estimation uncertainty, resulting in a higher risk adjustment."
        ),
        tags$li(
          strong("Diversification — "),
          "Portfolios with higher diversification benefits may require lower aggregate risk adjustments, as risks offset each other."
        ),
        tags$li(
          strong("Reinsurance Impact — "),
          "When risk is transferred through reinsurance, the risk adjustment is recognized separately for the cedant and the reinsurer, reflecting the risk retained and assumed."
        )
      )
    ),
    div(class = "module-section",
      h3("Methods of Determining Risk Adjustment", class = "section-title-top"),
      p("IFRS 17 does not mandate a specific calculation method for the risk adjustment. However, it sets out the principles that any method must meet:"),
      tags$ol(
        tags$li("The RA must reflect the compensation an entity requires for bearing uncertainty from non-financial risks."),
        tags$li("It must be explicitly and separately disclosed from other fulfilment cash flow components."),
        tags$li("The method chosen must be consistent with the entity’s own risk assessment — it should reflect how the entity would internally price and manage non-financial risk.")
      ),
      p("The three most common approaches:"),
      
      h4("1. Confidence Level Approach (Quantile Method)", class = "section-subtitle"),
      p("Sets the Risk Adjustment based on a selected confidence level — e.g., determining the value of future cash flows such that the entity is 75% or 90% confident it can meet its obligations."),
      
      h4("2. Cost of Capital Method", class = "section-subtitle"),
      p("Estimates the RA as the cost of holding capital to support non-financial risk over the lifetime of the contract."),
      tags$ul(
        tags$li("Risk capital amount (e.g., based on Value at Risk)"),
        tags$li("Capital holding period"),
        tags$li("Cost of capital rate (e.g., 6%)")
      ),
      
      h4("3. Conditional Tail Expectation (CTE)", class = "section-subtitle"),
      p("Also known as Tail Value at Risk, this method looks at the average loss beyond a selected confidence level, capturing extreme outcomes.")
    ),

    div(class = "module-section",
        h3("📝 Quiz: Answer the following questions to test your understanding of Discounting, CSM & Risk Adjustment."),
    ),

    # Module 7 Quiz UI
    box(
      title = "1. Which of the following is NOT a requirement when determining the Contractual Service Margin (CSM) under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q1"), label = NULL,
        choices = c(
          "Adjusting for the time value of money using current discount rates",
          "Including all acquisition cash flows, regardless of recoverability",
          "Recognizing the CSM over the coverage period based on services provided",
          "Ensuring no day-one profit is recognized"
        ), selected = character(0))
    ),

    box(
      title = "2. Which of the following is NOT a requirement when determining the discount rates under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q2"), label = NULL,
        choices = c(
          "Reflect the time value of money",
          "Be consistent with observable current market prices",
          "Be based on the entity’s internal cost of capital",
          "Reflect characteristics of the cash flows of the insurance contracts"
        ), selected = character(0))
    ),

    box(
      title = "3. When determining discount rates under IFRS 17, which best reflects how market variables should be considered?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q3"), label = NULL,
        choices = c(
          "Market variables should be incorporated if they influence the expected cash flows of the insurance contract.",
          "Market variables should be considered only if they are contractually guaranteed.",
          "Only risk-free rates should be used, excluding any market variables.",
          "Market variables are not relevant for insurance contracts measured using the PAA."
        ), selected = character(0))
    ),

    box(
      title = "4. Which best distinguishes the top-down from the bottom-up approach in deriving discount rates?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q4"), label = NULL,
        choices = c(
          "Top-down uses market yield curves; bottom-up starts with historical claims data",
          "Bottom-up derives rates from illiquid liabilities; top-down adjusts for asset characteristics",
          "Bottom-up always yields lower rates than top-down",
          "Top-down starts with reference portfolio yield and deducts non-insurance risks; bottom-up builds from risk-free + illiquidity premium"
        ), selected = character(0))
    ),

    box(
      title = "5. How is the liquidity characteristic addressed in the top-down approach under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q5"), label = NULL,
        choices = c(
          "Liquidity adjustments are not permitted in the top-down approach.",
          "The liquidity premium is added directly to the risk-free rate.",
          "The insurer deducts elements like credit risk and market risk to isolate a rate consistent with contract liquidity.",
          "A standard liquidity adjustment of 100 bp is applied regardless of contract type."
        ), selected = character(0))
    ),

    p(strong("6. Goobe Insurance expected cash flows and applicable discount rates:")),
    tags$table(class = "table table-bordered", style = "width:300px; text-align:center;",
      tags$thead(
        tags$tr(
          tags$th("Year"), tags$th("Cash Flow ($’000)"), tags$th("Discount Rate (%)")
        )
      ),
      tags$tbody(
        tags$tr(tags$td("1"), tags$td("500"), tags$td("9")),
        tags$tr(tags$td("2"), tags$td("700"), tags$td("10")),
        tags$tr(tags$td("3"), tags$td("600"), tags$td("11.5")),
        tags$tr(tags$td("4"), tags$td("400"), tags$td("13"))
      )
    ),

    box(
      title = "6.i) What is the present value of the Year 1 cash flow using the yield curve provided?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6_part1"), label = NULL,
        choices = c(
          "A) $455,734",
          "B) $472,546",
          "C) $512,789",
          "D) $458,716"
        ), selected = character(0))
    ),

    box(
      title = "6.ii) What is the total present value of all 4 years? of expected cash flows, using the yield curve provided?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6_part2"), label = NULL,
        choices = c(
          "A) $1,713,700",
          "B) $1,877,500",
          "C) $1,845,000",
          "D) $1,925,489"
        ), selected = character(0))
    ),

    box(
      title = "6.iii) Which of the following best describes the effect of using a steeper yield curve on long-term liabilities?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6_part3"), label = NULL,
        choices = c(
          "A) It increases the PV of long-term liabilities",
          "B) It reduces the PV of long-term liabilities",
          "C) It has no effect",
          "D) It only affects assets"
        ), selected = character(0))
    ),

    box(
      title = "6.iv) If the illiquidity premium for the cash flows is estimated at 0.5% per year, how would the bottom-up discount rate for Year 3 change?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q6_part4"), label = NULL,
        choices = c("A) 12.0%", "B) 11.0%", "C) 11.5%", "D) 10.5%"),
        selected = character(0))
    ),

    box(
      title = "7.	When a group of insurance contracts becomes onerous after initial recognition under IFRS 17, what happens to the Contractual Service Margin (CSM)?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q7"), label = NULL,
        choices = c(
          "It is increased to reflect higher expected losses.",
          "It remains unchanged.",
          "It is set to zero, and a loss component is established.",
          "It is transferred to the LIC."
        ), selected = character(0))
    ),

    box(
      title = "8. Can a loss component (LC) established for an onerous group of contracts under IFRS 17 be reversed in subsequent periods?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q8"), label = NULL,
        choices = c(
          "No, never.",
          "Yes, but only via risk-adjustment changes.",
          "Only if contracts are derecognized.",
          "Yes, if future cash-flow changes show the group is no longer onerous."
        ), selected = character(0))
    ),

    box(
      title = "9. In the context of IFRS 17, what does the Liability for Remaining Coverage (LRC) represent when the Contractual Service Margin (CSM) is nil?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q9"), label = NULL,
        choices = c(
          "Sum of fulfilment cash flows + loss component",
          "Only present value of future cash flows",
          "LIC only",
          "Risk Adjustment only"
        ), selected = character(0))
    ),

    box(
      title = "10. Which discount rate is used to accrete interest on the CSM?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q10"), label = NULL,
        choices = c(
          "Risk-free rate at reporting date",
          "Weighted average rate for incurred claims",
          "Current government bond rate",
          "Discount rate at initial recognition"
        ), selected = character(0))
    ),

    p("11. An insurance company issues a group of insurance contracts on 1 January 20X1. The following information is available:"),
    tags$ol(type = "a",
      tags$li("PV inflows = $1,200"),
      tags$li("PV outflows = $1,000"),
      tags$li("Risk Adjustment = $50"),
      tags$li("Rate = 5% p.a."),
      tags$li("Coverage = 4 yrs, evenly")
    ),

    box(
      title = "11.i) What is the initial CSM at 1 January 20X1?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q11_part1"), label = NULL,
        choices = c("A) $200", "B) $150", "C) $250", "D) $100"),
        selected = character(0))
    ),

    box(
      title = "11.ii) What is the CSM balance at 31 December 20X1, before release, assuming a 5% interest rate?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q11_part2"), label = NULL,
        choices = c("A) $150", "B) $157.5", "C) $160", "D) $155"),
        selected = character(0))
    ),

    box(
      title = "11.iii) If the CSM is released evenly over the 4-year coverage period, what is the CSM balance after release at 31 December 20X1?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q11_part3"), label = NULL,
        choices = c("A) $118.125", "B) $120", "C) $130", "D) $100"),
        selected = character(0))
    ),

    box(
      title = "12. Which characteristic leads to a higher risk adjustment?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q12"), label = NULL,
        choices = c(
          "High-frequency, low-severity risks",
          "Short-duration with predictable claims",
          "Narrow probability distributions",
          "Little known about emerging experience"
        ), selected = character(0))
    ),

    box(
      title = "13. Which risk is EXCLUDED from the IFRS 17 risk adjustment?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q13"), label = NULL,
        choices = c(
          "Lapse risk",
          "Expense risk",
          "Financial risk (e.g. interest-rate)",
          "Morbidity risk"
        ), selected = character(0))
    ),

    box(
      title = "14. Two contracts (5 yrs vs 15 yrs): which has higher risk adjustment & why?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q14"), label = NULL,
        choices = c(
          "5 yrs, due to faster runoff",
          "15 yrs, due to longer exposure to uncertainty",
          "Both same",
          "5 yrs, due to need for immediate reserves"
        ), selected = character(0))
    ),

    box(
      title = "15. Which method is NOT typically used to quantify the risk adjustment under IFRS 17?",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      radioButtons(ns("q15"), label = NULL,
        choices = c(
          "Cost of capital method",
          "Conditional Tail Expectation (CTE)",
          "Confidence level method",
          "Historical premium rate analysis"
        ), selected = character(0))
    ),

    box(
      title = "16. 15.	An insurer estimates that the present value of future cash outflows from a group of insurance contracts follows a normal distribution with (mean = 10 000 000; sd = 1 500 000; C.	The insurer uses a confidence level approach to determine the risk adjustment for non-financial risk):",
      status = "white", solidHeader = TRUE, width = 12, style = "border-left: 3px solid #DC5A17;",
      p("a) What is the value of the liability at the 75% confidence level (Use z=0.674 for 75% confidence)?"),
      radioButtons(ns("q15_part1"), label = NULL,
        choices = c("A) 10 506 000", "B) 11 011 000", "C) 11 674 000", "D) 12 350 000"),
        selected = character(0)
      ),
      p("b) What is the risk adjustment for non-financial risk at the 75% confidence level?"),
      radioButtons(ns("q15_part2"), label = NULL,
        choices = c("A) 674 000", "B) 750 000", "C) 1 011 000", "D) 1 674 000"),
        selected = character(0)
      ),
      p("c) If the insurer increases the confidence level to 90%, what is the new risk adjustment? (Use z = 1.282)?"),
      radioButtons(ns("q15_part3"), label = NULL,
        choices = c("A) 1 282 000", "B) 1 500 000", "C) 1 922 000", "D) 2 282 000"),
        selected = character(0)
      )
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
# Module 7: Correct Answers
# Module 7: Correct Answers
correct_answers_module7 <- list(
  q1           = list(answer="Including all acquisition cash flows, regardless of recoverability",
                        explanation="Only directly attributable & recoverable acquisition cash flows are included; non-recoverable costs are expensed immediately."),
  q2           = list(answer="Be based on the entity’s internal cost of capital",
                        explanation="Discount rates must reflect TVM, observable market prices & cash-flow characteristics, not the entity’s cost of capital."),
  q3           = list(answer="Market variables should be incorporated if they influence the expected cash flows of the insurance contract.",
                        explanation="If a market variable affects timing/amount of cash flows, it must be reflected in estimates and discount rates."),
  q4           = list(answer="Top-down starts with reference portfolio yield and deducts non-insurance risks; bottom-up builds from risk-free rates plus illiquidity premium",
                        explanation="Top-down uses asset yields minus non-insurance risks; bottom-up uses risk-free + liquidity premium."),
  q5           = list(answer="The insurer deducts elements like credit risk and market risk to isolate a rate consistent with contract liquidity.",
                        explanation="Top-down isolates the appropriate rate by removing non-insurance risks from portfolio yields."),
  q6_part1     = list(answer="D) $458,716",
                        explanation="PV₁ = 500 000 ÷ (1 + 9%) = 458 716."),
  q6_part2     = list(answer="A) $1,713,700",
                        explanation="Sum of each year’s PV at its respective rate = 1 713 700."),
  q6_part3     = list(answer="B) It reduces the PV of long-term liabilities",
                        explanation="Higher long-term rates (steeper curve) → lower PV of distant cash flows."),
  q6_part4     = list(answer="A) 12.0%",
                        explanation="11.5% risk-free + 0.5% illiquidity = 12.0%."),
  q7           = list(answer="It is set to zero, and a loss component is established.",
                        explanation="Onerous group → CSM = 0 + loss component for shortfall."),
  q8           = list(answer="Yes, if future cash-flow changes show the group is no longer onerous.",
                        explanation="Favourable updates can reverse a loss component if group becomes profitable again."),
  q9           = list(answer="Sum of fulfilment cash flows + loss component",
                        explanation="When CSM = 0, LRC = fulfilment cash flows + loss component."),
  q10          = list(answer="Discount rate at initial recognition",
                        explanation="CSM interest accretion uses the rate locked in at initial recognition."),
  q11_part1    = list(answer="B) $150",
                        explanation="CSM = 1 200 − 1 000 − 50 = 150."),
  q11_part2    = list(answer="B) $157.5",
                        explanation="Accretion: 150 × 1.05 = 157.5."),
  q11_part3    = list(answer="A) $118.125",
                        explanation="Release = 157.5 ÷ 4 = 39.375 → 157.5 − 39.375 = 118.125."),
  q12          = list(answer="Little known about emerging experience",
                        explanation="Greater uncertainty (lack of data) → higher risk adjustment (IFRS 17 B91)."),
  q13          = list(answer="Financial risk (e.g. interest-rate)",
                        explanation="Risk Adjustment covers non-financial risks; financial risks are in discounting or cash-flow estimates."),
  q14          = list(answer="15 yrs, due to longer exposure to uncertainty",
                        explanation="Longer duration → greater uncertainty → higher risk adjustment (B91(b))."),
  q15          = list(answer="Historical premium rate analysis",
                        explanation="IFRS 17 methods: confidence-level, CTE or cost-of-capital—not historical premium rates."),
  q15_part1    = list(answer="B) 11 011 000",
                        explanation="10 000 000 + 0.674 × 1 500 000 = 11 011 000."),
  q15_part2    = list(answer="C) 1 011 000",
                        explanation="11 011 000 − 10 000 000 = 1 011 000."),
  q15_part3    = list(answer="C) 1 922 000",
                        explanation="10 000 000 + 1.282 × 1 500 000 − 10 000 000 ≈ 1 923 000 (rounded to 1 922 000).")
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