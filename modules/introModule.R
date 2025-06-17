# Module: IFRS17TrainingIntro

IFRS17TrainingIntroUI <- function(id) {
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
  div(class = "intro-outer-wrapper",
      div(class ="ifrs17-intro-container",
          h1("IFRS 17 Digital Training Module"),
        div(class = "modules-section", 
          p(class = "intro-lead",
            "This self-paced module provides a structured overview of IFRS 17, including its objectives, core measurement models, and practical applications."
           )
          ),
          # hr(class = "divider"),
        div(class = "modules-section",  
            h2(class = "cover-title", "What This Module Covers"),
            tags$ul(class = "cover-list",
              tags$li(class = "cover-box",
                span(class = "cover-icon", "✅"),
                span(class = "cover-text", "Objectives and scope of IFRS 17")
              ),
              tags$li(class = "cover-box",
                span(class = "cover-icon", "🧩"),
                span(class = "cover-text", "How insurance contracts are grouped, measured, and presented")
              ),
              tags$li(class = "cover-box",
                span(class = "cover-icon", "📚"),
                span(class = "cover-text", "Interactive assessments, quizzes, and application-driven learning")
              )
          )
        ),


          div(class = "modules-section",
            h2(class = "modules-title", "📚 Course Modules"),
            div(class = "module-grid",
              div(class = "module-item", "📘 Module 1: Introduction and Scope of IFRS 17"),
              div(class = "module-item", "📄 Module 2: Combination and Separation of Insurance Contracts"),
              div(class = "module-item", "📦 Module 3: Level of Aggregation"),
              div(class = "module-item", "⏱️ Module 4: Recognition of Insurance Contracts"),
              div(class = "module-item", "📐 Module 5: Measurement on Initial Recognition"),
              div(class = "module-item", "📊 Module 6: Subsequent Measurement"),
              div(class = "module-item", "💰 Module 7: Discounting, CSM & Risk Adjustment"),
              div(class = "module-item", "⚠️ Module 8: Onerous Contracts"),
              div(class = "module-item", "📥 Module 9: Premium Allocation Approach"),
              div(class = "module-item", "📤 Module 10: Reinsurance Contracts Held"),
              div(class = "module-item", "💼 Module 11: Investment Contracts with DPF"),
              div(class = "module-item", "✏️ Module 12: Modification & Derecognition"),
              div(class = "module-item", "📑 Module 13: Presentation of Financial Statements"),
              div(class = "module-item", "📈 Module 14: Insurance Service Result"),
              div(class = "module-item", "📉 Module 15: Insurance Finance Income or Expenses")
            )
          ),

          # ——— New Objectives & Scope Section ———
          div(class = "modules-section",
          div(class = "intro-section-header", "Key Objectives & Scope of IFRS 17"),
          tags$ul(class = "objectives-list",
            tags$li("🏷️ Establish a single, consistent framework for all insurance contracts"),
            tags$li("📈 Recognize profits in line with service delivery"),
            tags$li("🔍 Improve comparability between insurers"),
            tags$li("🌍 Applies to insurance, reinsurance, and specific investment contract")
          )),
        div(class = "modules-section",         
          div(
            class = "intro-note",
            # Icon + header
            div(class = "note-header",
                span(class = "note-icon", "ℹ️"),
                span(class = "note-title", "Why It Matters")
            ),
            # Two shorter paragraphs, with key phrases emphasized
            p(
              "IFRS 17 transforms how insurers measure and report obligations. It promotes ", 
              strong("transparency"), " and ",
              strong("consistency"), " across global financial statements."
              
            ),
            p(
              "By the end of this course, you'll understand which contracts fall under IFRS 17, how to apply the standard effectively, and how it impacts insurers, regulators, and stakeholders."
            )
          )),
          div(
            class = "intro-nav",
            actionButton(
              ns("to_measurement"),
              label = tagList(icon("arrow-right"), "Next: Measurement Models"),
              class = "control-button-tavnav"
            )
          )

      )
  )
)

}



IFRS17TrainingIntroServer <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {
 
    to_measurement <- reactive(input$to_measurement)
    
    # return it
    to_measurement


  })
}

