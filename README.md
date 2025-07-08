# IFRS 17 Digital Training Module

An interactive, web-based training application built with **R Shiny** to help insurance professionals and students learn the principles and practical application of IFRS 17. It includes modular content, quizzes with immediate feedback, case studies, a glossary of key terms, and a certificate generation feature.

---

## Features

- **Modular Content**: Five modules covering introduction, measurement models, aggregation, CSM and revenue recognition, and presentation & disclosure.  
- **Interactive Quizzes**: Multiple-choice questions with dynamic feedback, section scores, and a progress bar.  
- **Case Study**: “Wakandan Insurers” real-world example to apply IFRS 17 concepts in practice.  
- **Glossary**: Comprehensive table of key IFRS 17 terms and definitions.  
- **Certificate Generation**: Printable PDF certificate rendered via **rmarkdown** when the participant achieves the passing score.  
- **Participant Tracking**: Input field for participant ID; quiz attempts and scores can be recorded.  
- **Responsive UI**: Built with **shinydashboard**, **plotly**, and **DT** for charts and tables.  
- **Deployment-Ready**: Configured for local launch or deployment on Posit Connect or shinyapps.io.  

---

## Table of Contents

1. [Prerequisites](#prerequisites)  
2. [Installation](#installation)  
3. [Running Locally](#running-locally)  
4. [Project Structure](#project-structure)  
5. [Usage](#usage)  
6. [Deployment](#deployment)  
7. [Contributing](#contributing)  
8. [License](#license)  
9. [Acknowledgements](#acknowledgements)  

---

## Prerequisites

- R (version 4.1 or higher)  
- R packages:  
  - **shiny**  
  - **shinydashboard**  
  - **plotly**  
  - **DT**  
  - **rmarkdown**  
  - **knitr**  
  - **shinyjs**  
  - **shinyauthr** (if authentication is needed)  

---

## Installation

1. **Clone the repository**  
   ```bash
   git clone https://github.com/your-org/ifrs17-training-module.git
   cd ifrs17-training-module

## Project Structure

├── app.R                  # Main Shiny application
├── modules/               # Individual module scripts
│   ├── module1_intro.R
│   ├── module2_measurement.R
│   ├── module3_aggregation.R
│   ├── module4_csm_revenue.R
│   └── module5_presentation.R
├── data/                  # Supporting data files (glossary, case study)
│   ├── glossary.csv
│   └── wakanda_case_study.RData
├── www/                   # Static assets (images, CSS)
│   └── kenbright.png      # Certificate logo
├── certificates/          # Certificate templates and output
│   └── certificate_template.Rmd
├── README.md              # Project documentation
└── LICENSE                # License file
