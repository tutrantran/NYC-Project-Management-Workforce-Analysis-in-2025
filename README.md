# 📊 NYC Project Management Workforce Analysis in 2025
### Education • Salary • Talent Shortages in NYC Public Sector

A data-driven workforce study analyzing how **education, compensation, and hiring demand** interact across Project Management roles in New York City government agencies.

Built with **R + tidyverse** to transform raw payroll and job posting data into actionable insights for workforce planning and talent strategy.

---

## 1. Project Goals
This project answers three practical questions:
1. Does higher education → higher salary?
2. What education levels do employers actually demand?
3. Which PM roles face talent shortages or oversupply?

## 2. Dataset Sources
- NYC Citywide Payroll (FY 2024–2025)
- NYC Job Postings (2023–2025)

After cleaning:
- 1.1M payroll rows → **31K PM roles**
- 5.8K postings → **784 PM postings**

## 3. Tech Stack
- R
- tidyverse (dplyr, ggplot2, stringr, readr)
- janitor
- lubridate
- ggrepel
- statistical modeling (correlation, regression, ANOVA)

## 4. Project Process
This project follows a simple, reproducible analytics workflow:

### 4.1. Business Understanding
- Define workforce and talent gap questions
- Clarify education–salary–demand hypotheses
- Identify decision-making goals for public agencies

### 4.2️. Data Collection
- NYC Payroll (FY 2024–2025)
- NYC Job Postings (2023–2025)

### 4.3. Data Cleaning & Preparation
- De-identify personal information
- Standardize job titles
- Filter PM roles using regex
- Convert all salaries → annual format
- Engineer features (education level, PMP/Scrum flags)

### 4.4. Modeling & Analysis
- Descriptive statistics
- Correlation & regression
- ANOVA + Tukey tests
- Demand frequency analysis
- Competitiveness Index (supply vs demand)

### 4.5. Visualization & Insights
- Salary distributions
- Education demand charts
- Hiring gap scatterplots
- Business interpretation & recommendations

### 4.6. Reporting
- Export plots & tables
- Generate final report
- Share actionable findings

## 5. Key Findings

### 5.1. Education & Salary
- Each education level adds **~$8.5K salary premium**
- Moderate positive correlation (ρ ≈ 0.44)
- Master's & PhD roles show highest pay bands

### 5.2. Employer Demand
- 45% require **Master’s degree**
- Certifications are rare  
  - PMP: 1.3%  
  - Scrum: 0.3%

### 5.3. Talent Gaps
High-demand shortages:
- Program Coordinator
- Project Director

Oversupply:
- Community Coordinator
- Construction Project Manager

## 6. Running the Project
To run the project in your local environment, follow these steps:
1. Install RStudio
2. Clone the repository to your local machine
3. Run the script

## 7. Skills Demonstrated
- Data wrangling & transformation
- Statistical modeling
- Workforce analytics
- Business storytelling with data
- Reproducible R workflows

## 8. Why This Matters
This analysis helps:
- Agencies plan hiring
- Align education supply with demand
- Identify skill gaps
- Guide workforce strategy
- Turning public data into practical decisions.
