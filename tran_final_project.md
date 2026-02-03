---
title: 'NYC Project Management Workforce Analysis - Education, Compensation & Talent Shortages in Public Sector of Project Management'
author: "Tu Tran Tran"
date: "2025-11-06"
output:
  word_document:
    toc: TRUE                # Table of contents
    toc_depth: 2             # Levels of headings in TOC
    fig_caption: TRUE        # Enable figure captions
    df_print: tibble         # Pretty printing for data frames
    keep_md: TRUE            # Keep intermediate markdown
    number_sections: FALSE    # Number section headings
  html_document: default
---



\newpage

# **ABSTRACT**

This project examines how education level relates to salary and job demand across Project Management (PM) roles in New York City public agencies. Using two datasets (NYC Citywide Payroll for FY 2024–2025 and NYC Job Postings), the analysis evaluates (1) whether higher education is associated with higher pay, (2) which education levels are most requested by employers, and (3) the competitiveness of PM job titles by comparing supply (employees) and demand (postings). After cleaning, standardizing, and de-identifying both datasets, statistical modeling and visualizations were conducted to uncover patterns. The results help identify salary trends, education expectations, and workforce shortages in the NYC PM labor market.

\newpage

# **1. Business understanding**

## **1.1. Context and motivation**

Nowadays, the New York City (NYC) public agencies are having the need to attract and retain strong Project Management (PM) talents. Therefore, the purpose of this research is to understand how much education level matter, which degrees/certifications are demanded for project management, and which project manager roles are structurally short of talent.

## **1.2. Business questions**

The research includes three sub-questions as below:

a)  Does education level correlate with salary in NYC PM roles?

Business purpose of the question: to understand the quantity ROI of education.

b)  Which education level is demanded most in PM job postings?

Business purpose of the question: to align the academic supply with the employer's demand.

c)  Which PM job titles are having the demand larger than the supply?

Business purpose of the question: to identify the priority shortages for hiring and upskilling.

\newpage

# **2. Data understanding**

## **2.1. Data overview**


``` r
## Load the required libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(readr)
library(scales)
library(ggrepel)
library(ggplot2)
library(dplyr)

dir.create("outputs", showWarnings = FALSE)

PM_REGEX <- "\\b(project|program|pmo|scrum|agile|admin|coordinator|manager)\\b"   # define once
```

This regex is used to detect and keep only Project-Management–related job titles in both the payroll and job-posting datasets as below:

-   "project" → Project Manager, Project Coordinator
-   "program" → Program Manager
-   "pmo" → PMO roles
-   "scrum" / "agile" → Agile/Scrum positions
-   "admin" / "coordinator" → Project Admin, Project Coordinator
-   "manager" → managerial PM titles

The "\\\\b" boundaries make sure the match is a full word, preventing false matches such as “programmer”. Therefore, creating PM_REGEX is necessary because both datasets include thousands of non-PM jobs (nurses, engineers, mechanics, police, cooks, etc.). As a result, the regex acts as a filter to isolate PM-related roles so that all later analysis stays focused, relevant, and accurate.

## **2.2. Fiscal Year (2024 + 2025)**

This data can be used to analyze how the City's financial resources are allocated and how much of the City's budget is being devoted to overtime. It was provided by the Financial Information Services Agency (FISA) & Office of Payroll Administration (OPA).

Regarding the year selection, 2023 was not selected because the labor marker was recovering from COVID-19, which can create noise into the dataset. Therefore, combining 2024 and 2025 fiscal years will produce a more stable labor market signal and avoid over-interpreting short-term volatility in postings volume or certification language.

Below is the ethical and analytical rationale for the Fiscal Year dataset cleaning phase.

| **Variable Group** | **Decision** | **Reason for Removal / Retention** |
|:-----------------|:-----------------|:----------------------------------|
| **first_name, last_name, mid_init, employee_id** | Dropped | Direct identifiers that can reveal the identity of specific employees. Required to be removed under privacy and research ethics standards. |
| **agency_start_date, leave_status_as_of_june_30** | Dropped | Quasi-identifiers that, when combined with agency + title + salary, can indirectly re-identify individuals, especially in rare roles. |
| **regular_hours, ot_hours** | Dropped | Personal work pattern data (behavioral information). Can lead to re-identification or confound salary analysis with overtime/tenure effects. |
| **regular_gross_paid, total_ot_paid, total_other_pay** | Dropped | Detailed, volatile compensation components (overtime, bonuses). Not relevant to the “education–salary” research question and risk revealing individual income patterns. |
| **base_salary** | Retained | Represents structural, standardized compensation across employees. Serves as the core dependent variable for comparing salary differences by education level. |
| **agency_name, title_description, pay_basis, fiscal_year** | Retained | Job-related attributes needed for grouping, modeling, annualizing salary, and aligning job titles with job postings. Non-identifiable when names/IDs are removed. |
| **posting_date, job_title fields, job descriptions (from Postings dataset)** | Retained | Required for mapping education levels, filtering PM-related roles, and aligning postings with fiscal year ranges. |
| **education_level_req, req_pmp, req_scrum** | Created/Derived | Needed for analytical objectives: modeling education–salary correlation and studying employer demand. Fully non-identifying. |




``` r
## Import data, set variable names to lowercase and replace space with _
nyc_fy_24 <- read.csv("Citywide_Payroll_Data_(Fiscal_Year_2024).csv") %>%
  clean_names()

nyc_fy_25 <- read.csv("Citywide_Payroll_Data_(Fiscal_Year_2025).csv") %>%
  clean_names()

## Binding fiscal years data to one dataset
bind_fy <- bind_rows(nyc_fy_24, nyc_fy_25)

## De-identification: drop variables that contain personal information or play as indentifiers
personal_cols <- c(
  "first_name","last_name","mid_init","agency_start_date",
  "leave_status_as_of_june_30","regular_hours","ot_hours",
  "regular_gross_paid","total_ot_paid","total_other_pay", "work_location_borough","payroll_number")

## Select variables that are required for the analyis
retained_cols <- c("fiscal_year","agency_name","title_description","pay_basis","base_salary")

fy_clean <- bind_fy %>%
  select(-any_of(personal_cols)) %>%
  select(any_of(retained_cols))
```

## **2.3. NYC Job Postings**

This dataset contains current job postings available on the City of New York’s official jobs site (<http://www.nyc.gov/html/careers/html/search/search.shtml>). Internal postings available to city employees and external postings available to the general public are included. This dataset can be used to view current and historical job postings available with the City of New York and to analyze City hiring trends. The time range for this dataset will be from July 1, 2023 to June 30, 2025 - the start of Fiscal Year 2024 to the end of Fiscal year 2025 to avoid misalignment and leakage bias.


``` r
## Import data, set variable names to lowercase and replace space with _

## Select variables that are required for the analyis
nyc_job_posts <- read.csv("Jobs_NYC_Postings_20251001.csv") %>%
  clean_names() %>%
  select(any_of(c(
    "job_id","agency","posting_date","posting_updated",
    "business_title","civil_service_title",
    "title_classification","title_code_no",
    "salary_range_from","salary_range_to","salary_frequency",
    "job_description","minimum_qual_requirements","preferred_skills",
    "career_level","job_category","full_timepart_time_indicator","work_location",
    "full_time_part_time_indicator"
  )))
```

\newpage

# **3. Data preparation**

## **3.1. Fiscal Year (2024 + 2025)**

The fiscal year payroll data was cleaned and standardized to ensure consistency across the 2024 and 2025 datasets. Job titles were normalized into title case, salary fields were parsed into numeric format, and pay basis values were cleaned before converting all compensation into annual salary. Hourly, daily, and annual pay schedules were standardized using appropriate conversion factors, and records with missing or non-positive salary were removed. To focus the analysis on project-management-related roles, titles were filtered using a PM-specific regex pattern. After these transformations, the combined dataset was reduced from 1,113,290 raw observations to 31,330 relevant PM-related records.


``` r
fy_clean <- fy_clean %>%
  # 1) Standardize title
  mutate(
    title_std = title_description %>%
      str_to_title() %>%
      str_squish()
  ) %>%
  # 2) Robust numeric parsing for salary
  mutate(
    # parse_number handles $, commas, factors, etc.
    base_salary_num = parse_number(as.character(base_salary)),
    # normalize pay basis BEFORE case_when
    pay_basis_norm = as.character(pay_basis) %>%
      str_to_lower() %>%
      str_replace_all("[^a-z ]", " ") %>%
      str_squish()
  ) %>%
  # 3) Annualize
  mutate(
    annual_salary = case_when(
      str_detect(pay_basis_norm, "per annum|annual|annum|year|yearly|per year") ~ base_salary_num,
      str_detect(pay_basis_norm, "per day|daily|day")                            ~ base_salary_num * 260,
      str_detect(pay_basis_norm, "per hour|hourly|hour|hr")                      ~ base_salary_num * 2080,
      TRUE                                                                       ~ base_salary_num
    )
  ) %>%
  filter(!is.na(annual_salary), annual_salary > 0) %>%
  filter(str_detect(str_to_lower(title_std), PM_REGEX))
## the observations reduced from 1,113,290 to 31,330
```

## **3.2. NYC Job Postings**

The NYC job postings data was processed to align with the fiscal year timeline and analytical requirements. Posting dates were converted to proper date format and filtered to the July 2023 - June 2025 window, reducing the dataset from 5,804 to 3,330 postings. Job titles were standardized, and a unified text field was created by combining title, job descriptions, qualification requirements, and preferred skills. This allowed accurate extraction of education requirements as well as PMP and Scrum certification indicators using keyword detection. Project-management-related postings were retained using the same regex pattern applied to the payroll data, resulting in 784 PM-focused job postings. Salary ranges were then converted into annualized values and midpoint salary estimates to ensure comparability across roles with different pay formats.


``` r
## Filter posting date from July 01,2023 to June 30, 2025 to match with the Fiscal years of 2024 and 2025
   
    ##  identify variable type
str(nyc_job_posts$posting_date) 
```

```
##  chr [1:5804] "06/24/2025" "12/26/2024" "04/11/2025" "09/26/2025" ...
```

``` r
    ## assigning posting_date variable to date value
clean_job_posts <- nyc_job_posts %>%
  mutate(posting_date = mdy(posting_date)) 

clean_job_posts <- clean_job_posts %>%
  filter(posting_date >= as.Date("2023-07-01") & posting_date <= as.Date("2025-06-30"))
## the observations reduced from 5804 to 3330

## Filter rows that contain related to education and position
clean_job_posts <- clean_job_posts %>%
  mutate(
    title_std = if_else(!is.na(business_title) & business_title != "",
                        business_title, civil_service_title) %>%
                str_to_title() %>%
                str_squish(),
    text_all  = paste(
      coalesce(business_title, ""),
      coalesce(civil_service_title, ""),
      coalesce(job_description, ""),
      coalesce(minimum_qual_requirements, ""),
      coalesce(preferred_skills, "")
    ) %>%
    tolower() %>%
    str_squish()
  ) %>%
  filter(str_detect(tolower(title_std), PM_REGEX))
##reduced to 784 results

## Education mapping + PMP/Scrum flags
clean_job_posts <- clean_job_posts %>%
  mutate(
    education_level_req = case_when(
      str_detect(text_all, "ph\\.?d|doctorate|doctoral") ~ "PhD",
      str_detect(text_all, "master")                     ~ "Master's",
      str_detect(text_all, "bachelor")                   ~ "Bachelor's",
      str_detect(text_all, "associate")                  ~ "Associate",
      TRUE                                               ~ "Other"
    ),
    req_pmp   = str_detect(text_all, "\\bpmp\\b"),
    req_scrum = str_detect(text_all, "\\bscrum\\b")
  )



## Convert salary to annual by creating 2 new columns: annual_salary_from, annual_salary_to (make comparable)
clean_job_posts <- clean_job_posts %>%
  mutate(
    salary_frequency  = tolower(salary_frequency),
    salary_range_from = suppressWarnings(as.numeric(salary_range_from)),
    salary_range_to   = suppressWarnings(as.numeric(salary_range_to)),
    annual_salary_from = case_when(
      salary_frequency == "hourly" ~ salary_range_from * 2080,
      salary_frequency == "daily"  ~ salary_range_from * 260,
      TRUE ~ salary_range_from
    ),
    annual_salary_to = case_when(
      salary_frequency == "hourly" ~ salary_range_to * 2080,
      salary_frequency == "daily"  ~ salary_range_to * 260,
      TRUE ~ salary_range_to
    ),
    annual_salary_mid = (annual_salary_from + annual_salary_to) / 2
  ) %>%
  filter(is.na(annual_salary_mid) | annual_salary_mid > 0)
```

\newpage

# **4. Modeling**

## **4.1. Education and Salary**

### **4.1.1. Prepare Education-Salary Dataset**


``` r
# Create numeric education levels for correlation/regression
edu_salary_data <- clean_job_posts %>%
  filter(!is.na(annual_salary_mid), annual_salary_mid > 0) %>%
  filter(education_level_req != "Other") %>%
  mutate(
    edu_num = case_when(
      education_level_req == "Associate" ~ 1,
      education_level_req == "Bachelor's" ~ 2,
      education_level_req == "Master's" ~ 3,
      education_level_req == "PhD" ~ 4,
      TRUE ~ NA_real_
    ),
    education_level_req = factor(education_level_req,
                                 levels = c("Associate", "Bachelor's", "Master's", "PhD"))
  ) %>%
  filter(!is.na(edu_num))


# Display sample
head(edu_salary_data %>% select(title_std, education_level_req, edu_num, annual_salary_mid))
```

```
## # A tibble: 6 × 4
##   title_std                        education_level_req edu_num annual_salary_mid
##   <chr>                            <fct>                 <dbl>             <dbl>
## 1 Performance Evaluation Coordina… Associate                 1            83033 
## 2 Travel Coordinator               Associate                 1            60889 
## 3 Office Manager                   Associate                 1            47008.
## 4 Assistant Project Manager        Associate                 1            63970.
## 5 Assistant Project Manager        Associate                 1            63970.
## 6 Case Manager, Bureau Of Tubercu… Associate                 1            57054.
```

This table shows a small sample of the dataset used to study the link between education and salary. Each row represents a PM-related job posting in NYC, with the standardized job title, the required education level, a numeric code for that level, and the annual salary midpoint. At the Associate level, salary midpoints already range around 57–83 thousand dollars. This gives us a concrete sense of the raw data before we run any statistical models.

### **4.1.2. Descriptive Statistics by Education Level**


``` r
# Summary statistics by education level
edu_salary_data <- clean_job_posts %>%
  filter(!is.na(annual_salary_mid), annual_salary_mid > 0) %>%
  filter(education_level_req != "Other") %>%
  mutate(
    edu_num = case_when(
      education_level_req == "Associate"  ~ 1,
      education_level_req == "Bachelor's" ~ 2,
      education_level_req == "Master's"   ~ 3,
      education_level_req == "PhD"        ~ 4
    ),
    education_level_req = factor(education_level_req,
                                 levels = c("Associate", "Bachelor's", "Master's", "PhD"))
  )
```

The descriptive averages (mean/median) increase monotonically from Associate → Bachelor's → Master's → PhD. This indicates that NYC PM job postings with higher educational requirements tend to offer higher salaries.

### **4.1.3. Spearman Correlation**


``` r
# Spearman correlation between education level (numeric) and salary
spearman_result <- cor.test(
  edu_salary_data$edu_num, 
  edu_salary_data$annual_salary_mid, 
  method = "spearman"
)

spearman_result
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  edu_salary_data$edu_num and edu_salary_data$annual_salary_mid
## S = 24868045, p-value < 2.2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.4439488
```

The Spearman correlation analysis shows a moderate positive monotonic relationship between education level and annual salary in NYC project management job postings. The correlation coefficient is rho = 0.44, indicating that higher required education levels tend to be associated with higher offered salaries. The p-value is below 2.2e–16, providing strong statistical evidence that this relationship is not due to random chance. Because Spearman correlation is rank based, it also confirms that the upward salary trend holds even without assuming linearity or normality, which makes it a robust complement to the linear regression findings.

### **4.1.4. Linear Regression**


``` r
# Linear regression: salary ~ education level (numeric)
lm_model <- lm(annual_salary_mid ~ edu_num, data = edu_salary_data)

# Model summary
summary(lm_model)
```

```
## 
## Call:
## lm(formula = annual_salary_mid ~ edu_num, data = edu_salary_data)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -44648 -11487  -4117  13355  73956 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  66177.7     1992.2   33.22   <2e-16 ***
## edu_num       8542.7      759.6   11.25   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19500 on 643 degrees of freedom
## Multiple R-squared:  0.1644,	Adjusted R-squared:  0.1631 
## F-statistic: 126.5 on 1 and 643 DF,  p-value: < 2.2e-16
```

``` r
# Extract key statistics (nếu cần dùng trong text/thesis, không in ra bằng cat)
r_squared      <- summary(lm_model)$r.squared
adj_r_squared  <- summary(lm_model)$adj.r.squared
f_statistic    <- summary(lm_model)$fstatistic
f_pvalue       <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
```

The linear regression model shows a statistically significant positive relationship between required education level and advertised salary in NYC project management job postings. The intercept of about \$66,178 represents the expected salary for roles requiring an Associate degree. The coefficient for education level (edu_num = 8543, p \< 2e–16) indicates that each step up in education level Associate to Bachelor’s, Bachelor’s to Master’s, and Master’s to PhD is associated with an average salary increase of roughly \$8,500. Although the effect is statistically strong, the R² value of 0.164 suggests that education explains only about 16 percent of the variation in salaries. This means education is an important predictor, but many other factors such as job seniority, agency type, specialization, and management responsibilities also influence salary levels. The highly significant F-test confirms that the overall model is meaningful, but it should be interpreted as capturing only one dimension of the broader salary structure in the NYC public-sector project management market.

### **4.1.5. ANOVA and Tukey HSD**


``` r
# ANOVA: test if mean salaries differ across education levels
aov_model <- aov(annual_salary_mid ~ education_level_req, data = edu_salary_data)

# ANOVA summary
summary(aov_model)
```

```
##                      Df    Sum Sq   Mean Sq F value Pr(>F)    
## education_level_req   3 4.834e+10 1.611e+10   42.28 <2e-16 ***
## Residuals           641 2.443e+11 3.811e+08                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
# Tukey HSD post-hoc test (pairwise comparisons)
tukey_result <- TukeyHSD(aov_model)
tukey_result
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = annual_salary_mid ~ education_level_req, data = edu_salary_data)
## 
## $education_level_req
##                           diff        lwr      upr     p adj
## Bachelor's-Associate  8285.187  -442.9997 17013.37 0.0698798
## Master's-Associate   17709.461 13225.0579 22193.86 0.0000000
## PhD-Associate        24131.418 16507.7682 31755.07 0.0000000
## Master's-Bachelor's   9424.274  1036.3616 17812.19 0.0204786
## PhD-Bachelor's       15846.231  5436.2725 26256.19 0.0005649
## PhD-Master's          6421.957  -809.6309 13653.55 0.1020119
```

``` r
anova_pvalue <- summary(aov_model)[[1]][["Pr(>F)"]][1]
```

The Tukey HSD post hoc test evaluates which education levels differ significantly in salary after the ANOVA identified overall differences. The results show that higher degrees are consistently associated with higher salaries, but not all pairwise differences are statistically significant at the 95 percent confidence level. The largest significant gaps occur between Master’s vs Associate (difference ≈ 17,709 USD, p \< 0.001), PhD vs Associate (≈ 24,131 USD, p \< 0.001), and PhD vs Bachelor’s (≈ 15,846 USD, p \< 0.001). These comparisons confirm strong salary premiums for advanced degrees. Meanwhile, two comparisons are not statistically significant: Bachelor’s vs Associate (p ≈ 0.07) and PhD vs Master’s (p ≈ 0.10). This means salaries for Bachelor’s vs Associate roles overlap substantially, and the salary jump from Master’s to PhD is not consistently large enough to be statistically detected in NYC PM job postings. Overall, the Tukey results support the conclusion that the most meaningful salary increases occur when moving from Associate to Master’s and from lower degrees to PhD.

### **4.1.6. Visualization: Violin + Boxplot**


``` r
# Violin plot + boxplot overlay
ggplot(edu_salary_data, aes(x = education_level_req, y = annual_salary_mid, fill = education_level_req)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Salary Distribution by Education Level (NYC PM Job Postings)",
    subtitle = "Violin plot with boxplot overlay | Red diamond = mean salary",
    x = "Education Level Required",
    y = "Annual Salary (Midpoint)",
    fill = "Education Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 11),
    legend.position = "none"
  )
```

![](tran_final_project_files/figure-docx/unnamed-chunk-11-1.png)<!-- -->

This violin and boxplot visualization shows the full salary distribution for each degree level. The shapes shift upward from Associate to PhD, with both the boxes and the red diamonds (means) moving higher as education increases. You can also see that Master’s and PhD roles have wider spread and higher upper tails, reflecting more senior or specialized PM positions. Overall, the plot visually reinforces our conclusion: higher education requirements are associated with higher and more dispersed salary offers.

## **4.2. Education Demand**

### **4.2.1. Count and Proportion of Postings by Education Level**


``` r
# Education demand analysis
edu_demand <- clean_job_posts %>%
  group_by(education_level_req) %>%
  summarise(
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    proportion = count / sum(count),
    percentage = round(proportion * 100, 1)
  ) %>%
  arrange(desc(count))

print(edu_demand)
```

```
## # A tibble: 5 × 4
##   education_level_req count proportion percentage
##   <chr>               <int>      <dbl>      <dbl>
## 1 Master's              354     0.452        45.2
## 2 Associate             195     0.249        24.9
## 3 Other                 139     0.177        17.7
## 4 PhD                    56     0.0714        7.1
## 5 Bachelor's             40     0.0510        5.1
```

When we focus on demand, the table shows how often each education level appears in PM-related job postings. About 45% of postings ask for a Master’s degree, and roughly 25% ask for an Associate degree. Around 18% fall into ‘Other’, which often specify experience rather than a formal degree. PhD and Bachelor’s requirements are relatively rare at about 7% and 5%, respectively. This pattern suggests that NYC public PM roles are heavily skewed toward graduate-level expectations.

### **4.2.2. PMP and Scrum Certification Demand**


``` r
# Calculate PMP and Scrum demand ratios
total_pm_postings <- nrow(clean_job_posts)
pmp_postings      <- sum(clean_job_posts$req_pmp,   na.rm = TRUE)
scrum_postings    <- sum(clean_job_posts$req_scrum, na.rm = TRUE)

# Ratios
pmp_ratio   <- pmp_postings   / total_pm_postings
scrum_ratio <- scrum_postings / total_pm_postings

# Summary table
cert_demand <- data.frame(
  Credential  = c("PMP", "Scrum"),
  Numerator   = c(pmp_postings, scrum_postings),
  Denominator = c(total_pm_postings, total_pm_postings),
  Ratio       = c(pmp_ratio, scrum_ratio),
  Percentage  = c(round(pmp_ratio * 100, 2), round(scrum_ratio * 100, 2))
)

cert_demand
```

```
## # A tibble: 2 × 5
##   Credential Numerator Denominator   Ratio Percentage
##   <chr>          <int>       <int>   <dbl>      <dbl>
## 1 PMP               10         784 0.0128        1.28
## 2 Scrum              2         784 0.00255       0.26
```

Looking at certifications, the demand is surprisingly low. Out of 784 PM postings, only 10 explicitly mention PMP and just 2 mention Scrum. That translates to roughly 1.3% of postings for PMP and 0.3% for Scrum. This tells us that, in the NYC public sector, project management roles are not certification-driven; agencies care much more about degrees and experience than about formal PM credentials.

### **4.2.3. Visualization: Education Demand Bar Chart**


``` r
# Bar chart for education demand
ggplot(edu_demand, aes(x = reorder(education_level_req, -count), y = count, fill = education_level_req)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(count, "\n(", percentage, "%)")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Education Demand in NYC PM Job Postings",
    subtitle = paste0("Total PM Postings: ", total_pm_postings),
    x = "Education Level Required",
    y = "Number of Job Postings",
    fill = "Education Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 11),
    legend.position = "none"
  )
```

![](tran_final_project_files/figure-docx/unnamed-chunk-14-1.png)<!-- -->

This bar chart visualizes the same education-demand numbers in a way that is easy to compare. The tall green bar on the left shows that Master’s degrees dominate PM job postings in NYC, followed by Associate degrees. ‘Other’ requirements finish third, while PhD and especially Bachelor’s requirements are much lower. The key takeaway is that if you want to compete for public-sector PM roles in NYC, graduate education is by far the most requested signal.

### **4.2.4. Visualization: Certification Demand**


``` r
# Bar chart for certification demand
ggplot(cert_demand, aes(x = Credential, y = Percentage, fill = Credential)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(Percentage, "%\n(", Numerator, "/", Denominator, ")")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("PMP" = "#66C2A5", "Scrum" = "#FC8D62")) +
  labs(
    title = "Certification Demand in NYC PM Job Postings",
    subtitle = "Proportion of postings requiring PMP or Scrum certifications",
    x = "Certification Type",
    y = "Percentage of Postings (%)",
    fill = "Certification"
  ) +
  ylim(0, max(cert_demand$Percentage) * 1.2) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )
```

![](tran_final_project_files/figure-docx/unnamed-chunk-15-1.png)<!-- -->

This chart highlights how rare certification requirements are in NYC public-sector PM roles. Only 1.28% of postings explicitly require a PMP, and Scrum is even lower at just 0.26%. This suggests that NYC agencies primarily hire based on education and experience, not certifications. Therefore, advanced degrees matter more than industry credentials in this sector.

## **4.3. Job Market Competitiveness**

### **4.3.1. Aggregate Employee and Posting Counts by Title**


``` r
# Count employees per standardized title (from payroll data)
employee_counts <- fy_clean %>%
  group_by(title_std) %>%
  summarise(
    employee_count = n(),
    .groups = "drop"
  )

# Count postings per standardized title (from job postings)
posting_counts <- clean_job_posts %>%
  group_by(title_std) %>%
  summarise(
    posting_count = n(),
    .groups = "drop"
  )

# Join datasets and compute Competitiveness Index
competitiveness_data <- full_join(employee_counts, posting_counts, by = "title_std") %>%
  mutate(
    employee_count = replace_na(employee_count, 0),
    posting_count  = replace_na(posting_count, 0)
  ) %>%
  filter(employee_count > 0 | posting_count > 0) %>%
  mutate(
    competitiveness_index = ifelse(employee_count > 0,
                                   posting_count / employee_count,
                                   Inf)
  ) %>%
  arrange(desc(competitiveness_index))

# Top 20
head(competitiveness_data, 20)
```

```
## # A tibble: 20 × 4
##    title_std                  employee_count posting_count competitiveness_index
##    <chr>                               <int>         <int>                 <dbl>
##  1 Accountable Manager                     0           120                   Inf
##  2 Accounting Coordinator                  0             2                   Inf
##  3 Action Center Coordinator…              0             2                   Inf
##  4 Admin. Engineer M-Ii - Ch…              0             2                   Inf
##  5 Admin. Engineer M-Ii - De…              0             2                   Inf
##  6 Administrative Coordinato…              0             2                   Inf
##  7 Architectural Project Man…              0             2                   Inf
##  8 Assistant Commissioner, R…              0             2                   Inf
##  9 Assistant Deputy Commissi…              0             2                   Inf
## 10 Assistant Environmental E…              0             2                   Inf
## 11 Assistant Project Engineer              0             4                   Inf
## 12 Assistant Project Manager               0           128                   Inf
## 13 Assoc. Project Manager                  0             4                   Inf
## 14 Associate Deputy Commissi…              0             2                   Inf
## 15 Asthma Case Manager, Offi…              0             2                   Inf
## 16 Billing Coordinator                     0             2                   Inf
## 17 Bob- Administrative Const…              0             4                   Inf
## 18 Bob- Administrative Const…              0             2                   Inf
## 19 Bob- Associate Project Ma…              0             2                   Inf
## 20 Bob- Associate Project Ma…              0             2                   Inf
```

This table shows titles where NYC currently has job postings but zero employees recorded in payroll, resulting in an infinite competitiveness index. It suggests new or emerging roles, un-staffed functions, or misaligned job title structures that are not yet represented in the workforce. These are high-priority shortages.

### **4.3.2. Visualization: Scatter Plot (Employees vs Postings)**


``` r
# Scatter plot: employees vs postings (log scale)
# Filter to finite CI + > 0
plot_data <- competitiveness_data %>%
  filter(is.finite(competitiveness_index),
         employee_count > 0,
         posting_count > 0)

# Top 10 for labeling
top_labels <- plot_data %>% slice_max(competitiveness_index, n = 10)

ggplot(plot_data, aes(x = employee_count, y = posting_count)) +
  
  # Points
  geom_point(aes(size = competitiveness_index,
                 color = competitiveness_index),
             alpha = 0.7) +

  # Correct 1:1 line for log-log:
  geom_line(
    data = tibble(x = c(min(plot_data$employee_count),
                        max(plot_data$employee_count))) %>%
      mutate(y = x),
    aes(x = x, y = y),
    color = "red", linewidth = 1, linetype = "dashed"
  ) +

  # Labels with repel
  geom_text_repel(
    data = top_labels,
    aes(label = title_std),
    size = 4,
    max.overlaps = 20,
    force = 3,
    box.padding = 1,
    point.padding = 0.5,
    segment.color = "grey50"
  ) +

  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  scale_color_gradient(low = "blue", high = "red",
                       name = "Competitiveness\nIndex") +
  scale_size_continuous(range = c(2, 10),
                        name = "Competitiveness\nIndex") +

  labs(
    title = "Job Market Competitiveness: Employees vs Job Postings",
    subtitle = "Log–log scale | Red dashed line = equilibrium (1:1 ratio)",
    x = "Number of Employees (Payroll Data, log10)",
    y = "Number of Job Postings (Jobs NYC, log10)",
    caption = "Size & color = Competitiveness Index (Postings / Employees)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )
```

![](tran_final_project_files/figure-docx/unnamed-chunk-17-1.png)<!-- -->

This scatterplot compares the current NYC government workforce (payroll employees) with job demand (NYC job postings) across project-management related titles using a log–log scale. The red dashed line represents a 1:1 equilibrium between supply and demand, making it easy to identify shortages or oversupply. Roles plotted above the line have more job postings than existing staff and therefore indicate hiring shortages. The clearest example is Program Coordinator, which shows extremely high demand relative to a very small workforce, resulting in the highest Competitiveness Index (CI = 5). Project Director also shows notable demand pressure. In contrast, positions such as Community Coordinator, Construction Project Manager, and IT Project Specialist appear far below the equilibrium line, reflecting large existing workforces and very limited demand, an indication of oversupply. Bubble size and color visually reinforce this pattern, with larger and redder points representing harder to fill positions. Overall, the chart highlights significant variation in hiring competitiveness across PM related roles, showing that some titles face strong talent shortages while others have an excess labor supply.

### **4.3.3. Visualization: Top 20 Competitiveness Index**


``` r
top20_ci_table <- competitiveness_data %>%
  filter(is.finite(competitiveness_index)) %>%
  slice_max(competitiveness_index, n = 20) %>%
  arrange(desc(competitiveness_index)) %>%
  select(
    Job_Title = title_std,
    Employees = employee_count,
    Postings  = posting_count,
    Competitiveness_Index = competitiveness_index
  )

top20_ci_table
```

```
## # A tibble: 124 × 4
##    Job_Title                    Employees Postings Competitiveness_Index
##    <chr>                            <int>    <int>                 <dbl>
##  1 Program Coordinator                  2       10              5       
##  2 Project Director                     4        8              2       
##  3 Project Coordinator                 41        6              0.146   
##  4 Project Manager                    203       28              0.138   
##  5 Office Manager                      19        2              0.105   
##  6 Legal Coordinator                   70        4              0.0571  
##  7 It Project Specialist              626        4              0.00639 
##  8 Construction Project Manager      1136        2              0.00176 
##  9 Community Coordinator            12968        8              0.000617
## 10 *Assist Coordinating Manager         7        0              0       
## # ℹ 114 more rows
```

This table ranks titles by competitiveness index. A higher index means more job postings relative to employees. Program Coordinator again ranks first with a cpmpetitive index of 5. Several roles show moderate but meaningful shortages, including Project Director, Project Coordinator, and Project Manager.

\newpage

# **5. CONCLUSION**

This study examined how education levels, certifications, and workforce dynamics shape the labor market for project management roles within New York City public-sector agencies. By integrating payroll records from Fiscal Years 2024–2025 with job posting data from July 2023 to June 2025, the analysis provides a clear view of what qualifications are rewarded, what credentials are demanded, and which PM job titles face the most significant talent shortages.

First, the education–salary analysis shows a consistent and statistically significant relationship. Salaries increase as required education rises, with each degree step associated with roughly an \$8,500 increase in offered pay. While education alone explains about 16 percent of salary variation, the results indicate that it functions as a meaningful but not exclusive driver of compensation. Advanced degrees, particularly Master’s and PhD qualifications, carry substantial salary premiums, as confirmed by ANOVA and Tukey post hoc comparisons.

Second, the demand analysis highlights that NYC public-sector PM roles are strongly oriented toward graduate education. Master’s degrees appear in nearly half of all postings, far surpassing all other categories. Meanwhile, industry certifications such as PMP and Scrum are rarely required, appearing in only about one to two percent of postings. This suggests that for NYC agencies, formal education and demonstrated experience matter far more than credential-based qualifications.

Finally, the job market competitiveness analysis reveals clear structural imbalances in the PM workforce. Several roles such as Program Coordinator and Project Director show high demand relative to existing staffing levels, indicating substantial talent shortages. Other roles, including Community Coordinator and Construction Project Manager, display an abundant supply of current employees compared with hiring needs. These differences demonstrate that PM labor demand in NYC is not uniform and that workforce planning should be targeted rather than generalized.

Overall, the findings show that NYC public-sector agencies reward higher education with higher pay, prefer graduate degrees over professional certifications, and face significant hiring gaps in specific PM roles. These insights can guide strategic decisions around education planning, recruiting efforts, and upskilling initiatives, helping the city attract and retain a strong and well-aligned project management workforce.
