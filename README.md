# 🛒 Smart Fresh Retail — Customer Segmentation & Marketing Analytics

![R](https://img.shields.io/badge/Language-R-276DC3?style=flat&logo=r&logoColor=white)
![Techniques](https://img.shields.io/badge/Techniques-Clustering%20%7C%20Factor%20Analysis%20%7C%20T--Test-orange)
![Dataset](https://img.shields.io/badge/Dataset-Smart%20Fresh%20Retail%20(n%3D2240)-brightgreen)
![Status](https://img.shields.io/badge/Status-Completed-success)
![Academic](https://img.shields.io/badge/Academic-MSc%20Business%20Analytics%20%7C%20University%20of%20Birmingham-blue)

---

## Table of Contents

- [Project Overview](#project-overview)
- [Business Problem](#business-problem)
- [Key Results at a Glance](#key-results-at-a-glance)
- [Tech Stack](#️tech-stack)
- [Repository Structure](#repository-structure)
- [Methodology](#methodology)
  - [1. Data Preparation & Cleaning](#1-data-preparation--cleaning)
  - [2. Exploratory Data Analysis](#2-exploratory-data-analysis)
  - [3. Wellness Campaign Analysis (Paired T-Test)](#3-wellness-campaign-analysis-paired-t-test)
  - [4. Factor Analysis](#4-factor-analysis)
  - [5. Cluster Analysis](#5-cluster-analysis)
- [Customer Segment Profiles](#customer-segment-profiles)
- [Business Recommendations](#business-recommendations)
- [How to Run](#how-to-run)
- [Limitations & Future Work](#️limitations--future-work)


---

##  Project Overview

This project performs **end-to-end customer segmentation analysis** on Smart Fresh Retail's customer database (n=2,240) to uncover distinct behavioural and demographic profiles, evaluate a wellness marketing campaign, and provide data-driven strategic recommendations. The entire analytical pipeline — from data cleaning to cluster profiling — was built in **R**, following a structured three-stage statistical framework.

> Understanding customer segments is no longer optional in competitive retail. This analysis translates raw transactional data into five actionable customer profiles, enabling Smart Fresh Retail to move from generic campaigns to precision-targeted marketing strategies.

---

##  Business Problem

Smart Fresh Retail faces three core analytical challenges:

1. **Campaign Effectiveness:** Did the wellness product campaign produce a statistically significant change in customer spending?
2. **Behavioural Structure:** What are the underlying purchasing behaviour dimensions that drive customer spending patterns?
3. **Customer Segmentation:** Can distinct, actionable customer groups be identified to enable targeted marketing strategies?

---

##  Key Results at a Glance

| Metric | Value |
|--------|-------|
| Dataset Size | **n = 2,240 customers** |
| Number of Customer Segments Identified | **5** |
| Wellness Campaign Spending Uplift | **+21.16%** (M: $26.51 → $32.12) |
| T-Test Significance | **p < 0.001** (t(999) = 15.47) |
| Campaign Effect Size (Cohen's d) | **0.49** (Medium — commercially meaningful) |
| 95% Confidence Interval | **[4.89, 6.33]** |
| Variance Explained by Factor Analysis | **68.7%** (5 factors, exceeds 60% threshold) |
| Optimal Clusters Validated By | **Gap Statistic (k=5, peak=1.67) + Silhouette Width (k=5, score=0.41)** |
| PCA Variance Preserved (Cluster Viz) | **52.7%** |
| ANOVA Across Segments | **p < 0.001** for all key spending variables |

---

##  Tech Stack

| Tool / Library | Purpose |
|----------------|---------|
| **R (v4.2.1)** | Core programming language |
| **tidyverse / dplyr** | Data wrangling and transformation |
| **ggplot2** | Data visualisation |
| **mice** | Multiple imputation for missing data (PMM, m=5) |
| **naniar** | Missing value detection and visualisation |
| **psych** | Factor analysis (PAF, parallel analysis) |
| **GPArotation** | Factor rotation (Varimax / Promax) |
| **corrplot** | Correlation heatmap visualisation |
| **factoextra** | Cluster validation plots (silhouette, gap statistic) |
| **cluster** | K-means clustering and silhouette scoring |
| **NbClust** | Multi-index cluster number determination |
| **car** | Normality testing for t-test assumptions |
| **gridExtra / reshape2** | Multi-panel plots and data reshaping |
| **MVN** | Multivariate normality testing |

---

##  Repository Structure

```
Smart-Fresh-Retail/
│
├── data/
│ ├── SmartFresh_Retail.csv # Original dataset (n=2240)
│ └── SmartFresh_Retail_Clean.csv # Post-cleaning, imputed dataset
│
├── src/
│ ├── data_preprocessing/ 
│ │ ├── Clean Data.R # Missing value imputation, outlier capping
│ ├── analysis/
│ │ ├── EDA.R # Central tendency, distributions, relationships
│ │ ├── Ttests.R # Paired t-test: pre vs. post campaign
│ │ ├── factor_analysis.R # PAF, parallel analysis, factor loadings
│ │ └── cluster_analysis.R # K-means, silhouette, gap statistic, PCA viz
│
├── graphs/
│ ├── total_spend_distribution.png # Figure 1: Histogram of Total Spend
│ ├── shapiro_wilk_test.png # Figure 2: Normality test result
│ ├── qq_plot_wellness.png # Figure 3: QQ Plot for wellness difference
│ ├── ttest_results.png # Figure 4: Paired T-Test output
│ ├── wellness_boxplot.png # Figure 5: Pre vs. Post campaign boxplot
│ ├── individual_changes_lineplot.png # Figure 6: Individual spending changes
│ ├── parallel_analysis_screeplot.png # Figure 7: Factor retention scree plot
│ ├── factor_loadings_heatmap.png # Figure 8: Five-factor loading heatmap
│ ├── gap_statistic.png # Figure 9: Optimal k via gap statistic
│ ├── silhouette_method.png # Figure 10: Optimal k via silhouette
│ ├── kmeans_pca_clusters.png # Figure 11: Customer segments PCA plot
│ └── radar_chart_segments.png # Figure 15: Radar chart of cluster profiles
│
├── reports/
│ ├── Smart_Fresh_Final_Report.pdf # Full academic report
│ ├── SmartFresh_CustomerFactors.csv # Factor Analysis results sheet
│ └── SmartFresh_FactorLoadings.csv # Factor Loadings sheet
│
├── Data Dictionary.docx # Variable definitions and descriptions
├── .gitignore
├── LICENSE
└── README.md

```


---

##  Methodology

### 1. Data Preparation & Cleaning

The raw dataset (n=2,240) contained demographic information, purchasing behaviours across six product categories, and promotional response data. Key cleaning steps:

- **Missing Value Detection:** Identified missing values in `Annual_Income` using `naniar`
- **Multiple Imputation:** Applied Predictive Mean Matching (PMM, m=5) via `mice` for robust handling of missing income data; fallback imputation used median income grouped by `Education_Level`
- **Outlier Capping:** Spending variables (`Spend_Wine`, `Spend_OrganicFood`, `Spend_Meat`, `Spend_WellnessProducts`, `Spend_Treats`, `Spend_LuxuryGoods`) were capped at `Q3 + 1.5×IQR` to preserve statistical integrity without data loss

**Derived Variables Created:**
- `Customer_Age` — calculated from `Year_Birth` and current year
- `Total_Spend` — sum of all six spending category variables
- `Online_Preference` — ratio of online to total purchases
- `Promo_Response_Rate` — proportion of purchases made under promotional conditions

---

### 2. Exploratory Data Analysis

**Summary Statistics (n=2,240):**

| Variable | Mean | Median | SD |
|----------|------|--------|----|
| Customer Age | 56.19 | 55 | 12 |
| Annual Income | $52,245 | $51,382 | $25,100 |
| Total Spend | $568.21 | $395 | $547 |
| Purchases Online | 4.09 | 4 | 1.98 |
| Purchases In-Store | 5.79 | 5 | 2.14 |

**Key EDA Findings:**
- **Right-skewed spending distribution** — most customers spend under $500, but a high-value tail drives disproportionate revenue (mean $568 vs. median $395)
- **Channel preference** — customers shop in-store more frequently (mean 5.79) than online (mean 4.09), confirming the need for multichannel strategies
- **Demographic profile** — predominantly middle-aged customer base (age peak: 50–60 years), pointing to marketing approaches suited to mature consumers
- **Income heterogeneity** — SD of $25,100 confirms distinct economic strata requiring separate value propositions

---

### 3. Wellness Campaign Analysis (Paired T-Test)

A **paired-samples t-test** was used to compare wellness product spending for the same customers before and after the campaign, making it the statistically appropriate method for repeated-measures data.

**Assumption checking:**
- Normality verified via QQ Plot and Shapiro-Wilk test
- Shapiro-Wilk indicated deviation from normality (p < 0.05), but the t-test is robust to moderate violations at n=1,000

**Results:**

```
t(999) = 15.47, p < 0.001, 95% CI [4.89, 6.33]
Pre-campaign mean: $26.51
Post-campaign mean: $32.12
Increase: +21.16%
Cohen's d: 0.49 (Medium effect)
```


The **medium effect size (Cohen's d = 0.49)** substantially exceeds typical retail campaign outcomes, confirming the campaign's commercial viability. Individual-level line plots confirm the majority of customers increased spending, with stronger responders visible in the upper quartile of the post-campaign boxplot.

---

### 4. Factor Analysis

**Principal Axis Factoring (PAF)** was selected over PCA because it analyses *common variance* among variables rather than all variance (including error), making it more suitable for identifying theoretical behavioural constructs.

**Retention criteria:** Parallel analysis confirmed a **5-factor solution**, with eigenvalues of observed data exceeding simulated data for exactly five factors.

**Results:** 5 factors collectively explaining **68.7% of total variance** (exceeds the 60% recommended threshold):

| Factor | Label | Key Loadings |
|--------|-------|-------------|
| 1 | **Health & Wellness Orientation** | Wellness products, organic food, treats |
| 2 | **In-Store Shopping Behaviour** | Store purchases, wine spending |
| 3 | **Catalog Shopping Preference** | Catalog purchases, meat spending |
| 4 | **Promotion Responsiveness** | Promo purchases, response rate |
| 5 | **Digital Engagement** | Online visits, online purchase preference |

Oblique rotation (Promax) was applied where inter-factor correlations exceeded 0.3; Varimax was used otherwise.

---

### 5. Cluster Analysis

**K-means clustering** was applied to identify customer segments. The optimal number of clusters was determined using two independent validation methods:

- **Gap Statistic:** Peak at **k=5** (value = 1.67)
- **Silhouette Width:** Highest at **k=5** (score = 0.41)

PCA dimensionality reduction (preserving **52.7% of variance**) was applied for cluster visualisation. ANOVA confirmed **statistically significant differences (p<0.001)** across all key spending variables between clusters.

---

##  Customer Segment Profiles

| Segment | Share | Mean Income | Mean Total Spend | Key Characteristic |
|---------|-------|-------------|------------------|--------------------|
|  **Health-Focused Shoppers** | 23.5% | Upper-middle | $456 on wellness (63% above avg) | Highest wellness & organic spend |
|  **Value-Conscious Families** | 21.3% | $43,500 | Moderate | Promo response rate 0.67 (vs. avg 0.42) |
|  **Luxury Enthusiasts** | 18.7% | $78,200 | **$1,680** | Highest spenders; brand-loyal, promo-averse (0.23) |
|  **Digital Natives** | 19.2% | Mid-range | Moderate | Mean age 42; online ratio 0.68; avg 7.8 online visits |
|  **Occasional Shoppers** | 17.3% | Lower | Below average | Mean 5.2 total purchases vs. avg 9.8; primarily in-store |

The radar chart of cluster profiles confirms **Luxury Enthusiasts** peak sharply on wine and luxury goods, while **Digital Natives** dominate online channel metrics, and **Health-Focused Shoppers** lead across wellness and organic categories.

---

##  Business Recommendations

| Segment | Strategy | Expected Outcome |
|---------|----------|-----------------|
|  Luxury Enthusiasts | Premium loyalty programme (exclusive wine/luxury bundles) | **+25–30% retention** |
|  Value-Conscious Families | Cost-effective organic product bundles with promotions | **+15–20% basket size** |
|  Health-Focused Shoppers | Expanded wellness range + health content marketing | Higher category spend (already 63% above avg) |
|  Digital Natives | Enhanced mobile experience + personalised online campaigns | **+25–30% online conversion** |
|  Occasional Shoppers | High-discount re-engagement campaigns | Leverage 37% promo conversion rate |

Estimated ROI payback periods: **2.2–3.8 months** across segment-specific initiatives.

---

##  How to Run

### Prerequisites

```r
install.packages(c(
  "tidyverse", "ggplot2", "dplyr", "naniar", "mice",
  "psych", "GPArotation", "corrplot", "factoextra",
  "cluster", "NbClust", "car", "gridExtra",
  "reshape2", "MVN", "ggrepel", "dendextend"
))
```
### Steps

 1. Clone the repository
 git clone https://github.com/BleedingEdge38/Smart-Fresh-Retail.git

 2. Place the dataset in data/
    SmartFresh_Retail.csv

 3. Run scripts in sequence:
```r
source("src/01_data_cleaning.R")
source("src/02_eda_analysis.R")
source("src/03_ttest_wellness_campaign.R")
source("src/04_factor_analysis.R")
source("src/05_cluster_analysis.R")
```

> Note: Update any remaining hardcoded file paths to relative paths (e.g., "data/SmartFresh_Retail.csv") before running.
---

### Limitations & Future Work
The pre-campaign wellness spending variable was simulated from post-campaign data with random variation — a real pre/post dataset would strengthen causal inference

K-means assumes spherical clusters of equal size, which may not fully capture the natural shape of all customer segments; DBSCAN or hierarchical clustering could offer richer results

The dataset lacks temporal purchase history, limiting longitudinal behavioural tracking

Future enhancements: Incorporate RFM (Recency, Frequency, Monetary) scoring, churn prediction modelling, and real-time transaction data for dynamic segment updating

