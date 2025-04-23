# NFL Quarterback Value Analysis: Performance, Salary, and Team Success (2019–2024)

This project analyzes the value of NFL quarterbacks by integrating performance statistics, salary data, and team success metrics. It uses detailed play-by-play data from `nflfastR` to evaluate whether quarterback salaries align with metrics like Yards After Catch (YAC), air yards, Expected Points Added (EPA), and win percentage.

---

## 🔍 Research Objectives

- How much of a QB's passing yardage is due to YAC vs air yards?
- Are high-YAC quarterbacks overpaid relative to their independent performance?
- Do certain passing styles correlate with higher team win percentages?
- Which metrics best predict quarterback salary and team success?

---

## 📊 Methodology

- **Data Source**: `nflfastR`, `nflreadr`, Spotrac salary data
- **Years Covered**: 2019–2024
- **Analysis Tools**: `tidyverse`, `ggplot2`, `broom`, `corrplot`, `knitr`, `ggrepel`
- **Key Metrics**: 
  - Completion %, YAC %, Air EPA, YAC EPA
  - EPA per Attempt, Win %, Salary per EPA
  - Deep Pass %, 3rd Down Conversion Rate

---

## 🛠️ Features

- 🧮 Full pipeline for QB data cleaning and processing
- 🧩 Merges salary data and team win/loss records
- 📈 Visualizes trends and QB style correlations with team success
- 📉 Regression models for salary and win percentage prediction
- 📊 Summary tables for top value QBs and passing styles

---



