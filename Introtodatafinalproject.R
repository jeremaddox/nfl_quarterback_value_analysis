# Modified NFL QB Analysis Script with Team Win-Loss Integration
# This script analyzes NFL QB performance with a focus on Yards After Catch (YAC)
# and its relationship with salary, other performance metrics, and team success

# Load required packages
library(nflreadr)    # For loading NFL data
library(nflfastR)    # For play-by-play analysis
library(tidyverse)   # For data manipulation and visualization
library(ggrepel)     # For better text labels in plots
library(viridis)     # For colorblind-friendly palettes
library(broom)       # For tidying model outputs
library(grid)        # For arranging plots
library(corrplot)    # For correlation plots
library(knitr)       # For pretty tables

# Set professional visualization theme
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12, color = "darkgrey"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
)

# 1. LOAD AND PROCESS DATA
years <- 2019:2024
print("Loading NFL play-by-play data (2019-2024)...")

# Function to load and process a year of QB data
process_year_data <- function(year) {
  print(paste("Processing year:", year))
  
  # Load play-by-play data
  pbp <- load_pbp(year) %>%
    filter(season_type == "REG")  # Focus on regular season only
  
  # Calculate QB metrics
  qb_metrics <- pbp %>%
    filter(!is.na(passer), !is.na(passer_id), play_type == "pass") %>%
    group_by(passer, passer_id, posteam) %>%
    summarize(
      # Basic passing metrics
      attempts = sum(pass_attempt, na.rm = TRUE),
      completions = sum(complete_pass, na.rm = TRUE),
      passing_yards = sum(yards_gained * complete_pass, na.rm = TRUE),
      passing_tds = sum(pass_touchdown, na.rm = TRUE),
      interceptions = sum(interception, na.rm = TRUE),
      
      # YAC metrics
      air_yards_total = sum(air_yards * complete_pass, na.rm = TRUE),
      yac_total = sum(yards_after_catch * complete_pass, na.rm = TRUE),
      
      # Advanced metrics
      air_epa_total = sum(air_epa, na.rm = TRUE),
      yac_epa_total = sum(yac_epa, na.rm = TRUE),
      total_epa = sum(qb_epa, na.rm = TRUE),
      mean_cpoe = mean(cpoe, na.rm = TRUE),
      
      # Additional metrics
      games_played = n_distinct(game_id),
      sacks = sum(sack, na.rm = TRUE),
      deep_pass_attempts = sum(air_yards >= 20 & pass_attempt == 1, na.rm = TRUE),
      deep_completions = sum(air_yards >= 20 & complete_pass == 1, na.rm = TRUE),
      third_down_attempts = sum(down == 3 & pass_attempt == 1, na.rm = TRUE),
      third_down_conversions = sum(down == 3 & pass_attempt == 1 & first_down == 1, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    filter(attempts >= 100) %>%  # Only QBs with sufficient volume
    
    # Calculate derived metrics
    mutate(
      completion_pct = (completions / attempts) * 100,
      yards_per_attempt = passing_yards / attempts,
      td_pct = (passing_tds / attempts) * 100,
      int_pct = (interceptions / attempts) * 100,
      sack_pct = (sacks / (attempts + sacks)) * 100,
      
      # YAC metrics
      yac_percentage = (yac_total / passing_yards) * 100,
      air_yards_percentage = (air_yards_total / passing_yards) * 100,
      yac_per_completion = yac_total / completions,
      air_yards_per_attempt = air_yards_total / attempts,
      
      # Efficiency metrics
      epa_per_attempt = total_epa / attempts,
      air_epa_per_attempt = air_epa_total / attempts,
      yac_epa_per_attempt = yac_epa_total / attempts,
      deep_pass_pct = (deep_pass_attempts / attempts) * 100,
      deep_success_rate = (deep_completions / deep_pass_attempts) * 100,
      third_down_success = (third_down_conversions / third_down_attempts) * 100,
      
      season = year
    )
  
  return(qb_metrics)
}

# Process data for all years
all_qb_data <- map_dfr(years, process_year_data)

# Save raw data for reference
write_csv(all_qb_data, "qb_metrics_2019_2024_new.csv")
print(paste("Saved QB performance data for", nrow(all_qb_data), "QB seasons"))

# 2. ADD TEAM WIN-LOSS DATA
print("Adding team win-loss data...")

# Function to get team win-loss records for each season
get_team_records <- function(years) {
  team_records <- map_dfr(years, function(year) {
    print(paste("Getting team records for:", year))
    # Load team game data
    games <- load_schedules(seasons = year) %>%
      filter(game_type == "REG")
    
    # Calculate win-loss records
    home_records <- games %>%
      group_by(team = home_team, season) %>%
      summarize(
        wins = sum(home_score > away_score, na.rm = TRUE),
        losses = sum(home_score < away_score, na.rm = TRUE),
        ties = sum(home_score == away_score & !is.na(home_score), na.rm = TRUE),
        games = n(),
        .groups = "drop"
      )
    
    away_records <- games %>%
      group_by(team = away_team, season) %>%
      summarize(
        wins = sum(away_score > home_score, na.rm = TRUE),
        losses = sum(away_score < home_score, na.rm = TRUE),
        ties = sum(away_score == home_score & !is.na(away_score), na.rm = TRUE),
        games = n(),
        .groups = "drop"
      )
    
    # Combine home and away records
    bind_rows(home_records, away_records) %>%
      group_by(team, season) %>%
      summarize(
        wins = sum(wins),
        losses = sum(losses),
        ties = sum(ties),
        win_pct = (wins + (0.5 * ties)) / sum(games),
        .groups = "drop"
      )
  })
  
  return(team_records)
}

# Get team records for all seasons
team_records <- get_team_records(years)

# Join team records with QB data
all_qb_data <- all_qb_data %>%
  left_join(team_records, by = c("posteam" = "team", "season" = "season"))

# Save updated data
write_csv(all_qb_data, "qb_team_metrics_2019_2024_new.csv")
print(paste("Added team win-loss data to QB metrics"))

# 3. ADD SALARY DATA
salary_file_path <- "C:/Users/Jmaddox/Downloads/nfl_qb_salaries_cleaned_2019_2024.csv"

if(file.exists(salary_file_path)) {
  print("Adding salary data to analysis...")
  
  # Load salary data
  qb_salaries <- read_csv(salary_file_path) %>%
    rename(
      player_name = Player,
      team = Team,
      season = Year
    ) %>%
    mutate(
      # Convert to numeric if needed
      salary_cap = as.numeric(Cap_Number),
      cash_spent = as.numeric(Cash_Spent),
      
      # Convert to millions for better readability
      salary_millions = salary_cap / 1000000,
      cash_millions = cash_spent / 1000000
    )
  
  # Get roster data for better name matching
  qb_roster_data <- load_rosters(years) %>%
    filter(position == "QB") %>%
    select(player_id = gsis_id, player_name = full_name, team, season) %>%
    distinct()
  
  # Add roster names to metrics
  qb_metrics_with_names <- all_qb_data %>%
    left_join(qb_roster_data, by = c("passer_id" = "player_id", "season"))
  
  # Try matching by player name
  qb_analysis_data <- qb_metrics_with_names %>%
    left_join(qb_salaries, by = c("player_name", "season"))
  
  # Check match rate
  matched_count <- sum(!is.na(qb_analysis_data$salary_millions))
  match_rate <- matched_count / nrow(all_qb_data)
  print(paste("Matched", matched_count, "QB salaries (", round(match_rate * 100, 1), "% match rate)"))
  
  # If poor matching, try with passer name
  if(match_rate < 0.3) {
    print("Trying alternative name matching...")
    
    qb_analysis_data <- all_qb_data %>%
      left_join(qb_salaries, by = c("passer" = "player_name", "season"))
    
    matched_count <- sum(!is.na(qb_analysis_data$salary_millions))
    match_rate <- matched_count / nrow(all_qb_data)
    print(paste("After alternative matching:", matched_count, "QB salaries matched (", 
                round(match_rate * 100, 1), "% match rate)"))
  }
  
  # Save matched data
  write_csv(qb_analysis_data, "qb_salary_metrics_2019_2024_new.csv")
} else {
  print("Salary data file not found. Analysis will proceed without salary information.")
  qb_analysis_data <- all_qb_data
}

# 4. DESCRIPTIVE STATISTICS AND TABLES
print("Generating descriptive statistics...")

# Overall summary statistics 
summary_stats <- all_qb_data %>%
  summarize(
    total_qbs = n_distinct(passer),
    total_seasons = n(),
    avg_comp_pct = mean(completion_pct, na.rm = TRUE),
    avg_yards_per_attempt = mean(yards_per_attempt, na.rm = TRUE),
    avg_yac_pct = mean(yac_percentage, na.rm = TRUE),
    avg_air_yards_pct = mean(air_yards_percentage, na.rm = TRUE),
    avg_td_pct = mean(td_pct, na.rm = TRUE),
    avg_int_pct = mean(int_pct, na.rm = TRUE),
    avg_epa_per_attempt = mean(epa_per_attempt, na.rm = TRUE),
    avg_win_pct = mean(win_pct, na.rm = TRUE)
  )

# Print summary table
print("NFL QB Overall Performance Summary (2019-2023):")
print(summary_stats)

# Yearly trends with team success
yearly_trends <- all_qb_data %>%
  group_by(season) %>%
  summarize(
    qbs = n(),
    avg_comp_pct = mean(completion_pct, na.rm = TRUE),
    avg_yac_pct = mean(yac_percentage, na.rm = TRUE),
    avg_air_pct = mean(air_yards_percentage, na.rm = TRUE),
    avg_yards_per_att = mean(yards_per_attempt, na.rm = TRUE),
    avg_epa_per_att = mean(epa_per_attempt, na.rm = TRUE),
    avg_air_epa = mean(air_epa_per_attempt, na.rm = TRUE),
    avg_yac_epa = mean(yac_epa_per_attempt, na.rm = TRUE),
    avg_win_pct = mean(win_pct, na.rm = TRUE),
    .groups = "drop"
  )

# Save trends table
write_csv(yearly_trends, "yearly_trends_table_new.csv")
print("Saved yearly trends table")

# 5. TEAM SUCCESS ANALYSIS
print("Analyzing relationship between QB metrics and team success...")

# 5.1 Create team success correlation data
success_correlation <- all_qb_data %>%
  filter(!is.na(win_pct)) %>%
  select(
    passer, season, posteam, win_pct, games_played,
    completion_pct, yards_per_attempt, td_pct, int_pct,
    yac_percentage, air_yards_percentage, 
    yac_epa_per_attempt, air_epa_per_attempt, epa_per_attempt,
    mean_cpoe, deep_pass_pct, third_down_success
  )

# Calculate correlations with team success
win_cors <- success_correlation %>%
  select(-passer, -season, -posteam, -games_played) %>%
  cor(use = "pairwise.complete.obs")

# Extract correlations with win percentage
win_cor_values <- win_cors["win_pct", ] %>%
  as.data.frame() %>%
  rownames_to_column("metric") %>%
  rename(correlation = ".") %>%
  arrange(desc(abs(correlation))) %>%
  filter(metric != "win_pct")

# Save correlation table
write_csv(win_cor_values, "win_correlation_table_new.csv")
print("Saved win correlation table")

# 5.2 Regression: What QB factors predict team success?
team_success_model <- lm(win_pct ~ epa_per_attempt + yac_percentage + 
                           air_yards_percentage + completion_pct + 
                           int_pct + third_down_success + 
                           factor(season), 
                         data = success_correlation)

# Create model summary table
success_reg_table <- tidy(team_success_model, conf.int = TRUE) %>%
  mutate(
    p_value = format.pval(p.value, digits = 3),
    estimate = round(estimate, 3),
    conf_int = paste0("[", round(conf.low, 3), ", ", round(conf.high, 3), "]")
  ) %>%
  select(term, estimate, conf_int, p_value)

# Save regression results
write_csv(success_reg_table, "team_success_regression_table_new.csv")
print("Saved team success regression analysis")
print(paste("Team success model adjusted R-squared:", 
            round(summary(team_success_model)$adj.r.squared, 3)))

# 6. VISUALIZATIONS
print("Creating key visualizations...")

# 6.1 First set: Yearly trends
# Plot YAC% vs Air Yards% trends
p_yac_trend <- ggplot(yearly_trends, aes(x = season)) +
  geom_line(aes(y = avg_yac_pct, color = "YAC %"), linewidth = 1.2) +
  geom_point(aes(y = avg_yac_pct, color = "YAC %"), size = 3) +
  geom_line(aes(y = avg_air_pct, color = "Air Yards %"), linewidth = 1.2) +
  geom_point(aes(y = avg_air_pct, color = "Air Yards %"), size = 3) +
  scale_color_manual(values = c("YAC %" = "#1F77B4", "Air Yards %" = "#FF7F0E")) +
  labs(
    title = "Evolution of NFL QB Passing Metrics (2019-2023)",
    subtitle = "League-wide trends in YAC vs Air Yards percentages",
    x = "Season",
    y = "Percentage (%)",
    color = "Metric"
  )

# Plot efficiency trends
p_epa_trend <- ggplot(yearly_trends, aes(x = season)) +
  geom_line(aes(y = avg_epa_per_att, color = "Total EPA"), linewidth = 1.2) +
  geom_point(aes(y = avg_epa_per_att, color = "Total EPA"), size = 3) +
  geom_line(aes(y = avg_air_epa, color = "Air EPA"), linewidth = 1.2) +
  geom_point(aes(y = avg_air_epa, color = "Air EPA"), size = 3) +
  geom_line(aes(y = avg_yac_epa, color = "YAC EPA"), linewidth = 1.2) +
  geom_point(aes(y = avg_yac_epa, color = "YAC EPA"), size = 3) +
  scale_color_manual(values = c("Total EPA" = "#2CA02C", "Air EPA" = "#D62728", "YAC EPA" = "#9467BD")) +
  labs(
    title = "QB Efficiency Trends (2019-2023)",
    subtitle = "Changes in Expected Points Added components over time",
    x = "Season",
    y = "EPA per Attempt",
    color = "Metric"
  )

# Save trend plots
ggsave("qb_yac_air_trends_new.png", p_yac_trend, width = 10, height = 6, dpi = 300)
ggsave("qb_epa_trends_new.png", p_epa_trend, width = 10, height = 6, dpi = 300)

# 6.2 Win percentage visualizations
# YAC percentage vs Win percentage
p_yac_wins <- ggplot(success_correlation, aes(x = yac_percentage, y = win_pct)) +
  geom_point(aes(color = epa_per_attempt, size = games_played), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linetype = "dashed") +
  scale_color_viridis_c(option = "D") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text_repel(
    data = success_correlation %>% 
      filter(win_pct > 0.65 | yac_percentage > quantile(yac_percentage, 0.9)) %>%
      head(10),
    aes(label = passer),
    size = 3,
    box.padding = 0.5,
    max.overlaps = 10
  ) +
  labs(
    title = "Does YAC Percentage Correlate with Winning?",
    subtitle = "Relationship between YAC% and team win percentage",
    x = "YAC Percentage",
    y = "Win Percentage",
    color = "EPA per Attempt",
    size = "Games Played"
  )

# Air yards percentage vs Win percentage
p_air_wins <- ggplot(success_correlation, aes(x = air_yards_percentage, y = win_pct)) +
  geom_point(aes(color = epa_per_attempt, size = games_played), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linetype = "dashed") +
  scale_color_viridis_c(option = "D") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text_repel(
    data = success_correlation %>% 
      filter(win_pct > 0.65 | air_yards_percentage > quantile(air_yards_percentage, 0.9)) %>%
      head(10),
    aes(label = passer),
    size = 3,
    box.padding = 0.5,
    max.overlaps = 10
  ) +
  labs(
    title = "Does Air Yards Percentage Correlate with Winning?",
    subtitle = "Relationship between Air Yards% and team win percentage",
    x = "Air Yards Percentage",
    y = "Win Percentage",
    color = "EPA per Attempt",
    size = "Games Played"
  )

# Combined EPA breakdown by win percentage
p_epa_components_wins <- success_correlation %>%
  mutate(
    win_group = cut(win_pct, 
                    breaks = c(0, 0.3, 0.45, 0.55, 0.7, 1),
                    labels = c("Poor (<30%)", "Below Avg (30-45%)", 
                               "Average (45-55%)", "Above Avg (55-70%)", 
                               "Excellent (>70%)")
    )
  ) %>%
  group_by(win_group) %>%
  summarize(
    qbs = n(),
    avg_epa = mean(epa_per_attempt, na.rm = TRUE),
    avg_air_epa = mean(air_epa_per_attempt, na.rm = TRUE),
    avg_yac_epa = mean(yac_epa_per_attempt, na.rm = TRUE),
    avg_yac_pct = mean(yac_percentage, na.rm = TRUE),
    avg_air_pct = mean(air_yards_percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(avg_air_epa, avg_yac_epa),
    names_to = "epa_type",
    values_to = "epa_value"
  ) %>%
  mutate(
    epa_type = factor(epa_type, 
                      levels = c("avg_air_epa", "avg_yac_epa"),
                      labels = c("Air EPA", "YAC EPA"))
  ) %>%
  ggplot(aes(x = win_group, y = epa_value, fill = epa_type)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = round(epa_value, 3)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(values = c("Air EPA" = "#D62728", "YAC EPA" = "#9467BD")) +
  labs(
    title = "EPA Components by Team Success Level",
    subtitle = "Does Air EPA or YAC EPA contribute more to winning?",
    x = "Team Win Percentage Group",
    y = "EPA per Attempt",
    fill = "EPA Component"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save win percentage visualizations
ggsave("yac_vs_wins_new.png", p_yac_wins, width = 10, height = 7, dpi = 300)
ggsave("air_yards_vs_wins_new.png", p_air_wins, width = 10, height = 7, dpi = 300)
ggsave("epa_components_by_win_pct_new.png", p_epa_components_wins, width = 12, height = 7, dpi = 300)
print("Saved team success visualizations")

# 6.3 Create correlation matrix
qb_cor_vars <- all_qb_data %>%
  select(
    win_pct, completion_pct, yards_per_attempt, td_pct, int_pct,
    yac_percentage, air_yards_percentage, epa_per_attempt, 
    mean_cpoe, deep_pass_pct, third_down_success
  )

# Calculate correlations
qb_cors <- cor(qb_cor_vars, use = "pairwise.complete.obs")

# Create correlation plot
png("qb_win_correlation_matrix_new.png", width = 10, height = 8, units = "in", res = 300)
corrplot(qb_cors, method = "color", type = "upper", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Matrix of QB Performance Metrics",
         mar = c(0,0,2,0))
dev.off()
print("Saved correlation matrix visualization")

# 7. SALARY ANALYSIS (if data available)
if(exists("qb_analysis_data") && "salary_millions" %in% names(qb_analysis_data)) {
  print("Performing salary-related analyses...")
  
  # Clean data for salary analysis
  qb_salary_analysis <- qb_analysis_data %>%
    filter(!is.na(salary_millions)) %>%
    filter(salary_millions > 0, passing_yards > 0) %>%
    mutate(
      yards_per_million = passing_yards / salary_millions,
      tds_per_million = passing_tds / salary_millions,
      epa_per_million = total_epa / salary_millions
    )
  
  # Regression 3: What factors predict QB salary?
  salary_model <- lm(log(salary_millions) ~ passing_yards + passing_tds + 
                       interceptions + completion_pct + epa_per_attempt + 
                       yac_percentage + win_pct + factor(season), 
                     data = qb_salary_analysis)
  
  # Create model summary table
  salary_reg_table <- tidy(salary_model, conf.int = TRUE) %>%
    mutate(
      p_value = format.pval(p.value, digits = 3),
      estimate = round(estimate, 3),
      conf_int = paste0("[", round(conf.low, 3), ", ", round(conf.high, 3), "]")
    ) %>%
    select(term, estimate, conf_int, p_value)
  
  # Save regression results
  write_csv(salary_reg_table, "salary_regression_table_new.csv")
  print("Saved salary regression analysis")
  print(paste("Salary model adjusted R-squared:", round(summary(salary_model)$adj.r.squared, 3)))
  
  # Visualize QB efficiency vs salary
  p_salary_epa <- ggplot(qb_salary_analysis, aes(x = salary_millions, y = epa_per_attempt)) +
    geom_point(aes(color = yac_percentage, size = win_pct * 100), alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE, color = "grey30") +
    scale_x_continuous(labels = scales::dollar_format(suffix = "M")) +
    scale_color_viridis_c(option = "C") +
    geom_text_repel(
      data = qb_salary_analysis %>% 
        filter(salary_millions > quantile(salary_millions, 0.75) | 
                 epa_per_attempt > quantile(epa_per_attempt, 0.75)) %>%
        head(12),
      aes(label = passer),
      size = 3,
      box.padding = 0.5,
      max.overlaps = 12
    ) +
    labs(
      title = "QB Efficiency vs Salary",
      subtitle = "Color indicates YAC%, size indicates win percentage",
      x = "Salary (Millions $)",
      y = "EPA per Attempt",
      color = "YAC Percentage",
      size = "Win Percentage"
    )
  
  # Visualize Win % vs Salary
  p_salary_wins <- ggplot(qb_salary_analysis, aes(x = salary_millions, y = win_pct)) +
    geom_point(aes(color = yac_percentage, size = epa_per_attempt), alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE, color = "grey30") +
    scale_x_continuous(labels = scales::dollar_format(suffix = "M")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_viridis_c(option = "C") +
    geom_text_repel(
      data = qb_salary_analysis %>% 
        filter(salary_millions > quantile(salary_millions, 0.75) | 
                 win_pct > 0.65) %>%
        head(12),
      aes(label = passer),
      size = 3,
      box.padding = 0.5,
      max.overlaps = 12
    ) +
    labs(
      title = "Team Success vs QB Salary",
      subtitle = "Are higher-paid QBs associated with more team wins?",
      x = "Salary (Millions $)",
      y = "Win Percentage",
      color = "YAC Percentage",
      size = "EPA per Attempt"
    )
  
  # Save salary visualizations
  ggsave("qb_efficiency_vs_salary_new.png", p_salary_epa, width = 10, height = 7, dpi = 300)
  ggsave("wins_vs_salary_new.png", p_salary_wins, width = 10, height = 7, dpi = 300)
  print("Saved salary visualizations")
  
  # Create top value QBs table with win percentage
  top_value_qbs <- qb_salary_analysis %>%
    group_by(passer) %>%
    summarize(
      seasons = n(),
      avg_salary = mean(salary_millions),
      avg_win_pct = mean(win_pct, na.rm = TRUE),
      total_epa = sum(total_epa),
      total_yards = sum(passing_yards),
      total_tds = sum(passing_tds),
      epa_per_million = total_epa / sum(salary_millions),
      avg_yac_pct = mean(yac_percentage),
      .groups = "drop"
    ) %>%
    filter(seasons >= 2) %>% # At least 2 seasons
    arrange(desc(epa_per_million)) %>%
    head(10)
  
  # Save top value QBs table
  write_csv(top_value_qbs, "top_value_qbs_table_new.csv")
  print("Saved top value QBs table")
}

# 8. FINAL SUMMARIES
# Create a summary table of the most successful QB styles
qb_style_success <- all_qb_data %>%
  filter(!is.na(win_pct)) %>%
  mutate(
    yac_style = ifelse(yac_percentage > median(yac_percentage, na.rm = TRUE), "High YAC", "Low YAC"),
    air_style = ifelse(air_yards_percentage > median(air_yards_percentage, na.rm = TRUE), "High Air", "Low Air"),
    qb_style = case_when(
      yac_style == "High YAC" & air_style == "Low Air" ~ "YAC-Dependent",
      yac_style == "Low YAC" & air_style == "High Air" ~ "Air Yards-Dependent",
      yac_style == "High YAC" & air_style == "High Air" ~ "Balanced (High Total)",
      yac_style == "Low YAC" & air_style == "Low Air" ~ "Balanced (Low Total)"
    )
  ) %>%
  group_by(qb_style) %>%
  summarize(
    qbs = n(),
    avg_win_pct = mean(win_pct, na.rm = TRUE),
    avg_epa = mean(epa_per_attempt, na.rm = TRUE),
    avg_comp_pct = mean(completion_pct, na.rm = TRUE),
    avg_ypa = mean(yards_per_attempt, na.rm = TRUE),
    avg_td_pct = mean(td_pct, na.rm = TRUE),
    avg_int_pct = mean(int_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_win_pct))

# Save QB style success table
write_csv(qb_style_success, "qb_style_success_table_new.csv")
print("Saved QB style success analysis")

# Create a visualization of QB styles and success
p_style_success <- qb_style_success %>%
  ggplot(aes(x = reorder(qb_style, avg_win_pct), y = avg_win_pct)) +
  geom_col(aes(fill = avg_epa), width = 0.7) +
  geom_text(
    aes(label = paste0(round(avg_win_pct * 100, 1), "%")),
    hjust = -0.2,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max(qb_style_success$avg_win_pct) * 1.2)
  ) +
  scale_fill_viridis_c(option = "D") +
  coord_flip() +
  labs(
    title = "Win Percentage by QB Passing Style",
    subtitle = "Which passing approach correlates most with team success?",
    x = "",
    y = "Average Win Percentage",
    fill = "EPA per Attempt"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

# Save style success visualization
ggsave("qb_style_success_new.png", p_style_success, width = 10, height = 6, dpi = 300)

# 9. CASE STUDIES
# Select a few notable QBs with different passing styles and their team success
case_studies <- all_qb_data %>%
  filter(!is.na(win_pct)) %>%
  group_by(passer) %>%
  summarize(
    seasons = n(),
    avg_yac_pct = mean(yac_percentage, na.rm = TRUE),
    avg_air_pct = mean(air_yards_percentage, na.rm = TRUE),
    avg_epa = mean(epa_per_attempt, na.rm = TRUE),
    avg_win_pct = mean(win_pct, na.rm = TRUE),
    total_yards = sum(passing_yards, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(seasons >= 2, total_yards > 5000) %>%
  mutate(
    yac_style = ifelse(avg_yac_pct > median(avg_yac_pct), "High YAC", "Low YAC"),
    air_style = ifelse(avg_air_pct > median(avg_air_pct), "High Air", "Low Air"),
    qb_style = case_when(
      yac_style == "High YAC" & air_style == "Low Air" ~ "YAC-Dependent",
      yac_style == "Low YAC" & air_style == "High Air" ~ "Air Yards-Dependent",
      yac_style == "High YAC" & air_style == "High Air" ~ "Balanced (High Total)",
      yac_style == "Low YAC" & air_style == "Low Air" ~ "Balanced (Low Total)"
    )
  ) %>%
  arrange(desc(avg_win_pct))

# Top 3 and bottom 3 QBs by win percentage
top_bottom_qbs <- bind_rows(
  head(case_studies, 5) %>% mutate(group = "Top 5"),
  tail(case_studies, 5) %>% mutate(group = "Bottom 5")
)

# Create comparative visualization
p_case_studies <- ggplot(top_bottom_qbs, aes(x = avg_yac_pct, y = avg_air_pct)) +
  geom_point(aes(color = avg_win_pct, size = avg_epa), alpha = 0.8) +
  geom_text_repel(
    aes(label = passer),
    size = 3.5,
    box.padding = 0.5,
    point.padding = 0.3
  ) +
  scale_color_viridis_c(
    option = "D", 
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Case Studies: QB Passing Style and Team Success",
    subtitle = "Top and bottom QBs by win percentage (2019-2023)",
    x = "YAC Percentage",
    y = "Air Yards Percentage",
    color = "Win Percentage",
    size = "EPA per Attempt"
  )

# Save case studies visualization
ggsave("qb_case_studies_new.png", p_case_studies, width = 10, height = 8, dpi = 300)
print("Saved case studies visualization")

# 10. CONCLUSIONS
print("Analysis completed with team success integration!")
print("Key findings:")
print("1. Relationship between QB style and team success revealed")
print("2. Added win percentage correlation with QB metrics")
print("3. Created visualizations of QB passing styles and team success")
print("4. Added case studies of successful and unsuccessful QB approaches")
print("5. Enhanced regression models to include team success metrics")

# Create final report summary table
final_summary <- data.frame(
  Metric = c(
    "Total QB seasons analyzed",
    "Correlation between YAC% and Win%",
    "Correlation between Air Yards% and Win%",
    "Correlation between EPA/Attempt and Win%",
    "Most successful QB style by Win%",
    "Team success model R-squared"
  ),
  Value = c(
    nrow(all_qb_data),
    round(win_cor_values$correlation[win_cor_values$metric == "yac_percentage"], 3),
    round(win_cor_values$correlation[win_cor_values$metric == "air_yards_percentage"], 3),
    round(win_cor_values$correlation[win_cor_values$metric == "epa_per_attempt"], 3),
    qb_style_success$qb_style[1],
    round(summary(team_success_model)$adj.r.squared, 3)
  )
)

# Save final summary
write_csv(final_summary, "final_analysis_summary_new.csv")
print("Saved final analysis summary")