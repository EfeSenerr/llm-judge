# Summary Code used for Report

# libraries ####
install.packages("irr")
install.packages("DescTools")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")

library(irr)
library(DescTools)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# df_analysis genesis ####

# base: df_combined_human and df_llm

df_llm <- evaluation_results_aggregated_0503_DIFF
# df_llm <- read_csv("path/to/your/llm_evaluation_updated_prompt.csv")

df_llm <- df_llm[-1,]
df_llm <- df_llm[-174,] # 0402_summaries_german_Eval_1

names(df_llm)[names(df_llm) == "Article.File.Name"] <- "batch_name"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_ampel-aus_0-200_processed.json"] <- "ampel_aus_0"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_D-Day_processed.json"] <- "DDay"
df_llm$`batch_name`[df_llm$`batch_name` == "articles_newswires_processed.json"] <- "articles_newswires"
df_llm$`batch_name`[df_llm$`batch_name` == "articles_newspapers_processed.json"] <- "articles_newspapers"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_ampel-aus_201-300_processed.json"] <- "ampel_aus_201"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_ampel-aus_300-400_processed.json"] <- "ampel_aus_300"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_Ampel_Koalition am Ende_52_processed.json"] <- "ampel_koalition"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_koalitionskrise_52_processed.json"] <- "koalitionskrise"

#unique id:
names(df_llm)[names(df_llm) == "Article_Number"] <- "article_number"
df_llm$unique_id <- paste(df_llm$article_number, df_llm$batch_name, sep = "_")

#delete useless info:
df_llm <- df_llm[, !names(df_llm) %in% c("ID", "batch_name", "article_number", "Article_Name")]

# df_combined_human
df_combined_human <- human_eval_combined_MASTER_DIFF
df_combined_human <- df_combined_human[, !names(df_combined_human) %in% c("batch_name", "article_number")]

# df_analysis
print(df_combined_human)
print(df_llm)
df_analysis <- merge(df_combined_human, df_llm, by = "unique_id", suffixes = c("_human", "_llm"))
df_analysis[is.na(df_analysis)] <- "N/A"


# sentiment pairs of columns:
sentiment_pairs <- list(
  c("Article_SPD_human", "Article_SPD_llm"),
  c("Article_FDP_human", "Article_FDP_llm"),
  c("Article_die_Gruenen_human", "Article_die_Gruenen_llm"),
  c("Article_Olaf_Scholz_human", "Article_Olaf_Scholz_llm"),
  c("Article_Christian_Lindner_human", "Article_Christian_Lindner_llm"),
  c("Article_Robert_Habeck_human", "Article_Robert_Habeck_llm"),
  c("Article_the_coalition_breakdown_human", "Article_the_coalition_breakdown_llm"),
  c("Summary_SPD_human", "Summary_SPD_llm"),
  c("Summary_FDP_human", "Summary_FDP_llm"),
  c("Summary_die_Gruenen_human", "Summary_die_Gruenen_llm"),
  c("Summary_Olaf_Scholz_human", "Summary_Olaf_Scholz_llm"),
  c("Summary_Christian_Lindner_human", "Summary_Christian_Lindner_llm"),
  c("Summary_Robert_Habeck_human", "Summary_Robert_Habeck_llm"),
  c("Summary_the_coalition_breakdown_human", "Summary_the_coalition_breakdown_llm")
)


# Spearman's Coef & Cohen's Kappa ####

# Kappa on Consistency:
kappa_accuracy <- kappa2(df_analysis[, c("Summary_Consistency_human", "Summary_Consistency_llm")])
print(kappa_accuracy)

# With "N/A" as a category ####
levels_sentiment <- c("-1", "0", "1", "N/A")
for (pair in sentiment_pairs) {
  df_analysis[[pair[1]]] <- factor(as.character(df_analysis[[pair[1]]]), levels = levels_sentiment, ordered = TRUE)
  df_analysis[[pair[2]]] <- factor(as.character(df_analysis[[pair[2]]]), levels = levels_sentiment, ordered = TRUE)
}

# Spearmans Coef:
for (pair in sentiment_pairs) {
  # Convert the factors to their underlying numeric codes.
  x <- as.numeric(df_analysis[[pair[1]]])
  y <- as.numeric(df_analysis[[pair[2]]])
  
  # Compute Spearman's rank correlation.
  corr <- cor(x, y, method = "spearman", use = "complete.obs")
  cat("Spearman correlation for", pair[1], "and", pair[2], ":", corr, "\n")
}

#### Spearman / Kendall Coeficient #####

article_sentiment_cols <- c("Article_SPD", "Article_FDP", "Article_die_Gruenen", 
                            "Article_Olaf_Scholz", "Article_Christian_Lindner", "Article_Robert_Habeck", "Summary_Consistency", "Article_the_coalition_breakdown")

cat("Pairwise Rank Correlations (Across All Articles):\n")
for (col in article_sentiment_cols) {
  col_human <- paste0(col, "_human")
  col_llm   <- paste0(col, "_llm")
  
  # Make sure the columns are numeric or factors with numeric levels.
  # (Here we assume the columns are already numeric or integer coded as -1, 0, 1.)
  # If not, you might need to convert them:
  df_analysis[[col_human]] <- as.numeric(as.character(df_analysis[[col_human]]))
  df_analysis[[col_llm]]   <- as.numeric(as.character(df_analysis[[col_llm]]))
  
  # Compute Spearman's rho and Kendall's tau using complete cases
  spearman_val <- cor(df_analysis[[col_human]], df_analysis[[col_llm]], 
                      method = "spearman", use = "complete.obs")
  kendall_val  <- cor(df_analysis[[col_human]], df_analysis[[col_llm]], 
                      method = "kendall", use = "complete.obs")
  
  cat("For", col, ":\n")
  cat("  Spearman's rho:", round(spearman_val, 3), "\n")
  cat("  Kendall's tau:  ", round(kendall_val, 3), "\n\n")
}

# Chi Square ####
# Identify human evaluation columns (those ending in "_human")
human_cols <- grep("^Article_.*_human$", names(df_analysis), value = TRUE)

# Reshape the data: each row represents an evaluation toward one party
df_long_human <- df_analysis %>%
  pivot_longer(
    cols = all_of(human_cols),
    names_to = "party",
    values_to = "sentiment"
  ) %>%
  # Clean up the party names by removing "Article_" prefix and "_human" suffix
  mutate(party = gsub("^Article_(.*)_human$", "\\1", party))

# Create the contingency table of party vs sentiment
ctable_human <- table(df_long_human$party, df_long_human$sentiment)
print(ctable_human)

# Perform the chi-square test for human evaluations
chi_human <- chisq.test(ctable_human)
print(chi_human)


# Identify LLM evaluation columns (those ending in "_llm")
llm_cols <- grep("^Article_.*_llm$", names(df_analysis), value = TRUE)

# Reshape the data for LLM evaluations
df_long_llm <- df_analysis %>%
  pivot_longer(
    cols = all_of(llm_cols),
    names_to = "party",
    values_to = "sentiment"
  ) %>%
  # Clean up the party names
  mutate(party = gsub("^Article_(.*)_llm$", "\\1", party))

# Create the contingency table for LLM evaluations
ctable_llm <- table(df_long_llm$party, df_long_llm$sentiment)
print(ctable_llm)

# Perform the chi-square test for LLM evaluations
chi_llm <- chisq.test(ctable_llm)
print(chi_llm)

install.packages("vcd")
library(vcd)
cramer_v_human <- assocstats(ctable_human)$cramer
cat("Cramér's V (Human):", cramer_v_human, "\n")

cramer_v_llm <- assocstats(ctable_llm)$cramer
cat("Cramér's V (LLM):", cramer_v_llm, "\n")

# MISSING: Viszalization ####

# Spearman vs Cohens

# Article and Summary data, kappa & coeff:

# Spearman with weighted "N/A"s:

df_NA_extra <- df_analysis
df_NA_extra$Summary_Consistency_human <- factor(ifelse(is.na(df_NA_extra$Summary_Consistency_human),
                                                       "Missing",
                                                       df_NA_extra$Summary_Consistency_human),
                                                levels = c("0", "1", "Missing"))

df_NA_extra$Summary_Consistency_llm <- factor(ifelse(is.na(df_NA_extra$Summary_Consistency_llm),
                                                     "Missing",
                                                     df_NA_extra$Summary_Consistency_llm),
                                              levels = c("0", "1", "Missing"))

kappa_result <- kappa2(df_NA_extra[, c("Summary_Consistency_human", "Summary_Consistency_llm")])

for (pair in sentiment_pairs) {
  # Convert the factors to their underlying numeric codes.
  x <- as.numeric(df_analysis[[pair[1]]])
  y <- as.numeric(df_analysis[[pair[2]]])
  
  # Compute Spearman's rank correlation.
  corr <- cor(x, y, method = "spearman", use = "complete.obs")
  cat("Spearman correlation for", pair[1], "and", pair[2], ":", corr, "\n")
}


# Create a data frame with updated Spearman correlation values
df_spearman <- tibble(
  Subject = c("SPD", "FDP", "die_Gruenen", "Olaf_Scholz", "Christian_Lindner", "Robert_Habeck", "the_coalition_breakdown"),
  Article = c(0.2586184, 0.100703, 0.04027035, 0.4364535, 0.1820071, 0.6793662, -0.01039733),
  Summary = c(0.4685695, 0.3124763, 0.3541685, 0.7999531, 0.2743197, 0.5710582, 0.1928372)
) %>%
  pivot_longer(cols = c(Article, Summary), names_to = "Evaluator", values_to = "Spearman")

# Create a data frame with Cohen's kappa values
df_kappa <- tibble(
  Subject = c("SPD", "FDP", "die_Gruenen", "Olaf_Scholz", "Christian_Lindner", "Robert_Habeck", "the_coalition_breakdown"),
  Article = c(0.1780278, 0.1461962, 0.2308098, 0.4467944, 0.2301808, 0.4480409, 0.07232238),
  Summary = c(0.2317835, 0.1245136, 0.1885681, 0.3728297, 0.06368222, 0.3446602, 0.07989398)
) %>%
  pivot_longer(cols = c(Article, Summary), names_to = "Evaluator", values_to = "Kappa")

# Set common y-axis limits and breaks (0 to 1) for both plots.
common_y_limits <- c(0, 1)
common_y_breaks <- seq(0, 1, by = 0.1)

# Spearman Correlation Plot
p_spearman <- ggplot(df_spearman, aes(x = Subject, y = Spearman, fill = Evaluator)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Spearman Correlation: Human vs. LLM", 
       y = "Spearman Correlation", 
       x = "Subject") +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = common_y_limits, breaks = common_y_breaks) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Cohen's Kappa Plot
p_kappa <- ggplot(df_kappa, aes(x = Subject, y = Kappa, fill = Evaluator)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cohen's Kappa: Human vs. LLM", 
       y = "Cohen's Kappa", 
       x = "Subject") +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = common_y_limits, breaks = common_y_breaks) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine the two plots side by side using patchwork
combined_plot <- p_spearman + p_kappa +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Comparison of Spearman Correlation and Cohen's Kappa by Subject",
                  subtitle = "Article vs. Summary Evaluations")

# Display the combined plot
print(combined_plot)


# Differences analyzed ####

# Define the subjects (make sure the names match your column name parts)
subjects <- c("coalition", "spd", "fdp", "gruene", "lindner", "habeck", "scholz")

# Initialize a results data frame to store the correlations
results_diff <- data.frame(Subject = character(), Spearman_rho = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# Loop over subjects to calculate correlation between human and LLM differences
for (s in subjects) {
  # Construct column names for human and LLM difference scores
  human_col <- paste0("diff_", s, "_human")
  llm_col <- paste0("diff_", s, "_llm")
  
  # Check that both columns exist in your data frame
  if (all(c(human_col, llm_col) %in% names(df_analysis))) {
    test <- cor.test(df_analysis[[human_col]], df_analysis[[llm_col]], method = "spearman")
    results_diff <- rbind(results_diff,
                          data.frame(Subject = s,
                                     Spearman_rho = test$estimate,
                                     p_value = test$p.value,
                                     stringsAsFactors = FALSE))
  } else {
    warning(paste("Columns not found for subject:", s))
  }
}

# Print the correlation results
print(results_diff)

library(ggplot2)
library(patchwork)

# Defining subjects
subjects <- c("coalition", "spd", "fdp", "gruene", "lindner", "habeck", "scholz")

# Initialize an empty list to store plots
plots_list <- list()

# Loop over subjects to create scatter plots for each subject
for (s in subjects) {
  human_col <- paste0("diff_", s, "_human")
  llm_col   <- paste0("diff_", s, "_llm")
  
  if (all(c(human_col, llm_col) %in% names(df_analysis))) {
    p <- ggplot(df_analysis, aes_string(x = human_col, y = llm_col)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = paste("Difference Correlation for", toupper(s)),
           x = paste("Human difference (", s, ")", sep = ""),
           y = paste("LLM difference (", s, ")", sep = "")) +
      theme_minimal()
    # Store the plot in the list, using the subject name as the list name
    plots_list[[s]] <- p
  }
}

# Combine all plots into one layout with 2 columns
combined_plots <- wrap_plots(plots_list, ncol = 2)

# Display the combined plots 
print(combined_plots) 

# Same Data different visualization:

scatter_plots <- lapply(subjects, function(s) {
  human_col <- paste0("diff_", s, "_human")
  llm_col   <- paste0("diff_", s, "_llm")
  
  if (all(c(human_col, llm_col) %in% names(df_analysis))) {
    ggplot(df_analysis, aes_string(x = human_col, y = llm_col)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      labs(title = paste("Agreement for", toupper(s)),
           x = paste("Human Difference (", s, ")", sep = ""),
           y = paste("LLM Difference (", s, ")", sep = "")) +
      theme_minimal()
  }
})

# Combine and display all scatter plots in a grid
combined_scatter <- wrap_plots(scatter_plots, ncol = 2)
print(combined_scatter)

# Comparisson of Summary Sentiment
df_analysis[is.na(df_analysis)] <- "N/A"
# Ensure all sentiment columns have the same levels
levels_sentiment <- c("-1", "0", "1", "N/A")
df_analysis <- df_analysis %>%
  mutate(across(matches("^Summary_(SPD|FDP|die_Gruenen|Olaf_Scholz|Christian_Lindner|Robert_Habeck|the_coalition_breakdown)_(human|llm)$"),
                ~ factor(as.character(.), levels = levels_sentiment, ordered = TRUE)))

# Select only the sentiment columns (exclude consistency columns)
# Summary
df_long <- df_analysis %>%
  select(matches("^Summary_(SPD|FDP|die_Gruenen|Olaf_Scholz|Christian_Lindner|Robert_Habeck|the_coalition_breakdown)_(human|llm)$")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Subject", "Evaluator"),
    names_pattern = "Summary_(.+)_(human|llm)",
    values_to = "Sentiment"
  )
ggplot(df_long, aes(x = Sentiment, fill = Evaluator)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Subject) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Comparison of Summary Sentiment Ratings: Human vs. LLM",
       x = "Sentiment Rating", y = "Count")

# Article:
df_long <- df_analysis %>%
  select(matches("^Article(SPD|FDP|die_Gruenen|Olaf_Scholz|Christian_Lindner|Robert_Habeck|the_coalition_breakdown)_(human|llm)$")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Subject", "Evaluator"),
    names_pattern = "Article_(.+)_(human|llm)",
    values_to = "Sentiment"
  )

ggplot(df_long, aes(x = Sentiment, fill = Evaluator)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Subject) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Comparison of Article Sentiment Ratings: Human vs. LLM",
       x = "Sentiment Rating", y = "Count")

# simple agreement scores: 

# Define the subjects (adjust these to match your column names)
subjects <- c("SPD", "FDP", "die_Gruenen", "Olaf_Scholz", "Christian_Lindner", "Robert_Habeck", "the_coalition_breakdown")

# Initialize a data frame to store the results
agreement_results <- data.frame(Subject = character(), Agreement = numeric(), stringsAsFactors = FALSE)

# Loop over subjects to calculate percent agreement for article evaluations
for (s in subjects) {
  human_col <- paste0("Article_", s, "_human")
  llm_col   <- paste0("Article_", s, "_llm")
  
  if (all(c(human_col, llm_col) %in% names(df_analysis))) {
    # Create a contingency table for the two raters
    tab <- table(df_analysis[[human_col]], df_analysis[[llm_col]])
    
    # Percent agreement: sum of diagonal elements divided by total observations
    agreement <- sum(diag(tab)) / sum(tab)
    
    # Store the result
    agreement_results <- rbind(agreement_results, data.frame(Subject = s, Agreement = agreement))
  } else {
    warning(paste("Columns not found for subject:", s))
  }
}

print(agreement_results)

# Loop over subjects to calculate percent agreement for summary evaluations
for (s in subjects) {
  human_col <- paste0("Summary", s, "_human")
  llm_col   <- paste0("Summary", s, "_llm")
  
  if (all(c(human_col, llm_col) %in% names(df_analysis))) {
    # Create a contingency table for the two raters
    tab <- table(df_analysis[[human_col]], df_analysis[[llm_col]])
    
    # Percent agreement: sum of diagonal elements divided by total observations
    agreement <- sum(diag(tab)) / sum(tab)
    
    # Store the result
    agreement_results <- rbind(agreement_results, data.frame(Subject = s, Agreement = agreement))
  } else {
    warning(paste("Columns not found for subject:", s))
  }
}

print(agreement_results)


# aggregate table ####
for (colname in names(df_combined_human)) {
  cat("Counts for column:", colname, "\n")
  print(table(df_combined_human[[colname]], useNA = "ifany"))
  cat("\n")
}



