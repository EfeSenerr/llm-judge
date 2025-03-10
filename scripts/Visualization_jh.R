# visualization
install.packages("ggplot2")
install.packages("reshape2")
install.packages("tidyr")

library(dplyr)
library(tidyr)
library(ggplot2)

# For one subject, e.g., "SPD" for human summary sentiment and consistency:
ggplot(df_analysis, aes(x = Summary_SPD_human, fill = Summary_Consistency_human)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of SPD Sentiment Ratings by Consistency (Human)",
    x = "Sentiment Rating", y = "Count"
  ) +
  scale_fill_discrete(name = "Consistency")



# heatmap ####

# Create a contingency table and convert to a data frame for ggplot
tab_df <- as.data.frame(table(df_analysis$Summary_SPD_human, df_analysis$Summary_Consistency_human))
colnames(tab_df) <- c("Sentiment", "Consistency", "Count")

ggplot(tab_df, aes(x = Sentiment, y = Consistency, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Heatmap: SPD Sentiment vs. Consistency (Human)",
    x = "Summary SPD Sentiment", y = "Summary Consistency (Human)"
  )

# Create a contingency table for SPD (human)
tab <- table(df_analysis$Summary_SPD_human, df_analysis$Summary_Consistency_human)
mosaicplot(tab,
  main = "Mosaic Plot: SPD Sentiment vs. Consistency (Human)",
  xlab = "Summary SPD Sentiment", ylab = "Summary Consistency (Human)",
  col = c("lightblue", "lightgreen", "lightpink")
)


# human llm comparisson ####

# Reshape the data:

# Ensure all sentiment columns have the same levels
levels_sentiment <- c("-1", "0", "1", "N/A")
df_analysis <- df_analysis %>%
  mutate(across(
    matches("^Summary_(SPD|FDP|die_Gruenen|Olaf_Scholz|Christian_Lindner|Robert_Habeck)_(human|llm)$"),
    ~ factor(as.character(.), levels = levels_sentiment, ordered = TRUE)
  ))

# Select only the sentiment columns (exclude consistency columns)
df_long <- df_analysis %>%
  select(matches("^Summary_(SPD|FDP|die_Gruenen|Olaf_Scholz|Christian_Lindner|Robert_Habeck)_(human|llm)$")) %>%
  # Pivot longer to create columns for Subject and Evaluator
  pivot_longer(
    cols = everything(),
    names_to = c("Prefix", "Subject", "Evaluator"),
    names_sep = "_",
    values_to = "Sentiment"
  ) %>%
  select(-Prefix) # remove the redundant "Summary" prefix

ggplot(df_long, aes(x = Sentiment, fill = Evaluator)) +
  geom_bar(position = "dodge") +
  facet_wrap(~Subject) +
  scale_x_discrete(drop = FALSE) +
  labs(
    title = "Comparison of Summary Sentiment Ratings: Human vs. LLM",
    x = "Sentiment Rating", y = "Count"
  )



# Test Grouped bars ####
# Define the subjects corresponding to your sentiment columns
subjects <- c("SPD", "FDP", "die_Gruenen", "Olaf_Scholz", "Christian_Lindner", "Robert_Habeck")

# Reshape the data: for each subject, extract the human sentiment for summaries and the global summary consistency (human)
df_long <- lapply(subjects, function(subj) {
  sentiment_col <- paste0("Summary_", subj, "_human")
  consistency_col <- "Summary_Consistency_human"
  data.frame(
    Subject = subj,
    Sentiment = df_analysis[[sentiment_col]],
    Consistency = df_analysis[[consistency_col]]
  )
}) %>% bind_rows()

# Check the structure of the reshaped data (optional)
str(df_long)

# Create a grouped (dodge) bar chart with one facet per subject.
ggplot(df_long, aes(x = Sentiment, fill = Consistency)) +
  geom_bar(position = "dodge") +
  facet_wrap(~Subject) +
  labs(
    title = "Distribution of Summary Sentiment Ratings by Consistency (Human)",
    x = "Sentiment Rating",
    y = "Count"
  ) +
  scale_fill_discrete(name = "Summary Consistency (Human)")

# Dot/Point Plot assosiation ####
library(ggplot2)

# Suppose you have computed gamma values for each subject and stored them in a data frame:
gamma_df <- data.frame(
  Subject = c("SPD", "FDP", "die_Gruenen", "Olaf_Scholz", "Christian_Lindner", "Robert_Habeck"),
  Gamma = c(0.657, -0.060, 0.704, 0.227, -0.175, NA)
)

ggplot(gamma_df, aes(x = Gamma, y = Subject)) +
  geom_point(size = 3) +
  labs(
    title = "Goodman and Kruskal's Gamma by Subject",
    x = "Gamma Value", y = "Subject"
  ) +
  theme_minimal()
