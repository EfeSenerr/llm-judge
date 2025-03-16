# Libraries ####
library(irr)
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)

# krippendorfer alpha

# "data/processed/4th/XXX"
df_human <- read.csv("_OneDrive/_SyncStudium/WS24-25/Aligning GenAI/GenAI_Analysis/MASTER_human-eval.csv")
df_llm <- read.csv("_OneDrive/_SyncStudium/WS24-25/Aligning GenAI/GenAI_Analysis/MASTER_machine-eval.csv")
df_human_test <- read_excel("_OneDrive/_SyncStudium/WS24-25/Aligning GenAI/GenAI_Analysis/heval_duplicates_BENE-TEST.xlsx")

df_analysis <- merge(df_human, df_llm, by = "datapoint_new", suffixes = c("_human", "_llm"))

# human_cols <- names(df_analysis)[grepl("^(atc|smm)_.*_human$", names(df_analysis))]
# print(human_cols)

# kripp alpha for llm and heval ####
all_cols <- colnames(df_analysis)

# Extract unique evaluation aspects by removing the suffixes (_llm or _human)
aspects <- unique(sub("_(llm|human)$", "", all_cols))

alpha_results <- list()

for (aspect in aspects) {
  col_human <- paste0(aspect, "_human")
  col_llm   <- paste0(aspect, "_llm")
  
  if (all(c(col_human, col_llm) %in% all_cols)) {
    rating_matrix <- as.matrix(df_analysis[, c(col_human, col_llm)])
    # Transpose to get evaluators as rows, articles as columns: (2 raters / 80+ aspects)
    rating_matrix <- t(rating_matrix)
    
    alpha_value <- kripp.alpha(rating_matrix, method = "ordinal")
    alpha_results[[aspect]] <- alpha_value
  } else {
    message("Missing columns for aspect: ", aspect)
  }
}

print(alpha_results)


# visualization ####
alpha_values <- sapply(alpha_results, function(x) x$value)

alpha_df <- data.frame(
  Aspect = names(alpha_values),
  Alpha = as.numeric(alpha_values)
)

print(alpha_df)
getwd()
write.csv(alpha_df, "llm-human_krippAlpha.csv", row.names = FALSE)


alpha_df$AspectGroup <- factor(alpha_df$AspectGroup, 
                               levels = c("CoalitionBreakdown", "SPD", "Scholz", "FDP", "Lindner", "Greens", "Habeck"))


exclude_aspects <- c("source", "evaluator", "dataset", "smm_consistency", "datapoint") # "dataset", "datapoint" can be left in as 1 baseine
alpha_df<- alpha_df[alpha_df$Aspect != exclude_aspects, ]
alpha_df <- alpha_df %>%
  mutate(AspectGroup = case_when(
    grepl("coalition", Aspect)  ~ "CoalitionBreakdown",
    grepl("spd", Aspect) ~ "SPD",
    grepl("scholz", Aspect) ~ "Scholz",
    grepl("fdp", Aspect) ~ "FDP",
    grepl("lindner", Aspect) ~ "Lindner",
    grepl("green", Aspect) ~ "Greens",
    grepl("habeck", Aspect) ~ "Habeck"
  ))

# Define the manual mapping of groups to colors:
color_values <- c("CoalitionBreakdown" = "#a4a2a8",
                  "SPD"                = "#b46e9d",
                  "Scholz"             = "#7c1158",
                  "FDP"                = "#59dce8",
                  "Lindner"            = "#00b7c7",
                  "Greens"             = "#eca64b",
                  "Habeck"             = "#cc7400",
                  )

# Now plot using AspectGroup for fill, include a legend and fix the y-axis limits:
ggplot(alpha_df, aes(x = reorder(Aspect, Alpha), y = Alpha, fill = AspectGroup)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Krippendorff's Alpha for Human vs. LLM Evaluations",
    x = "Evaluation Aspect",
    y = "Krippendorff's Alpha",
    fill = "Evaluation Group"
  ) +
  scale_fill_manual(
    values = color_values, 
    labels = c("Coalition Breakdown", "SPD", "Scholz", "FDP", "Lindner", "Greens", "Habeck")) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_minimal() +   theme(panel.grid.major = element_blank(),
#                            panel.grid.minor = element_blank(),
                            panel.background = element_blank())

