### Environment
# getwd()
# setwd("")
# Sys.getenv("VAR1")
# rm(list = ls())  # clean global env
# install.packages("")

rm(list = ls())

library(tidyverse)
library(openxlsx)
library(irr)
library(ggplot2)
library(svglite)

### IMPORT
df_icr <- read.csv(
    "output/alpha_results_krippICR.csv",
    header = TRUE,
    sep = ",",
    na.strings = c("", "NA"),
    fill = TRUE, # fill empty cells with NA
    stringsAsFactors = FALSE
)
df_llm <- read.csv(
    "output/alpha_results_krippLLM.csv",
    header = TRUE,
    sep = ",",
    na.strings = c("", "NA"),
    fill = TRUE, # fill empty cells with NA
    stringsAsFactors = FALSE
)

### VISUALIZATIONS

# df_icr_atc <- df_icr %>%
#     filter(str_detect(variable, "atc_"))

# df_icr_smm <- df_icr %>%
#     filter(variable != "smm_consistency") %>%
#     filter(str_detect(variable, "smm_"))


# Bar chart
# ggplot(df_icr_atc, aes(x = variable, y = value, fill = variable)) +
#     geom_bar(stat = "identity", width = 0.6) +
#     # scale_fill_brewer(palette = "Blues") +
#     labs(
#         title = "Krippendorff's Alpha by Variable",
#         x = "Variable",
#         y = "Krippendorff's Alpha"
#     ) +
#     geom_text(aes(label = round(value, 3)), vjust = -0.5) +
#     ylim(0, 1) +
#     theme_minimal() +
#     theme(legend.position = "none")


# Lollipop
# ggplot(df_icr_atc, aes(x = variable, y = value, color = variable)) +
#     geom_segment(aes(x = variable, xend = variable, y = 0, yend = value),
#         size = 1.5
#     ) +
#     geom_point(size = 5) +
#     geom_text(aes(label = round(value, 3)), vjust = -1) +

#     # scale_color_brewer(palette = "Set2") +
#     labs(
#         title = "Krippendorff's Alpha by Variable",
#         x = "Variable",
#         y = "Krippendorff's Alpha"
#     ) +
#     ylim(0, 1) +
#     theme_minimal() +
#     theme(legend.position = "none")


# # aggregate
# ggplot(df_icr, aes(y = value)) +
#     geom_boxplot(fill = "lightblue") +
#     theme_minimal() +
#     labs(
#         title = "Boxplot of Values",
#         y = "Value"
#     )


# BOXPLOTS

# for ICR
plot_data <- df_icr %>%
    filter(str_detect(variable, "^smm_|^atc_")) %>%
    filter(variable != "smm_consistency") %>%
    mutate(group = case_when(
        str_detect(variable, "^smm_") ~ "Summary",
        str_detect(variable, "^atc_") ~ "Article",
        TRUE ~ NA_character_
    ))

boxp <- ggplot(plot_data, aes(x = group, y = value)) +
    geom_boxplot(outlier.size = 3) +
    scale_y_continuous(limits = c(NA, 1)) + # min, max
    # stat_boxplot(geom = "errorbar", width = 0.15) +
    scale_fill_grey(start = 0.6, end = 0.2) +
    # scale_fill_brewer(palette = "Set1") +
    labs(
        title = "Krippendorff's Alpha: Human vs Human,\nby Document Type\n(Intercoder Reliability)",
        x = "",
        y = "K. Alpha"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(
            hjust = 0.5, # Center title (0.5 = center)
            size = 16,
            face = "bold"
        ),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        legend.position = "none"
    )

ggsave("output/plot_alpha_irc_boxp.png", plot = boxp, width = 10, height = 7, units = "in", dpi = 300, bg = "white")


# for LLM

plot_data <- df_llm %>%
    filter(str_detect(variable, "^smm_|^atc_")) %>%
    filter(variable != "smm_consistency") %>%
    mutate(group = case_when(
        str_detect(variable, "^smm_") ~ "Summary",
        str_detect(variable, "^atc_") ~ "Article",
        TRUE ~ NA_character_
    ))

boxp <- ggplot(plot_data, aes(x = group, y = value)) +
    geom_boxplot(outlier.size = 3) +
    scale_y_continuous(limits = c(NA, 1)) + # min, max
    # stat_boxplot(geom = "errorbar", width = 0.15) +
    scale_fill_grey(start = 0.6, end = 0.2) +
    # scale_fill_brewer(palette = "Set1") +
    labs(
        title = "Krippendorff's Alpha: LLM vs Human,\nby Document Type",
        x = "",
        y = "K. Alpha"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(
            hjust = 0.5, # Center title (0.5 = center)
            size = 16,
            face = "bold"
        ),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        legend.position = "none"
    )

ggsave("output/plot_alpha_llm_boxp.png", plot = boxp, width = 10, height = 7, units = "in", dpi = 300, bg = "white")
