### ALLOCATE
evaluator_names <- c("Ayten", "Hannah", "Monika", "Charlotte", "Jannis", "Bene", "Henri", "Efe", "Sophia", "Tim")
# "Ayten"*15, "Hannah"*8, "Monika"*8, "Charlotte", "Jannis", "Bene", "Henri", "Efe", "Sophia"*6, "Tim"

# Create vector for each name containing respective datapoints
for (name in evaluator_names) {
    assign(name, heval_short %>%
        filter(evaluator == name) %>%
        pull(datapoint_new))
}
rm(name) # useless artifact

# Subtract name vectors from datapoint_vector
#
#   Ayten
datapoint_vector <- heval_short %>% pull(datapoint_new)
datapoint_vector <- setdiff(datapoint_vector, Ayten)
Ayten_3rd <- datapoint_vector %>%
    sample(15)

#   Hannah
datapoint_vector <- heval_short %>% pull(datapoint_new)
datapoint_vector <- setdiff(datapoint_vector, Ayten_3rd)
datapoint_vector <- setdiff(datapoint_vector, Hannah)
Hannah_3rd <- datapoint_vector %>%
    sample(8)

#  Monika
datapoint_vector <- heval_short %>% pull(datapoint_new)
datapoint_vector <- setdiff(datapoint_vector, Ayten_3rd)
datapoint_vector <- setdiff(datapoint_vector, Hannah_3rd)
datapoint_vector <- setdiff(datapoint_vector, Monika)
Monika_3rd <- datapoint_vector %>%
    sample(8)

#  Sophia
datapoint_vector <- heval_short %>% pull(datapoint_new)
datapoint_vector <- setdiff(datapoint_vector, Ayten_3rd)
datapoint_vector <- setdiff(datapoint_vector, Hannah_3rd)
datapoint_vector <- setdiff(datapoint_vector, Monika_3rd)
datapoint_vector <- setdiff(datapoint_vector, Sophia)
Sophia_3rd <- datapoint_vector %>%
    sample(6)

# Clean
# datapoint_vector should be = 41 or [heval_short]-allocated
rm(list = evaluator_names)
# rm(heval_short)

### PREP ALLOCATE OUTPUT
#   filter df for rows where datapoint_new is in name vector

heval_duplicates <- heval_duplicates %>%
    mutate(source = "heval3") %>%
    add_row(!!!setNames(as.list(heval_labels), names(heval_duplicates)), .before = 1) # insert labels row

Ayten_3rd_df <- heval %>%
    filter(datapoint_new %in% Ayten_3rd) %>%
    mutate(source = "heval3") %>% # set source name
    mutate(evaluator = "Ayten") # set evaluator name
Ayten_3rd_df <- Ayten_3rd_df %>%
    mutate(across(-c(source, evaluator, dataset, datapoint, datapoint_new), ~NA)) %>% # delete all sentiment values
    add_row(!!!setNames(as.list(heval_labels), names(Ayten_3rd_df)), .before = 1) # insert labels row

Hannah_3rd_df <- heval %>%
    filter(datapoint_new %in% Hannah_3rd) %>%
    mutate(source = "heval3") %>% # set source name
    mutate(evaluator = "Hannah") # set evaluator name
Hannah_3rd_df <- Hannah_3rd_df %>%
    mutate(across(-c(source, evaluator, dataset, datapoint, datapoint_new), ~NA)) %>% # delete all sentiment values
    add_row(!!!setNames(as.list(heval_labels), names(Hannah_3rd_df)), .before = 1) # insert labels row

Monika_3rd_df <- heval %>%
    filter(datapoint_new %in% Monika_3rd) %>%
    mutate(source = "heval3") %>% # set source name
    mutate(evaluator = "Monika") # set evaluator name
Monika_3rd_df <- Monika_3rd_df %>%
    mutate(across(-c(source, evaluator, dataset, datapoint, datapoint_new), ~NA)) %>% # delete all sentiment values
    add_row(!!!setNames(as.list(heval_labels), names(Monika_3rd_df)), .before = 1) # insert labels row

Sophia_3rd_df <- heval %>%
    filter(datapoint_new %in% Sophia_3rd) %>%
    mutate(source = "heval3") %>% # set source name
    mutate(evaluator = "Sophia") # set evaluator name
Sophia_3rd_df <- Sophia_3rd_df %>%
    mutate(across(-c(source, evaluator, dataset, datapoint, datapoint_new), ~NA)) %>% # delete all sentiment values
    add_row(!!!setNames(as.list(heval_labels), names(Sophia_3rd_df)), .before = 1) # insert labels row


### SAVE DATASET

# Allocated evals
write.xlsx(heval_duplicates,
    file = "data/allocate/heval_duplicates.xlsx",
    sheetName = "heval3_dupl",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.xlsx(Ayten_3rd_df,
    file = "data/allocate/Ayten3rd.xlsx",
    sheetName = "Ayten_3rd_df",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.xlsx(Hannah_3rd_df,
    file = "data/allocate/Hannah3rd.xlsx",
    sheetName = "Hannah_3rd_df",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.xlsx(Monika_3rd_df,
    file = "data/allocate/Monika3rd.xlsx",
    sheetName = "Monika_3rd_df",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.xlsx(Sophia_3rd_df,
    file = "data/allocate/Sophia3rd.xlsx",
    sheetName = "Sophia_3rd_df",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
