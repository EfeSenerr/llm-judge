# Data

## Collection

### Articles scraped

(JSON / CSV)

### LLM-generated summaries (GPT-4o)

(CSV)

### LLM-generated evaluations (Llama)

- Old prompt `evaluation_results_aggregated_2.csv`
  - = `llm_evalution_2.csv`
- New prompt: `evaluation_results_aggregated_0503.csv`
  - = `llm_evaluation_updated_prompt.csv`
  - ✅ processed: `llm_evaluation.csv`
    - (added col 1 "ID-Global",
    - dropped col 2 "Article File Name",
    - dropped 3 rows)

### Human-generated evaluations

- Batch #1: `Human_Evaluation_Results.csv`
  - = `human_eval_1_all.csv`
  - processed: `human_evalutation.csv`
    - (dropped col 1 "Evaluator Name",
    - dropped articles_number 320, 39, 86, 153, 42, 132, 39, 21, 64, 7)
- Batch #2: `Human_Evaluation_Results_differences.csv`
  - = `human_eval_2_all.csv`
