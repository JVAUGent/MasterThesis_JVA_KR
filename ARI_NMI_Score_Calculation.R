library(readr)
library(dplyr)
library(aricode)

# ==============================================================================
# 1. FILE & COLUMN CONFIGURATION
# ==============================================================================

# Update this to match your exported file name (.csv or .tsv)
input_file <- "C:/Users/Jeroe/OneDrive - UGent/Thesis/Thesis/Results/DEA Cytoscape/HepG2/Trend/node_table.csv" 

# Target columns directly from your Cytoscape export
col_1 <- "__mclCluster"
col_2 <- "__leidenCluster"

# ==============================================================================
# 2. IMPORT & DATA CLEANING
# ==============================================================================

if (!file.exists(input_file)) {
  stop(sprintf("File '%s' not found. Please check your working directory.", input_file))
}

# read_delim auto-detects whether your file is comma-, tab-, or semicolon-separated
raw_data <- read_delim(input_file, show_col_types = FALSE)

# Verify required columns exist
if (!all(c(col_1, col_2) %in% colnames(raw_data))) {
  stop("Specified cluster columns were not found in the imported file.")
}

# Extract only the necessary columns and scrub any missing values
eval_data <- raw_data %>%
  select(all_of(c(col_1, col_2))) %>%
  filter(!is.na(.data[[col_1]]) & !is.na(.data[[col_2]])) %>%
  mutate(
    v1 = as.character(.data[[col_1]]),
    v2 = as.character(.data[[col_2]])
  )

# ==============================================================================
# 3. SCORE CALCULATION & REPORTING
# ==============================================================================

ari_score <- ARI(eval_data$v1, eval_data$v2)
nmi_score <- NMI(eval_data$v1, eval_data$v2)

# Build summary table
results <- data.frame(
  Comparison = paste(col_1, "vs", col_2),
  Nodes_Evaluated = nrow(eval_data),
  ARI = round(ari_score, 4),
  NMI = round(nmi_score, 4)
)

cat("\n===================================================\n")
cat("       CYTOSCAPE CLUSTER VALIDATION METRICS        \n")
cat("===================================================\n")
print(results, row.names = FALSE)
cat("===================================================\n\n")

# Optional: Export summary table to file
write_tsv(results, "C:/Users/Jeroe/OneDrive - UGent/Thesis/Thesis/Results/DEA Cytoscape/HepG2/Trend/mcl_vs_leiden_validation_summary.tsv")

