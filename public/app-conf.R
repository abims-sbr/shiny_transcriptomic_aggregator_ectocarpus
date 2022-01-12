### Shiny app configurations ###

# Name of the project
project <- "Ectocarpus"

# Instance status ("public" OR "private")
instance_tag <- "public"

# Columns to ignore in metadata file
gene_col_blacklist <- c("description", "content")
sample_col_blacklist <- c("replicats")

# Project Files
tpms_input <- "test-data/TPMs-1_test.csv"
genes_data_input <- "test-data/genes_data_test_duplicated.csv"
samples_data_input <- "test-data/samples_data_test.csv"
