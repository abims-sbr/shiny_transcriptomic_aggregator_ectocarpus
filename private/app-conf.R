### Shiny app configurations ###

# Name of the project
project <- "Ectocarpus"

# Instance status
instance_tag <- "private" 	#c("public", "private")

# Columns to ignore in genes metadata file
column_blacklist <- c("description", "content")

# Project Files
tpms_input <- "real-data/TPMs.csv"
genes_data_input <- "real-data/Genes_data.csv"
samples_data_input <- "real-data/Samples_data.csv"