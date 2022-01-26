# Get data from files
# TPMS
observeEvent(input$tpms_file, {
	tpms_data_table(getDataFrameFromFile(input$tpms_file$datapath))
})
# Genes Data
observeEvent(input$genes_data_file, {
	genes_data_file<-getDataFrameFromFile(input$genes_data_file$datapath)
	genes_data_table(merge_duplicated_data(genes_data_file))
})
# Samples Data
observeEvent(input$samples_data_file, {
	samples_data_file <- getDataFrameFromFile(input$samples_data_file$datapath)
	if("private" %in% tolower(colnames(samples_data_file))){
		# If public instance, took of private sample data
		if(instance_tag == "public"){
			samples_data_file <- samples_data_file[toupper(samples_data_file[,"private"]) == "FALSE",]
		}
		# Remove private column from the table
		samples_data_file["private"] <- NULL
	}
	samples_data_table(samples_data_file)   	
})

# Gene List (Optional)
observeEvent(input$genes_list_file, {
	genes_list(c(read.csv(input$genes_list_file$datapath, header=FALSE, sep="\t", stringsAsFactors=FALSE, check.names=FALSE)))
})

# Reset Files
observeEvent(input$reset_files, {
	tpms_data_table(NULL)
	genes_data_table(NULL)
	samples_data_table(NULL)
	genes_list(NULL)
})