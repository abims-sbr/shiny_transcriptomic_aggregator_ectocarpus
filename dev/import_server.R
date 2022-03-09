# Get data from files
observeEvent(input$import_files, {
	# Genes
	if(!is.null(input$genes_data_file)){
		genes_data_table(getDataFrameFromFile(input$genes_data_file$datapath))	
	}
	# Samples Data
	if(!is.null(input$samples_data_file)){
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
	}
	# TPMS
	if(!is.null(input$tpms_file)){
		tpms_data_table(getDataFrameFromFile(input$tpms_file$datapath))	
	}
})

# Reset Files
observeEvent(input$reset_files, {

	if(!is.null(genes_data_input)){
    	genes_data_table(getDataFrameFromFile(genes_data_input))
    } else {
    	genes_data_table(NULL)
    }
    reset('genes_data_file')

    if(!is.null(samples_data_input)){
		samples_data_file <- getDataFrameFromFile(samples_data_input)
		if("private" %in% tolower(colnames(samples_data_file))){
			# If public instance, took of private sample data
			if(instance_tag == "public"){
				samples_data_file <- samples_data_file[toupper(samples_data_file[,"private"]) == "FALSE",]
			}
			# Remove private column from the table
			samples_data_file["private"] <- NULL
		}
		samples_data_table(samples_data_file)
	} else {
		samples_data_table(NULL)
	}
	reset('samples_data_file')

	if(!is.null(tpms_input)){
		tpms_data_table(getDataFrameFromFile(tpms_input))
    } else {
    	tpms_data_table(NULL)
    }
    reset('tpms_file')

})