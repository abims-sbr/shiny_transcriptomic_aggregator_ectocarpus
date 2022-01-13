# Samples Filters
output$samples_filters <- renderUI({
	samples_data <- samples_data_table()
	tagList(
		bsCollapsePanel(
			title = "Samples Metadata Filters",
			fluidRow(
		      	lapply(1:ncol(samples_data), function(i) {
		      		if (!(tolower(colnames(samples_data)[i]) %in% sample_col_blacklist)) {
		      			if (tolower(colnames(samples_data)[i]) != "sample_id"){
				      		column(4,
								selectInput(
				        			inputId = colnames(samples_data)[i],
				        			label = paste0(capitalize(gsub("_", " ", colnames(samples_data)[i])), " :"),
				                	choices = c("All", sort(unique(unlist(strsplit(as.character(samples_data[,i]), ","))))),
				                	width = "200px",
				                	selected = "All"
				            	)
				      		)
				      	}
			      	}
		      	})
			),
			fluidRow(
				column(9),
				column(3,
					actionButton(
						inputId = "reset_samples_values",
						label = "Reset filters" 
					)
				)
			)				
		)
	)
})
# Reset Samples Filters Values
observeEvent(input$reset_samples_values, {
	samples_data <- samples_data_table()
	lapply(1:ncol(samples_data), function(i){
		if (!(tolower(colnames(samples_data)[i]) %in% sample_col_blacklist)) {
			if (tolower(colnames(samples_data)[i]) != "sample_id") {
	        	updateSelectInput(
	        		session = session, 
	        		inputId = colnames(samples_data)[i],
	        		selected = "All"
	        	)
	        }
	    }			
	})
})

## Genes Filters
output$genes_filters <- renderUI({
	genes_data_table <- genes_data_table()
	tagList(
		bsCollapsePanel(
			title = "Genes Metadata Filters",
			fluidRow(
				lapply(1:ncol(genes_data_table), function(i) {
					if (tolower(colnames(genes_data_table)[i]) != "description") {
						column(4,
							selectInput(
			        			inputId = colnames(genes_data_table)[i],
			        			label = paste0(capitalize(gsub("_", " ", colnames(genes_data_table)[i])), " :"),
			                	choices = c("All", sort(unique(unlist(strsplit(as.character(genes_data_table[,i]), ","))))),
			                	selected = "All",
			                	width = "200px"				                
							)
						)
					}
				})
			),
			fluidRow(
				column(9),
				column(3,
					actionButton(
						inputId = "reset_genes_values",
						label = "Reset filters" 
					)
				)
			)
		)
	)
})
# Reset Genes Filters Values
observeEvent(input$reset_genes_values, {
	genes_data_table <- genes_data_table()
	lapply(1:ncol(genes_data_table), function(i){
    	updateSelectInput(
    		session = session, 
    		inputId = colnames(genes_data_table)[i],
    		selected = "All"
    	)
	})
})

# Sample List
output$samples_id_filter <- renderUI({
	samples_data <- samples_data_table()
	wellPanel(
		style = paste0("overflow-y:auto; max-height: 650px"),
		HTML("<h4><b>Sample id :</h4></b>"),
		hr(),
		actionButton(
			inputId = "select_all",
			label = "Select all"
		),
		actionButton(
			inputId = "unselect_all",
			label = "Unselect all"
		),
		hr(),
		br(),
    	checkboxGroupInput(
    		inputId = "sample_id",
    		label = NULL,
           	choices = as.character(samples_data[,1][order(nchar(samples_data[,1]), samples_data[,1])]),
           	selected = as.character(samples_data[,1][order(nchar(samples_data[,1]), samples_data[,1])]),
        	width = "200px"
		)
	)
})
# Update Samples List (Select All)
observeEvent(input$select_all, {
	samples_data <- samples_data_table()
	updateCheckboxGroupInput(
		session = session,
		inputId = "sample_id",
      	choices = as.character(samples_data[,1][order(nchar(samples_data[,1]), samples_data[,1])]),
      	selected = as.character(samples_data[,1][order(nchar(samples_data[,1]), samples_data[,1])])
	)
})
# Update Samples List (Unselect All)
observeEvent(input$unselect_all, {
	samples_data <- samples_data_table()
	updateCheckboxGroupInput(
		session = session,
		inputId = "sample_id",
      	choices = as.character(samples_data[,1][order(nchar(samples_data[,1]), samples_data[,1])]),
      	selected = character(0)
	)
})

# DataTable
output$table <- renderDataTable({
	samples_metadata <- samples_data_table()
	genes_data_table <- genes_data_table()
	tpms_data <- tpms_data_table()

	withProgress(message = 'Making table', value = 0, {
		incProgress(1/5, detail = paste("Apply samples filters"))
		# Apply samples filters
		samples_data <- lapply(1:ncol(samples_metadata), function(i){
			if (!(tolower(colnames(samples_metadata)[i]) %in% sample_col_blacklist)) {
				if (tolower(colnames(samples_metadata)[i]) == "sample_id") {
					samples_data <- subset(samples_metadata, samples_metadata[,i] %in% input$sample_id)
				} else {
					if (input[[colnames(samples_metadata)[i]]] != "All") {
					samples_data <- samples_metadata[samples_metadata[,i] %like% input[[colnames(samples_metadata)[i]]],]	
					}
				}
			}
		})
		samples_data <- Reduce(intersect, Filter(Negate(is.null), samples_data))

		incProgress(1/5, detail = paste("Update other samples filters"))				
		# Update other samples filters
		lapply(1:ncol(samples_data), function(i){
			if (!(tolower(colnames(samples_data)[i]) %in% sample_col_blacklist)) {
				if (tolower(colnames(samples_data)[i]) != "sample_id") {
					if (input[[colnames(samples_data)[i]]] == "All") {
					    updateSelectInput(
					       	session = session, 
					       	inputId = colnames(samples_data)[i],
					       	choices = c("All", sort(unique(unlist(strsplit(as.character(samples_data[,i]), ","))))),
					       	selected = "All"
			    		)
					}
				}
			}
		})

    	# Merge TPMS and genes metadata files according to filters
	    gene_id <- tpms_data[1] # Supposing gene_id is the 1st column
	    filtered_tpms_data <- cbind(gene_id, tpms_data[colnames(tpms_data) %in% samples_data[,1]]) # Supposing sample_id is the 1st column
	    merged_genes_tpms <- merge(genes_data_table, filtered_tpms_data, by=colnames(genes_data_table[1]))
	    final_table <- merged_genes_tpms

		incProgress(1/5, detail = paste("Apply genes filters"))
	    # Apply genes filters
	    final_table <- lapply(1:ncol(genes_data_table), function(i){
			if (!(tolower(colnames(genes_data_table)[i]) %in% gene_col_blacklist)) {
				if (input[[colnames(genes_data_table)[i]]] != "All") {
					final_table <- merged_genes_tpms[merged_genes_tpms[,i] %like% input[[colnames(genes_data_table)[i]]],]
				} else {
					final_table <- merged_genes_tpms
				}
			}
		})
		final_table <- Reduce(intersect, Filter(Negate(is.null), final_table))

		incProgress(1/5, detail = paste("Update other genes filters"))				
		# Update other genes filters
		lapply(1:ncol(genes_data_table), function(i){
			if (!(tolower(colnames(genes_data_table)[i]) %in% gene_col_blacklist)) {
		    	if (input[[colnames(genes_data_table)[i]]] == "All") {
				    updateSelectInput(
				       	session = session, 
				       	inputId = colnames(final_table)[i],
				       	choices = c("All", sort(unique(unlist(strsplit(as.character(final_table[,i]), ","))))),
				       	selected = "All"
		    		)
				}
			}
		})

    	# Update table by gene list file
    	genes_list <- genes_list()
    	if (length(genes_list) > 0) {
    		final_table <- subset(final_table, final_table[,1] %in% genes_list[[1]])	
    	}

	    incProgress(1/5, detail = paste("Building table"))
	})

    final_table(final_table)
    final_table
},
	rownames = FALSE,
    extensions = c("Buttons", "FixedHeader"),
    options = list(
    	dom = 'Blfrtip',
    	order = list(0, 'asc'),
    	fixedHeader = TRUE,
    	pageLength = 15,
			lengthMenu = list(c(10, 15, 25, 50, 100, 200, -1),list("10", "15", "25","50","100","200","all")),
        scrollX = TRUE,
        buttons = list(
        	'copy', 'csv', 'excel', 'print',
        	list(extend = "pdf",
        		pageSize="A4",
        		filename = "table",
        		header=TRUE,
        		title=NULL,
        		orientation="landscape"
        	)
        )
    )
)