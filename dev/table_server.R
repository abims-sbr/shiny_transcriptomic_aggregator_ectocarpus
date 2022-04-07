# Create a vector with all the sample data table columns except blacklist ones in conf.R
samples_inputs <- reactive ({
	samples_data <- samples_data_table()
	samples_inputs <- unlist(lapply(1:ncol(samples_data), function(i){
		if (!(tolower(colnames(samples_data)[i]) %in% sample_col_blacklist)){
			samples_inputs <- colnames(samples_data)[i]
		}
	}))
})

# Build samples filters
output$samples_filters <- renderUI({
	tagList(
		bsCollapsePanel(
			title = "Samples Metadata Filters",
			fluidRow(
				# Create one filter by value in samples inputs
		      	lapply(1:length(samples_inputs()), function(i) {
		      		if (samples_inputs()[i] != "sample_id") {
			      		column(4,
							selectizeInput(
			        			inputId = samples_inputs()[i],
			        			label = paste0(capitalize(gsub("_", " ", samples_inputs()[i])), " :"),
			                	choices = c("All", sort(unique(unlist(strsplit(as.character(samples_data_table()[,samples_inputs()[i]]), ","))))),
			                	selected = "All",
			                	multiple = FALSE
			            	)
			      		)
			      	}
		      	})
			),
			fluidRow(
				column(3,
					actionButton(
						inputId = "reset_samples_values",
						label = "Reset sample filters" 
					)
				),
				column(9)
			)				
		)
	)
})
# Reset samples filters values
observeEvent(input$reset_samples_values, {
	lapply(1:length(samples_inputs()), function(i){
		if (samples_inputs()[i] != "sample_id") {
			# Update all the samples filters values to "All"
	    	updateSelectizeInput(
	    		session = session, 
	    		inputId = samples_inputs()[i],
	    		selected = "All"
	    	)
	    }
	})
})

# Create a vector with all the genes data table columns except ones in conf.R
genes_inputs <- reactive ({
	genes_data <- genes_data_table()
	genes_inputs <- unlist(lapply(1:ncol(genes_data), function(i){
		if (!(tolower(colnames(genes_data)[i]) %in% gene_col_blacklist)){
			genes_inputs <- colnames(genes_data)[i]
		}
	}))
})

# Build genes filters
output$genes_filters <- renderUI({
	tagList(
		bsCollapsePanel(
			title = "Genes Metadata Filters",
			fluidRow(
				column(12,
					textInput(
	        			inputId = "gene_list",
	        			label = "Gene list",
	                	value = "",
	                	placeholder = "Comma-separated list of genes [gene1,gene2,etc] (leave blank to not use)",
	                	width = "100%"
					)
				)
			),
			fluidRow(
				# Create one filter by genes data column except blacklist ones in conf.R
				lapply(1:length(genes_inputs()), function(i) {
					if (genes_inputs()[i] != "gene_id") {
						column(4,
							selectizeInput(
			        			inputId = genes_inputs()[i],
			        			label = paste0(capitalize(gsub("_", " ", genes_inputs()[i])), " :"),
			                	choices = c("All", sort(unique(unlist(strsplit(as.character(genes_data_table()[,genes_inputs()[i]]), ","))))),
			                	selected = "All",
			                	multiple = FALSE
							)
						)
					}
				})
			),
			fluidRow(
				column(3,
					actionButton(
						inputId = "reset_genes_values",
						label = "Reset gene filters" 
					)
				),				
				column(9)
			)
		)
	)
})
# Reset genes filters values
observeEvent(input$reset_genes_values, {
	updateTextInput(
    		session = session, 
    		inputId = "gene_list",
    		value = ""
    )
	lapply(1:length(genes_inputs()), function(i){
		# Update all the genes filters values to "All"
    	updateSelectizeInput(
    		session = session, 
    		inputId = genes_inputs()[i],
    		selected = "All"
    	)
	})
})

# Reset ALL filters
observeEvent(input$reset_all_filters, {
	lapply(1:length(samples_inputs()), function(i){
		if (samples_inputs()[i] != "sample_id") {
			# Update all the samples filters values to "All"
	    	updateSelectizeInput(
	    		session = session, 
	    		inputId = samples_inputs()[i],
	    		selected = "All"
	    	)
	    }
	})
	updateTextInput(
    		session = session, 
    		inputId = "gene_list",
    		value = ""
    )
	lapply(1:length(genes_inputs()), function(i){
		# Update all the genes filters values to "All"
    	updateSelectizeInput(
    		session = session, 
    		inputId = genes_inputs()[i],
    		selected = "All"
    	)
	})
	updateCheckboxGroupInput(
		session = session,
		inputId = "sample_id",
      	choices = as.character(samples_data_table()[,1][order(samples_data_table()[,1])]),
      	selected = as.character(samples_data_table()[,1][order(samples_data_table()[,1])])
	)
})

# Build samples list
output$samples_id_filter <- renderUI({
	tagList(
		bsCollapse(
			open = "Sample id",
			bsCollapsePanel(
				title = "Sample id",
				fluidRow(
					actionButton(
						inputId = "select_all",
						label = "Select all"
					),
					actionButton(
						inputId = "unselect_all",
						label = "Unselect all"
					)
				),
				hr(),
				br(),
		    	checkboxGroupInput(
		    		inputId = "sample_id",
		    		label = NULL,
		           	choices = as.character(samples_data_table()[,1][order(samples_data_table()[,1])]),
		           	selected = as.character(samples_data_table()[,1][order(samples_data_table()[,1])]),
		        	width = "200px"
				)
		    )
		)
	)	
})
# Update samples list (Select all)
observeEvent(input$select_all, {
	updateCheckboxGroupInput(
		session = session,
		inputId = "sample_id",
      	choices = as.character(samples_data_table()[,1][order(samples_data_table()[,1])]),
      	selected = as.character(samples_data_table()[,1][order(samples_data_table()[,1])])
	)
})
# Update samples list (Unselect all)
observeEvent(input$unselect_all, {
	updateCheckboxGroupInput(
		session = session,
		inputId = "sample_id",
		choices = as.character(samples_data_table()[,1][order(samples_data_table()[,1])]),
      	selected = character(0)
	)
})

# Build the table
output$table <- renderDT({
	final_table()
},
	rownames = FALSE,
    extensions = c("Buttons"),
    options = list(
    	dom = 'Blfrtip',
    	order = list(0, 'asc'),
    	pageLength = 10,
			lengthMenu = list(c(10, 15, 25, 50, 100, 200, -1),list("10", "15", "25","50","100","200","all")),
        scrollX = TRUE,
        buttons = list(
        	'copy', 'print',
        	list(extend = "csv",pageSize="A4",filename = "table",header=TRUE,title=NULL,orientation="landscape"),
        	list(extend = "excel",pageSize="A4",filename = "table",header=TRUE,title=NULL,orientation="landscape"),
        	list(extend = "pdf",pageSize="A4",filename = "table",header=TRUE,title=NULL,orientation="landscape")
        )
    )
)

# Apply filters on data
observeEvent(input$apply_filters, {

	withProgress(message = 'Building table', value = 0, {

		incProgress(1/3, detail = paste("Apply samples filters"))
		# Browse samples filters and apply them
		samples_data <- lapply(1:length(samples_inputs()), function(i){
			# Remove the samples that are unchecked in sample list
			if (samples_inputs()[i] == "sample_id") {
				samples_data <- subset(samples_data_table(), samples_data_table()[,i] %in% input[[samples_inputs()[i]]])
			} else {
				# Apply samples filters on the samples data table 
				if (input[[samples_inputs()[i]]] != "All") {
					samples_data <- subset(samples_data_table(), samples_data_table()[,i] == input[[samples_inputs()[i]]])
				}
			}
		})
		# Reduce take a list of tables filtered by columns and merge them keeping only the similarities between each table
		# Filter and Negate remove NULL tables that are not filtered
		samples_data <- Reduce(merge, Filter(Negate(is.null), samples_data))
					
	    # Filter TPMs data table by samples respecting samples filters
		tpms_data <- cbind(tpms_data_table()["gene_id"], tpms_data_table()[colnames(tpms_data_table()) %in% samples_data[,"sample_id"]])
		# Merge filtered TPMS and genes metadata files
		new_table <- merge(genes_data_table(), tpms_data, by=colnames(genes_data_table()["gene_id"]))

		incProgress(1/3, detail = paste("Apply genes filters"))
		# Browse genes filters and apply them
	    filtered_table <- lapply(1:length(genes_inputs()), function(i){
	    	if (genes_inputs()[i] == "gene_id") {
	    		if (input$gene_list != "") {
	    			filtered_table <- subset(new_table, new_table[,i] %in% unlist(strsplit(input$gene_list, ",")))
	    		} else {
	    			filtered_table <- new_table
	    		}
	    	} else {
				if (input[[genes_inputs()[i]]] != "All") {
					filtered_table <- subset(new_table, new_table[,i] == input[[genes_inputs()[i]]])
				} else {
					filtered_table <- new_table
				}
			}
		})
		filtered_table <- Reduce(merge, Filter(Negate(is.null), filtered_table))
		incProgress(1/3, detail = paste("Render table"))
		final_table(filtered_table)
		
	})
})