# Boxplot UI
output$boxplot_tab <- renderUI({
	if(!is.null(tpms_data_table()) && !is.null(genes_data_table()) && !is.null(samples_data_table())){
		tagList(
			selectInput(
					inputId = "metadata",
					label = "Metadata",
					choices = c("Samples", "Genes")
			),
    		fluidRow(
    			conditionalPanel(
					condition = "input.metadata == 'Samples'",
    				column(6,
		    			uiOutput("metadata_sample_x")
		    		),
	    			# Y Metadata : log2(TPMs)
	    			column(6,
		    			uiOutput("metadata_sample_col")
		    		)
	    		),
	    		conditionalPanel(
					condition = "input.metadata == 'Genes'",
    				column(6,
		    			uiOutput("metadata_gene_x")
		    		),
	    			# Y Metadata : log2(TPMs)
	    			column(6,
		    			uiOutput("metadata_gene_col")
		    		)
	    		)
	    	),
	    	fluidRow(
	    		column(12,
		    		checkboxInput(
		    			inputId = "use_replicats",
		    			label = "Mean by replicats",
						value = FALSE
		    		)
		    	)
	    	),
	    	hr(),
	    	fluidRow(
	    		column(8,
			    	selectInput(
	        			inputId = "boxplot_ext",
	        			label = "Export format :",
	                	choices = c("PNG", "PDF", "SVG", "EPS"),
	                	width = "200px"
	    			)
	    		),
	    		column(4,
	    			br(),
			    	downloadButton(
			    		outputId = "boxplot",
			    		label = "Boxplot",
			    		width = "100%"
	    			)
	    		)
		    )
		)
	}
})

# Boxplot selected metadata
output$metadata_sample_x <- renderUI({
	samples_data <- samples_data_table()
	tagList(
		selectInput(
			inputId = "meta_sample_x",
			label = "X metadata",
			choices = c(colnames(samples_data)),
			width = "180px"
		)
	)
})
output$metadata_sample_col <- renderUI({
	samples_data <- samples_data_table()
	tagList(
		selectInput(
			inputId = "meta_sample_col",
			label = "Color by :",
			choices = subset(c(colnames(samples_data)), !(c(colnames(samples_data)) %in% input$meta_sample_x)),
			width = "180px"
		)
	)
})
output$metadata_gene_x <- renderUI({
	genes_data <- genes_data_table()
	genes_data_columns <- c(colnames(genes_data[colnames(genes_data) != "description"]))
	tagList(
		selectInput(
			inputId = "meta_gene_x",
			label = "X metadata :",
			choices = genes_data_columns,
			width = "180px"
		)
	)
})
output$metadata_gene_col <- renderUI({
	genes_data <- genes_data_table()
	genes_data_columns <- c(colnames(genes_data[colnames(genes_data) != "description"]))
	tagList(
		selectInput(
			inputId = "meta_gene_col",
			label = "Color by :",
			choices = subset(genes_data_columns, !(genes_data_columns %in% input$meta_gene_x)),
			width = "180px"
		)
	)
})

# Download boxplot
output$boxplot <- downloadHandler (		
	filename = function() {
		paste0("boxplot.", tolower(input$boxplot_ext))
	},
	content = function(file){
		final_table <- final_table()
		samples_data <- samples_data_table()

		boxplot_data<-final_table[!(colnames(final_table) %in% colnames(genes_data_table()))]

		if (input$use_replicats == TRUE) {
			# Make mean table from replicats
			rows_mean <- lapply(1:ncol(boxplot_data), function(i){
				rows_mean <- rowMeans(boxplot_data[,c(samples_data[i,"sample_id"], samples_data[i,"replicats"])])
			})
			names(rows_mean) <- samples_data[,"sample_id"]
			boxplot_data <- as.data.frame(list.cbind(rows_mean))		
		}

		if (input$metadata == "Genes") {
			# Table for genes metadata
			genes_for_boxplot <- cbind(final_table[input$meta_gene_x],final_table[input$meta_gene_col], boxplot_data)
			melted_data<-melt(genes_for_boxplot, id=c(as.character(input$meta_gene_x),as.character(input$meta_gene_col)))

			buildBoxplot(melted_data, file, ext = input$boxplot_ext, x = input$meta_gene_x , col = input$meta_gene_col)

		} else if (input$metadata == "Samples") {
			# Transpose table for samples metadata
			transpo_tpms <- data.frame(t(boxplot_data))
			factors <- sapply(transpo_tpms, is.factor)
			transpo_tpms[factors] <- lapply(transpo_tpms[factors], as.character)
			colnames(transpo_tpms) <- final_table[,1]

			samples_id <- rownames(transpo_tpms)
			samples_data <- samples_data[samples_data[["sample_id"]] %in% samples_id,]
			
			transpo_tpms <- data.frame(transpo_tpms, row.names=NULL)
			samples_for_boxplot <- cbind(samples_data[input$meta_sample_x],samples_data[input$meta_sample_col], transpo_tpms)
			melted_data <- melt(samples_for_boxplot, id=c(as.character(input$meta_sample_x), as.character(input$meta_sample_col)))

			buildBoxplot(melted_data, file, ext = input$boxplot_ext, x = input$meta_sample_x , col = input$meta_sample_col)	
		}
	}
)