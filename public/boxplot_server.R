# Boxplot selected metadata
output$metadata_sample_x <- renderUI({
	tagList(
		selectInput(
			inputId = "meta_sample_x",
			label = "X metadata",
			choices = samples_inputs()
		)
	)
})
output$metadata_sample_col <- renderUI({
	tagList(
		selectInput(
			inputId = "meta_sample_col",
			label = "Color by :",
			choices = subset(samples_inputs(), !(samples_inputs() %in% input$meta_sample_x))
		)
	)
})
output$metadata_gene_x <- renderUI({
	tagList(
		selectInput(
			inputId = "meta_gene_x",
			label = "X metadata :",
			choices = genes_inputs()
		)
	)
})
output$metadata_gene_col <- renderUI({
	tagList(
		selectInput(
			inputId = "meta_gene_col",
			label = "Color by :",
			choices = subset(genes_inputs(), !(genes_inputs() %in% input$meta_gene_x))
		)
	)
})

# Create graph and download button after clicking on visualize button
observeEvent(input$build_boxplot, {
	output$boxplot_visualize <- renderUI({
		box(
			title = "Boxplot",
			status = "primary",
			solidHeader = TRUE,
			width = 12,
			fluidRow(
				plotOutput(
					outputId = "boxplot"
				)
			),
			fluidRow(
				column(8),
				column(2,
			    	selectInput(
	        			inputId = "boxplot_ext",
    	    			label = "Download format :",
        	        	choices = c("PNG", "PDF", "SVG", "EPS"),
            	    	width = "200px"
					)
				),
				column(2,
					br(),
					downloadButton(
			    		outputId = "boxplot_file",
			    		label = "Download",
			    		class = "btn btn-primary",
		    			width = "100%"
    				)
    			)
			)
		)
	})
})

# Plot output
output$boxplot <- renderPlot({
	print(boxplot())
})

# Download boxplot
output$boxplot_file <- downloadHandler (		
	filename = function() {
		paste0("boxplot.", tolower(input$boxplot_ext))
	},
	content = function(file){
		if (input$boxplot_ext == "PNG"){
			png(file, width = 1500, height = 1000)
		} else if (input$boxplot_ext == "PDF") {
			pdf(file, width = 1500, height = 1000)
		} else if (input$boxplot_ext == "SVG") {
			svg(file, width = 15, height = 10)
		} else if (input$boxplot_ext == "EPS") {
			setEPS()
			postscript(file)
		}
		print(boxplot())
		dev.off()
	}
)

# Initialize boxplot variable
boxplot <- reactiveVal()
# Build the plot clicking on the visualize button
observeEvent(input$build_boxplot, {
	# TODO : Check the code, mainly use_replicats
	boxplot_data <- final_table()[!(colnames(final_table()) %in% genes_inputs())]

	if (input$use_replicats == TRUE) {
		# Make mean table from replicats
		rows_mean <- lapply(1:ncol(boxplot_data), function(i){
			rows_mean <- rowMeans(boxplot_data[,c(samples_data_table()[i,"sample_id"], samples_data_table()[i,"replicats"])])
		})
		names(rows_mean) <- samples_data_table()[,"sample_id"]
		boxplot_data <- as.data.frame(list.cbind(rows_mean))		
	}

	if (input$metadata == "Genes") {
		# Table for genes metadata
		genes_for_boxplot <- cbind(final_table()[input$meta_gene_x],final_table()[input$meta_gene_col], boxplot_data)
		melted_data<-melt(genes_for_boxplot, id=c(as.character(input$meta_gene_x),as.character(input$meta_gene_col)))
		x <- input$meta_gene_x
		col <- input$meta_gene_col
	} else if (input$metadata == "Samples") {
		# Transpose table for samples metadata
		transpo_tpms <- data.frame(t(boxplot_data))
		factors <- sapply(transpo_tpms, is.factor)
		transpo_tpms[factors] <- lapply(transpo_tpms[factors], as.character)
		colnames(transpo_tpms) <- final_table()[,1]

		samples_id <- rownames(transpo_tpms)
		samples_data <- samples_data_table()[samples_data_table()[["sample_id"]] %in% samples_id,]
		
		transpo_tpms <- data.frame(transpo_tpms, row.names=NULL)
		samples_for_boxplot <- cbind(samples_data[input$meta_sample_x],samples_data[input$meta_sample_col], transpo_tpms)
		melted_data <- melt(samples_for_boxplot, id=c(as.character(input$meta_sample_x), as.character(input$meta_sample_col)))
		x <- input$meta_sample_x
		col <- input$meta_sample_col
	}
	# Build the plot with ggplot function
	plot <- print(
		ggplot(
			data = melted_data,
			aes(
				x = as.character(get(x)),
				y = log2(as.numeric(value)),
				fill = as.character(get(col))
			)
		)
		+ geom_boxplot(
			aes(x = get(as.character(x))),
			outlier.colour = "black",
			outlier.size = 1
		)
		+ scale_color_brewer(palette = "Set1")
		#+ ggtitle("Boxplot")
		+ xlab(paste0(x))
		+ ylab("log2(TPM)")
		+ guides(fill=guide_legend(title=paste0(col)))
	)
	boxplot(plot)
})