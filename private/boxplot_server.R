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
			choices = samples_inputs()
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
			choices = genes_inputs()
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
				column(6),
				column(2,
			    	selectInput(
	        			inputId = "boxplot_ext",
    	    			label = "Download format :",
        	        	choices = c("PNG", "PDF", "SVG", "EPS"),
            	    	width = "200px"
					)
				),
				column(1,
			    	numericInput(
	        			inputId = "boxplot_width",
    	    			label = "Width (px)",
        	        	value = 1500
					)
				),
				column(1,
			    	numericInput(
	        			inputId = "boxplot_height",
    	    			label = "Heigth (px)",
        	        	value = 1000
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
	boxplot()
})

# Download boxplot
output$boxplot_file <- downloadHandler (		
	filename = function() {
		paste0("boxplot.", tolower(input$boxplot_ext))
	},
	content = function(file){
		if (input$boxplot_ext == "PNG"){
			png(file, width = input$boxplot_width, height = input$boxplot_height)
		} else if (input$boxplot_ext == "PDF") {
			pdf(file, width = input$boxplot_width/100, height = input$boxplot_height/100)
		} else if (input$boxplot_ext == "SVG") {
			svg(file, width = input$boxplot_width/100, height = input$boxplot_height/100)
		} else if (input$boxplot_ext == "EPS") {
			setEPS()
			postscript(file, width = input$boxplot_width/100, height = input$boxplot_height/100)
		}
		print(boxplot())
		dev.off()
	}
)

# Initialize boxplot variable
boxplot <- reactiveVal()
# Build the plot clicking on the visualize button
observeEvent(input$build_boxplot, {
	
	# Get TPM from final_table
	boxplot_data <- final_table()[!(colnames(final_table()) %in% colnames(genes_data_table()))]

	if (input$metadata == "Genes") {
		x <- input$meta_gene_x
		col <- input$meta_gene_col

		# Bind input x and input color columns to TPM boxplot data
		genes_for_boxplot <- cbind(final_table()[x],final_table()[col], boxplot_data)
		# Prepare data for ggplot
		melted_data<-melt(setDT(genes_for_boxplot), id=c(as.character(input$meta_gene_x),as.character(input$meta_gene_col)))
	
	} else if (input$metadata == "Samples") {
		x <- input$meta_sample_x
		col <- input$meta_sample_col
		
		# Transpose table for samples metadata
		tboxplot_data <- t(boxplot_data)
		# Get filtered samples_data_table
		samples_data <- subset(samples_data_table(), samples_data_table()[,"sample_id"] %in% rownames(tboxplot_data))
		# Rename columns by gene_id and rows by sample_name
		colnames(tboxplot_data) <- final_table()[,"gene_id"]
	
		samples_for_boxplot <- cbind(samples_data[x],samples_data[col], tboxplot_data)
		melted_data <- melt(setDT(samples_for_boxplot), id=c(as.character(x), as.character(col)))
	}
	# Build the plot with ggplot function
	plot <- ggplot(
		data = melted_data,
		aes(
			x = as.character(get(x)),
			y = log2(as.numeric(value)),
			fill = as.character(get(col))
		)
	) +
	geom_boxplot(
		aes(x = get(as.character(x))),
		outlier.colour = "black",
		outlier.size = 1
	) +
	scale_color_brewer(palette = "Set1") +
	#ggtitle("Boxplot") +
	xlab(paste0(x)) +
	theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
	ylab("log2(TPM)") +
	guides(fill=guide_legend(title=paste0(col)))
	
	# Fill the boxplot reactive variable with the plot
	boxplot(plot)
})
