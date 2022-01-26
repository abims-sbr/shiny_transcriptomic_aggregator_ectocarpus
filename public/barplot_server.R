# Create graph and download button after clicking on visualize button
observeEvent(input$build_barplot, {
	output$barplot_visualize <- renderUI({
		box(
			title = "Barplot",
			status = "primary",
			solidHeader = TRUE,
			width = 12,
			fluidRow(
				plotOutput(
					outputId = "barplot"
				)
			),
			fluidRow(
				column(8),
				column(2,
			    	selectInput(
	        			inputId = "barplot_ext",
    	    			label = "Download format :",
        	        	choices = c("PNG", "PDF", "SVG", "EPS"),
            	    	width = "200px"
					)
				),
				column(2,
					br(),
					downloadButton(
			    		outputId = "barplot_file",
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
output$barplot <- renderPlot({
	print(barplot())
})

# Download barplot
output$barplot_file <- downloadHandler (		
	filename = function() {
		paste0("barplot.", tolower(input$barplot_ext))
	},
	content = function(file){
		if (input$barplot_ext == "PNG"){
			png(file, width = 1500, height = 1000)
		} else if (input$barplot_ext == "PDF") {
			pdf(file, width = 1500, height = 1000)
		} else if (input$barplot_ext == "SVG") {
			svg(file, width = 15, height = 10)
		} else if (input$barplot_ext == "EPS") {
			setEPS()
			postscript(file)
		}
		print(barplot())
		dev.off()
	}
)

# Initialize barplot variable
barplot <- reactiveVal()
# Build the plot clicking on the visualize button
# TODO : Look how to build the plot
observeEvent(input$build_barplot, {
	# Build the plot with ggplot function
	barplot_data <- final_table()[!(colnames(final_table()) %in% genes_inputs())]
	mean_table <- data.frame(colMeans(barplot_data)) #table with sample name and tpm mean by sample
	samples_id <- rownames(mean_table)
	rownames(mean_table) <- NULL
	barplot_table <- cbind(samples_id, mean_table)
	names(barplot_table)[2] <- "average_tpms"

	plot <- print(
		ggplot(
			data = barplot_table,
			aes(
				x = samples_id,
				y = average_tpms
			)
		)
		+ geom_bar(
			stat = "identity"
		)
	)
	# Fill the barplot reactive variable with the plot
	barplot(plot)
})
