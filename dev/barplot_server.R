# Barplot list of conditions
output$barplot_metadata_ui <- renderUI({
	tagList(
		selectInput(
			inputId = "barplot_metadata",
			label = "Samples metadata",
			choices = samples_inputs()[-1]
		)
	)
})

# Create graph and download button after clicking on visualize button
observeEvent(input$build_barplot, {
	output$barplot_visualize <- renderUI({
		fluidRow(
			box(
				title = "Barplot",
				status = "primary",
				solidHeader = TRUE,
				width = 12,
				jqui_resizable(
					plotOutput(
						outputId = "barplot"
					)
				),
				fluidRow(
					column(6),
					column(2,
				    	selectInput(
		        			inputId = "barplot_ext",
	    	    			label = "Download format :",
	        	        	choices = c("PNG", "PDF", "SVG", "EPS"),
	            	    	width = "200px"
						)
					),
					column(1,
				    	numericInput(
		        			inputId = "barplot_width",
	    	    			label = "Width (px)",
	        	        	value = 1500
						)
					),
					column(1,
				    	numericInput(
		        			inputId = "barplot_height",
	    	    			label = "Heigth (px)",
	        	        	value = 1000
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
		)
	})
})

# Plot output
output$barplot <- renderPlot({
	barplot()
})

# Download barplot
output$barplot_file <- downloadHandler (		
	filename = function() {
		paste0("barplot.", tolower(input$barplot_ext))
	},
	content = function(file){
		if (input$barplot_ext == "PNG"){
			png(file, width = input$barplot_width, height = input$barplot_height)
		} else if (input$barplot_ext == "PDF") {
			pdf(file, width = input$barplot_width/100, height = input$barplot_height/100)
		} else if (input$barplot_ext == "SVG") {
			svg(file, width = input$barplot_width/100, height = input$barplot_height/100)
		} else if (input$barplot_ext == "EPS") {
			setEPS()
			postscript(file, width = input$barplot_width/100, height = input$barplot_height/100)
		}
		print(boxplot())
		dev.off()
	}	
)

# Initialize barplot variable
barplot <- reactiveVal()
# Build the plot clicking on the visualize button
observeEvent(input$build_barplot, {

	barplot_data <- final_table()[!(colnames(final_table()) %in% colnames(genes_data_table()))]

	if(input$xaxis == "Sample"){
		mean_tpms <- data.frame(sapply(barplot_data, mean))
		sd_tpms <- data.frame(sapply(barplot_data, sd))
		barplot_table <- merge(mean_tpms, sd_tpms, by="row.names")
		names(barplot_table)[1] <- "samples_id"
		names(barplot_table)[2] <- "mean_TPMs"
		names(barplot_table)[3] <- "sd_TPMs"
		x <- "samples_id"
	} else if (input$xaxis == "Condition"){
		mean_data <- data.frame(sapply(barplot_data, mean))
		mean_tpms <- mean_data[order(row.names(mean_data)),,drop=FALSE]
		sd_data <- data.frame(sapply(barplot_data, sd))
		sd_tpms <- sd_data[order(row.names(sd_data)),,drop=FALSE]

		sample_data <- subset(samples_data_table(), samples_data_table()[,"sample_id"] %in% colnames(barplot_data))
		sample_data_sorted <- sample_data[order(sample_data$sample_id),]

		mean_metadata <- cbind(sample_data_sorted["sample_id"], sample_data_sorted[input$barplot_metadata], mean_tpms, sd_tpms)
		names(mean_metadata)[3] <- "mean_TPMs"
		names(mean_metadata)[4] <- "sd_TPMs"
		barplot_table <- ddply(mean_metadata, input$barplot_metadata, summarise, mean_TPMs=mean(mean_TPMs), sd_TPMs=mean(sd_TPMs))
		x <- input$barplot_metadata
	}

	# Build the plot with ggplot function
	plot <- ggplot(
		data = barplot_table,
		aes(
			x = as.character(get(x)),
			y = mean_TPMs
		)
	) +
	geom_bar(
		stat = "identity",
		fill = "steelblue"
	) +
	geom_errorbar(
		aes(
			ymin = mean_TPMs-sd_TPMs, 
			ymax = mean_TPMs+sd_TPMs
		), 
		width=.2,
        position=position_dodge(.9)
    ) +
    geom_text(
		aes(
			label = as.character(round(mean_TPMs,2))
		),
		vjust = 1.5,
		colour = "white"
	) +
	xlab(x) +
	theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
	ylab("mean(TPMs)")

	# Fill the barplot reactive variable with the plot
	barplot(plot)
})
