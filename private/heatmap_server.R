observeEvent(input$build_heatmap, {
	output$heatmap_visualize <- renderUI({
		box(
			title = "Heatmap",
			status = "primary",
			solidHeader = TRUE,
			width = 12,
			fluidRow(
				plotOutput(
					outputId = "heatmap"
				)
			),
			fluidRow(
				column(6),
				column(2,
	    			selectInput(
	        			inputId = "heatmap_ext",
	        			label = "Export format :",
	                	choices = c("PNG", "PDF", "SVG", "EPS"),
	                	width = "200px"
	    			)
				),
				column(1,
			    	numericInput(
	        			inputId = "heatmap_width",
    	    			label = "Width (px)",
        	        	value = 1500
					)
				),
				column(1,
			    	numericInput(
	        			inputId = "heatmap_height",
    	    			label = "Heigth (px)",
        	        	value = 1000
					)
				),
				column(2,
					br(),
	    			downloadButton(
			    		outputId = "heatmap_file",
			    		label = "Heatmap",
			    		width = "100%"
	    			)
				)
			)
    	)
	})
})

# Plot output
output$heatmap <- renderPlot({
	heatmap()
})

# Download heatmap
output$heatmap_file <- downloadHandler (
	filename = function() {
		paste0("heatmap.", tolower(input$heatmap_ext))
	},
	content = function(file) {
		if (input$heatmap_ext == "PNG"){
			png(file, width = input$heatmap_width, height = input$heatmap_height)
		} else if (input$heatmap_ext == "PDF") {
			pdf(file, width = input$heatmap_width/100, height = input$heatmap_height/100)
		} else if (input$heatmap_ext == "SVG") {
			svg(file, width = input$heatmap_width/100, height = input$heatmap_height/100)
		} else if (input$heatmap_ext == "EPS") {
			setEPS()
			postscript(file, width = input$heatmap_width/100, height = input$heatmap_height/100)
		}
		print(heatmap())
		dev.off()
	}
)

# Initialize heatmap variable
heatmap <- reactiveVal()
# TODO : Build the plot only if a subset of genes is selected or warn the user there are lot of data or nb gene max (~100) ?
# Build the plot clicking on the visualize button
observeEvent(input$build_heatmap, {

	heatmap_data <- final_table()[!(colnames(final_table()) %in% colnames(genes_data_table()))]
	rownames(heatmap_data) <- final_table()[,1]
	heatmap_matrix <- data.matrix(heatmap_data)

	if(input$color == "YellowBlue"){
		color_palette <- colorRampPalette(c("#FFCE00","#FFFFFF","#6B8BA3"))(10)
	} else {
		color_palette <- brewer.pal(10,input$color)
	}

	plot <- pheatmap(
		scale(heatmap_matrix, center=TRUE, scale=TRUE),
		color=color_palette,
		scale="row",
		angle_col="45"
	)
	heatmap(plot)
})
