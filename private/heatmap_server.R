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
				column(8),
				column(2,
	    			selectInput(
	        			inputId = "heatmap_ext",
	        			label = "Export format :",
	                	choices = c("PNG", "PDF", "SVG", "EPS"),
	                	width = "200px"
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
			png(file, width = 1500, height = 1000)
		} else if (input$heatmap_ext == "PDF") {
			pdf(file, width = 1500, height = 1000)
		} else if (input$heatmap_ext == "SVG") {
			svg(file, width = 15, height = 10)
		} else if (input$heatmap_ext == "EPS") {
			setEPS()
			postscript(file)
		}
		print(heatmap())
		dev.off()
	}
)

# Initialize heatmap variable
heatmap <- reactiveVal()
# Build the plot clicking on the visualize button
observeEvent(input$build_heatmap, {

	tpms_table <- final_table()[!(colnames(final_table()) %in% genes_inputs())]
	rownames(tpms_table) <- final_table()[,1]
	tpms_matrix <- data.matrix(tpms_table)

	if(input$color == "YellowBlue"){
		color_palette <- colorRampPalette(c("#FFCE00","#FFFFFF","#6B8BA3"))(10)
	} else {
		color_palette <- brewer.pal(10,input$color)
	}

	# TODO : Test for subset of genes
	plot <- pheatmap(
		scale(tpms_matrix, center=TRUE, scale=TRUE),
		color=color_palette,
		scale="row",
		angle_col="45"
	)
	heatmap(plot)
})