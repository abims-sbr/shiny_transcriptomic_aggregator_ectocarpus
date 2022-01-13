output$heatmap_tab <- renderUI({
	if(!is.null(tpms_data_table()) && !is.null(genes_data_table()) && !is.null(samples_data_table())){
		tagList(
			box(
				title = "Heatmap",
				status = "primary",
				solidHeader = TRUE,
				width = 12,
				fluidRow(
					# Color palette
					column(12,
						selectInput(
							inputId = "color",
							label = "Color palette :",
							choices = c(RedBlue="RdYlBu","YellowBlue","Blues"),
							selected = "RedBlue"
						)
					)
				),
				fluidRow(
					column(8,
		    			selectInput(
		        			inputId = "heatmap_ext",
		        			label = "Export format :",
		                	choices = c("PNG", "PDF", "SVG", "EPS"),
		                	width = "200px"
		    			)
					),
					column(4,
						br(),
		    			downloadButton(
				    		outputId = "heatmap",
				    		label = "Heatmap",
				    		width = "100%"
		    			)
					)
		    	)
		    )
		)
	}
})

# Download heatmap
output$heatmap <- downloadHandler (
	filename = function() {
		paste0("heatmap.", tolower(input$heatmap_ext))
	},
	content = function(file) {
		final_table <- final_table()
		tpms_table <- final_table[!(colnames(final_table) %in% colnames(genes_data_table()))]
		rownames(tpms_table) <- final_table[,1]
		tpms_matrix <- data.matrix(tpms_table)

		if(input$color == "YellowBlue"){
			color_palette <- colorRampPalette(c("#FFCE00","#FFFFFF","#6B8BA3"))(10)
		} else {
			color_palette <- brewer.pal(10,input$color)
		}

		buildHeatmap(tpms_matrix, file, ext = input$heatmap_ext, color = color_palette)
	}
)