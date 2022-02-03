# Dotplot selected samples
output$dotplot_x_sample <- renderUI({
	tagList(
		selectInput(
			inputId = "dotplot_sample_x",
			label = "X sample :",
			choices = colnames(final_table()[,-(1:ncol(genes_data_table()))])
		)
	)
})
output$dotplot_y_sample <- renderUI({
	tagList(
		selectInput(
			inputId = "dotplot_sample_y",
			label = "Y sample :",
			choices = subset(colnames(final_table()[,-(1:ncol(genes_data_table()))]), !(colnames(final_table()[,-(1:ncol(genes_data_table()))]) %in% input$dotplot_sample_x ))
		)
	)
})

# Create graph and download button after clicking on visualize button
observeEvent(input$build_dotplot, {
	output$dotplot_visualize <- renderUI({
		box(
			title = "Dotplot",
			status = "primary",
			solidHeader = TRUE,
			width = 12,
			fluidRow(
				plotOutput(
					outputId = "dotplot"
				)
			),
			fluidRow(
				column(8),
				column(2,
					selectInput(
		    			inputId = "dotplot_ext",
		    			label = "Download format :",
		            	choices = c("PNG", "PDF", "SVG", "EPS"),
		            	width = "200px"
					)
				),
				column(2,
					br(),
			    	downloadButton(
			    		outputId = "dotplot_file",
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
output$dotplot <- renderPlot({
	print(dotplot())
})

# Download dotplot
output$dotplot <- downloadHandler (	
	filename = function() {
		paste0("dotplot.", tolower(input$dotplot_ext))
	},
	content = function(file){
		if (input$dotplot_ext == "PNG"){
			png(file, width = 1500, height = 1000)
		} else if (input$dotplot_ext == "PDF") {
			pdf(file, width = 1500, height = 1000)
		} else if (input$dotplot_ext == "SVG") {
			svg(file, width = 15, height = 10)
		} else if (input$dotplot_ext == "EPS") {
			setEPS()
			postscript(file)
		}
		print(dotplot())
		dev.off()
	}
)

dotplot <- reactiveVal()
# TODO : aes x and y not work
observeEvent(input$build_dotplot, {
	plot <- print(
		ggplot(
			data = final_table(),
			aes(
				x = log2(get(input$dotplot_sample_x)),
				y = log2(get(input$dotplot_sample_y)),
				fill = final_table()[,1]
			)
		)
		+ geom_dotplot(
			binaxis = 'y',
			stackdir = 'center',
			dotsize = 0.5
		)
		+ scale_color_brewer(palette = "Set1")
		#+ ggtitle("Dotplot")
		+ xlab(paste0(input$dotplot_sample_x, " (log2(TPM))"))
		+ ylab(paste0(input$dotplot_sample_y, " (log2(TPM))"))
		+ guides(fill=guide_legend(title="Genes"))
	)
	dotplot(plot)
})