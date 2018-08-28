# Set default repo from CRAN
options(repos=structure(c(CRAN="https://cran.rstudio.com/")))
# Update installed packages
#update.packages(ask=FALSE, checkBuilt=TRUE)
# Install some packages
install.packages(c('shiny', 'shinyBS'))

# Load packages
library(shiny)
library(shinyBS)
library(DT)
library(Hmisc)
library(reshape)

# Load functions
source("lib/merge_duplicated_data.R")
source("lib/read_tables.R")


# User interface
ui <- bootstrapPage(
	includeCSS("static/css/styles.css"),
	br(),
	fluidRow(
		# Import Files Panel
		column(3,
			bsCollapse(
				open = "filters",
				bsCollapsePanel(
					title = h3("Import data"),
					value = "filters",
				 	fileInput(
				   		inputId = "tpms_file", 
				   		label = "Import TPMs File (.csv)",
				        multiple = FALSE,
				        accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
				        width = "100%"
				    ),
				   	fileInput(
				   		inputId = "genes_data_file", 
				   		label = "Import Genes Data File (.csv)",
				        multiple = FALSE,
				        accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
				        width = "100%"
				    ),
				  	fileInput(
				   		inputId = "samples_data_file", 
				   		label = "Import Samples Data File (.csv)",
				        multiple = FALSE,
				        accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
				        width = "100%"
				    )
			    )
		    ),
			uiOutput("graphs_tab")		    
		),
		# Filters Panels
		column(9,
			uiOutput("samples_filters"),
			uiOutput("genes_filters")
		)
	),
	tags$hr(),
	# DataTable
	fluidRow(
		column(
			width = 12,
			dataTableOutput('table')
		)
	)
)


# Server function
server <- function(input, output, session){

	# Import files
	samples_data_table <- eventReactive(input$samples_data_file, {
        samples_data_table <- getDataFrameFromFile(input$samples_data_file$datapath)
    })

    genes_data_table <- eventReactive(input$genes_data_file, {
        genes_data_file <- getDataFrameFromFile(input$genes_data_file$datapath)
        genes_data_table <- merge_duplicated_data(genes_data_file)
    })

    tpms_data_table <- eventReactive(input$tpms_file, {
        tpms_data_table <- getDataFrameFromFile(input$tpms_file$datapath)
    })

    final_table <- reactiveVal(0)

	# Generate UI Samples Filters
	output$samples_filters <- renderUI({
		samples_data_table <- samples_data_table()
		tagList(
			bsCollapsePanel(
				title = h3("Samples Metadata Filters"),
				fluidRow(
			      	lapply(1:ncol(samples_data_table), function(i) {
			      		column(3,
							selectInput(
			        			inputId = colnames(samples_data_table)[i],
			        			label = paste0(capitalize(gsub("_", " ", colnames(samples_data_table)[i])), " :"),
			                	choices = c("All", sort(unique(samples_data_table[,i]))),
			                	width = "200px",
			                	selected = "All"
			            	)
			      		)
			      	})
				),
				fluidRow(
					column(11),
					column(1,
						actionButton(
							inputId = "reset_samples_values",
							label = "Reset" 
						)
					)
				)				
			)
		)
	})

	# Generate UI Genes Filters
	output$genes_filters <- renderUI({
		genes_data_table <- genes_data_table()
		tagList(
			bsCollapsePanel(
				title = h3("Genes Metadata Filters"),
				fluidRow(
					lapply(1:ncol(genes_data_table), function(i) {
						column(3,
							selectInput(
			        			inputId = colnames(genes_data_table)[i],
			        			label = paste0(capitalize(gsub("_", " ", colnames(genes_data_table)[i])), " :"),
			                	choices = c("All", sort(unique(genes_data_table[,i]))),
			                	width = "200px",
			                	selected = "All"
							)
						)
					})
				),
				fluidRow(
					column(11),
					column(1,
						actionButton(
							inputId = "reset_genes_values",
							label = "Reset" 
						)
					)
				)
			)
		)
	})

	# Graphs tab
	output$graphs_tab <- renderUI({
		if(!is.null(input$tpms_file) && !is.null(input$genes_data_file) && !is.null(input$samples_data_file)){
			tagList(
				bsCollapsePanel(
			    	title = h3("Graphs"),
			    	tabsetPanel(
			    		type = "tabs",
			    		tabPanel(
			    			title = "Heatmap",
			    			br(),
			    			downloadButton(
					    		outputId = "heatmap",
					    		label = "Heatmap",
					    		width = "100%"
			    			)
			    		),
			    		tabPanel(
			    			title = "Boxplot",
			    			br(),
			    			fluidRow(
			    				column(6,
			    					h4("X Metadata :"),
					    			uiOutput("boxplot_var_one")
					    		),
					    		column(6,
					    			h4("Y Metadata :"),
					    			uiOutput("boxplot_var_two")
					    		)
				    		),			    			
					    	downloadButton(
					    		outputId = "boxplot",
					    		label = "Boxplot",
					    		width = "100%"
			    			)
			    		),
			    		tabPanel(
			    			title = "Dotplot",
			    			br(),
			    			fluidRow(
			    				column(6,
			    					h4("X sample :"),
					    			uiOutput("dotplot_sample_one")
					    		),
					    		column(6,
					    			h4("Y sample :"),
					    			uiOutput("dotplot_sample_two")
					    		)
				    		),
					    	downloadButton(
					    		outputId = "dotplot",
					    		label = "Dotplot",
					    		width = "100%"
			    			)
			    		)
			    	)
		    	)
			)
		}
	})

	# Dotplot selected samples
	output$dotplot_sample_one <- renderUI({
		samples_data <- samples_data_table()
		tagList(
			selectInput(
				inputId = "dotplot_sample1",
				label = NULL,
				choices = unique(samples_data[,1]),
				width = "180px"
			)
		)
	})

	output$dotplot_sample_two <- renderUI({
		samples_data <- samples_data_table()
		tagList(
			selectInput(
				inputId = "dotplot_sample2",
				label = NULL,
				choices = subset(unique(samples_data[,1]), !(unique(samples_data[,1]) %in% input$dotplot_sample1)),
				width = "180px"
			)
		)
	})

	# Reset filters values
	observeEvent(input$reset_samples_values, {
		samples_data_table <- samples_data_table()
		for ( i in 1:ncol(samples_data_table)) {
        	updateSelectInput(
        		session = session, 
        		inputId = colnames(samples_data_table)[i],
        		selected = "All"
        	)
		}
	})
	observeEvent(input$reset_genes_values, {
		genes_data_table <- genes_data_table()
		for ( i in 1:ncol(genes_data_table)) {
        	updateSelectInput(
        		session = session, 
        		inputId = colnames(genes_data_table)[i],
        		selected = "All"
        	)
		}
  	})

	# DataTable
	output$table <- renderDataTable(
		{
        	samples_data <- samples_data_table()
			genes_data_table <- genes_data_table()
			tpms_data <- tpms_data_table()

			# Apply filters
			for ( i in 1:ncol(samples_data)) {
        		if (input[[colnames(samples_data)[i]]] != "All") {
				    samples_data <- samples_data[samples_data[i] == input[[colnames(samples_data)[i]]],]
				}
			}

			# Update other filters values
			for ( j in 1:ncol(samples_data)) {
				if ( input[[colnames(samples_data)[j]]] == "All") {
				    updateSelectInput(
				       	session = session, 
				       	inputId = colnames(samples_data)[j],
				       	choices = c("All", unique(samples_data[,j])),
				       	selected = "All"
		    		)
				}
	    	}

	    	updateSelectInput(
        		session = session, 
        		inputId = "dotplot_sample1",
        		choices = unique(samples_data[,1])
        	)
        	updateSelectInput(
        		session = session, 
        		inputId = "dotplot_sample2",
        		choices = subset(unique(samples_data[,1]), !(unique(samples_data[,1]) %in% input$dotplot_sample1))
        	)

	    	# Merge TPMS and genes metadata files according to filters
		    gene_id <- tpms_data[1] # Supposing gene_id is the 1st column
		    filtered_tpms_data <- cbind(gene_id, tpms_data[colnames(tpms_data) %in% samples_data[,1]]) # Supposing sample_id is the 1st column
		    merged_genes_tpms <- merge(genes_data_table, filtered_tpms_data, by="gene_id")
		    final_table <- merged_genes_tpms

		    #Ajouter un filtre des lignes de merged_data selon les genes metadata
		    for ( i in 1:ncol(genes_data_table)){
		    	if (input[[colnames(genes_data_table)[i]]] != "All") {
			    	for ( j in 1:nrow(genes_data_table)) {
				    	final_table <- merged_genes_tpms[merged_genes_tpms[,i] == input[[colnames(genes_data_table)[i]]],]
				    }
				}
			}
		    final_table(final_table)
		    final_table
		},
		rownames = FALSE,
	    extensions = c("Buttons", "FixedHeader"),
        options = list(
        	dom = 'Blfrtip',
        	order = list(0, 'asc'),
        	fixedHeader = TRUE,
        	pageLength = 25,
  			lengthMenu = c(10, 25, 50, 100, 200),
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            scrollX = TRUE
        )
	)

	# Heatmap
	output$heatmap <- downloadHandler (
		filename = "heatmap.png",
		content = function(file){
			final_table <- final_table()
			tpms_data <- final_table[,-c(2:4)]

			tpms_table <- tpms_data[,-1]
			rownames(tpms_table) <- tpms_data[,1]

			png(file, width = 1500, height = 1000) #pdf(file)
			print(heatmap(as.matrix(scale(tpms_table)), col=colorRampPalette(c("red","white","blue"))(10)))
			dev.off()
		}
  	)

  	# Boxplot
	output$boxplot <- downloadHandler (		
		filename = "boxplot.png",
		content = function(file){
			final_table <- final_table()

			data_for_boxplot<-final_table[,-c(2,3)]
			filtered_data<-melt(data_for_boxplot, id=c("gene_id","gene_group"))

			png(file, width = 1500, height = 1000) #pdf(file)
			print(ggplot(data = filtered_data, aes(x=gene_group, y=log2(as.numeric(value)), fill=gene_group)) + geom_boxplot(aes(x=gene_group), outlier.colour="black", outlier.size=1) + facet_wrap( ~ variable, scales="free") + scale_color_brewer(palette="Set1") + ylab("log2(TPM)"))
			dev.off()
		}
  	)

  	# Dotplot
  	output$dotplot <- downloadHandler (	
		filename = "dotplot.png",
		content = function(file){
			final_table <- final_table()

			data_for_boxplot<-final_table[,-c(2,3)]
			filtered_data<-melt(data_for_boxplot, id=c("gene_id","gene_group"))

			png(file, width = 1500, height = 1000) #pdf(file)
			print(ggplot(data = final_table, aes(x=get(input$dotplot_sample1), y=get(input$dotplot_sample2), fill=final_table[,1])) + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) + scale_color_brewer(palette="Set1")) #+ geom_label(aes(label = final_table[,1])
			dev.off()
		}
	)
}

shinyApp(ui, server)


# Build graphs
if(FALSE){
	# Fonction pour remplacer les "," par des "." pour les valeurs de tpms
	substi<-function(x) {gsub("[,]",".",x) } 
	
	data_for_boxplot<-merged_data[,-c(2,3)]
	filtered_data<-melt(data_for_boxplot, id=c("gene_id","gene_group"))
	

	## BOX PLOT
	# Données et couleur
	ggplot(data = filtered_data, aes(x=gene_group, y=as.numeric(substi(value)), fill=gene_group)) #color=gene_group)) 
	# Boîtes et points
	+ geom_boxplot(aes(x=gene_group), outlier.colour="black", outlier.size=1)
	# Séparation en plusieurs graphes
	+ facet_wrap( ~ variable, scales="free") 
	# Palette de couleur
	+ scale_color_brewer(palette="Set1")


	## DOT PLOT
	ggplot(data = tpms, aes(x=sample1, y=sample2, fill=gene_id)) + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) + scale_color_brewer(palette="Set1") + geom_label(aes(label = gene_id))


	## HEATMAP
	reorder_cormat <- function(cormat){
		# Utiliser la corrélation entre les variables comme mesure de distance
		dd <- as.dist((1-cormat)/2)
		hc <- hclust(dd)
		cormat <-cormat[hc$order, hc$order]
	}
	# Remplacer les , par des . dans le fichier tpms
	cormat <- round(cor(tpms_data_table[,-c(1)]),2)
	reordered_cormat <- reorder_cormat(cormat)

	# Pour centrer réduire les valeurs (Intervalle pas entre -1 et 1)
	scale(reordered_cormat, center=TRUE, scale=TRUE)
	# Donne un intervalle quasi entre -1 et 1, mais est ce que c'est juste ???
	cov(scale(reordered_cormat, center=TRUE, scale=TRUE))

	melted_cormat<- melt(reordered_cormat)
	ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) #+ geom_tile(color="white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab")
	# Séparateur de case
	+ geom_tile(color="white")
	#Gradient de couleur 
	+ scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab")
}