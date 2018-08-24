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
		    bsCollapsePanel(
		    	title = h3("Graphs"),
		    	fluidRow(
		    		column(4,
				    	downloadButton(
				    		outputId = "heatmap",
				    		label = "Heatmap",
				    		width = "100%"
		    			)
		    		),
		    		column(4,
				    	downloadButton(
				    		outputId = "boxplot",
				    		label = "Boxplot",
				    		width = "100%"
		    			)
		    		),
		    		column(4,
				    	downloadButton(
				    		outputId = "dotplot",
				    		label = "Dotplot",
				    		width = "100%"
		    			)
		    		)
		    	)
		    )
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
		dataTableOutput('table')
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
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
	)

	# Heatmap
	output$heatmap <- downloadHandler (
		filename = "heatmap.png",
		content = function(file){
			tpms_data <- tpms_data_table()

			reorder_cormat <- function(cormat){
				# Utiliser la corrélation entre les variables comme mesure de distance
				dd <- as.dist((1-cormat)/2)
				hc <- hclust(dd)
				cormat <-cormat[hc$order, hc$order]
			}
			
			cormat <- round(cor(tpms_data[,-c(1)]),2)
			reordered_cormat <- reorder_cormat(cormat)
			# Améliore les écarts et donc la distinction, mais est-ce que c'est bon ?
			centered_cormat <- cov(scale(reordered_cormat, center=TRUE, scale=TRUE))
			# Sinon
			#centered_cormat <- scale(reordered_cormat, center=TRUE, scale=FALSE)
			melted_cormat<- melt(centered_cormat)

			png(file, width = 1000) #pdf(file)
			print(ggplot(data = melted_cormat, aes(x=X1, y=X2, fill=value)) + geom_tile(color="white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
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

			png(file, width = 1000) #pdf(file)
			print(ggplot(data = filtered_data, aes(x=gene_group, y=as.numeric(value), fill=gene_group)) + geom_boxplot(aes(x=gene_group), outlier.colour="black", outlier.size=1) + facet_wrap( ~ variable, scales="free") + scale_color_brewer(palette="Set1"))
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

			png(file, width = 1000) #pdf(file)
			print(ggplot(data = filtered_data, aes(x=gene_group, y=as.numeric(value), fill=gene_group)) + geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=1.2) + facet_wrap( ~ variable, scales="free") + scale_color_brewer(palette="Set1"))
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
	ggplot(data = filtered_data, aes(x=gene_group, y=as.numeric(substi(value)), fill=gene_group))
	+ geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=1.2)
	+ facet_wrap( ~ variable, scales="free") + scale_color_brewer(palette="Set1")


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