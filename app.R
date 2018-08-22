# Set default repo from CRAN
options(repos=structure(c(CRAN="https://cran.rstudio.com/")))
# Update installed packages
#update.packages(ask=FALSE, checkBuilt=TRUE)
# Install some packages
install.packages(c('shiny'))

# Load packages
library(shiny)
library(DT)

source("lib/merge_duplicated_data.R")
source("lib/read_tables.R")

# Import files
tpms_data_table<-read.csv("test-data/TPMs-1_test.csv", sep="\t", header=TRUE, quote="")

#genes_data_file<-read.csv("test-data/genes_data_test.csv", sep="\t", header=TRUE, quote="", stringsAsFactors = FALSE)
genes_data_file<-read.csv("test-data/genes_data_test_duplicated.csv", sep="\t", header=TRUE, quote="", stringsAsFactors = FALSE)
genes_data_table<-merge_duplicated_data(genes_data_file)

samples_data_table<-read.csv("test-data/samples_data_test.csv", sep="\t", header=TRUE, quote="")


# User interface
ui <- bootstrapPage(
	includeCSS("static/css/styles.css"),
	br(),
	fluidRow(
		column(2,
			wellPanel(
		      	fileInput(
		      		inputId = "tpms_file", 
		      		label = "Import TPMs File (.csv)",
		            multiple = FALSE,
		            accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
		        ),
		      	fileInput(
		      		inputId = "genes_data_file", 
		      		label = "Import Genes Data File (.csv)",
		            multiple = FALSE,
		            accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
		        ),
		      	fileInput(
		      		inputId = "sample_data_file", 
		      		label = "Import Samples Data File (.csv)",
		            multiple = FALSE,
		            accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
		        )
		    )
		),
		column(10,
			wellPanel(
				h3("Samples Metadata Filters"),
				fluidRow(
			      	lapply(1:ncol(samples_data_table), function(i) {
			      		column(3,
							selectInput(
			        			inputId = colnames(samples_data_table)[i],
			        			label = paste0(colnames(samples_data_table)[i], " :"),
			                	choices = c("All", unique(samples_data_table[i])),
			                	width = "200px"
			            	)
			      		)
			      	})
				)
			)
			#selectInput(
			#	inputId = colnames(genes_data_file)[i],  #paste0('a', i), 
			#	label = colnames(genes_data_file)[i], #paste0('SelectA', i),
			#	choices = unique(genes_data_file[i]) #sample(LETTERS, 5)
			#)
		)
	),
	tags$hr(),
	fluidRow(
		dataTableOutput('table')
	)
)


# Server function
server <- function(input, output, session){

	output$table <- renderDataTable(
		{
			# Import file via fileInput
			if(FALSE){
				req(input$tpms_file)
				req(input$genes_data_file)
				req(input$sample_data_file)

				tpms_data_table <- getDataFrameFromFile(input$tpms_file$datapath)
				#tpms_data_table <- read.csv(
				#	input$tpms_file$datapath,
	            # 	header = TRUE,
	            # 	sep = input$sep
	            #)

	            genes_data_table <- getDataFrameFromFile(input$genes_data_file$datapath)
	            #genes_data_table <- read.csv(
				#	input$genes_data_file$datapath,
	            # 	header = TRUE,
	            # 	sep = input$sep
	            #)

	            merged_data<-merge(tpms_data_table, genes_data_table, by="gene_id")

	            sample_data_table <- getDataFrameFromFile(input$sample_data_file$datapath)
	            #sample_data_table <- read.csv(
				#	input$sample_data_file$datapath,
	            # 	header = TRUE,
	            # 	sep = input$sep
	            #)
        	}

			samples_data <- samples_data_table
			tpms_data <- tpms_data_table

			for ( i in 1:ncol(samples_data)) {
        		if (input[[colnames(samples_data)[i]]] != "All") {
				    samples_data <- samples_data[samples_data[i] == input[[colnames(samples_data)[i]]],]
				}
			}

		    gene_id<-tpms_data$gene_id
		    # Supposing id is the first column
		    filtered_tpms_data <- cbind(gene_id, tpms_data[colnames(tpms_data) %in% samples_data[,1]])
		    merged_data <- merge(genes_data_table, filtered_tpms_data, by="gene_id")
		    merged_data
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
}

shinyApp(ui, server)

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
	melted_cormat<- melt(reordered_cormat)
	ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) #+ geom_tile(color="white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab")
	# Séparateur de case
	+ geom_tile(color="white")
	#Gradient de couleur 
	+ scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab")

}