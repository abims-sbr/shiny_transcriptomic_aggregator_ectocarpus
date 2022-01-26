# Set default repo from CRAN
options(repos=structure(c(CRAN="https://cran.rstudio.com/")))
# Update installed packages
#update.packages(ask=FALSE, checkBuilt=TRUE)
# Install some packages
install.packages(c('shiny', 'shinydashboard', 'shinyjs', 'shinyBS', 'shinyWidgets', 'markdown', 'DT', 'data.table', 'pheatmap', 'ggplot2', 'Hmisc', 'reshape', 'rlist', 'dplyr'))

# Load packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(markdown)
library(data.table)
library(DT)
library(ggplot2)
library(pheatmap)
library(Hmisc)
library(RColorBrewer)
library(reshape)
library(rlist)
library(dplyr)

# Load configurations
source("conf.R", local = TRUE)
# Load functions
source("lib/read_tables.R")

# User interface
ui <- tagList(
	dashboardPage(
		dashboardHeader(title = HTML(paste0("Shiny Transcriptomic Aggregator - ", project)), titleWidth = 450),
		dashboardSidebar(
			sidebarMenu(id = "tabs",
				menuItem("User guide", tabName = "guide_tab", icon = icon("info-circle")),
				menuItem("Import data", tabName = "import_tab", icon = icon("file-import")),
				menuItem("Table", tabName = "table_tab", icon = icon("table")),
				menuItem("Barplot", tabName = "barplot_tab", icon = icon("chart-bar")),
				menuItem("Boxplot", tabName = "boxplot_tab", icon = icon("chart-bar")),
				#menuItem("Dotplot", tabName = "dotplot_tab", icon = icon("chart-bar")),
				menuItem("Heatmap", tabName = "heatmap_tab", icon = icon("chart-bar"))
			)
		),
		dashboardBody(
			useShinyjs(),
			includeCSS("www/custom.css"),
			tabItems(
				source("guide_ui.R", local = TRUE)$value,
				source("import_ui.R", local = TRUE)$value,
				source("table_ui.R", local = TRUE)$value,
				source("barplot_ui.R", local = TRUE)$value,
				source("boxplot_ui.R", local = TRUE)$value,
				source("dotplot_ui.R", local = TRUE)$value,
				source("heatmap_ui.R", local = TRUE)$value
			)
		)
	),
	# Footer
	tags$footer(
		"Copyright (c) 2018-2019 - Station Biologique de Roscoff - ABiMS"
	)
)
# Server function
server <- function(input, output, session){

    # Assign default data
    if(!is.null(tpms_input)){
	    tpms_data_table <- reactiveVal(value=getDataFrameFromFile(tpms_input))
    }
    if(!is.null(genes_data_input)){
    	genes_data_table <- reactiveVal(value=getDataFrameFromFile(genes_data_input))
    }
    if(!is.null(samples_data_input)){
    	samples_data_file <- getDataFrameFromFile(samples_data_input)
		if("private" %in% tolower(colnames(samples_data_file))){
			# If public instance, took of private sample data
			if(instance_tag == "public"){
				samples_data_file <- samples_data_file[toupper(samples_data_file[,"private"]) == "FALSE",]
			}
			# Remove private column from the table
			samples_data_file["private"] <- NULL
		}
    	samples_data_table <- reactiveVal(value=samples_data_file)
	}

	# Variable Initialisation
	final_table <- reactiveVal()
	genes_list <- reactiveVal(value=NULL)

	observe ({
		# Update table by gene list file
		initial_table <- merge(genes_data_table(), tpms_data_table(), by=colnames(genes_data_table()["gene_id"]))
		# TODO : Better implemant gene_list filter with other filters
		if (length(genes_list()) > 0) {
			initial_table <- subset(initial_table, initial_table[,1] %in% genes_list()[[1]])
		}		
		final_table(initial_table)
	})

	source("import_server.R", local = TRUE)
	source("table_server.R", local = TRUE)
	source("barplot_server.R", local = TRUE)
	source("boxplot_server.R", local = TRUE)
	source("dotplot_server.R", local = TRUE)
	source("heatmap_server.R", local = TRUE)
}
shinyApp(ui, server)