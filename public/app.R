# Set default repo from CRAN
options(repos=structure(c(CRAN="https://cran.rstudio.com/")))
# Update installed packages
#update.packages(ask=FALSE, checkBuilt=TRUE)
# Install some packages
install.packages(c('shiny', 'shinydashboard', 'shinyjs', 'shinyBS', 'markdown', 'DT', 'data.table', 'gplots', 'Hmisc', 'reshape', 'rlist'))

# Load packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(markdown)
library(data.table)
library(DT)
library(gplots)
library(Hmisc)
library(RColorBrewer)
library(reshape)
library(rlist)
library(dplyr)

# Load configurations
source("conf.R", local = TRUE)
# Load functions
source("lib/merge_duplicated_data.R")
source("lib/read_tables.R")
source("lib/build_graphs.R")

# User interface
ui <- tagList(
	dashboardPage(
		dashboardHeader(title = HTML(paste0("Shiny Transcriptomic Aggregator - ", project)), titleWidth = 450),
		dashboardSidebar(
			sidebarMenu(id = "tabs",
				menuItem("User guide", tabName = "guide_tab", icon = icon("info-circle")),
				menuItem("Input data", tabName = "input_tab", icon = icon("file-import")),
				menuItem("Table", tabName = "table_tab", icon = icon("table")),
				menuItem("Barplot", tabName = "barplot_tab", icon = icon("chart-bar")),
				menuItem("Boxplot", tabName = "boxplot_tab", icon = icon("chart-bar")),
				menuItem("Dotplot", tabName = "dotplot_tab", icon = icon("chart-bar")),
				menuItem("Heatmap", tabName = "heatmap_tab", icon = icon("chart-bar"))
			)
		),
		dashboardBody(
			useShinyjs(),
			includeCSS("www/custom.css"),
			tabItems(
				source("guide_ui.R", local = TRUE)$value,
				source("input_ui.R", local = TRUE)$value,
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

	## Variable Initialisation	
    final_table <- reactiveVal(0)
    genes_list <- reactiveVal(value=NULL)
    if(!is.null(tpms_input)){
	    tpms_data_table <- reactiveVal(value=getDataFrameFromFile(tpms_input))
    }
    if(!is.null(genes_data_input)){
    	genes_data_file <- getDataFrameFromFile(genes_data_input)
    	genes_data_table <- reactiveVal(value=merge_duplicated_data(genes_data_file))
    }
    if(!is.null(samples_data_input)){
    	samples_data_file <- getDataFrameFromFile(samples_data_input)
		if("private" %in% tolower(colnames(samples_data_file))){
			if(instance_tag == "public"){
				samples_data_file <- samples_data_file[toupper(samples_data_file[,"private"]) == "FALSE",]
			}
			samples_data_file["private"] <- NULL
		}
    	samples_data_table <- reactiveVal(value=samples_data_file)
	}

	source("input_server.R", local = TRUE)
	source("table_server.R", local = TRUE)
	source("boxplot_server.R", local = TRUE)
	source("dotplot_server.R", local = TRUE)
	source("heatmap_server.R", local = TRUE)
}
shinyApp(ui, server)