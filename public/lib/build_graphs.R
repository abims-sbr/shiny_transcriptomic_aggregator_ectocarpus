# Barplot
buildBarplot <- function(table, file, ext, x, col){
	if (ext == "PNG"){
		png(file, width = 1500, height = 1000)
	} else if (ext == "PDF") {
		pdf(file, width = 1500, height = 1000)
	} else if (ext == "SVG") {
		svg(file, width = 15, height = 10)
	} else if (ext == "EPS") {
		setEPS()
		postscript(file)
	}
	print(
		ggplot(
			data = table,
			aes(
				x = as.character(get(x)),
				y = log2(as.numeric(value)) #log ?
			)
		)
		+ geom_bar(
			stat = "identity"
		)
	)
	dev.off()
}

# Boxplot
buildBoxplot <- function(table, file, ext, x, col){
	if (ext == "PNG"){
		png(file, width = 1500, height = 1000)
	} else if (ext == "PDF") {
		pdf(file, width = 1500, height = 1000)
	} else if (ext == "SVG") {
		svg(file, width = 15, height = 10)
	} else if (ext == "EPS") {
		setEPS()
		postscript(file)
	}
	print(
		ggplot(
			data = table,
			aes(
				x = as.character(get(x)),
				y = log2(as.numeric(value)),
				fill = as.character(get(col))
			)
		)
		+ geom_boxplot(
			aes(x = get(as.character(x))),
			outlier.colour = "black",
			outlier.size = 1
		)
		+ scale_color_brewer(palette = "Set1")
		#+ ggtitle("Boxplot")
		+ xlab(paste0(x))
		+ ylab("log2(TPM)")
		+ guides(fill=guide_legend(title=paste0(col)))
	)
	dev.off()
}

# Dotplot
buildDotplot <- function(table, file, ext, x, y){
	if (ext == "PNG"){
		png(file, width = 1500, height = 1000)
	} else if (ext == "PDF") {
		pdf(file, width = 1500, height = 1000)
	} else if (ext == "SVG") {
		svg(file, width = 15, height = 10)
	} else if (ext == "EPS") {
		setEPS()
		postscript(file)
	}
	print(
		ggplot(
			data = table,
			aes(
				x = log2(get(x)),
				y = log2(get(y)),
				fill = table[,1]
			)
		)
		+ geom_dotplot(
			binaxis = 'y',
			stackdir = 'center',
			dotsize = 0.5
		)
		+ scale_color_brewer(palette = "Set1")
		#+ ggtitle("Dotplot")
		+ xlab(paste0(x, " (log2(TPM))"))
		+ ylab(paste0(y, " (log2(TPM))"))
		+ guides(fill=guide_legend(title="Genes"))
	)
	dev.off()
}

# Heatmap
buildHeatmap <- function(matrix, file, ext, color){
	if (ext == "PNG"){
		png(file, width = 1500, height = 1000)
	} else if (ext == "PDF") {
		pdf(file, width = 1500, height = 1000)
	} else if (ext == "SVG") {
		svg(file, width = 15, height = 10)
	} else if (ext == "EPS") {
		setEPS()
		postscript(file)
	}
	print(	
		heatmap.2(
			scale(matrix, center=TRUE, scale=TRUE),
			col=color,
			scale="row", 
			margins=c(10,10), 
			srtCol=45,  
			xlab="Samples", 
			ylab="Genes", 
			#main="Heatmap"
		)
	)
	dev.off()
}
