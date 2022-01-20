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
