# Set default repo from CRAN
options(repos=structure(c(CRAN="https://cran.rstudio.com/")))
# Update installed packages
update.packages(ask=FALSE, checkBuilt=TRUE)
# Install some packages
install.packages(c('shiny', 'shinydashboard', 'shinyjs', 'shinyBS', 'shinyWidgets', 'markdown', 'DT', 'data.table', 'pheatmap', 'ggplot2', 'Hmisc', 'reshape', 'rlist', 'dplyr', 'plyr'))
