#@author G. Le Corguille
# It allow different of field separators
getDataFrameFromFile <- function(filename, header=T) {
    myDataFrame <- read.csv(filename, header=header, sep=";", stringsAsFactors=F)
    if (ncol(myDataFrame) < 2) myDataFrame <- read.csv(filename, header=header, sep="\t", stringsAsFactors=F)
    if (ncol(myDataFrame) < 2) myDataFrame <- read.csv(filename, header=header, sep=",", stringsAsFactors=F)
    if (ncol(myDataFrame) < 2) {
        error_message="Your tabular file seems not well formatted. The column separators accepted are ; , and tabulation"
        print(error_message)
        stop(error_message)
    }
    return(myDataFrame)
}