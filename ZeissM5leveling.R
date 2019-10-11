read.ZEISSM5leveling <- function(file) {
    ## Read in fixed-with file with standard strings length
    raw <- read.fwf(file = file, widths = c(7,10,32,23,23,23))
    ## Clear empty rows
    raw.csak <- raw[!is.na(raw[,1]),]
    ## File closing END is available
    if(raw.csak[nrow(raw.csak),1] == "END    ") {
        ## Clear file closing END
        raw.csak <- raw.csak[-nrow(raw.csak),]
    } else {
        warning("File not terminated correctly with END!")
    }
    ## All row in first colum M5 format descriptor?
    stopifnot(raw.csak[,1] == "For M5|")
    ## Delete M5 format strings
    raw.csak <- raw.csak[,-1]
    ## Adr column contains only memory row numbers?
    stopifnot(substr(raw.csak[,1],1,3) == "Adr")
    row.names(raw.csak) <- as.numeric(substr(raw.csak[,1],5,9))
    ## Rowumbers of rows with measured data
    meres.row <- grep("KD1",raw.csak[,3])
    csak.meas <- raw.csak[meres.row,]
    data.frame(code = substr(csak.meas[,4],1,2), diff = as.numeric(substr(csak.meas[,4],12,17)))
}
