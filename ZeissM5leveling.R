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
    ## Delete Adr column
    raw.csak <- raw.csak[,-1]
    ## Is it a levelling line? Used only backward forward only? Tests.
    stopifnot(substr(raw.csak[1,1],5,14) == "Start-Line",
              substr(raw.csak[1,1],24,25) == "BF")
    ## Store line number
    Line.nr <- as.numeric(substr(raw.csak[1,1],30,31))
    ## Identical line number, if not separate different
    stopifnot(as.numeric(substr(raw.csak[,1],30,31)) == 2)
    ## Remove first row with line start parameters
    raw.csak <- raw.csak[-1,]
    ## Test unit and code
    stopifnot(substr(raw.csak[1,"V6"],1,1) == "Z",
              substr(raw.csak[1,"V6"],19,22) == "m   ")
    ## Start point height
    Height.at.start <- as.numeric(substr(raw.csak[1,"V6"],4,17))
    ## End point height
    Height.at.end <- as.numeric(substr(raw.csak[nrow(raw.csak)-2,"V6"],4,17))
    ## Calculated end point height
    Height.at.end.calc <- as.numeric(substr(raw.csak[nrow(raw.csak)-1,"V6"],4,17))
    ## Rowumbers of rows with measured data
    meres.row <- grep("KD1",raw.csak[,1])
    csak.meas <- raw.csak[meres.row,]
    data.frame(code = substr(csak.meas[,2],1,2), diff = as.numeric(substr(csak.meas[,2],12,17)))
}
