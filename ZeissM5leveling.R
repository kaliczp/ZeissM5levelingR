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
    stopifnot(as.numeric(substr(raw.csak[,1],30,31)) == Line.nr)
    ## Is leveling line completed? Test.
    stopifnot(substr(raw.csak[nrow(raw.csak),1],5,12) == "End-Line")
    ## Remove first row with line start parameters
    ## and last row
    raw.csak <- raw.csak[-c(1,nrow(raw.csak)),]
    ## Test unit and code
    stopifnot(substr(raw.csak[1,"V6"],1,1) == "Z",
              substr(raw.csak[1,"V6"],19,22) == "m   ")
    ## Start point height
    Height.at.start <- as.numeric(substr(raw.csak[1,"V6"],4,17))
    ## End point height
    Height.at.end <- as.numeric(substr(raw.csak[nrow(raw.csak)-1,"V6"],4,17))
    ## Calculated end point height
    Height.at.end.calc <- as.numeric(substr(raw.csak[nrow(raw.csak),"V6"],4,17))
    ## Calculated forward differences
    Diff.forward <- as.numeric(substr(raw.csak[nrow(raw.csak),"V5"],4,17))
    ## Calculated backward differences
    Diff.backward <- as.numeric(substr(raw.csak[nrow(raw.csak),"V4"],4,17))
    ## Calculated error
    Error.calc <- as.numeric(substr(raw.csak[nrow(raw.csak)-1,"V5"],4,17))
    ## Summarize data for output
    Instr.table <- data.frame(Height.at.start, Height.at.end,
                              Height.at.end.calc, Error.calc)
    Instr.sum <- data.frame(Diff.forward, Diff.backward)
    ## Remove start and end rows
    raw.csak <- raw.csak[-c(1,nrow(raw.csak)-1,nrow(raw.csak)),]
    ## Rownumbers of text objects (TO)
    Text.row.numbers <- grep("TO",raw.csak[,1])
    ## Save text objects
    Text.objects <- raw.csak[Text.row.numbers,]
    ## Remove text objects from data base
    raw.csak <- raw.csak[-Text.row.numbers,]
    ## Sustract differences from plan of sight
    Diff <-  as.numeric(substr(raw.csak[,2],12,17))
    ## Collect fore-, back-, inermediatesight codes
    D.code  <- substr(raw.csak[,2],1,2)
    ## Set the right sign for fore-, and intermediate sights
    Diff[D.code == "Rf"] <- Diff[D.code == "Rf"] * -1
    Diff[D.code == "Rz"] <- Diff[D.code == "Rz"] * -1
    ## Intrument and calculated difference comparison
    Intsr.calc.diff <- abs(sum(Diff[!D.code == "Rz"])) - Error.calc
    ## Warning if error larger than 2 mm.
    if(Intsr.calc.diff > 0.002) {
        warning(paste("Check the difference between instrument and R", Intsr.calc.diff, "m"))
    }
    ## Compile dataframe based on instrument measurements
    Raw.df <- data.frame(
        Pnr = as.numeric(substr(raw.csak[,1],5,12)),
        D.code = D.code,
        Diff = Diff,
        HD = as.numeric(substr(raw.csak[,3],12,17)),
        sR = as.numeric(substr(raw.csak[,4],12,17))
    )
    ## Calculate height for individual points
    ## Check individual points
    Nr.points <- c(grep("Rf",Raw.df[,"D.code"]), grep("Rz",Raw.df[,"D.code"]))
    ## Based on the length create data.frame for result
    Length.result <- length(Nr.points)
    Result <- data.frame(Pnr=integer(Length.result), Height=numeric(Length.result))
    ## Fill data.frame with point nr and height
    for(tti in 1:Length.result) {
        Result[tti, "Pnr"] <- Raw.df[Nr.points[tti], "Pnr"]
        Raw.only <- Raw.df[1:Nr.points[tti],]
        ## If there is some inermediate points exclude them
        if(any(Raw.only[, "D.code"] == "Rz")) {
            Diffs.with.inter <- Raw.only[-nrow(Raw.only), "Diff"]
            Interm.line <- Raw.only[-nrow(Raw.only),"D.code"] == "Rz"
            Diffs.without.inter <- Diffs.with.inter[!Interm.line]
            Diffs.only  <- c(Diffs.without.inter, Raw.df[Nr.points[tti], "Diff"])
        } else {
            Diffs.only <- Raw.only[, "Diff"]
        }
        ## Calculate height of the actual point
        Result[tti, "Height"] <- sum(c(Height.at.start, Diffs.only))
    }
    ## Compile list of results
    list(Result = Result, Raw.Table = Raw.df,
         Instr.table = Instr.table, Instr.sum = Instr.sum)
}
