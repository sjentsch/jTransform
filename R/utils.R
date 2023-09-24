hmeDir <- function() {
    Sys.getenv(ifelse(jmvReadWrite:::getOS() == "windows", "USERPROFILE", "HOME"))
}

splStr <- function(strVec = NULL, strClp = ", ", maxLng = 80, lstInd = 2) {
    blnLst <- is.list(strVec)
    maxLng <- maxLng + nchar(trimws(strClp))
    if (blnLst) strVec <- c(names(strVec), unlist(strVec, use.names = FALSE))
    strLng <- c(unname(sapply(strVec, nchar)) + nchar(strClp))
    strPos <- 0
    strOut <- c()
    while (max(strPos) < length(strVec)) {
        strPos <- c(max(strPos), max(which(cumsum(strLng[(max(strPos) + 1):length(strVec)]) <= maxLng - ifelse(blnLst && strPos[1] > 0, lstInd, 0))) + max(strPos))
        strOut <- c(strOut, paste0(ifelse(blnLst && strPos[1] > 0, rep(" ", lstInd), ""), trimws(paste0(strVec[seq(strPos[1] + 1, min(strPos[2], length(strVec)))], collapse = strClp))))
    }
    if (blnLst) strOut <- paste(c(sub(strClp, ": ", strOut[1]), strOut[-1]), collapse = "\n")
    strOut
}

oldPvw <- function(dtaFrm = NULL, varFst = c(), varMax = 8) {
    if (length(varFst) > 0) {
        varOrd <- c(varFst, setdiff(names(dtaFrm), varFst))[seq(min(dim(dtaFrm)[2], varMax))]
        varTxt <- sprintf("Variable%s %s %s", ifelse(length(varFst) > 1, "s", ""), paste0(varOrd, collapse = ", "), ifelse(length(varFst) > 1, "are", "is"))
    } else {
        varOrd <- names(dtaFrm)[seq(min(dim(dtaFrm)[2], varMax))]
    }
    sprintf("\nVariables in the output:\n%s\n\n\n%s\n\n(%smax. 10 rows and %s variables are shown)\n",
      paste(splStr(names(dtaFrm)), collapse = ",\n"),
      paste(capture.output(print(dtaFrm[seq(min(10, dim(dtaFrm)[1])), varOrd], row.names = FALSE)), collapse="\n"),
      ifelse(length(varFst) > 0, sprintf("%s shown first in this preview,\n in the created data set the order is as in the variable list above,\n ", varTxt), ""),
      ifelse(length(varOrd) < dim(dtaFrm)[2], sprintf("max. %d", length(varOrd)), "all"))
}

crtPvw <- function(crrTbl = NULL, dtaFrm = NULL) {
    dtaTbl <- crrTbl$state
print(str(dtaTbl))
print(str(dtaFrm))
    if (is.null(dtaTbl)) {
        attr(dtaFrm, "notes") <- c()
        crrRow <- dim(dtaFrm)[1]
        maxRow <- min(crrRow, 20)
        dtaFrm <- cbind(data.frame(rowNme = seq(crrRow)), dtaFrm)
        crrCol <- dim(dtaFrm)[2]
        maxCol <- min(crrCol, 10)
        dtaFrm <- dtaFrm[seq(maxRow), seq(maxCol)]
print(str(dtaFrm))
        if (crrRow > maxRow) {
            attr(dtaFrm, "notes") <- c(attr(dtaFrm, "notes"), sprintf("There are %d more rows in the dataset not shown here.",   crrRow - maxRow))
            dtaFrm[maxRow, ] <- "..."
        }
        if (crrCol > maxCol) {
            attr(dtaFrm, "notes") <- c(attr(dtaFrm, "notes"), sprintf("There are %d more colums in the dataset not shown here.", crrCol - maxCol))
            dtaFrm[, maxCol] <- "..."
        }
        crrTbl$setState(dtaFrm)
    }
}

rszTbl <- function(crrTbl = NULL, tgtRow = NA, tgtCol = NA) {
    
}
