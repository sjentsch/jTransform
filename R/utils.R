hmeDir <- function() {
    Sys.getenv(ifelse(jmvReadWrite:::getOS() == "windows", "USERPROFILE", "HOME"))
}

splStr <- function(strVec = NULL, strClp = ", ", maxLng = 80, lstInd = 2) {
    blnLst <- is.list(strVec)
    if (blnLst) strVec <- c(names(strVec), unlist(strVec, use.names = FALSE))
    strLng <- c(unname(sapply(strVec, nchar)) + nchar(strClp))
    strPos <- 0
    strOut <- c()
    while (max(strPos) < length(strVec)) {
        strPos <- c(max(strPos), max(which(cumsum(strLng[(max(strPos) + 1):length(strVec)]) <= maxLng - ifelse(blnLst && strPos[1] > 0, lstInd, 0))) + max(strPos))
        strOut <- c(strOut, paste0(ifelse(blnLst && strPos[1] > 0, rep(" ", lstInd), ""), paste0(strVec[seq(strPos[1] + 1, min(strPos[2], length(strVec)))], collapse = strClp)))
    }
    if (blnLst) strOut <- paste(c(sub(strClp, ": ", strOut[1]), strOut[-1]), collapse = "\n")
    strOut
}

crtPvw <- function(dtaFrm = NULL, varFst = c()) {
    if (length(varFst) > 0) {
        varOrd <- c(varFst, setdiff(names(dtaFrm), varFst))[seq(min(dim(dtaFrm)[2], 8))]
        varTxt <- sprintf("Variable%s %s %s", ifelse(length(varFst) > 1, "s", ""), paste0(varFst, collapse = ", "), ifelse(length(varFst) > 1, "are", "is"))
    } else {
        varOrd <- names(dtaFrm)[seq(min(dim(dtaFrm)[2], 8))]
    }
    sprintf("\nVariables in the output:\n%s\n\n\n%s\n\n(%smax. 10 rows and %s variables are shown)\n",
      paste(splStr(names(dtaFrm), maxLng = 81), collapse = ",\n"),
      paste(capture.output(print(dtaFrm[seq(min(10, dim(dtaFrm)[1])), varOrd], row.names = FALSE)), collapse="\n"),
      ifelse(length(varFst) > 0, sprintf("%s shown first in this preview,\n in the created data set the order is as in the variable list above,\n ", varTxt), ""),
      ifelse(length(varOrd) < dim(dtaFrm)[2], sprintf("max. %d", length(varOrd)), "all"))
}
