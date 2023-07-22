isWin  <- function() {
    .Platform$OS.type != "unix"
}

hmeDir <- function() {
    Sys.getenv(ifelse(isWin(), "USERPROFILE", "HOME"))
}

jmvEXE <- function() {
    file.path(Sys.getenv("JAMOVI_HOME"), "bin", ifelse(isWin(), "jamovi.exe", "jamovi"))
}

splStr <- function(strVec = c(), strClp = ", ", maxLng = 80) {
    strLng <- unname(sapply(strVec, nchar)) + nchar(strClp)
    strOut <- c()
    strPos <- 0
    while (max(strPos) < length(strVec)) {
        strPos <- c(max(strPos), max(which(cumsum(strLng[(max(strPos) + 1):length(strVec)]) <= maxLng)) + max(strPos))
        strOut <- c(strOut, paste0(strVec[seq(strPos[1] + 1, min(strPos[2], length(strVec)))], collapse = strClp))
    }
    strOut
}

crtPvw <- function(dtaFrm = NULL) {
    sprintf("\nVariables in the output:\n%s\n\n\n%s\n\n(max. 10 rows and max. 8 variables are shown)\n",
      paste(splStr(names(dtaFrm), maxLng = 81), collapse = ",\n"),
      paste(capture.output(print(dtaFrm[seq(min(10, dim(dtaFrm)[1])), seq(min(8, dim(dtaFrm)[2]))], row.names = FALSE)), collapse="\n"))
}
