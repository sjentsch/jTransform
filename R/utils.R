isWin  <- function() {
    .Platform$OS.type != "unix"
}

hmeDir <- function() {
    Sys.getenv(ifelse(isWin, "USERPROFILE", "HOME"))
}

jmvEXE <- function() {
    file.path(Sys.getenv("JAMOVI_HOME"), "bin", ifelse(isWin, "jamovi.exe", "jamovi"))
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
