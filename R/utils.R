hmeDir <- function() {
    Sys.getenv(ifelse(jmvReadWrite:::getOS() == "windows", "USERPROFILE", "HOME"))
}

prpPvw <- function(crrTbl = NULL, dtaFrm = NULL, colFst = c(), nonLtd = FALSE) {
    # maxRow, maxCol and useIdx are defined in globals.R
    seqRow <- seq(crrTbl$rowCount + 1,  min(dim(dtaFrm)[1], ifelse(nonLtd, Inf, maxRow)))
    seqCol <- seq(ifelse(useIdx, 1, 2), min(dim(dtaFrm)[2], ifelse(nonLtd, Inf, maxCol)))
    # determine column names, and if required, put the columns in colFst at the beginning
    colNme <- names(dtaFrm)
    if (length(colFst) > 0) {
        if (!all(colFst == colNme[seq_along(colFst)])) nteFsC(crrTbl = crrTbl, colFst = colFst)
        colNme <- unique(c(colFst, colNme))
    }
    # create a list vector with empty entries (to be assigned when adding a new row), change title for
    # the first column (if useIdx is FALSE) and add further columns and rows to the current table
    valRow <- stats::setNames(as.list(rep("", length(seqCol) + 1)), c("fstCol", colNme[seqCol]))
    if (!useIdx) crrTbl$getColumn(1)$setTitle(colNme[1])
    for (i in seqCol) crrTbl$addColumn(name = colNme[i], title = colNme[i])
    for (i in seqRow) crrTbl$addRow(rowKey = i, values = valRow)
}

rstPvw <- function(crrTbl = NULL) {
    numRow <- crrTbl$rowCount
    colNme <- names(crrTbl$columns)
    crrTbl$deleteRows()
    for (i in seq(numRow)) crrTbl$addRow(rowKey = i, stats::setNames(as.list(rep("", length(colNme))), colNme))
}

fllPvw <- function(crrTbl = NULL, dtaFrm = NULL) {
    # useIdx is defined in globals.R
    dtaRow <- dim(dtaFrm)[1]
    dtaCol <- dim(dtaFrm)[2] + ifelse(useIdx, 1, 0)
    pvwRow <- crrTbl$rowCount
    pvwCol <- length(crrTbl$columns)
    pvwClN <- vapply(crrTbl$columns, "[[", character(1), "title", USE.NAMES = FALSE)
    # add first column with row number (if useIdx), and restrict the data frame to the maximal
    # number of rows and columns (pwvClN[-1] because the row numbers are just added by cbind)
    if (useIdx) {
        dtaFrm <- cbind(data.frame(fstCol = seq(pvwRow)), dtaFrm[seq(pvwRow), pvwClN[-1]])
    } else {
        dtaFrm <-                                         dtaFrm[seq(pvwRow), pvwClN]
    }
    # convert columns for display (factor -> character)
    cnvCol <- vapply(dtaFrm, is.factor, logical(1))
    dtaFrm[, cnvCol] <- sapply(dtaFrm[, cnvCol], as.character)
    if (pvwCol < dtaCol) dtaFrm[, pvwCol] <- "..."
    for (i in seq(pvwRow)) {
        # if useIdx, just use the data frame as it is, for not useIdx, the name of the first
        # column has to be set to fstCol (original name from the table definition in r.yaml)
        if (useIdx) {
            crrRow <- as.list(dtaFrm[i, ])
        } else {
            crrRow <- stats::setNames(as.list(dtaFrm[i, ]), c("fstCol", names(dtaFrm)[-1]))
        }
        crrRow[vapply(crrRow, is.na, logical(1))] <- ""
        if (i == pvwRow && pvwRow < dtaRow) crrRow[-1] <- "..."
        crrTbl$setRow(rowNo = i, crrRow)
        # fmtAdC and fmtAdR are defined in globals.R
        if (i == 1      && pvwCol < dtaCol) crrTbl$addFootnote(pvwCol, sprintf(fmtAdC, dtaCol - pvwCol), rowNo = i)
        if (i == pvwRow && pvwRow < dtaRow) crrTbl$addFootnote(1,      sprintf(fmtAdR, dtaRow - pvwRow), rowNo = i)
    }
}

crtInf <- function(crrInf = NULL, dtaFrm = NULL, hlpMsg = c()) {
    if (!is.null(dtaFrm) > 0) {
        outInf <- c(sprintf(fmtVrI, dim(dtaFrm)[2], dim(dtaFrm)[1], paste0(names(dtaFrm), collapse = ", ")), hlpMsg)
    } else {
        outInf <- hlpMsg
    }
    if (length(outInf) > 0 && all(nchar(outInf) > 0)) {
        crrInf$setContent(paste0("<p>", paste0(outInf, collapse = "</p><p>"), "</p>"))
    }
}

nteFsC <- function(crrTbl = NULL, colFst = c()) {
    # fmtFsC is defined in globals.R
    crrTbl$setNote("Note", sprintf(fmtFsC, ifelse(length(colFst) > 1, "s", ""),
                                           paste0(colFst, collapse = ", "),
                                           ifelse(length(colFst) > 1, "are", "is")))
}

optSnR <- function(crrOpt = NULL) {
    lstSnR <- list(whlTrm = crrOpt$whlTrm,
                   incCmp = crrOpt$incCmp, incRcd = crrOpt$incRcd, incID  = crrOpt$incID,
                   incNom = crrOpt$incNom, incOrd = crrOpt$incOrd, incNum = crrOpt$incNum)
    if (utils::hasName(crrOpt, "ignCse")) lstSnR["ignCse"] <- crrOpt$ignCse
    if (utils::hasName(crrOpt, "varSel") && !is.null(crrOpt$varSel) && length(crrOpt$varSel) > 0) {
       lstSnR[[ifelse(crrOpt$incExc == "include", "varInc", "varExc")]] <- crrOpt$varSel
    }
    return(lstSnR)
}

fmtSrc <- function(fcnNme = "", crrArg = NULL) {
    dflArg <- eval(parse(text = paste0("formals(", fcnNme, ")")))
    for (nmeArg in names(crrArg)) {
        if (identical(crrArg[[nmeArg]], dflArg[[nmeArg]])) crrArg[nmeArg] <- NULL
    }

    gsub("^list\\(", paste0(fcnNme, "(\n    dtaInp = data,"), gsub("=", " = ", jmvcore::sourcify(crrArg)))
}
