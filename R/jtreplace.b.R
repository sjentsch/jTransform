#' @importFrom jmvcore .
jtReplaceClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtReplaceClass",
    inherit = jtReplaceBase,
    private = list(

        .init = function() {
            if (private$.chkVar()) {
                # resize / prepare the output table (prpPvw in utils.R)
                if (!is.null(self$data) && dim(self$data)[1] > 0) dtaFrm <- self$data else dtaFrm <- self$readDataset()
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = dtaFrm)
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # check whether rplTrm has the correct format and whether the data set has at least one row
            if (private$.chkVar() && dim(self$data)[1] >= 1) {
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if (self$options$btnCrt) {
                      do.call(jmvReadWrite::replace_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use “Create” as general information
                # and create a preview of the data (crtInf and fllPvw in utils.R)
                } else {
                    rplDta <- do.call(jmvReadWrite::replace_omv, private$.crrArg())
                    crtInf(crrInf = self$results$genInf, infMsg = private$.crtMsg())
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = rplDta)
                    private$.mrkDff(crrTbl = self$results$pvwDta, dtaOld = self$data, dtaNew = rplDta)
                }
            } else {
                # show getting started as general information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, infMsg = hlpRpl)
            }
        },

        .chkDff = function(dtaOld = NULL, dtaNew = NULL) {
             (any(is.na(dtaOld) != is.na(dtaNew)) || any(dtaOld[!is.na(dtaOld)] != dtaNew[!is.na(dtaNew)]))
        },

        .chkVar = function() {
            (length(self$options$rplTrm) > 0 && all(vapply(self$options$rplTrm, function(x) !is.null(x[[1]]) && nzchar(x[[1]]), logical(1))))
        },

        .crrArg = function() {
            rplLst <- lapply(self$options$rplTrm, function(x) {
                                                      x[vapply(x, is.null, logical(1))] <- ""
                                                      c(x[[1]], x[[2]])
                                                  })
            c(list(dtaInp = self$data, fleOut = NULL, rplLst = rplLst), optSnR(self$options))
        },

        .crtMsg = function() {
            crtMsg <- sprintf("%s <strong>%s</strong> %s", .("Pressing the"),
                              .("\"Create\"-button opens the modified data set"), .(" in a new jamovi window."))
            if (!is.null(private$.crrDta)) {
                c(sprintf("<strong>%s</strong> (%d %s in %d %s): %s", .("Variables in the Output Data Set"),
                    dim(private$.crrDta)[2], .("variables"), dim(private$.crrDta)[1], .("rows"),
                    paste0(names(private$.crrDta), collapse = ", ")),
                  crtMsg)
            } else {
                crtMsg
            }
        },

        .mrkDff = function(crrTbl = NULL, dtaOld = NULL, dtaNew = NULL) {
            selFac <- vapply(dtaOld, is.factor, logical(1))
            if (any(selFac)) {
                dtaOld[, selFac] <- as.data.frame(vapply(dtaOld[, selFac], as.character, character(1)))
                dtaNew[, selFac] <- as.data.frame(vapply(dtaNew[, selFac], as.character, character(1)))
            }
            selRow <- seq(ifelse(dim(dtaOld)[1] > maxRow, maxRow - 1, dim(dtaOld)[1]))
            selCol <- seq(ifelse(dim(dtaOld)[2] > maxCol, maxCol - 1, dim(dtaOld)[2]))
            if        (private$.chkDff(dtaOld[selRow, selCol], dtaNew[selRow, selCol])) {
                crrTbl$setNote("diff", .("+ Value was replaced / modified."))
                for (i in selCol) {
                    for (j in selRow) {
                        if (private$.chkDff(dtaOld[j, i], dtaNew[j, i])) {
                            crrTbl$addSymbol(rowNo = j, ifelse(!useIdx && i == 1, "fstCol", names(dtaOld)[i]), "+")
                        }
                    }
                }
            } else if (private$.chkDff(dtaOld,                 dtaNew)) {
                crrTbl$setNote("diff", .("Replacements were made, but they are outside the scope (rows / columns) of this preview."))
            } else {
                crrTbl$setNote("diff", .("There were no replacements made (in the whole dataset)."))
            }
        }

    ),

    public = list(

        asSource = function() {
            if (private$.chkVar()) fmtSrc("jmvReadWrite::replace_omv", private$.crrArg()[-seq(2)])
        }

    )
)
