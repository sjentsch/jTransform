
# This file is a generated template, your changes will not be overwritten

jtReplaceClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtReplaceClass",
    inherit = jtReplaceBase,
    private = list(

        .init = function() {
            if (private$.chkVar()) {
                # resize / prepare the output table (prpPvw in utils.R)
                prpPvw(crrTbl = self$results$pvwDta, dtaFrm = self$readDataset())
            } else {
                # reset the output table (rstPvw in utils.R)
                rstPvw(crrTbl = self$results$pvwDta)
            }
        },

        .run = function() {
            # check whether rplTrm has the correct format and whether the data set has at least one row
            if (private$.chkVar() && dim(self$data)[2] >= 1) {
                # if CREATE was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if (self$options$btnCrt) {
                      do.call(jmvReadWrite::replace_omv, private$.crrArg()[-2])
                # if not, show the variable list and how to use CREATE as general information
                # and create a preview of the data (crtInf and fllPvw in utils.R)
                } else {
                    rplDta <- do.call(jmvReadWrite::replace_omv, private$.crrArg())
                    crtInf(crrInf = self$results$genInf, dtaFrm = rplDta, hlpMsg = hlpCrt)
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = rplDta)
                    private$.mrkDff(crrTbl = self$results$pvwDta, dtaOld = self$data, dtaNew = rplDta)
                }
            } else {
                # show getting started as general information (crtInf in utils.R)
                crtInf(crrInf = self$results$genInf, hlpMsg = hlpRpl)
            }
        },

        .chkDff = function(dtaOld = NULL, dtaNew = NULL) {
             (any(is.na(dtaOld) != is.na(dtaNew)) || any(is.null(dtaOld) != is.null(dtaNew)) || any(dtaOld != dtaNew))
        },
        
        .chkVar = function() {
            (length(self$options$rplOnN) > 0 && all(sapply(self$options$rplOnN, function(x) !any(sapply(x, is.null)) && nzchar(x[1]))))
        },

        .crrArg = function() {
            c(list(dtaInp = self$data, fleOut = NULL, rplLst = sapply(self$options$rplOnN, unlist, use.names = FALSE, simplify = FALSE)), optSnR(self$options))
        },

        .mrkDff = function(crrTbl = NULL, dtaOld = NULL, dtaNew = NULL) {
            selRow <- seq(ifelse(dim(dtaOld)[1] > maxRow, maxRow - 1, dim(dtaOld)[1]))
            selCol <- seq(ifelse(dim(dtaOld)[2] > maxCol, maxCol - 1, dim(dtaOld)[2]))
            if        (private$.chkDff(dtaOld[selRow, selCol], dtaNew[selRow, selCol])) {
                crrTbl$setNote('diff', '+ Value was replaced / modified.')
                for (i in selCol) {
                    for (j in selRow) {
                        if (private$.chkDff(dtaOld[j, i], dtaNew[j, i])) {
                            crrTbl$addSymbol(rowNo = j, ifelse(!useIdx && i == 1, "fstCol", names(dtaOld)[i]), '+')
                        }
                    }
                }
            } else if (private$.chkDff(dtaOld,                 dtaNew)) {
                crrTbl$setNote('diff', 'Replacements were made, but they are outside the scope (rows / columns) of this preview.')
            } else {
                crrTbl$setNote('diff', 'There were no replacements made (in the whole dataset).')
            }
        }

    )
)
