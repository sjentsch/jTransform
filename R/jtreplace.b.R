#' @importFrom jmvcore .
jtReplaceClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtReplaceClass",
    inherit = jtReplaceBase,
    private = list(
        .crrCmd = "jmvReadWrite::replace_omv",
        .crrDta = NULL,
        .nonLtd = FALSE,

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,

        # marking differences is not included in the common function
        .run = function() {
            # assemble or reset data set / create information
            private$.dtaInf()
            if (private$.chkVar() && dim(self$data)[1] >= 1) {
                # if “Create” was pressed (btnCrt == TRUE), open a new jamovi session with the data
                if ("btnCrt" %in% names(self$options) && self$options$btnCrt) {
                    do.call(eval(parse(text = private$.crrCmd)), private$.crrArg())
                # if not, create a preview of the data (fllPvw in utils.R) and mark replaced values
                } else {
                    fllPvw(crrTbl = self$results$pvwDta, dtaFrm = private$.crrDta, nteRnC = private$.nteRnC())
                    private$.mrkDff(crrTbl = self$results$pvwDta, dtaNew = private$.crrDta,
                                    dtaOld = if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset())
                }
            }
        },

        .chkDff = function(dtaOld = NULL, dtaNew = NULL) {
             (any(is.na(dtaOld) != is.na(dtaNew)) || any(dtaOld[!is.na(dtaOld)] != dtaNew[!is.na(dtaNew)]))
        },

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkVar = function() {
            (length(self$options$rplTrm) > 0 &&
             all(vapply(self$options$rplTrm, function(x) !is.null(x[[1]]) && nzchar(x[[1]]), logical(1))))
        },

        .colFst = commonFunc$private_methods$.colFst,

        .crrArg = function() {
            rplLst <- lapply(self$options$rplTrm, function(x) {
                                                      x[vapply(x, is.null, logical(1))] <- ""
                                                      c(x[[1]], x[[2]])
                                                  })
            c(list(dtaInp = if (!is.null(self$data) && dim(self$data)[1] > 0) self$data else self$readDataset(),
                   rplLst = rplLst), optSnR(self$options))
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .nteRnC = commonFunc$private_methods$.nteRnC,

        .mrkDff = function(crrTbl = NULL, dtaNew = NULL, dtaOld = NULL) {
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

        asSource = commonFunc$public_methods$asSource

    )
)
