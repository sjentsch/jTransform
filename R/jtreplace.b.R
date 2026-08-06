#' @importFrom jmvcore .
jtReplaceClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "jtReplaceClass",
    inherit = jtReplaceBase,
    private = list(
        .crrCmd = "jmvReadWrite::replace_omv",
        .nonLtd = FALSE,
        .sfxTtl = "rplc",
        .xfmCol = c(),
        .xfmDta = NULL,
        .xfmFst = FALSE,
        .xfmRow = NA,

        # common functions are in incFnc.R
        .init = commonFunc$private_methods$.init,
        .run  = commonFunc$private_methods$.run,

        # marking differences is not included in the common function
        .chkDff = function(dtaOld = NULL, dtaNew = NULL) {
             (any(is.na(dtaOld) !=  is.na(dtaNew)) || any(dtaOld[!is.na(dtaOld)] !=  dtaNew[!is.na(dtaNew)]))
        },

        .chkDtF = commonFunc$private_methods$.chkDtF,

        .chkVar = function() {
            (length(self$options$rplTrm) > 0 &&
             all(vapply(self$options$rplTrm, function(x) !is.null(x[[1]]) && nzchar(x[[1]]), logical(1))))
        },

        .colFst = commonFunc$private_methods$.colFst,

        .crrArg = function(getDta = TRUE) {
            rplLst <- lapply(self$options$rplTrm, function(x) {
                                                      x[vapply(x, is.null, logical(1))] <- ""
                                                      c(x[[1]], x[[2]])
                                                  })
            c(if (getDta) private$.getDta(), list(rplLst = rplLst), optSnR(self$options))
        },

        .crtMsg = commonFunc$private_methods$.crtMsg,
        .dtaInf = commonFunc$private_methods$.dtaInf,
        .dtaMsg = commonFunc$private_methods$.dtaMsg,
        .getDta = commonFunc$private_methods$.getDta,
        .nteRnC = commonFunc$private_methods$.nteRnC,

        .mrkDff = function(crrTbl = NULL, dtaNew = NULL, dtaOld = NULL) {
            selFac <- vapply(dtaOld, is.factor, logical(1))
            if (any(selFac)) {
                dtaOld[, selFac] <- as.data.frame(vapply(dtaOld[, selFac, drop = FALSE], as.character, character(nrow(dtaOld))))
                dtaNew[, selFac] <- as.data.frame(vapply(dtaNew[, selFac, drop = FALSE], as.character, character(nrow(dtaNew))))
            }
            selRow <- seq(ifelse(nrow(dtaOld) > maxRow, maxRow - 1, nrow(dtaOld)))
            selCol <- seq(ifelse(ncol(dtaOld) > maxCol, maxCol - 1, ncol(dtaOld)))
            if        (private$.chkDff(dtaOld[selRow, selCol], dtaNew[selRow, selCol])) {
                crrTbl$setNote("diff", .("+ Value was replaced / modified."))
                for (i in selCol) {
                    for (j in selRow) {
                        if (private$.chkDff(dtaOld[j, i], dtaNew[j, i])) {
                            crrTbl$addSymbol(rowNo = j, ifelse(!useIdx && i ==  1, "fstCol", names(dtaOld)[i]), "+")
                        }
                    }
                }
            } else if (private$.chkDff(dtaOld,                 dtaNew)) {
                crrTbl$setNote("diff", .("Replacements were made, but they are outside the scope (rows / columns) of this preview."))
            } else {
                crrTbl$setNote("diff", .("There were no replacements made (in the whole dataset)."))
            }
        },

        .runXfm = commonFunc$private_methods$.runXfm

    ),

    public = list(

        asSource = commonFunc$public_methods$asSource

    )
)
