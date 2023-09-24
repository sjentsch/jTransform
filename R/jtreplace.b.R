
# This file is a generated template, your changes will not be overwritten

jtReplaceClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtReplaceClass",
    inherit = jtReplaceBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            rplTrm <- self$options$rplTrm
            if (length(rplTrm) > 0 && nzchar(rplTrm) && dim(self$data)[2] >= 1) {
                # assemble the arguments for replace_omv
                rplLst <- sapply(sapply(sapply(strsplit(rplTrm, ";|\n")[[1]], trimws, simplify = FALSE, USE.NAMES = FALSE), strsplit, ",|="), trimws, simplify = FALSE)
                rplLst <- rplLst[sapply(rplLst, length) > 0]
                if (any(sapply(rplLst, length) != 2)) {
                    if (length(rplLst[[1]]) != 2) self$results$txtPvw$setContent("Original and replacement values must come in pairs, separated by a comma.")
                    return()
                }
                crrArg <- list(dtaInp = self$data, fleOut = NULL, rplLst = rplLst, whlTrm = self$options$whlTrm,
                               incCmp = self$options$incCmp, incRcd = self$options$incRcd, incID  = self$options$incID,
                               incNom = self$options$incNom, incOrd = self$options$incOrd, incNum = self$options$incNum)
                varSel <- self$options$varSel
                if (!is.null(varSel) && length(varSel) > 0) {
                    if (self$options$incExc == "include") crrArg <- c(crrArg, list(varInc = varSel)) else crrArg <- c(crrArg, list(varExc = varSel))
                }
                # if CREATE was pressed (btnOut == TRUE), open a new jamovi session with the data
                if (self$options$btnOut) {
                      do.call(jmvReadWrite::replace_omv, crrArg[-2])
                      return(TRUE)
                # if not, create a preview of the data (crtPvw in utils.R)
                } else {
                    srcTrm <- list(srcTrm = paste0(rep("^", crrArg$whlTrm), paste(sapply(rplLst, "[[", 1), collapse = ifelse(crrArg$whlTrm, "$|^", "|")), rep("$", crrArg$whlTrm)))
                    varFst <- names(do.call(jmvReadWrite::search_omv, c(srcTrm, crrArg[setdiff(names(crrArg), c("rplLst", "varInc", "varExc"))])))
                    self$results$txtPvw$setContent(oldPvw(do.call(jmvReadWrite::replace_omv, crrArg), varFst = varFst))
                }
            } else {
                self$results$txtPvw$setContent("")
            }

        }

    )
)
