
# This file is a generated template, your changes will not be overwritten

jtReplaceClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtReplaceClass",
    inherit = jtReplaceBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            rplTrm <- self$options$rplTrm
            if (length(rplTrm) > 0 && nzchar(rplTrm)) {
                # assemble the arguments for replace_omv
                rplLst <- sapply(sapply(sapply(strsplit(rplTrm, ";|\n")[[1]], trimws, simplify = FALSE, USE.NAMES = FALSE), strsplit, ",|="), trimws, simplify = FALSE)
                crrArg <- list(dtaInp = self$data, fleOut = NULL, rplLst = rplLst, whlTrm = self$options$whlTrm,
                               incCmp = self$options$incCmp, incRcd = self$options$incRcd, incID  = self$options$incID,
                               incNom = self$options$incNom, incOrd = self$options$incOrd, incNum = self$options$incNum)
                # if CREATE was pressed (btnOut == TRUE), open a new jamovi session with the data
                if (self$options$btnOut) {
                      do.call(jmvReadWrite::replace_omv, crrArg[-2])
                      return(TRUE)
                # if not, create a preview of the data (crtPvw in utils.R)
                } else {
                    srcTrm <- list(srcTrm = paste0(rep("^", crrArg$whlTrm), paste(sapply(rplLst, "[[", 1), collapse = ifelse(crrArg$whlTrm, "$|^", "|")), rep("$", crrArg$whlTrm)))
                    varFst <- names(do.call(jmvReadWrite::search_omv, c(srcTrm, crrArg[-3])))
                    self$results$txtPvw$setContent(crtPvw(do.call(jmvReadWrite::replace_omv, crrArg), varFst = varFst))
                }
            } else {
                self$results$txtPvw$setContent("")
            }

        }

    )
)
