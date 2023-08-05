
# This file is a generated template, your changes will not be overwritten

jtSearchClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtSearchClass",
    inherit = jtSearchBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            srcTrm <- trimws(self$options$srcTrm)
            if (nzchar(srcTrm) && dim(self$data)[2] >= 1) {
                # assemble the arguments for search_omv and conduct search
                crrArg <- list(dtaInp = self$data, srcTrm = srcTrm, whlTrm = self$options$whlTrm, ignCse = self$options$ignCse,
                               incCmp = self$options$incCmp, incRcd = self$options$incRcd, incID  = self$options$incID,
                               incNom = self$options$incNom, incOrd = self$options$incOrd, incNum = self$options$incNum)

                srcRes <- do.call(jmvReadWrite::search_omv, crrArg)
                if (length(srcRes) > 0) {
                    outRes <- paste0(c(sprintf("Value “%s” (%s) found in...\n", srcTrm, ifelse(crrArg$whlTrm, "exact match", "partial or exact match")),
                                       unlist(sapply(seq_along(srcRes), function(i) splStr(srcRes[i]), simplify = FALSE))), collapse = "\n\n")
                    self$results$srcRes$setContent(outRes)
                } else {
                    self$results$srcRes$setContent(sprintf("Value “%s” (%s) not found", srcTrm, ifelse(crrArg$whlTrm, "exact match", "partial match")))
                }
            } else {
                self$results$srcRes$setContent("")
            }

        }

    )
)
