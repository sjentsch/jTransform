jtSortClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtSortClass",
    inherit = jtSortBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            if (length(self$options$varSrt) > 0 && dim(self$data)[2] > 1) {
                # assemble the arguments for sort_omv
                crrArg <- list(dtaInp = self$data, fleOut = NULL, varSrt = paste0(gsub("descend", "-", gsub("ascend", "",
                            sapply(self$options$ordSrt, "[[", "order"))), sapply(self$options$ordSrt, "[[", "var")))

                # if CREATE was pressed (btnOut == TRUE), open a new jamovi session with the data
                if (self$options$btnOut) {
                    do.call(jmvReadWrite::sort_omv, crrArg[-2])
                    self$results$txtPvw$setContent(self$results$txtPvw)
                # if not, create a preview of the data (crtPvw in utils.R)
                } else {
                    self$results$txtPvw$setContent(crtPvw(do.call(jmvReadWrite::sort_omv, crrArg), varFst = crrArg$varSrt))
                }
            } else {
                self$results$txtPvw$setContent("")
            }

        }
        
    )
)
