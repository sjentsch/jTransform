jtTransposeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtTransposeClass",
    inherit = jtTransposeBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            if (length(self$options$varOth) > 1) {
                # assemble the arguments for transpose_omv
                crrArg <- list(dtaInp = self$data, fleOut = NULL, varOth = self$options$varOth,
                               varNme = ifelse(is.null(self$options$varNme), "", self$options$varNme))

                # if CREATE was pressed (btnOut == TRUE), open a new jamovi session with the data
                if (self$options$btnOut) {
                    do.call(jmvReadWrite::transpose_omv, crrArg[-2])
                    self$results$txtPvw$setContent(self$results$txtPvw)
                # if not, create a preview of the data (crtPvw in utils.R)
                } else {
                    self$results$txtPvw$setContent(oldPvw(do.call(jmvReadWrite::transpose_omv, crrArg)))
                }
            } else {
                self$results$txtPvw$setContent("")
            }

        }

    )
)
