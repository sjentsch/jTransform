jtArrangeColsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtArrangeColsClass",
    inherit = jtArrangeColsBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            if (length(self$options$varOrd) > 1) {
                # assemble the arguments for arrange_cols_omv
                crrArg <- list(dtaInp = self$data, fleOut = NULL, varOrd = self$options$varOrd)

                # if CREATE was pressed (btnOut == TRUE), open a new jamovi session with the data
                if (self$options$btnOut) {
                    do.call(jmvReadWrite::arrange_cols_omv, crrArg[-2])
                    self$results$txtPvw$setContent(self$results$txtPvw)
                # if not, create a preview of the data (crtPvw in utils.R)
                } else {
                    self$results$txtPvw$setContent(crtPvw(do.call(jmvReadWrite::arrange_cols_omv, crrArg)))
                }
            } else {
                self$results$txtPvw$setContent("")
            }

        }

    )
)
