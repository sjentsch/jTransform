jtLong2WideClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtLong2WideClass",
    inherit = jtLong2WideBase,
    private = list(

        .run = function() {

            # check whether all required variables are present
            if (length(self$options$varID) > 0 && length(self$options$varTme) > 0 && length(self$options$varTgt) > 0 && nzchar(self$options$varSep)) {
                # assemble the arguments for long2wide_omv
                crrArg <- list(dtaInp = self$data, fleOut = NULL, varID  = self$options$varID,  varTme = self$options$varTme, varTgt = self$options$varTgt,
                               varExc = self$options$varExc, varSep = self$options$varSep, varOrd = self$options$varOrd, varAgg = self$options$varAgg)

                # if CREATE was pressed (btnOut == TRUE), open a new jamovi session with the data
                if (self$options$btnOut) {
                    do.call(jmvReadWrite::long2wide_omv, crrArg[-2])
                    self$results$txtPvw$setContent(self$results$txtPvw)
                # if not, create a preview of the data (crtPvw in utils.R)
                } else {
                    self$results$txtPvw$setContent(oldPvw(do.call(jmvReadWrite::long2wide_omv, crrArg)))
                }
            } else {
                self$results$txtPvw$setContent("")
            }

        }

    )
)
